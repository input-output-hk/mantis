# This file originates from SBTix
{ runCommand, fetchurl, lib, stdenv, jdk, jre, sbt, writeText, makeWrapper, gawk }:
with stdenv.lib;

let sbtTemplate = repoDefs: versioning:
    let
        buildSbt = writeText "build.sbt" ''
          scalaVersion := "${versioning.scalaVersion}"
        '';

        mainScala = writeText "Main.scala" ''
          object Main extends App {
            println("hello nix")
          }
        '';

        buildProperties = writeText "build.properties" ''
          sbt.version=${versioning.sbtVersion}
        '';

        # SBT Launcher Configuration
        # http://www.scala-sbt.org/0.13.5/docs/Launcher/Configuration.html
        sbtixRepos = writeText "sbtixRepos" ''
        [repositories]
          ${concatStringsSep "\n  " repoDefs}
        local
        '';
    in stdenv.mkDerivation (rec {
            name = "sbt-setup-template";

            dontPatchELF      = true;
            dontStrip         = true;

            # set environment variable to affect all SBT commands
            SBT_OPTS = ''
             -Dsbt.ivy.home=./.ivy2/
             -Dsbt.boot.directory=./.sbt/boot/
             -Dsbt.global.base=./.sbt
             -Dsbt.global.staging=./.staging
             -Dsbt.override.build.repos=true
             -Dsbt.repository.config=${sbtixRepos}
            '';

            unpackPhase = ''
              runHook preUnpack

              ln -s ${buildSbt}  ./build.sbt
              ln -s ${mainScala} ./Main.scala

              mkdir -p ./project

              ln -s ${buildProperties} ./project/build.properties

              runHook postUnpack
            '';

            buildInputs = [ jdk sbt ];

            buildPhase = ''
              runHook preBuild

              sbt update

              runHook postBuild
            '';

            installPhase =''
              runHook preInstall

              mkdir -p $out
              # Copy the hidden ivy lock files. Only keep ivy cache folder, not ivy local. local might be empty now but I want to be sure it is not polluted in the future.
              rm -rf ./.ivy2/local
              cp -r --remove-destination ./.ivy2 $out/ivy
              cp -r --remove-destination ./.sbt $out/sbt

              runHook postInstall
            '';
    });

  mergeSbtTemplates = templates: runCommand "merge-sbt-template" {}
        (let
            copyTemplate = template:
                [ "cp -rns ${template}/ivy $out"
                  "cp -rns ${template}/sbt $out"
                  "chmod -R u+rw $out"
                ];
        in
            lib.concatStringsSep "\n" (["mkdir -p $out"] ++ lib.concatLists (map copyTemplate templates))
        );

in rec {
    mkRepo = name: artifacts: runCommand name {}
        (let
            parentDirs = filePath:
                concatStringsSep "/" (init (splitString "/" filePath));
            linkArtifact = outputPath: urlAttrs:
                [ ''mkdir -p "$out/${parentDirs outputPath}"''
                  ''ln -fsn "${fetchurl urlAttrs}" "$out/${outputPath}"''
                ];
        in
            lib.concatStringsSep "\n" (lib.concatLists (lib.mapAttrsToList linkArtifact artifacts)));

    repoConfig = {repos, nixrepo, name}:
        let
            repoPatternOptional = repoPattern:
                optionalString (repoPattern != "") ", ${repoPattern}";
            repoPath = repoName: repoPattern:
                [ "${name}-${repoName}: file://${nixrepo}/${repoName}${repoPatternOptional repoPattern}" ];
        in
            lib.concatLists (lib.mapAttrsToList repoPath repos);

    ivyRepoPattern = "[organization]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]";

    mergeAttr = attr: repo:
        fold (a: b: a // b) {} (catAttrs attr repo);

    buildSbtProject = args@{repo, name, buildInputs ? [], sbtixBuildInputs ? [], sbtOptions ? "", ...}:
      let
          versionings = unique (flatten (catAttrs "versioning" repo));
          artifacts = mergeAttr "artifacts" repo;
          repos = mergeAttr "repos" repo;
          nixrepo = mkRepo "${name}-repo" artifacts;
          thisFetchedDependencies = { inherit repos nixrepo name; };

          fetchedDependencies = [thisFetchedDependencies] ++ concatMap (d: d.fetchedRepos) sbtixBuildInputs;
          repoDefs = concatMap repoConfig fetchedDependencies;

          builtDependencies = concatMap (d: [ d ] ++ d.builtRepos) sbtixBuildInputs;

          sbtSetupTemplate = mergeSbtTemplates(map (sbtTemplate repoDefs) versionings);

          # SBT Launcher Configuration
          # http://www.scala-sbt.org/0.13.5/docs/Launcher/Configuration.html
          sbtixRepos = writeText "sbtixRepos" ''
            [repositories]
              ${concatStringsSep "\n  " (map (d: "${d.name}: file://${d}, ${ivyRepoPattern}") builtDependencies)}
              ${concatStringsSep "\n  " repoDefs}
            local
            '';

      in stdenv.mkDerivation (rec {
            dontPatchELF      = true;
            dontStrip         = true;

            # COURSIER_CACHE env variable is needed if one wants to use non-sbtix repositories in the below repo list, which is sometimes useful.
            COURSIER_CACHE = "./.coursier/cache/v1";

            # configurePhase = ''
            #   cp -Lr ${sbtSetupTemplate}/ivy ./.ivy2
            #   cp -Lr ${sbtSetupTemplate}/sbt ./.sbt
            #   chmod -R 755 ./.ivy2/
            #   chmod -R 755 ./.sbt/
            # '';

            # set environment variable to affect all SBT commands
            SBT_OPTS = ''
              -Dsbt.ivy.home=./.ivy2/
              -Dsbt.boot.directory=./.sbt/boot/
              -Dsbt.global.base=./.sbt
              -Dsbt.global.staging=./.staging
              -Dsbt.override.build.repos=true
              -Dsbt.repository.config=${sbtixRepos}
              ${sbtOptions}
            '';

            buildPhase = ''
              runHook preBuild

              pwd && sbt compile

              runHook postBuild
            '';
        } // args // {
            repo = null;
            buildInputs = [ makeWrapper jdk sbt ] ++ buildInputs;
        }) // {
            builtRepos = builtDependencies;
            fetchedRepos = fetchedDependencies;
        };

    buildSbtLibrary = args: buildSbtProject ({
        installPhase = ''
          runHook preInstall

          sbt publishLocal
          mkdir -p $out/
          cp ./.ivy2/local/* $out/ -r

          runHook postInstall
        '';
    } // args);

    buildSbtProgram = args: buildSbtProject ({
        installPhase = ''
          runHook preInstall

          sbt stage
          mkdir -p $out/
          cp target/universal/stage/* $out/ -r
          for p in $(find $out/bin/* -executable); do
            wrapProgram "$p" --prefix PATH : ${jre}/bin --prefix PATH : ${gawk}/bin
          done

          runHook postInstall
        '';
    } // args);
}