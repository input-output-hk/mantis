ci: {
    version: 1
    steps: {
        nixexpr: {
          label: "ensure Nix expressions are up-to-date"
          command: [ "./update-nix.sh", "--check" ]
          outputs: [
              "/local/repo/nix-expr.patch",
          ]
        }
        scalafix: {
            label:   "scalafix & scalafmt"
            command: #sbt + ["formatCheck"]
        }
        compile: {
            label:   "compile everything"
            command: #sbt + ["compile-all"]
            after: ["scalafix"]
        }
        scalastyle: {
            command: #sbt + ["coverage", "scalastyle", "test:scalastyle"]
            after: ["compile"]
        }
        test_bytes: {
            label: "bytes tests"
            after: ["compile"]
            command: #sbt + ["coverage", "bytes/test"]
            outputs: [
                "/local/repo/bytes/target/test-reports/**/*",
                "/local/repo/bytes/target/scala-2.13/scoverage-report/**/*",
                "/local/repo/bytes/target/scala-2.13/coverage-report/**/*",
            ]
        }
        test_crypto: {
            label: "Crypto tests"
            after: ["compile"]
            command: #sbt + ["coverage", "crypto/test"]
            outputs: [
                "/local/repo/crypto/target/test-reports/**/*",
                "/local/repo/crypto/target/scala-2.13/scoverage-report/**/*",
                "/local/repo/crypto/target/scala-2.13/coverage-report/**/*",
            ]
        }
        test_rlp: {
            label: "RLP tests"
            after: ["compile"]
            command: #sbt + ["coverage", "rlp/test"]
            outputs: [
                "/local/repo/rlp/target/test-reports/**/*",
                "/local/repo/rlp/target/scala-2.13/scoverage-report/**/*",
                "/local/repo/rlp/target/scala-2.13/coverage-report/**/*",
            ]
        }
        test_unit: {
            label: "Unit tests"
            after: ["compile"]
            command: #sbt + ["coverage", "test"]
            outputs: [
                "/local/repo/target/test-reports/**/*",
                "/local/repo/target/scala-2.13/scoverage-report/**/*",
                "/local/repo/target/scala-2.13/coverage-report/**/*",
            ]
        }
        test_evm: {
            label: "EVM tests"
            after: ["compile"]
            command: #sbt + ["coverage", "evm:test"]
            outputs: [
                "/local/repo/target/test-reports/**/*",
                "/local/repo/target/scala-2.13/scoverage-report/**/*",
                "/local/repo/target/scala-2.13/coverage-report/**/*",
            ]
        }
        test_ets: {
            label: "ETS tests"
            after: ["compile"]
            command: ["./test-ets.sh"]
            outputs: [
                "/local/repo/mantis-log.txt",
                "/local/repo/retesteth-GeneralStateTests-log.txt",
                "/local/repo/retesteth-BlockchainTests-log.txt",
            ]
        }
        test_integration: {
            label: "integration tests"
            after: ["compile"]
            command: #sbt + ["coverageOff", "it:test"]
            outputs: [
                "/local/repo/target/test-reports/**/*",
            ]
        }
        additional: {
            label: "additional compilation & dist"
            after: ["compile", "test_integration"]
            command: #sbt + ["coverageOff", "benchmark:compile dist"]
            outputs: [
                "/local/repo/target/universal/mantis-*.zip",
            ]
        }
        publish: {
            label: "Publishing libraries to Maven"
            after: ["test_crypto", "test_rlp", "test_unit"]
            command: [ "./publish.sh" ] // TODO
            outputs: [
                "/local/repo/target/universal/mantis-*.zip",
            ]
        }
    }
}
// #sbt: ["sbt", "-v", "-mem", "2048", "-J-Xmx4g", "-Dsbt.ivy.home=/cache/ivy2", "-Dsbt.boot.directory=/cache/sbt", "-Dmaven.repo.local=/cache/maven", "-Dnix=true"]
#sbt: ["sbt", "-v", "-mem", "8192", "-Dnix=true"]

// https://docs.github.com/en/developers/webhooks-and-events/webhooks/webhook-events-and-payloads#webhook-payload-object-33
#isNotDraft: pull_request.draft == false
#thisCommit: pull_request.head.sha

// shared defaults for all steps
#step: {
    enable:       pull_request.base.ref == "mig-bitte-ci" && #isNotDraft
    cpu:          5000
    memory:       9000
    term_timeout: 60 * 60 * 3
    kill_timeout: term_timeout + 30
    flakes: "github:input-output-hk/mantis/\(#thisCommit)": [
        "sbt",
        "coreutils",
        "findutils",
        "gnused",
        "gnupg",
        "solc",
        "lllc",
        "jdk8",
        "retesteth",
        "netcat-gnu",
        "gnugrep",
        "protoc-wrapper",
    ]
}
