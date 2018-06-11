with import <nixpkgs> {};

callPackage ./release.nix {
  nixpkgs = <nixpkgs>;
  mantisSrc = fetchgit {
    url = "https://github.com/input-output-hk/mantis.git";
    rev = "refs/heads/phase/iele_testnet";
    sha256 = "1qp8qqhrqycsslv4mggqwrgz1gypdx118lg6flkj9s0qivf8b4dh";
  };
  kevmSrc = fetchgit {
    url = "https://github.com/kframework/evm-semantics.git";
    rev = "refs/heads/master";
    sha256 = "1lpr8vbcaswkss8k3v0fz6l9hjbfq9d7jc3vkjg14f9c039h5qsa";
  };
  sbtVerifySrc = fetchgit {
    url = "https://github.com/input-output-hk/sbt-verify.git";
    rev = "refs/tags/v0.4.1";
    sha256 = "0nwkc4wf02hcxf4bfh62lscbfmavhj6zqmkcp7rc9p381khzg8ac";
  };
  ieleSrc = fetchgit {
    url = "https://github.com/runtimeverification/iele-semantics.git";
    rev = "refs/heads/master";
    sha256 = "1nnziajs92wvzdy07k2klzi5q4cxhvlm6xmqrh0hg5fgg1c42gps";
  };
  secp256k1Src = fetchgit {
    url = "https://github.com/bitcoin-core/secp256k1";
    rev = "refs/heads/master";
    sha256 = "16z67zp2af0c669jfr9b7dbdklhjd51mj3qwx98v982w18k325rv";
  };
  blockchainKPluginSrc = fetchgit {
    url = "https://github.com/runtimeverification/blockchain-k-plugin";
    rev = "refs/heads/master";
    sha256 = "0ir11b9c0ckvzwq3ysgcbs1gfv82i72yhis38xgnma00dmf5858f";
  };
  rvkSrc = fetchgit {
    url = "https://github.com/runtimeverification/k";
    rev = "refs/heads/master";
    sha256 = "1712w2rqzqqrvvghkhbf43vnqh2vr1bmmav9fvqm86kvscqikzy7";
  };
  ethereumTestsSrc = fetchgit {
    url = "https://github.com/ethereum/tests.git";
    rev = "refs/heads/master";
    sha256 = "0i0zsf2ai3v3wlkm62il2ndzlzsgymkqc5l60w4kq963mmhrx31v";
  };
  tangleSrc = fetchgit {
    url = "https://github.com/ehildenb/pandoc-tangle";
    rev = "refs/heads/master";
    sha256 = "0i0zsf2ai3v3wlkm62il2ndzlzsgymkqc5l60w4kq963mmhrx31v";
  };
}
