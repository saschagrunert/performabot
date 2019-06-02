let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable";
    url = "https://github.com/nixos/nixpkgs/archive/" +
      "03d6bb52b250d74d096e2286b02c51f595fa90ee.tar.gz";
    sha256 = "0bngjc4d190bgzhgxv9jszqkapx3ngdpj08rd2arz19ncggflnxv";
  };
in nixpkgs
