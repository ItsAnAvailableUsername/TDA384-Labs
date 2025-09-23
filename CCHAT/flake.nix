{
  description = "TDA384-Labs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs:
    let
      pkgs = import inputs.nixpkgs {
        system = "x86_64-linux";
      };
    in
    {
      devShells."x86_64-linux".default = pkgs.mkShellNoCC {
        packages = [ pkgs.gnumake pkgs.erlang pkgs.efmt ];
      };
    };
}
