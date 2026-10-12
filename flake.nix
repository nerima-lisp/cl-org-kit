{
  description = "Org mode reader, writer, query and format-preserving editor for Common Lisp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Sibling libraries are pinned to release tags and consumed as source
    # trees (`flake = false`) to build a `lispDerivation` below.
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.1.1";
      flake = false;
    };

    # Kept on the tag aitools pins, so embedding cl-org-kit there does not
    # force a second cl-regex-kit version into its closure.
    cl-regex-kit = {
      url = "github:nerima-lisp/cl-regex-kit/v2.1.1";
      flake = false;
    };

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
      flake = false;
    };

    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v1.1.1";
      flake = false;
    };

    # Transitive only: cl-regex-kit needs cl-concurrent-kit, which needs
    # cl-boundary-kit and cl-date-kit; cl-boundary-kit needs cl-host-kit.
    # Each lispDerivation builds in its own sandbox, so each kit's
    # dependencies go in that kit's own lispDependencies.
    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.6.1";
      flake = false;
    };

    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v2.3.0";
      flake = false;
    };

    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.3.1";
      flake = false;
    };

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      flake = false;
    };

    # Consumed for its `lib` output (`mkLintCheck`), which a `flake = false`
    # source tree cannot provide.
    paredit-cli = {
      url = "github:nerima-lisp/paredit-cli/v1.6.3";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      cl-parser-kit,
      cl-regex-kit,
      cl-codec-kit,
      cl-date-kit,
      cl-concurrent-kit,
      cl-boundary-kit,
      cl-host-kit,
      cl-weave,
      paredit-cli,
      treefmt-nix,
    }:
    let
      lib = nixpkgs.lib;

      # x86_64-linux is what CI gates; aarch64-darwin is the development
      # machine.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      kit =
        ctx: name: src: deps:
        ctx.cl.lispDerivation {
          pname = name;
          version = ctx.cl.fromAsdSystem "${src}/${name}.asd";
          inherit src;
          lispSystem = name;
          lispDependencies = deps;
        };
    in
    cl-nix-forge.lib.${builtins.head systems}.mkPackageFlake {
      inherit self systems nixpkgs;

      pname = "cl-org-kit";

      # The version comes from the `:version` form in cl-org-kit.asd.
      asd = ./cl-org-kit.asd;

      root = ./.;

      meta = {
        description = "Org mode reader, writer, query and format-preserving editor for Common Lisp";
        homepage = "https://github.com/nerima-lisp/cl-org-kit";
        license = lib.licenses.mit;
        platforms = lib.platforms.unix;
      };

      lispDependencies =
        ctx:
        let
          hostKit = kit ctx "cl-host-kit" cl-host-kit [ ];
          dateKit = kit ctx "cl-date-kit" cl-date-kit [ ];
          parserKit = kit ctx "cl-parser-kit" cl-parser-kit [ ];
          boundaryKit = kit ctx "cl-boundary-kit" cl-boundary-kit [ hostKit ];
          concurrentKit = kit ctx "cl-concurrent-kit" cl-concurrent-kit [
            boundaryKit
            dateKit
          ];
        in
        [
          parserKit
          (kit ctx "cl-regex-kit" cl-regex-kit [
            concurrentKit
            parserKit
          ])
          (kit ctx "cl-codec-kit" cl-codec-kit [ ])
          dateKit
        ];

      lispCheckDependencies = ctx: [ (kit ctx "cl-weave" cl-weave [ ]) ];

      timeoutSeconds = 600;
      killAfterSeconds = 30;

      docs.root = ./docs;

      treefmt.evalModule = treefmt-nix.lib.evalModule;

      devShellPackages = ctx: [
        ctx.pkgs.sbcl
        ctx.pkgs.perl
        paredit-cli.packages.${ctx.system}.default
      ];

      extraOutputs = ctx: {
        checks.paredit-lint = paredit-cli.lib.${ctx.system}.mkLintCheck {
          src = ./.;
          name = "cl-org-kit-paredit-lint";
        };
      };
    };
}
