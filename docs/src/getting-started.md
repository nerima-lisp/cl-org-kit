# Getting Started

`cl-org-kit` supports SBCL only. Its runtime dependencies are
`cl-parser-kit`, `cl-regex-kit`, `cl-codec-kit`, and `cl-date-kit`; the test
system also uses `cl-weave`.

## Nix

Add the project and its runtime dependencies to the top-level `inputs`
attribute in your `flake.nix`, using the tags listed in the ASDF section below.
Merge these entries with any existing inputs:

```nix
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  cl-org-kit = {
    url = "github:nerima-lisp/cl-org-kit/v0.3.0";
    flake = false;
  };
  cl-parser-kit = {
    url = "github:nerima-lisp/cl-parser-kit/v1.1.1";
    flake = false;
  };
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
};
```

Add this `outputs` attribute alongside `inputs` in the same `flake.nix`. Merge
the shell into any existing outputs. It targets Apple Silicon macOS and
registers the project and every runtime source dependency:

```nix
outputs = inputs@{ nixpkgs, ... }:
let
  system = "aarch64-darwin";
  pkgs = import nixpkgs { inherit system; };
  asdfInputs = with inputs; [
    cl-org-kit
    cl-parser-kit
    cl-regex-kit
    cl-codec-kit
    cl-date-kit
    cl-concurrent-kit
    cl-boundary-kit
    cl-host-kit
  ];
  sourceRegistryEntries = builtins.concatStringsSep " "
    (map (source: "(:tree \"${source.outPath}/\")") asdfInputs);
in {
  devShells.${system}.default = pkgs.mkShell {
    packages = [ pkgs.sbcl ];
    shellHook = ''
      export CL_SOURCE_REGISTRY='(:source-registry ${sourceRegistryEntries} :inherit-configuration)'
    '';
  };
};
```

To run the test system, also add the test-only dependency shown in the ASDF
table below to `inputs` and `asdfInputs`. The repository's Nix development
shell and pinned dependencies are described in [Development](project/development.md).

## ASDF

For repository development, run `nix develop` from the repository root. The
[Nix development shell](project/development.md) provides SBCL and registers the
checkout and pinned runtime dependencies with ASDF. Outside that environment,
check out the runtime dependencies at these tested tags:

| System | Repository | Tag | Use |
| --- | --- | --- | --- |
| `cl-org-kit` | [GitHub](https://github.com/nerima-lisp/cl-org-kit) | `v0.3.0` | Project |
| `cl-parser-kit` | [GitHub](https://github.com/nerima-lisp/cl-parser-kit) | `v1.1.1` | Runtime |
| `cl-regex-kit` | [GitHub](https://github.com/nerima-lisp/cl-regex-kit) | `v2.1.1` | Runtime |
| `cl-codec-kit` | [GitHub](https://github.com/nerima-lisp/cl-codec-kit) | `v0.6.0` | Runtime |
| `cl-date-kit` | [GitHub](https://github.com/nerima-lisp/cl-date-kit) | `v1.1.1` | Runtime |
| `cl-concurrent-kit` | [GitHub](https://github.com/nerima-lisp/cl-concurrent-kit) | `v0.6.1` | Transitive runtime dependency |
| `cl-boundary-kit` | [GitHub](https://github.com/nerima-lisp/cl-boundary-kit) | `v2.3.0` | Transitive runtime dependency |
| `cl-host-kit` | [GitHub](https://github.com/nerima-lisp/cl-host-kit) | `v0.3.1` | Transitive runtime dependency |
| `cl-weave` | [GitHub](https://github.com/nerima-lisp/cl-weave) | `v1.3.0` | Test system only |

Clone each repository at the tag in the table. Place the project and runtime
dependency checkouts under one directory; for tests, also place `cl-weave`
there. Replace the path below with that directory, then start SBCL from the
same shell:

```sh
export CL_SOURCE_REGISTRY='(:source-registry (:tree "/path/to/lisp-projects/") :inherit-configuration)'
```

Then load the system in SBCL:

```lisp
(require :asdf)
(asdf:load-system "cl-org-kit")
```

The package is `ORG-KIT`.
