{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        tex = pkgs.texlive.combine {
          inherit (pkgs.texlive)
            scheme-small

            adjustbox
            biber
            biblatex
            cleveref
            csquotes
            fontsetup
            hyperxmp
            latexmk
            luacode
            lualatex-math
            menukeys
            newcomputermodern
            relsize
            siunitx
            soul
            xstring
            ;
        };
      in
        {
          packages.gotraceui = pkgs.buildGoModule {
            name = "gotraceui";
            src = self;
            vendorSha256 = "sha256-ZFFtNJZI7blB+W2VlAld25mtNvEOFHUZnqWDgUgC8tI=";

            subPackages = ["cmd/gotraceui"];

            nativeBuildInputs = [ pkgs.pkg-config ];
            buildInputs = with pkgs;
              (if stdenv.isLinux then [
                vulkan-headers
                libxkbcommon
                wayland
                xorg.libX11
                xorg.libXcursor
                xorg.libXfixes
                libGL
              ] else if stdenv.isDarwin then [
                darwin.apple_sdk_11_0.frameworks.Foundation
                darwin.apple_sdk_11_0.frameworks.Metal
                darwin.apple_sdk_11_0.frameworks.QuartzCore
                darwin.apple_sdk_11_0.frameworks.AppKit
                darwin.apple_sdk_11_0.MacOSX-SDK
              ] else [ ]);

            ldflags = ["-X gioui.org/app.ID=co.honnef.Gotraceui"];

            postInstall = ''
              cp -r share $out/
            '';

            meta = with nixpkgs.lib; {
              description = "An efficient frontend for Go execution traces";
              homepage = "https://github.com/dominikh/gotraceui";
              license = licenses.mit;
              platforms = platforms.all;
            };
          };

          packages.manual = pkgs.stdenvNoCC.mkDerivation rec {
            name = "manual";
            src = self;
            buildInputs = [ tex ];

            buildPhase = ''
              mkdir -p .cache/texmf-var
              export TEXMFHOME=.cache
              export TEXMFVAR=.cache/texmf-var
              latexmk -cd -f -pdf -lualatex -interaction=nonstopmode -bibtex-cond1 doc/manual/manual.tex
            '';

            installPhase = ''
              mkdir -p $out
              cp doc/manual/manual.pdf $out/gotraceui.pdf
            '';
          };

          packages.default = self.packages.${system}.gotraceui;

          apps.gotraceui = flake-utils.lib.mkApp { drv = self.packages.${system}.gotraceui; };
          apps.default = self.apps.${system}.gotraceui;

          devShells.gotraceui = pkgs.mkShell {
            inputsFrom = builtins.attrValues self.packages.${system};
            nativeBuildInputs = [ pkgs.python3Packages.fonttools ];
          };

          devShells.default = self.devShells.${system}.gotraceui;
        }
    );
}
