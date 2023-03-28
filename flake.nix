{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        {
          packages.gotraceui = pkgs.buildGoModule {
            name = "gotraceui";
            src = self;
            vendorSha256 = "sha256-qnHU/Ht5+BGVoGbz2h9/k3gD1L2qAW0eZJ2xBzJatHQ=";

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

          packages.default = self.packages.${system}.gotraceui;

          apps.gotraceui = flake-utils.lib.mkApp { drv = self.packages.${system}.gotraceui; };
          apps.default = self.apps.${system}.gotraceui;

          devShells.gotraceui = pkgs.mkShell {
            inputsFrom = builtins.attrValues self.packages.${system};
          };

          devShells.default = self.devShells.${system}.gotraceui;
        }
    );
}
