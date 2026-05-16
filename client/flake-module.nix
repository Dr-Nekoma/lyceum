{
  perSystem =
    {
      pkgs,
      lib,
      lyceum,
      self',
      ...
    }:
    let
      inherit (lyceum) app_version erlangVersion zigVersion;
      zig_app = "lyceum-client";

      # Graphical/system libs shared between the client build and the
      # dev shell. Used both directly as buildInputs and to compose
      # LD_LIBRARY_PATH for the wrapped binary.
      graphicalLibs = with pkgs; [
        glfw
        libGL
        libxkbcommon
        libxcb
        libxft
        libx11
        libx11.dev
        libxrandr
        libxinerama
        libxcursor
        libxi
        wayland
        wayland-protocols
      ];

      linuxPkgs =
        graphicalLibs
        ++ (with pkgs; [
          inotify-tools
          libpulseaudio
          pkg-config
          wayland-scanner
        ]);

      linuxLibPath = lib.makeLibraryPath (
        graphicalLibs
        ++ (with pkgs; [
          liburing
          raylib
        ])
      );

      darwinPkgs = with pkgs.darwin.apple_sdk.frameworks; [
        CoreFoundation
        CoreServices
      ];

      platformPkgs =
        lib.optionals pkgs.stdenv.isLinux linuxPkgs
        ++ lib.optionals pkgs.stdenv.isDarwin darwinPkgs;

      # Strip .nix files so edits to this module don't invalidate the
      # client build cache.
      src = lib.cleanSourceWith {
        src = ./.;
        filter =
          name: type:
          (lib.cleanSourceFilter name type) && !(lib.hasSuffix ".nix" (baseNameOf name));
      };
    in
    {
      # nix build .#client
      packages.client = pkgs.stdenv.mkDerivation {
        pname = zig_app;
        version = app_version;
        inherit src;

        zigBuildFlags = [
          "-fsys=raylib"
          "--release=fast"
          "-Dassets=${toString ./assets}"
        ];

        nativeBuildInputs = [
          zigVersion.hook
          pkgs.makeWrapper
        ];

        buildInputs = [
          pkgs.raylib
          erlangVersion
        ]
        ++ platformPkgs;

        # To re-generate the nix lockfile:
        # just client-deps
        postConfigure = ''
          mkdir -p "$ZIG_GLOBAL_CACHE_DIR/p"
          for pkg in ${pkgs.callPackage ./build.zig.zon.nix { zig = zigVersion; }}/*; do
            ln -s "$pkg" "$ZIG_GLOBAL_CACHE_DIR/p/$(basename "$pkg")"
          done
        '';

        postInstall = ''
          wrapProgram "$out/bin/${zig_app}" --prefix LD_LIBRARY_PATH ":" "${linuxLibPath}"
        '';
      };

      # nix run .#client
      apps.client = {
        type = "app";
        program = "${self'.packages.client}/bin/${zig_app}";
      };

      # Client-side dev-shell contributions, merged into the shared
      # devenv shell defined at the top level.
      devenv.shells.default = {
        languages.zig = {
          enable = true;
          package = zigVersion;
        };

        packages = [ pkgs.raylib ] ++ platformPkgs;

        env = {
          GLFW_SCALE_TO_MONITOR = "GLFW_TRUE";
        };

        scripts = {
          client.exec = "just client";
          client-release.exec = "just client-release";
        };
      };
    };
}
