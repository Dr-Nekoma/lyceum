const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) !void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "lyceum-client",
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path("src/client.zig"),
        .target = target,
        .optimize = optimize,
    });

    // TODO: figure out how to properly link a zig system library
    if (b.lazyDependency("raylib_zig", .{
        .target = target,
        .optimize = optimize,
    })) |raylib_zig| {
        if (b.systemIntegrationOption("raylib", .{})) {
            exe.linkSystemLibrary("raylib");
        } else {
            exe.linkLibrary(raylib_zig.artifact("raylib"));
        }
        exe.root_module.addImport("raylib", raylib_zig.module("raylib"));
    }

    if (b.lazyDependency("zerl", .{
        .target = target,
        .optimize = optimize,
    })) |zerl| {
        exe.root_module.addImport("zerl", zerl.module("zerl"));
    }

    const assets = b.addInstallDirectory(.{
        .source_dir = b.path("assets"),
        .install_dir = .prefix,
        .install_subdir = "assets",
    });

    exe.step.dependOn(&assets.step);

    // https://ziglang.org/learn/build-system/#conditional-compilation
    const assets_opt = b.option([]const u8, "assets", "custom path for the assets directory") orelse "./assets";
    const options = b.addOptions();
    options.addOption([]const u8, "assets", assets_opt);
    exe.root_module.addOptions("build_options", options);

    if (b.lazyImport(@This(), "zerl")) |zerl_build| {
        if (std.posix.getenv("LIBRARY_PATH")) |lib_path| {
            try zerl_build.add_erlang_paths(b, lib_path);
        }
        if (std.posix.getenv("PATH")) |path| {
            try zerl_build.add_erlang_paths(b, path);
        }
    }

    // const strip = b.option(
    //     bool,
    //     "strip",
    //     "Strip debug info to reduce binary size, defaults to false",
    // ) orelse false;
    // exe.root_module.strip = strip;

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    const no_bin = b.option(bool, "no-bin", "skip emitting binary") orelse false;
    if (no_bin) {
        b.getInstallStep().dependOn(&exe.step);
    } else {
        b.installArtifact(exe);
    }

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/client.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
