const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "dfa",
        .root_source_file = b.path("src/main.zig"),
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });

    b.installArtifact(exe);
    const run_exe = b.addRunArtifact(exe);
    var it = std.process.args();
    var hasArgsForRun = false;
    while (it.next()) |arg| {
        if (hasArgsForRun) {
            run_exe.addArg(arg);
        }
        if (std.mem.eql(u8, arg, "--")) {
            hasArgsForRun = true;
        }
    }

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);
}
