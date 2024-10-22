const std = @import("std");
const assert = std.debug.assert;

const cfg = @import("cfg.zig");
const parse = @import("parse.zig");
const live = @import("analysis/live_variable.zig");
const reach = @import("analysis/reaching_definition.zig");
const avexpr = @import("analysis/available_expression.zig");
const lcm = @import("analysis/lazy_code_motion.zig");

const Analysis = enum(usize) {
    LiveVariables = 0,
    AvailableExpressions,
    ReachingDefinitions,
    LazyCodeMotion,
    count,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.print("GPA: {}\n", .{gpa.deinit()});

    var arg_it = std.process.args();
    const prog_name = arg_it.next().?;

    var maybe_path: ?[]const u8 = null;
    var analyses = std.StaticBitSet(@intFromEnum(Analysis.count)).initEmpty();

    while (arg_it.next()) |arg| {
        if (std.mem.eql(u8, arg, "-i")) maybe_path = arg_it.next();
        if (std.mem.eql(u8, arg, "--all")) analyses.setRangeValue(
            .{ .start = 0, .end = @intFromEnum(Analysis.count) },
            true,
        );
        inline for (@typeInfo(Analysis).Enum.fields) |f| {
            if (f.value == @intFromEnum(Analysis.count)) continue;
            if (std.mem.eql(u8, f.name, arg)) analyses.set(@intCast(f.value));
        }
    }

    const path = maybe_path orelse {
        std.log.err("Usage: {s} -i <file_path> {{analysis}}", .{prog_name});
        std.process.exit(1);
    };

    std.log.info("Processing '{s}':", .{path});

    const file_size = (try std.fs.cwd().statFile(path)).size;
    const buf = try gpa.allocator().alloc(u8, file_size);
    defer gpa.allocator().free(buf);

    const src = try std.fs.cwd().readFile(path, buf);
    var g = try parse.parse(src, gpa.allocator());
    defer g.deinit();

    var an_it = analyses.iterator(.{});
    var f_name_buf: [256]u8 = .{0} ** 256;
    var f_path_buf: [1024]u8 = .{0} ** 1024;
    while (an_it.next()) |a| {
        const variant: Analysis = @enumFromInt(a);
        std.debug.assert(variant != Analysis.count);

        const a_name = @tagName(variant);
        const f_name = try std.fmt.bufPrint(&f_name_buf, "{s}.dot", .{a_name});
        const f_path = try std.fs.cwd().realpath(f_name, &f_path_buf);
        std.log.info("----->Running {s}, save as {s}", .{ a_name, f_path });
        var f = try std.fs.cwd().createFile(f_name, .{});
        defer f.close();

        switch (variant) {
            .LiveVariables => {
                var analysis = try live.LiveVariables.init(&g, gpa.allocator());
                defer analysis.deinit();
                try analysis.dump(f.writer().any());
            },
            .AvailableExpressions => {
                var analysis = try avexpr.AvailableExpressions.init(&g, gpa.allocator());
                defer analysis.deinit();
                try analysis.dump(f.writer().any());
            },
            .ReachingDefinitions => {
                var analysis = try reach.ReachingDefinitions.init(&g, gpa.allocator());
                defer analysis.deinit();
                try analysis.dump(f.writer().any());
            },
            .LazyCodeMotion => {
                var analysis = try lcm.LazyCodeMotion.init(&g, gpa.allocator());
                defer analysis.deinit();
                try analysis.dump(f.writer().any());
            },
            else => unreachable,
        }
    }
}
