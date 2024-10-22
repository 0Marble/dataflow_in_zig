const std = @import("std");
const assert = std.debug.assert;

const cfg = @import("cfg.zig");
const parse = @import("parse.zig");
const live = @import("analysis/live_variable.zig");
const reach = @import("analysis/reaching_definition.zig");
const avexpr = @import("analysis/available_expression.zig");
const lcm = @import("analysis/lazy_code_motion.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.print("GPA: {}\n", .{gpa.deinit()});

    const path = "p2.prg";
    const file_size = (try std.fs.cwd().statFile(path)).size;

    const buf = try gpa.allocator().alloc(u8, file_size);
    defer gpa.allocator().free(buf);

    const src = try std.fs.cwd().readFile(path, buf);

    var g = try parse.parse(src, gpa.allocator());
    defer g.deinit();

    {
        var f = try std.fs.cwd().createFile("lcm.dot", .{});
        defer f.close();
        var analysis = try lcm.LazyCodeMotion.init(&g, gpa.allocator());
        defer analysis.deinit();
        try analysis.dump(f.writer().any());
    }
}
