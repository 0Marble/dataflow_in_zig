const std = @import("std");

pub const Op = enum { Add, Sub, Mul, Div, None };

pub const Expr = struct {
    a: []const u8,
    op: Op,
    b: []const u8,

    pub fn format(self: @This(), fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opts;
        _ = fmt;

        try writer.print("{s}", .{self.a});
        switch (self.op) {
            .Add => try writer.print(" + {s}", .{self.b}),
            .Sub => try writer.print(" - {s}", .{self.b}),
            .Mul => try writer.print(" * {s}", .{self.b}),
            .Div => try writer.print(" / {s}", .{self.b}),
            .None => {},
        }
    }

    pub fn eql(self: Expr, other: Expr) bool {
        return std.mem.eql(u8, self.a, other.a) and self.op == other.op and std.mem.eql(u8, self.b, other.b);
    }
};

pub const Command = struct {
    lhs: []const u8,
    rhs: Expr,
    idx: usize,

    pub fn format(self: @This(), fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opts;

        try writer.print("{s} = {}", .{ self.lhs, self.rhs });
    }
};
