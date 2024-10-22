const std = @import("std");
const cfg = @import("cfg.zig");
const Command = @import("command.zig").Command;
const Expr = @import("command.zig").Expr;

pub fn parse(src: []const u8, allocator: std.mem.Allocator) !cfg.Cfg(Command) {
    var source = Source.init(src, allocator);
    defer source.deinit();

    var g = try cfg.Cfg(Command).init(allocator);
    errdefer g.deinit();
    var labels = std.StringHashMap(usize).init(allocator);
    defer labels.deinit();

    while (true) {
        try source.skipWhitespace();
        var label = try parseIdent(&source);
        var kind: u2 = 0;
        if (std.mem.eql(u8, label, "entry")) {
            kind |= 1;
            label = try parseIdent(&source);
        }
        if (std.mem.eql(u8, label, "exit")) {
            kind |= 2;
            label = try parseIdent(&source);
        }

        const entry = try labels.getOrPut(label);
        if (!entry.found_existing) {
            entry.value_ptr.* = try g.addBlock();
        }
        const block = entry.value_ptr.*;

        if (kind & 1 != 0) try g.setEntry(block);
        if (kind & 2 != 0) try g.setExit(block);

        std.log.info("Parsing block '{s}'", .{label});

        _ = try source.readExpect(':');
        while (true) {
            var cmd: Command = undefined;
            var has_idx = false;
            try source.skipWhitespace();
            if (source.finished()) break;

            if (source.peek() == '(') {
                _ = try source.read();
                const num_str = try parseIdent(&source);
                cmd.idx = try std.fmt.parseInt(usize, num_str, 10);
                _ = try source.readExpect(')');
                has_idx = true;
            }
            cmd.lhs = try parseIdent(&source);

            if (std.mem.eql(u8, cmd.lhs, "goto")) {
                for ("goto") |_| source.unread();
                if (has_idx) {
                    std.log.err("Index not allowed for 'goto', at {}", .{source.loc});
                    return ParseError.IndexAtGoto;
                }
                break;
            } else if (!has_idx) {
                std.log.err("Index expected before a command, at {}", .{source.loc});
                return ParseError.MissingIndex;
            }

            try source.skipWhitespace();
            _ = try source.readExpect('=');

            cmd.rhs = try parseExpr(&source);

            try g.addCommand(cmd, block);
        }

        if (source.finished()) break;
        const gt = try parseIdent(&source);
        if (!std.mem.eql(u8, gt, "goto")) {
            for (gt) |_| source.unread();
            continue;
        }
        _ = try source.readExpect(':');

        while (true) {
            const next_label = try parseIdent(&source);
            const next_entry = try labels.getOrPut(next_label);
            if (!next_entry.found_existing) next_entry.value_ptr.* = try g.addBlock();
            try g.connectBlocks(block, next_entry.value_ptr.*);

            try source.skipWhitespace();
            const sep = try source.readExpect(.{ ';', ',' });
            if (sep == ';') break;
        }
    }

    return g;
}

fn parseIdent(src: *Source) ![]const u8 {
    try src.skipWhitespace();
    const start = src.i;
    var end = src.i;
    while (src.peek()) |c| {
        if (std.ascii.isAlphanumeric(c) or c == '_') {
            _ = try src.read();
            end = src.i;
        } else {
            break;
        }
    }
    if (start == end) {
        std.log.err("No identifier given, at {}", .{src.loc});
        return ParseError.MissingIdent;
    }

    return src.src[start..end];
}

fn parseExpr(src: *Source) !Expr {
    var res: Expr = undefined;
    res.a = try parseIdent(src);
    try src.skipWhitespace();
    const next = try src.readExpect(.{ '+', '-', '*', '/', ';' });
    switch (next) {
        '+' => res.op = .Add,
        '-' => res.op = .Sub,
        '*' => res.op = .Mul,
        '/' => res.op = .Div,
        ';' => res.op = .None,
        else => unreachable,
    }

    if (res.op != .None) {
        res.b = try parseIdent(src);
        try src.skipWhitespace();
        _ = try src.readExpect(';');
    }

    return res;
}

const Location = struct {
    line: usize = 0,
    col: usize = 0,

    pub fn format(self: @This(), fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = opts;
        try writer.print("line {}, col {}", .{ self.line + 1, self.col + 1 });
    }
};

const ParseError = error{
    UnexpectedEnd,
    MissingIndex,
    IndexAtGoto,
    MissingIdent,
    SymbolMismatch,
    BlockRedefinition,
};

const Span = struct {
    start: usize,
    end: usize,
};

const Source = struct {
    src: []const u8,
    loc: Location = .{},
    lines: std.ArrayList(Span),
    start: usize = 0,
    i: usize = 0,

    pub fn init(src: []const u8, allocator: std.mem.Allocator) Self {
        return Self{
            .src = src,
            .lines = std.ArrayList(Span).init(allocator),
        };
    }
    pub fn deinit(self: *Self) void {
        self.lines.deinit();
    }

    fn skipWhitespace(self: *Self) !void {
        while (self.peek()) |c| {
            if (!std.ascii.isWhitespace(c)) break;
            _ = try self.read();
        }
    }

    fn finished(self: *Self) bool {
        return self.i >= self.src.len;
    }

    const Self = @This();
    fn read(self: *Self) !u8 {
        if (self.i >= self.src.len) return ParseError.UnexpectedEnd;
        const c = self.src[self.i];
        if (c == '\n') {
            self.loc.line += 1;
            self.loc.col = 0;
            if (self.lines.items.len < self.loc.line) {
                try self.lines.append(.{ .start = self.start, .end = self.i });
            }
            self.start = self.i + 1;
        } else {
            self.loc.col += 1;
        }
        self.i += 1;
        return c;
    }
    fn peek(self: *Self) ?u8 {
        if (self.i >= self.src.len) return null;
        return self.src[self.i];
    }
    fn unread(self: *Self) void {
        if (self.i > 0) self.i -= 1;
        const c = self.src[self.i];
        if (c == '\n') {
            self.loc.line -= 1;
            self.loc.col = self.lines.items[self.loc.line].end - 1;
        } else {
            self.loc.col -= 1;
        }
    }

    fn readExpect(self: *Self, expect: anytype) !u8 {
        const ti = @typeInfo(@TypeOf(expect));
        const c = try self.read();
        switch (ti) {
            .Int, .ComptimeInt => {
                const e = @as(u8, @intCast(expect));
                if (c != e) {
                    std.log.err("Expected '{c}', got '{c}', at {}", .{ c, e, self.loc });
                    return ParseError.SymbolMismatch;
                }
            },
            .Struct => |info| {
                if (!info.is_tuple) @compileError("Expected a tuple struct");
                inline for (info.fields, 0..) |*f, i| {
                    const fti = @typeInfo(f.type);
                    switch (fti) {
                        .Int, .ComptimeInt => {
                            const e = @as(u8, @intCast(expect[i]));
                            if (c == e) {
                                return c;
                            }
                        },
                        else => @compileError("Expected a u8 variant in 'expected'"),
                    }
                }
                std.log.err("Unexpected symbol '{c}', at {}", .{ c, self.loc });
                return ParseError.SymbolMismatch;
            },
            else => @compileError("Unsupported type for 'expected'"),
        }
        return c;
    }
};
