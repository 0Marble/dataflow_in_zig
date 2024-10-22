const std = @import("std");
const dfa = @import("dfa.zig");
const Command = @import("../command.zig").Command;
const cfg = @import("../cfg.zig");

const Cfg = cfg.Cfg(Command);
const Block = Cfg.Block;

pub const AvailableExpressions = struct {
    ae: AeTransfer,
    dfa: dfa.Dfa,
    g: *const Cfg,

    const Self = @This();
    pub fn init(g: *const Cfg, allocator: std.mem.Allocator) !Self {
        var self = Self{
            .ae = try AeTransfer.init(g, allocator),
            .g = g,
            .dfa = undefined,
        };
        errdefer self.ae.deinit();

        var full = try std.DynamicBitSet.initFull(allocator, self.ae.exprs.items.len);
        defer full.deinit();
        var empty = try std.DynamicBitSet.initEmpty(allocator, self.ae.exprs.items.len);
        defer empty.deinit();

        self.dfa = try dfa.Dfa.init(
            Command,
            self.g,
            .Forward,
            self.ae.transfer(),
            dfa.IntersectMeet.meet(),
            &empty,
            &full,
            allocator,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.ae.deinit();
        self.dfa.deinit();
    }

    pub fn dump(self: *const Self, writer: std.io.AnyWriter) !void {
        var p = AePrinter{ .ae = &self.ae, .dfa = &self.dfa };
        try self.g.dump(writer, p.printer());
    }
};

const AeTransfer = struct {
    const Expr = @import("../command.zig").Expr;
    exprs: std.ArrayList(Expr),
    expr_ids: std.ArrayList(usize),
    gkt: dfa.GenKillTransfer(Command),

    pub fn init(graph: *const Cfg, allocator: std.mem.Allocator) !Self {
        var self = Self{
            .gkt = undefined,
            .exprs = std.ArrayList(Expr).init(allocator),
            .expr_ids = std.ArrayList(usize).init(allocator),
        };
        var cnt: usize = 0;
        for (graph.blocks.items) |*block| {
            for (block.commands.items) |cmd| {
                if (cmd.idx > cnt) cnt = cmd.idx;
            }
        }
        try self.expr_ids.resize(cnt + 1);

        for (graph.blocks.items) |*block| {
            outer: for (block.commands.items) |cmd| {
                const expr = Expr{ .a = cmd.rhs.a, .op = cmd.rhs.op, .b = cmd.rhs.b };
                for (self.exprs.items, 0..) |other, idx| {
                    if (expr.eql(other)) {
                        self.expr_ids.items[cmd.idx] = idx;
                        continue :outer;
                    }
                }
                const idx = self.exprs.items.len;
                try self.exprs.append(expr);
                self.expr_ids.items[cmd.idx] = idx;
            }
        }
        self.gkt = try dfa.GenKillTransfer(Command).init(
            allocator,
            graph.blocks.items.len,
            self.exprs.items.len,
            undefined,
            Self.ae_transfer,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.exprs.deinit();
        self.expr_ids.deinit();
        self.gkt.deinit();
    }

    const Self = @This();
    fn ae_transfer(
        data: *anyopaque,
        block_idx: usize,
        block: *const Block,
        in: *const std.DynamicBitSet,
        gen: *std.DynamicBitSet,
        kill: *std.DynamicBitSet,
    ) void {
        _ = block_idx;
        const self: *Self = @alignCast(@ptrCast(data));
        std.debug.assert(self.gkt.data == @as(*anyopaque, @ptrCast(self)));

        for (block.commands.items) |cmd| {
            gen.set(self.expr_ids.items[cmd.idx]);
            kill.unset(self.expr_ids.items[cmd.idx]);

            var in_it = in.iterator(.{});
            while (in_it.next()) |i| {
                const expr = self.exprs.items[i];
                if (std.mem.eql(u8, expr.a, cmd.lhs) or expr.op != .None and std.mem.eql(u8, expr.b, cmd.lhs)) {
                    kill.set(i);
                }
            }
            var gen_it = gen.iterator(.{});
            while (gen_it.next()) |i| {
                const expr = self.exprs.items[i];
                if (std.mem.eql(u8, expr.a, cmd.lhs) or expr.op != .None and std.mem.eql(u8, expr.b, cmd.lhs)) {
                    gen.unset(i);
                }
            }
        }
    }
    fn transfer(self: *Self) dfa.Transfer(Command) {
        self.gkt.data = @ptrCast(self);
        return self.gkt.transfer();
    }
};

const AePrinter = struct {
    const Self = @This();
    ae: *const AeTransfer,
    dfa: *const dfa.Dfa,

    fn print(
        data: *anyopaque,
        w: std.io.AnyWriter,
        block: *const Cfg.Block,
        block_idx: usize,
    ) anyerror!void {
        const self: *Self = @alignCast(@ptrCast(data));
        _ = block;

        const in = &self.dfa.ins.items[block_idx];
        const out = &self.dfa.outs.items[block_idx];
        const gen = &self.ae.gkt.gens.items[block_idx];
        const kill = &self.ae.gkt.kills.items[block_idx];

        try w.print("{{in:", .{});
        try self.printExprSet(w, in);
        try w.print("|out:", .{});
        try self.printExprSet(w, out);
        try w.print("}}|{{gen:", .{});
        try self.printExprSet(w, gen);
        try w.print("|kill:", .{});
        try self.printExprSet(w, kill);
        try w.print("}}", .{});
    }

    fn printExprSet(self: *const Self, w: std.io.AnyWriter, set: *const std.DynamicBitSet) !void {
        try w.print("[", .{});

        var it = set.iterator(.{});
        while (it.next()) |x| {
            const expr = self.ae.exprs.items[x];
            try w.print("'{}', ", .{expr});
        }

        try w.print("]", .{});
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};
