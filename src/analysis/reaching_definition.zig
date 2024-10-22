const std = @import("std");
const dfa = @import("dfa.zig");
const Command = @import("../command.zig").Command;
const cfg = @import("../cfg.zig");

const Cfg = cfg.Cfg(Command);
const Block = Cfg.Block;

pub const ReachingDefinitions = struct {
    rd: RdTransfer,
    dfa: dfa.Dfa,
    g: *const Cfg,

    const Self = @This();
    pub fn init(g: *const Cfg, allocator: std.mem.Allocator) !Self {
        var self = Self{
            .g = g,
            .rd = try RdTransfer.init(g, allocator),
            .dfa = undefined,
        };
        errdefer self.rd.deinit();

        var empty = try std.DynamicBitSet.initEmpty(allocator, self.rd.cmds.items.len);
        defer empty.deinit();

        self.dfa = try dfa.Dfa.init(
            Command,
            g,
            .Forward,
            self.rd.transfer(),
            dfa.UnionMeet.meet(),
            &empty,
            &empty,
            allocator,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.dfa.deinit();
        self.rd.deinit();
    }

    pub fn dump(self: *const Self, writer: std.io.AnyWriter) !void {
        var p = RdPrinter{ .rd = &self.rd, .dfa = &self.dfa };
        try self.g.dump(writer, p.printer());
    }
};

const RdTransfer = struct {
    cmds: std.ArrayList(Command),
    gkt: dfa.GenKillTransfer(Command),

    const Self = @This();
    pub fn init(graph: *const Cfg, allocator: std.mem.Allocator) !Self {
        var cnt: usize = 0;
        for (graph.blocks.items) |*block| {
            for (block.commands.items) |cmd| {
                if (cmd.idx > cnt) cnt = cmd.idx;
            }
        }
        var self = Self{
            .cmds = std.ArrayList(Command).init(allocator),
            .gkt = undefined,
        };
        try self.cmds.resize(cnt + 1);
        for (graph.blocks.items) |*block| {
            for (block.commands.items) |cmd| {
                self.cmds.items[cmd.idx] = cmd;
            }
        }
        self.gkt = try dfa.GenKillTransfer(Command).init(
            allocator,
            graph.blocks.items.len,
            self.cmds.items.len,
            undefined,
            Self.rd_transfer,
        );

        return self;
    }
    pub fn deinit(self: *Self) void {
        self.cmds.deinit();
        self.gkt.deinit();
    }

    fn rd_transfer(
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
            var in_it = in.iterator(.{});
            while (in_it.next()) |i| {
                if (std.mem.eql(u8, self.cmds.items[i].lhs, cmd.lhs)) kill.set(i);
            }
            var gen_it = gen.iterator(.{});
            while (gen_it.next()) |i| {
                if (std.mem.eql(u8, self.cmds.items[i].lhs, cmd.lhs)) gen.unset(i);
            }

            kill.unset(cmd.idx);
            gen.set(cmd.idx);
        }
    }

    fn transfer(self: *Self) dfa.Transfer(Command) {
        self.gkt.data = @ptrCast(self);
        return self.gkt.transfer();
    }
};

const RdPrinter = struct {
    const Self = @This();
    rd: *const RdTransfer,
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
        const gen = &self.rd.gkt.gens.items[block_idx];
        const kill = &self.rd.gkt.kills.items[block_idx];

        try w.print("{{in:", .{});
        try dfa.printSet(w, in);
        try w.print("|out:", .{});
        try dfa.printSet(w, out);
        try w.print("}}|{{gen:", .{});
        try dfa.printSet(w, gen);
        try w.print("|kill:", .{});
        try dfa.printSet(w, kill);
        try w.print("}}", .{});
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};
