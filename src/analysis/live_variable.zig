const std = @import("std");
const dfa = @import("dfa.zig");
const Command = @import("../command.zig").Command;
const cfg = @import("../cfg.zig");

const Cfg = cfg.Cfg(Command);
const Block = Cfg.Block;

pub const LiveVariables = struct {
    g: *const Cfg,
    dfa: dfa.Dfa,
    lv: LvTransfer,

    const Self = @This();
    pub fn init(g: *const Cfg, allocator: std.mem.Allocator) !Self {
        var self: Self = .{
            .g = g,
            .dfa = undefined,
            .lv = try LvTransfer.init(g, allocator),
        };
        errdefer self.lv.deinit();

        var empty = try std.DynamicBitSet.initEmpty(allocator, self.lv.names.items.len);
        defer empty.deinit();

        self.dfa = try dfa.Dfa.init(
            Command,
            g,
            .Backward,
            self.lv.transfer(),
            dfa.UnionMeet.meet(),
            &empty,
            &empty,
            allocator,
        );
        return self;
    }
    pub fn deinit(self: *Self) void {
        self.dfa.deinit();
        self.lv.deinit();
    }

    pub fn dump(self: *const Self, writer: std.io.AnyWriter) !void {
        var p = LvPrinter{ .lv = &self.lv, .dfa = &self.dfa };
        try self.g.dump(writer, p.printer());
    }
};

const LvTransfer = struct {
    const Self = @This();
    gkt: dfa.GenKillTransfer(Command),
    inds: std.StringHashMap(usize),
    names: std.ArrayList([]const u8),

    pub fn init(graph: *const Cfg, allocator: std.mem.Allocator) !Self {
        var self = Self{
            .gkt = undefined,
            .inds = std.StringHashMap(usize).init(allocator),
            .names = std.ArrayList([]const u8).init(allocator),
        };
        var cnt: usize = 0;

        for (graph.blocks.items) |*b| {
            for (b.commands.items) |cmd| {
                const e1 = try self.inds.getOrPut(cmd.lhs);
                if (!e1.found_existing) {
                    e1.value_ptr.* = cnt;
                    try self.names.append(cmd.lhs);
                    cnt += 1;
                }
                const e2 = try self.inds.getOrPut(cmd.rhs.a);
                if (!e2.found_existing) {
                    e2.value_ptr.* = cnt;
                    try self.names.append(cmd.rhs.a);
                    cnt += 1;
                }

                if (cmd.rhs.op != .None) {
                    const e3 = try self.inds.getOrPut(cmd.rhs.b);
                    if (!e3.found_existing) {
                        e3.value_ptr.* = cnt;
                        try self.names.append(cmd.rhs.b);
                        cnt += 1;
                    }
                }
            }
        }

        self.gkt = try dfa.GenKillTransfer(Command).init(
            allocator,
            graph.blocks.items.len,
            cnt,
            undefined,
            Self.lv_transfer,
        );

        return self;
    }
    pub fn deinit(self: *Self) void {
        self.gkt.deinit();
        self.inds.deinit();
        self.names.deinit();
    }

    fn lv_transfer(
        data: *anyopaque,
        block_idx: usize,
        block: *const Block,
        out: *const std.DynamicBitSet,
        use: *std.DynamicBitSet,
        def: *std.DynamicBitSet,
    ) void {
        const self: *Self = @alignCast(@ptrCast(data));
        std.debug.assert(self.gkt.data == @as(*anyopaque, @ptrCast(self)));

        _ = out;
        _ = block_idx;
        const cnt = def.unmanaged.bit_length;
        dfa.initSetToEmpty(def, cnt) catch unreachable;
        dfa.initSetToEmpty(use, cnt) catch unreachable;
        for (block.commands.items, 0..) |_, i| {
            const cmd = block.commands.items[block.commands.items.len - i - 1];
            const lhs = self.inds.get(cmd.lhs).?;
            const a = self.inds.get(cmd.rhs.a).?;
            use.unset(lhs);
            def.unset(a);
            if (cmd.rhs.op != .None) {
                const b = self.inds.get(cmd.rhs.b).?;
                def.unset(b);
                use.set(b);
            }

            use.set(a);
            def.set(lhs);
        }
    }
    fn transfer(self: *Self) dfa.Transfer(Command) {
        self.gkt.data = @ptrCast(self);
        return self.gkt.transfer();
    }
};

const LvPrinter = struct {
    const Self = @This();
    lv: *const LvTransfer,
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
        const use = &self.lv.gkt.gens.items[block_idx];
        const def = &self.lv.gkt.kills.items[block_idx];

        try w.print("{{in:", .{});
        try self.printCharSet(w, in);
        try w.print("|out:", .{});
        try self.printCharSet(w, out);
        try w.print("}}|{{use:", .{});
        try self.printCharSet(w, use);
        try w.print("|def:", .{});
        try self.printCharSet(w, def);
        try w.print("}}", .{});
    }

    fn printCharSet(self: *const Self, w: std.io.AnyWriter, set: *const std.DynamicBitSet) !void {
        try w.print("[", .{});
        var it = set.iterator(.{});
        while (it.next()) |c| try w.print("{s}, ", .{self.lv.names.items[c]});
        try w.print("]", .{});
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};
