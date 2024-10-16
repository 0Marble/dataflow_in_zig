const std = @import("std");
const assert = std.debug.assert;

const cfg = @import("cfg.zig");
const dfa = @import("dfa.zig");

const Op = enum { Add, Sub, Mul, Div, None };

const Command = struct {
    lhs: u8,
    a: u8,
    b: u8,
    op: Op,
    idx: usize,

    pub fn format(self: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{c} = {c}", .{ self.lhs, self.a });
        switch (self.op) {
            .Add => try writer.print(" + {c}", .{self.b}),
            .Sub => try writer.print(" - {c}", .{self.b}),
            .Mul => try writer.print(" * {c}", .{self.b}),
            .Div => try writer.print(" / {c}", .{self.b}),
            .None => {},
        }
    }
};

const Cfg = cfg.Cfg(Command);
const Block = Cfg.Block;

pub const RdTransfer = struct {
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
                if (self.cmds.items[i].lhs == cmd.lhs) kill.set(i);
            }
            var gen_it = gen.iterator(.{});
            while (gen_it.next()) |i| {
                if (self.cmds.items[i].lhs == cmd.lhs) gen.unset(i);
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

const AeTransfer = struct {
    const BinExpr = struct {
        u8,
        Op,
        u8,
    };
    fn eql(self: BinExpr, other: BinExpr) bool {
        return self[0] == other[0] and self[1] == other[1] and self[2] == other[2];
    }
    exprs: std.ArrayList(BinExpr),
    expr_ids: std.ArrayList(usize),
    gkt: dfa.GenKillTransfer(Command),

    pub fn init(graph: *Cfg, allocator: std.mem.Allocator) !Self {
        var self = Self{
            .gkt = undefined,
            .exprs = std.ArrayList(BinExpr).init(allocator),
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
                const expr = BinExpr{ cmd.a, cmd.op, cmd.b };
                for (self.exprs.items, 0..) |other, idx| {
                    if (eql(expr, other)) {
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
                if (expr[0] == cmd.lhs or expr[1] != .None and expr[2] == cmd.lhs) {
                    kill.set(i);
                }
            }
            var gen_it = gen.iterator(.{});
            while (gen_it.next()) |i| {
                const expr = self.exprs.items[i];
                if (expr[0] == cmd.lhs or expr[1] != .None and expr[2] == cmd.lhs) {
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

pub const LvTransfer = struct {
    const Self = @This();
    gkt: dfa.GenKillTransfer(Command),

    pub fn init(graph: *Cfg, allocator: std.mem.Allocator) !Self {
        var self = Self{ .gkt = undefined };
        self.gkt = try dfa.GenKillTransfer(Command).init(
            allocator,
            graph.blocks.items.len,
            256,
            undefined,
            Self.lv_transfer,
        );

        return self;
    }
    pub fn deinit(self: *Self) void {
        self.gkt.deinit();
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
            use.unset(cmd.lhs);
            def.unset(cmd.a);
            if (cmd.op != .None) def.unset(cmd.b);

            if (std.ascii.isAlphabetic(cmd.a)) use.set(cmd.a);
            if (cmd.op != .None and std.ascii.isAlphabetic(cmd.b)) use.set(cmd.b);
            def.set(cmd.lhs);
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
            try w.print("'{c}", .{expr[0]});
            switch (expr[1]) {
                .Add => try w.print(" + {c}', ", .{expr[2]}),
                .Sub => try w.print(" - {c}', ", .{expr[2]}),
                .Mul => try w.print(" * {c}', ", .{expr[2]}),
                .Div => try w.print(" / {c}', ", .{expr[2]}),
                .None => try w.print("', ", .{}),
            }
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
        _ = self;
        try w.print("[", .{});
        var it = set.iterator(.{});
        while (it.next()) |c| try w.print("{c}, ", .{@as(u8, @intCast(c))});
        try w.print("]", .{});
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.print("GPA: {}\n", .{gpa.deinit()});

    var graph = try cfg.Cfg(Command).init(gpa.allocator());
    defer graph.deinit();

    var commands = try std.ArrayList(Command).initCapacity(gpa.allocator(), 12);
    defer commands.deinit();
    try commands.append(undefined);

    try commands.append(.{ .lhs = 'a', .a = '1', .op = .None, .b = 0, .idx = 1 });
    try commands.append(.{ .lhs = 'b', .a = '2', .op = .None, .b = 0, .idx = 2 });
    try commands.append(.{ .lhs = 'c', .a = 'a', .op = .Add, .b = 'b', .idx = 3 });
    try commands.append(.{ .lhs = 'd', .a = 'c', .op = .Sub, .b = 'a', .idx = 4 });
    try commands.append(.{ .lhs = 'd', .a = 'b', .op = .Add, .b = 'd', .idx = 5 });
    try commands.append(.{ .lhs = 'd', .a = 'a', .op = .Add, .b = 'b', .idx = 6 });
    try commands.append(.{ .lhs = 'e', .a = 'e', .op = .Add, .b = '1', .idx = 7 });
    try commands.append(.{ .lhs = 'b', .a = 'a', .op = .Add, .b = 'd', .idx = 8 });
    try commands.append(.{ .lhs = 'e', .a = 'c', .op = .Sub, .b = 'a', .idx = 9 });
    try commands.append(.{ .lhs = 'a', .a = 'b', .op = .Mul, .b = 'd', .idx = 10 });
    try commands.append(.{ .lhs = 'b', .a = 'a', .op = .Sub, .b = 'd', .idx = 11 });

    const b1 = try graph.addBlock();
    try graph.addCommand(commands.items[1], b1);
    try graph.addCommand(commands.items[2], b1);
    const b2 = try graph.addBlock();
    try graph.addCommand(commands.items[3], b2);
    try graph.addCommand(commands.items[4], b2);
    const b3 = try graph.addBlock();
    try graph.addCommand(commands.items[5], b3);
    const b4 = try graph.addBlock();
    try graph.addCommand(commands.items[6], b4);
    try graph.addCommand(commands.items[7], b4);
    const b5 = try graph.addBlock();
    try graph.addCommand(commands.items[8], b5);
    try graph.addCommand(commands.items[9], b5);
    const b6 = try graph.addBlock();
    try graph.addCommand(commands.items[10], b6);
    try graph.addCommand(commands.items[11], b6);
    try graph.setEntry(b1);
    try graph.connectBlocks(b1, b2);
    try graph.connectBlocks(b2, b3);
    try graph.connectBlocks(b3, b4);
    try graph.connectBlocks(b4, b3);
    try graph.connectBlocks(b3, b5);
    try graph.connectBlocks(b5, b2);
    try graph.connectBlocks(b5, b6);
    try graph.setExit(b6);

    {
        var rd = try RdTransfer.init(&graph, gpa.allocator());
        defer rd.deinit();

        var empty = try std.DynamicBitSet.initEmpty(gpa.allocator(), rd.cmds.items.len);
        defer empty.deinit();

        var res = try dfa.Dfa.init(
            Command,
            &graph,
            .Forward,
            rd.transfer(),
            dfa.UnionMeet.meet(),
            &empty,
            &empty,
            gpa.allocator(),
        );
        defer res.deinit();

        var f = try std.fs.cwd().createFile("rd.dot", .{});
        defer f.close();
        var printer = RdPrinter{ .rd = &rd, .dfa = &res };
        try graph.dump(f.writer().any(), printer.printer());
    }
    {
        var ae = try AeTransfer.init(&graph, gpa.allocator());
        defer ae.deinit();

        var empty = try std.DynamicBitSet.initEmpty(gpa.allocator(), ae.exprs.items.len);
        defer empty.deinit();
        var full = try std.DynamicBitSet.initFull(gpa.allocator(), ae.exprs.items.len);
        defer full.deinit();

        var res = try dfa.Dfa.init(
            Command,
            &graph,
            .Forward,
            ae.transfer(),
            dfa.IntersectMeet.meet(),
            &empty,
            &full,
            gpa.allocator(),
        );
        defer res.deinit();

        var f = try std.fs.cwd().createFile("ae.dot", .{});
        defer f.close();
        var printer = AePrinter{ .ae = &ae, .dfa = &res };
        try graph.dump(f.writer().any(), printer.printer());
    }
    {
        var lv = try LvTransfer.init(&graph, gpa.allocator());
        defer lv.deinit();

        var empty = try std.DynamicBitSet.initEmpty(gpa.allocator(), 256);
        defer empty.deinit();

        var res = try dfa.Dfa.init(
            Command,
            &graph,
            .Backward,
            lv.transfer(),
            dfa.UnionMeet.meet(),
            &empty,
            &empty,
            gpa.allocator(),
        );
        defer res.deinit();

        var f = try std.fs.cwd().createFile("lv.dot", .{});
        defer f.close();
        var printer = LvPrinter{ .lv = &lv, .dfa = &res };
        try graph.dump(f.writer().any(), printer.printer());
    }
}
