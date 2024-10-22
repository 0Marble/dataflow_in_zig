const std = @import("std");
const dfa = @import("dfa.zig");
const Command = @import("../command.zig").Command;
const Expr = @import("../command.zig").Expr;
const cfg = @import("../cfg.zig");

const Cfg = cfg.Cfg(Command);
const Block = Cfg.Block;

pub const LcmCommon = struct {
    exprs: std.ArrayList(Expr),
    expr_ids: std.ArrayList(usize),
    g: Cfg,
    allocator: std.mem.Allocator,
    full: std.DynamicBitSet,
    empty: std.DynamicBitSet,

    const Self = @This();
    pub fn init(g: *const Cfg, allocator: std.mem.Allocator) !Self {
        var self = Self{
            .exprs = std.ArrayList(Expr).init(allocator),
            .expr_ids = std.ArrayList(usize).init(allocator),
            .g = undefined,
            .allocator = allocator,
            .full = undefined,
            .empty = undefined,
        };
        errdefer {
            self.exprs.deinit();
            self.expr_ids.deinit();
        }
        self.g = try Self.preprocess(g, allocator);
        errdefer self.g.deinit();

        var cnt: usize = 0;
        for (self.g.blocks.items) |*block| {
            for (block.commands.items) |cmd| {
                if (cmd.idx > cnt) cnt = cmd.idx;
            }
        }
        try self.expr_ids.resize(cnt + 1);

        for (self.g.blocks.items) |*block| {
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

        self.full = try std.DynamicBitSet.initFull(allocator, self.exprs.items.len);
        errdefer self.full.deinit();
        self.empty = try std.DynamicBitSet.initEmpty(allocator, self.exprs.items.len);
        errdefer self.empty.deinit();

        return self;
    }

    fn preprocess(g: *const Cfg, allocator: std.mem.Allocator) !Cfg {
        var res = try Cfg.init(allocator);
        errdefer res.deinit();
        const Pair = struct { usize, usize };
        var map = std.ArrayList(Pair).init(allocator);
        defer map.deinit();
        try map.resize(g.blocks.items.len);
        map.items[g.entry] = .{ res.entry, res.entry };
        map.items[g.exit] = .{ res.exit, res.exit };

        for (g.blocks.items, 0..) |*b, old_i| {
            if (old_i == g.exit or old_i == g.entry) continue;

            const i = try res.addBlock();
            var prev = i;
            if (b.commands.items.len != 0) {
                try res.addCommand(b.commands.items[0], i);

                for (1..b.commands.items.len) |k| {
                    const j = try res.addBlock();
                    try res.addCommand(b.commands.items[k], j);
                    try res.connectBlocks(prev, j);
                    prev = j;
                }
            }

            map.items[old_i] = Pair{ i, prev };
        }
        for (g.blocks.items, 0..) |_, old_i| {
            for (g.succs.items[old_i].items) |j| {
                var start = map.items[old_i][1];
                if (g.preds.items[j].items.len > 1) {
                    const extra = try res.addBlock();
                    try res.connectBlocks(start, extra);
                    start = extra;
                }
                try res.connectBlocks(start, map.items[j][0]);
            }
        }

        return res;
    }

    pub fn deinit(self: *Self) void {
        self.full.deinit();
        self.empty.deinit();
        self.exprs.deinit();
        self.expr_ids.deinit();
        self.g.deinit();
    }
    pub fn printExprSet(self: *const Self, w: std.io.AnyWriter, set: *const std.DynamicBitSet) !void {
        try w.print("[", .{});

        var it = set.iterator(.{});
        while (it.next()) |x| {
            const expr = self.exprs.items[x];
            try w.print("'{}', ", .{expr});
        }

        try w.print("]", .{});
    }

    pub fn kills(self: *const Self, cmd: Command, tgt: usize) bool {
        const expr = self.exprs.items[tgt];
        return std.mem.eql(u8, cmd.lhs, expr.a) or expr.op != .None and std.mem.eql(u8, cmd.lhs, expr.b);
    }
};

pub const AnticipatedExpressions = struct {
    gkt: dfa.GenKillTransfer(Command),
    dfa: dfa.Dfa,
    common: *const LcmCommon,

    pub fn init(common: *const LcmCommon) !Self {
        var self = Self{
            .gkt = undefined,
            .dfa = undefined,
            .common = common,
        };
        self.gkt = try dfa.GenKillTransfer(Command).init(
            common.allocator,
            common.g.blocks.items.len,
            self.common.exprs.items.len,
            &self,
            Self.transfer,
        );
        errdefer self.gkt.deinit();

        self.dfa = try dfa.Dfa.init(
            Command,
            &self.common.g,
            .Backward,
            self.gkt.transfer(),
            dfa.IntersectMeet.meet(),
            &common.empty,
            &common.full,
            common.allocator,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.gkt.deinit();
        self.dfa.deinit();
    }

    const Self = @This();
    fn transfer(
        data: *anyopaque,
        block_idx: usize,
        block: *const Block,
        in: *const std.DynamicBitSet,
        use: *std.DynamicBitSet,
        kill: *std.DynamicBitSet,
    ) void {
        _ = block_idx;
        const self: *Self = @alignCast(@ptrCast(data));
        std.debug.assert(self.gkt.data == @as(*anyopaque, @ptrCast(self)));

        for (block.commands.items, 0..) |_, i| {
            const cmd = block.commands.items[block.commands.items.len - 1 - i];
            const expr_id = self.common.expr_ids.items[cmd.idx];

            var in_it = in.iterator(.{});
            while (in_it.next()) |idx| {
                if (self.common.kills(cmd, idx)) {
                    kill.set(idx);
                }
            }

            var use_it = use.iterator(.{});
            while (use_it.next()) |idx| {
                if (self.common.kills(cmd, idx)) {
                    use.unset(idx);
                }
            }

            use.set(expr_id);
        }
    }

    fn print(
        data: *anyopaque,
        w: std.io.AnyWriter,
        block: *const Cfg.Block,
        block_idx: usize,
    ) anyerror!void {
        const self: *Self = @alignCast(@ptrCast(data));
        _ = block;

        const in = &self.dfa.ins.items[block_idx];
        try w.print("anticipated:", .{});
        try self.common.printExprSet(w, in);
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};

pub const AvailableExpressions = struct {
    common: *const LcmCommon,
    anticipated: *const AnticipatedExpressions,
    dfa: dfa.Dfa,

    const Self = @This();
    pub fn init(anticipated: *const AnticipatedExpressions) !Self {
        var self = Self{
            .common = anticipated.common,
            .anticipated = anticipated,
            .dfa = undefined,
        };

        self.dfa = try dfa.Dfa.init(
            Command,
            &self.common.g,
            .Forward,
            dfa.Transfer(Command){ .data = @ptrCast(&self), .fptr = Self.transfer },
            dfa.IntersectMeet.meet(),
            &self.common.empty,
            &self.common.full,
            self.common.allocator,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.dfa.deinit();
    }

    fn transfer(
        data: *anyopaque,
        block_idx: usize,
        block: *const cfg.Cfg(Command).Block,
        out: *std.DynamicBitSet,
        in: *const std.DynamicBitSet,
    ) void {
        const self: *Self = @alignCast(@ptrCast(data));

        dfa.copySet(out, in) catch unreachable;
        out.setUnion(self.anticipated.dfa.ins.items[block_idx]);

        for (block.commands.items) |cmd| {
            var it = out.iterator(.{});
            while (it.next()) |idx| {
                if (self.common.kills(cmd, idx)) out.unset(idx);
            }
        }
    }

    fn print(
        data: *anyopaque,
        w: std.io.AnyWriter,
        block: *const Cfg.Block,
        block_idx: usize,
    ) anyerror!void {
        const self: *Self = @alignCast(@ptrCast(data));
        _ = block;

        const in = &self.dfa.ins.items[block_idx];
        try w.print("available:", .{});
        try self.common.printExprSet(w, in);
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};

pub const PostponableExpressions = struct {
    earliest: std.ArrayList(std.DynamicBitSet),
    dfa: dfa.Dfa,
    common: *const LcmCommon,

    const Self = @This();
    pub fn init(
        anticipated: *const AnticipatedExpressions,
        available: *const AvailableExpressions,
        common: *const LcmCommon,
    ) !Self {
        var self = Self{
            .earliest = std.ArrayList(std.DynamicBitSet).init(common.allocator),
            .common = common,
            .dfa = undefined,
        };
        errdefer {
            for (self.earliest.items) |*x| x.deinit();
            self.earliest.deinit();
        }

        for (available.dfa.ins.items, anticipated.dfa.ins.items) |av, an| {
            var earliest = try std.DynamicBitSet.initEmpty(common.allocator, av.unmanaged.bit_length);
            earliest.setUnion(av);
            earliest.toggleAll();
            earliest.setIntersection(an);
            try self.earliest.append(earliest);
        }

        self.dfa = try dfa.Dfa.init(
            Command,
            &self.common.g,
            .Forward,
            dfa.Transfer(Command){ .data = @ptrCast(&self), .fptr = Self.transfer },
            dfa.IntersectMeet.meet(),
            &self.common.empty,
            &self.common.full,
            self.common.allocator,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        for (self.earliest.items) |*x| x.deinit();
        self.earliest.deinit();
        self.dfa.deinit();
    }

    fn transfer(
        data: *anyopaque,
        block_idx: usize,
        block: *const cfg.Cfg(Command).Block,
        out: *std.DynamicBitSet,
        in: *const std.DynamicBitSet,
    ) void {
        const self: *Self = @alignCast(@ptrCast(data));

        dfa.copySet(out, in) catch unreachable;
        out.setUnion(self.earliest.items[block_idx]);

        for (block.commands.items) |cmd| {
            out.unset(self.common.expr_ids.items[cmd.idx]);
        }
    }

    fn print(
        data: *anyopaque,
        w: std.io.AnyWriter,
        block: *const Cfg.Block,
        block_idx: usize,
    ) anyerror!void {
        const self: *Self = @alignCast(@ptrCast(data));
        _ = block;

        const in = &self.dfa.ins.items[block_idx];
        const earliest = &self.earliest.items[block_idx];
        try w.print("postponable:", .{});
        try self.common.printExprSet(w, in);
        try w.print("|earliest:", .{});
        try self.common.printExprSet(w, earliest);
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};

pub const UsedExpressions = struct {
    latest: std.ArrayList(std.DynamicBitSet),
    dfa: dfa.Dfa,
    common: *const LcmCommon,

    const Self = @This();
    pub fn init(
        postponable: *const PostponableExpressions,
        common: *const LcmCommon,
    ) !Self {
        var self = Self{
            .latest = std.ArrayList(std.DynamicBitSet).init(common.allocator),
            .common = common,
            .dfa = undefined,
        };
        errdefer {
            for (self.latest.items) |*x| x.deinit();
            self.latest.deinit();
        }

        const len = common.exprs.items.len;
        var temp = try std.DynamicBitSet.initEmpty(common.allocator, len);
        defer temp.deinit();

        for (common.g.blocks.items, 0..) |*b, i| {
            var latest = try std.DynamicBitSet.initEmpty(common.allocator, len);
            const succs = common.g.succs.items[i].items;
            if (succs.len != 0) {
                try dfa.copySet(&latest, &postponable.earliest.items[succs[0]]);
                latest.setUnion(postponable.dfa.ins.items[succs[0]]);
                std.debug.assert(latest.unmanaged.bit_length == len);

                for (1..succs.len) |j| {
                    try dfa.copySet(&temp, &postponable.earliest.items[succs[j]]);
                    temp.setUnion(postponable.dfa.ins.items[succs[j]]);
                    latest.setIntersection(temp);
                }
            }
            latest.toggleAll();

            for (b.commands.items) |cmd| {
                latest.set(common.expr_ids.items[cmd.idx]);
            }

            try dfa.copySet(&temp, &postponable.earliest.items[i]);
            temp.setUnion(postponable.dfa.ins.items[i]);

            latest.setIntersection(temp);

            try self.latest.append(latest);
        }

        self.dfa = try dfa.Dfa.init(
            Command,
            &self.common.g,
            .Backward,
            dfa.Transfer(Command){ .data = @ptrCast(&self), .fptr = Self.transfer },
            dfa.UnionMeet.meet(),
            &self.common.empty,
            &self.common.empty,
            self.common.allocator,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        for (self.latest.items) |*x| x.deinit();
        self.latest.deinit();
        self.dfa.deinit();
    }

    fn transfer(
        data: *anyopaque,
        block_idx: usize,
        block: *const cfg.Cfg(Command).Block,
        out: *std.DynamicBitSet,
        in: *const std.DynamicBitSet,
    ) void {
        const self: *Self = @alignCast(@ptrCast(data));

        dfa.copySet(out, in) catch unreachable;

        for (block.commands.items, 0..) |_, i| {
            const cmd = block.commands.items[block.commands.items.len - 1 - i];
            out.set(self.common.expr_ids.items[cmd.idx]);
        }
        const latest = &self.latest.items[block_idx];
        latest.toggleAll();
        out.setIntersection(latest.*);
        latest.toggleAll();
    }

    fn print(
        data: *anyopaque,
        w: std.io.AnyWriter,
        block: *const Cfg.Block,
        block_idx: usize,
    ) anyerror!void {
        const self: *Self = @alignCast(@ptrCast(data));
        _ = block;

        const out = &self.dfa.outs.items[block_idx];
        const latest = &self.latest.items[block_idx];
        try w.print("used_out:", .{});
        try self.common.printExprSet(w, out);
        try w.print("|latest:", .{});
        try self.common.printExprSet(w, latest);
    }

    pub fn printer(self: *Self) cfg.PrintBlockMetadata(Command) {
        return .{
            .data = @ptrCast(self),
            .fptr = Self.print,
        };
    }
};

pub const LazyCodeMotion = struct {
    anticipated: AnticipatedExpressions,
    available: AvailableExpressions,
    postponable: PostponableExpressions,
    used: UsedExpressions,
    common: *LcmCommon,

    const Self = @This();
    pub fn init(g: *const Cfg, allocator: std.mem.Allocator) !Self {
        const common = try allocator.create(LcmCommon);
        common.* = try LcmCommon.init(g, allocator);

        var self: Self = undefined;
        self.common = common;
        errdefer common.allocator.destroy(common);

        self.anticipated = try AnticipatedExpressions.init(self.common);
        errdefer self.anticipated.deinit();
        self.available = try AvailableExpressions.init(&self.anticipated);
        errdefer self.available.deinit();
        self.postponable = try PostponableExpressions.init(
            &self.anticipated,
            &self.available,
            self.common,
        );
        errdefer self.postponable.deinit();
        self.used = try UsedExpressions.init(&self.postponable, self.common);
        errdefer self.used.deinit();

        return self;
    }

    pub fn deinit(self: *Self) void {
        const alloc = self.common.allocator;
        self.common.deinit();
        alloc.destroy(self.common);
        self.anticipated.deinit();
        self.available.deinit();
        self.postponable.deinit();
        self.used.deinit();
    }

    fn print(
        data: *anyopaque,
        w: std.io.AnyWriter,
        block: *const Cfg.Block,
        block_idx: usize,
    ) anyerror!void {
        const self: *Self = @alignCast(@ptrCast(data));

        try AnticipatedExpressions.print(@ptrCast(&self.anticipated), w, block, block_idx);
        try w.print("|", .{});
        try AvailableExpressions.print(@ptrCast(&self.available), w, block, block_idx);
        try w.print("|", .{});
        try PostponableExpressions.print(@ptrCast(&self.postponable), w, block, block_idx);
        try w.print("|", .{});
        try UsedExpressions.print(@ptrCast(&self.used), w, block, block_idx);
    }

    pub fn dump(self: *Self, writer: std.io.AnyWriter) !void {
        const p = cfg.PrintBlockMetadata(Command){ .data = @ptrCast(self), .fptr = Self.print };
        try self.common.g.dump(writer, p);
    }
};
