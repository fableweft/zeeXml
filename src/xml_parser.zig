//TODO add string interning
// add tests

const std = @import("std");
const Tokenizer = @import("xml_tokenizer.zig").xml_tokenizer;
const Event = @import("xml_event.zig").xml_event;
const Attribute = @import("xml_event.zig").Attribute;

pub fn xml_parser(comptime UnderlyingReader: type) type {
    const log = std.log.scoped(.xml_parser);

    return struct {
        allocator: std.mem.Allocator,
        tokenizer: Tokenizer(UnderlyingReader),
        open_elements: std.ArrayListUnmanaged(Element),
        namespaces: *NamespaceStack,
        root_closed: bool,
        last_detailed_error: ?DetailedError,

        pub const ParserError = error{
            UnexpectedEOF,
            UnexpectedEndTag,
            MismatchedEndTag,
            MultipleRoots,
            UnboundPrefix,
        };

        pub const ErrorKind = union(enum) {
            tokenizer: Tokenizer(UnderlyingReader).TokenizerError,
            parser: ParserError,
        };

        pub const DetailedError = struct {
            kind: ErrorKind,
            line: usize,
            column: usize,
            message: []const u8,
            context: ?Tokenizer(UnderlyingReader).ErrorContext,

            pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = fmt;
                _ = options;
                try writer.print("XML parse error at line {d}, column {d}: {s}\n", .{ self.line, self.column, self.message });
                if (self.context) |ctx| {
                    try writer.writeAll("Context: \"");
                    for (ctx.context, 0..) |char, i| {
                        if (i == ctx.error_offset) {
                            try writer.print("[{c}]", .{char});
                        } else {
                            try writer.print("{c}", .{char});
                        }
                    }
                    try writer.writeAll("\"\n");
                }
            }
        };

        fn setAndLogError(self: *Self, kind: ErrorKind, message: []const u8, line: usize, column: usize, context: ?Tokenizer(UnderlyingReader).ErrorContext) void {
            self.last_detailed_error = DetailedError{
                .kind = kind,
                .line = line,
                .column = column,
                .message = message,
                .context = context,
            };
            log.err("{}", .{self.last_detailed_error.?});
        }

        fn emitError(self: *Self, kind: ParserError, message: []const u8) void {
            self.setAndLogError(.{ .parser = kind }, message, self.tokenizer.line, self.tokenizer.column, self.tokenizer.getErrorContext(50));
        }

        // Element tracked in open_elements
        const Element = struct {
            name: []const u8,
            uri: ?[]const u8,
            local_name: []const u8,
        };

        // namespace stack
        const NamespaceStack = struct {
            bindings: std.StringHashMapUnmanaged([]const u8),
            parent: ?*NamespaceStack,
            allocator: std.mem.Allocator,

            fn init(allocator: std.mem.Allocator, parent: ?*NamespaceStack) !*NamespaceStack {
                const ns = try allocator.create(NamespaceStack);
                ns.* = .{
                    .bindings = .{},
                    .parent = parent,
                    .allocator = allocator,
                };
                return ns;
            }

            fn deinit(self: *NamespaceStack) void {
                var it = self.bindings.iterator();
                while (it.next()) |entry| {
                    self.allocator.free(entry.key_ptr.*);
                    self.allocator.free(entry.value_ptr.*);
                }
                self.bindings.deinit(self.allocator);
            }

            fn find(self: *const NamespaceStack, prefix: []const u8) ?[]const u8 {
                if (self.bindings.get(prefix)) |uri| return uri;
                if (self.parent) |p| return p.find(prefix);
                return null;
            }

            fn add(self: *NamespaceStack, prefix: []const u8, uri: []const u8) !void {
                const prefix_copy = try self.allocator.dupe(u8, prefix);
                const uri_copy = try self.allocator.dupe(u8, uri);
                try self.bindings.put(self.allocator, prefix_copy, uri_copy);
            }
        };

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator, reader: UnderlyingReader) !Self {
            const root_ns = try NamespaceStack.init(allocator, null);
            return .{
                .allocator = allocator,
                .tokenizer = try Tokenizer(UnderlyingReader).init(allocator, reader),
                .open_elements = .empty,
                .namespaces = root_ns,
                .root_closed = false,
                .last_detailed_error = null,
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.open_elements.items) |elem| {
                self.allocator.free(elem.name);
                if (elem.uri) |uri| self.allocator.free(uri);
                self.allocator.free(elem.local_name);
            }

            self.open_elements.deinit(self.allocator);

            var current_ns = self.namespaces;
            while (true) {
                const parent = current_ns.parent;
                current_ns.deinit();
                self.allocator.destroy(current_ns);
                if (parent == null) break;
                current_ns = parent.?;
            }

            self.tokenizer.deinit();
        }

        // extract prefix from name
        fn getPrefix(name: []const u8) []const u8 {
            if (std.mem.indexOfScalar(u8, name, ':')) |pos| return name[0..pos];
            return "";
        }

        // extract local name
        fn getLocalName(name: []const u8) []const u8 {
            if (std.mem.indexOfScalar(u8, name, ':')) |pos| return name[pos + 1 ..];
            return name;
        }

        // resolve namespace uri
        fn findUri(self: *Self, prefix: []const u8, is_attribute: bool) ?[]const u8 {
            if (is_attribute and std.mem.eql(u8, prefix, "xml")) return "http://www.w3.org/XML/1998/namespace";
            if (prefix.len == 0 and is_attribute) return null;
            return self.namespaces.find(prefix);
        }

        // add namespace declarations
        fn addNamespaces(self: *Self, attrs: []const Tokenizer(UnderlyingReader).Attribute) !void {
            for (attrs) |attr| {
                if (std.mem.eql(u8, attr.name, "xmlns")) {
                    try self.namespaces.add("", attr.value);
                } else if (std.mem.startsWith(u8, attr.name, "xmlns:") and attr.name.len > 6) {
                    try self.namespaces.add(attr.name[6..], attr.value);
                }
            }
        }

        //create event attributes
        fn makeAttributes(self: *Self, tokenizer_attrs: []const Tokenizer(UnderlyingReader).Attribute) ![]Attribute {
            var attrs = try self.allocator.alloc(Attribute, tokenizer_attrs.len);
            for (tokenizer_attrs, 0..) |t_attr, i| {
                const prefix = getPrefix(t_attr.name);
                const local = getLocalName(t_attr.name);
                const is_ns_attr = std.mem.eql(u8, t_attr.name, "xmlns") or std.mem.startsWith(u8, t_attr.name, "xmlns:");
                const uri = if (is_ns_attr) "http://www.w3.org/2000/xmlns/" else self.findUri(prefix, true);
                if (prefix.len > 0 and uri == null and !is_ns_attr) {
                    self.emitError(ParserError.UnboundPrefix, "Unknown namespace prefix in attribute");
                    return ParserError.UnboundPrefix;
                }
                attrs[i] = .{
                    .name = try self.allocator.dupe(u8, t_attr.name),
                    .value = try self.allocator.dupe(u8, t_attr.value),
                    .uri = if (uri) |u| try self.allocator.dupe(u8, u) else null,
                    .local_name = try self.allocator.dupe(u8, local),
                };
            }
            return attrs;
        }

        // free event memory helper since caller must free resources later
        pub fn freeEvent(self: *Self, event: Event) void {
            switch (event) {
                .start_element => |elem| {
                    self.allocator.free(elem.name);
                    if (elem.uri) |uri| self.allocator.free(uri);
                    self.allocator.free(elem.local_name);
                    for (elem.attributes) |attr| {
                        self.allocator.free(attr.name);
                        self.allocator.free(attr.value);
                        if (attr.uri) |uri| self.allocator.free(uri);
                        self.allocator.free(attr.local_name);
                    }
                    self.allocator.free(elem.attributes);
                },
                .end_element => |elem| {
                    self.allocator.free(elem.name);
                    if (elem.uri) |uri| self.allocator.free(uri);
                    self.allocator.free(elem.local_name);
                },
                .self_closing_element => |elem| {
                    self.allocator.free(elem.name);
                    if (elem.uri) |uri| self.allocator.free(uri);
                    self.allocator.free(elem.local_name);
                    for (elem.attributes) |attr| {
                        self.allocator.free(attr.name);
                        self.allocator.free(attr.value);
                        if (attr.uri) |uri| self.allocator.free(uri);
                        self.allocator.free(attr.local_name);
                    }
                    self.allocator.free(elem.attributes);
                },
                .text => |t| self.allocator.free(t.content),
                .comment => |c| self.allocator.free(c.content),
                .processing_instruction => |pi| {
                    self.allocator.free(pi.target);
                    self.allocator.free(pi.data);
                },
                .cdata => |cd| self.allocator.free(cd.content),
                .doctype => |dt| self.allocator.free(dt.declaration),
                .xml_declaration => |xd| {
                    self.allocator.free(xd.version);
                    if (xd.encoding) |e| self.allocator.free(e);
                    if (xd.standalone) |s| self.allocator.free(s);
                },
            }
        }

        pub fn nextEvent(self: *Self) !?Event {
            const maybe_token = self.tokenizer.nextToken() catch |err| {
                if (self.tokenizer.last_detailed_error) |tok_err| {
                    self.last_detailed_error = .{
                        .kind = .{ .tokenizer = tok_err.kind },
                        .line = tok_err.line,
                        .column = tok_err.column,
                        .message = tok_err.message,
                        .context = tok_err.context,
                    };
                }
                return err;
            };

            // if we got a token process it
            if (maybe_token) |token| {
                defer self.tokenizer.freeToken(token); //free token when we done with it
                switch (token) {
                    .start_tag => |tag| {
                        const prefix = getPrefix(tag.name);
                        const local = getLocalName(tag.name);

                        // push new namespace scope n add declarations first
                        const new_ns = try NamespaceStack.init(self.allocator, self.namespaces);
                        self.namespaces = new_ns;
                        try self.addNamespaces(tag.attributes);

                        // then now resolve the uri with updated namespaces
                        const uri = self.findUri(prefix, false) orelse if (prefix.len > 0) {
                            self.emitError(ParserError.UnboundPrefix, "Unknown namespace prefix in element");
                            return ParserError.UnboundPrefix;
                        } else null;

                        // keep track of open elements for later matching
                        try self.open_elements.append(self.allocator, .{
                            .name = try self.allocator.dupe(u8, tag.name),
                            .uri = if (uri) |u| try self.allocator.dupe(u8, u) else null,
                            .local_name = try self.allocator.dupe(u8, local),
                        });

                        const attrs = try self.makeAttributes(tag.attributes);

                        return Event{ .start_element = .{
                            .name = try self.allocator.dupe(u8, tag.name),
                            .uri = if (uri) |u| try self.allocator.dupe(u8, u) else null,
                            .local_name = try self.allocator.dupe(u8, local),
                            .attributes = attrs,
                        } };
                    },
                    .end_tag => |name| {
                        if (self.open_elements.items.len == 0) {
                            self.emitError(ParserError.UnexpectedEndTag, "End tag with no open elements");
                            return ParserError.UnexpectedEndTag;
                        }
                        const last = self.open_elements.pop() orelse unreachable;
                        if (!std.mem.eql(u8, last.name, name)) {
                            self.emitError(ParserError.MismatchedEndTag, "End tag does not match start tag");
                            return ParserError.MismatchedEndTag;
                        }
                        const old_ns = self.namespaces;
                        self.namespaces = old_ns.parent orelse unreachable;
                        old_ns.deinit();
                        self.allocator.destroy(old_ns);

                        if (self.open_elements.items.len == 0) self.root_closed = true;

                        const event = Event{ .end_element = .{
                            .name = try self.allocator.dupe(u8, last.name),
                            .uri = if (last.uri) |u| try self.allocator.dupe(u8, u) else null,
                            .local_name = try self.allocator.dupe(u8, last.local_name),
                        } };

                        self.allocator.free(last.name);
                        if (last.uri) |uri| self.allocator.free(uri);
                        self.allocator.free(last.local_name);

                        return event;
                    },
                    .self_closing_tag => |tag| {
                        if (self.root_closed) {
                            self.emitError(ParserError.MultipleRoots, "Extra root element found");
                            return ParserError.MultipleRoots;
                        }
                        const prefix = getPrefix(tag.name);
                        const local = getLocalName(tag.name);
                        const uri = self.findUri(prefix, false) orelse if (prefix.len > 0) {
                            self.emitError(ParserError.UnboundPrefix, "Unknown namespace prefix in element");
                            return ParserError.UnboundPrefix;
                        } else null;

                        const temp_ns = try NamespaceStack.init(self.allocator, self.namespaces);
                        defer {
                            temp_ns.deinit();
                            self.allocator.destroy(temp_ns);
                        }
                        try self.addNamespaces(tag.attributes);
                        const attrs = try self.makeAttributes(tag.attributes);

                        if (self.open_elements.items.len == 0) self.root_closed = true;

                        return Event{ .self_closing_element = .{
                            .name = try self.allocator.dupe(u8, tag.name),
                            .uri = if (uri) |u| try self.allocator.dupe(u8, u) else null,
                            .local_name = try self.allocator.dupe(u8, local),
                            .attributes = attrs,
                        } };
                    },
                    .text => |content| return Event{ .text = .{ .content = try self.allocator.dupe(u8, content) } },
                    .comment => |c| return Event{ .comment = .{ .content = try self.allocator.dupe(u8, c) } },
                    .pi => |pi| return Event{ .processing_instruction = .{
                        .target = try self.allocator.dupe(u8, pi.target),
                        .data = try self.allocator.dupe(u8, pi.data),
                    } },
                    .cdata => |cd| return Event{ .cdata = .{ .content = try self.allocator.dupe(u8, cd) } },
                    .doctype => |dt| return Event{ .doctype = .{ .declaration = try self.allocator.dupe(u8, dt) } },
                    .xml_declaration => |xd| return Event{ .xml_declaration = .{
                        .version = try self.allocator.dupe(u8, xd.version),
                        .encoding = if (xd.encoding) |e| try self.allocator.dupe(u8, e) else null,
                        .standalone = if (xd.standalone) |s| try self.allocator.dupe(u8, s) else null,
                    } },
                }
            } else {
                //if we got nothing check for unfinished elements
                if (self.open_elements.items.len > 0) {
                    self.emitError(ParserError.UnexpectedEOF, "End of file with open elements");
                    return ParserError.UnexpectedEOF;
                }
                return null; // done no more events
            }
        }
    };
}
