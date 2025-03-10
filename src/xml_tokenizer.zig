const std = @import("std");

pub fn xml_tokenizer(comptime UnderlyingReader: type) type {
    return struct {
        buffered_reader: std.io.BufferedReader(8192, UnderlyingReader), // 8KB buffered reader
        allocator: std.mem.Allocator,
        buffer: std.ArrayList(u8),
        line: usize,
        column: usize,
        pos: usize,
        last_detailed_error: ?DetailedError = null,
        expecting_cr_lf: bool = false,

        pub const TokenizerError = error{
            UnexpectedEOF,
            InvalidToken,
            InvalidUTF8,
            InvalidCharacter,
            MalformedComment,
            MalformedProcessingInstruction,
            ContentTooLong,
            UnterminatedCDATASection,
            InvalidEntity,
            UnterminatedEntity,
        };

        pub const ErrorContext = struct {
            context: []const u8,
            error_offset: usize,
        };

        pub const DetailedError = struct {
            kind: TokenizerError,
            line: usize,
            column: usize,
            message: []const u8,
            context: ?ErrorContext,

            pub fn format(self: DetailedError) void {
                std.debug.print("XML error at line {d}, column {d}: {s}\n", .{ self.line, self.column, self.message });
                if (self.context) |ctx| {
                    std.debug.print("Context: \"", .{});
                    for (ctx.context, 0..) |char, i| {
                        if (i == ctx.error_offset) {
                            std.debug.print("[{c}]", .{char});
                        } else {
                            std.debug.print("{c}", .{char});
                        }
                    }
                    std.debug.print("\"\n", .{});
                }
            }
        };

        pub fn getErrorContext(self: *Self, window_size: usize) ?ErrorContext {
            if (self.buffer.items.len == 0) return null;
            const pos = @min(self.pos, self.buffer.items.len);
            const start = if (pos > window_size) pos - window_size else 0;
            const end = @min(self.buffer.items.len, pos + window_size);
            const context = self.buffer.items[start..end];
            const error_offset = pos - start; // offset of error within contex
            return ErrorContext{ .context = context, .error_offset = error_offset };
        }

        pub fn emitError(self: *Self, kind: TokenizerError, message: []const u8) void {
            self.last_detailed_error = DetailedError{
                .kind = kind,
                .line = self.line,
                .column = self.column,
                .message = message,
                .context = self.getErrorContext(50),
            };
        }

        pub const Attribute = struct {
            name: []u8,
            value: []u8,
        };

        pub const Token = union(enum) {
            start_tag: struct { name: []u8, attributes: []Attribute },
            end_tag: []u8,
            self_closing_tag: struct { name: []u8, attributes: []Attribute },
            text: []u8,
            comment: []u8,
            pi: struct { target: []u8, data: []u8 },
            cdata: []u8,
            doctype: []u8,
            xml_declaration: struct { version: []u8, encoding: ?[]u8, standalone: ?[]u8 },
        };

        const Self = @This();
        pub const default_chunk_size = 8192;

        pub fn init(allocator: std.mem.Allocator, underlying_reader: UnderlyingReader) !Self {
            return Self{
                .buffered_reader = std.io.bufferedReaderSize(default_chunk_size, underlying_reader),
                .allocator = allocator,
                .buffer = try std.ArrayList(u8).initCapacity(allocator, default_chunk_size),
                .line = 1,
                .column = 1,
                .pos = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.buffer.deinit();
        }

        pub fn reader(self: *Self) std.io.BufferedReader(default_chunk_size, UnderlyingReader).Reader {
            return self.buffered_reader.reader();
        }

        pub fn readNextChunk(self: *Self) !bool {
            if (self.pos >= self.buffer.items.len) {
                try self.buffer.resize(0);
                self.pos = 0;
            }
            // if there is unprocessed data shift it to beginning
            else if (self.pos > 0) {
                const remaining = self.buffer.items[self.pos..];
                std.mem.copyForwards(u8, self.buffer.items[0..remaining.len], remaining);
                try self.buffer.resize(remaining.len);
                self.pos = 0;
            }

            try self.buffer.ensureUnusedCapacity(default_chunk_size);

            // read new data into buffer starting at the current end
            const end_pos = self.buffer.items.len;
            const available_space = self.buffer.capacity - end_pos;
            const read_slice = self.buffer.allocatedSlice()[end_pos..][0..@min(available_space, default_chunk_size)];
            const bytes_read = try self.buffered_reader.reader().read(read_slice);

            if (bytes_read == 0) return false;

            // resize buffer to include the newly read data
            try self.buffer.resize(end_pos + bytes_read);
            return true;
        }

        pub fn getCurrentBufferContents(self: *Self) []const u8 {
            return self.buffer.items;
        }

        // get the next token from the XML stream or return null at EOF
        pub fn nextToken(self: *Self) !?Token {
            try self.skipWhitespace();
            if (self.pos >= self.buffer.items.len) {
                const has_more_data_data = try self.readNextChunk();
                if (!has_more_data_data)
                    return null; // end of input my guy just come back lol
            }

            const c = self.buffer.items[self.pos];
            if (c == '<') { // is start of a tag or special construct
                self.pos += 1;
                self.column += 1;
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data_data = try self.readNextChunk();
                    if (!has_more_data_data) {
                        self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file while parsing tag");
                        return TokenizerError.UnexpectedEOF; // legit EOF mid tag
                    }
                }
                if (self.pos >= self.buffer.items.len) {
                    self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file after '<'");
                    return TokenizerError.UnexpectedEOF; // shouldnt happen after read
                }
                switch (self.buffer.items[self.pos]) {
                    '/' => { // end tag
                        self.pos += 1;
                        self.column += 1;
                        return try self.parseEndTag();
                    },
                    '!' => { // comment or other special token
                        self.pos += 1;
                        self.column += 1;
                        if (self.pos + 7 < self.buffer.items.len and std.mem.eql(u8, self.buffer.items[self.pos .. self.pos + 7], "[CDATA[")) {
                            self.pos += 7;
                            self.column += 7;
                            return try self.parseCdata();
                        } else if (self.pos + 7 < self.buffer.items.len and std.mem.eql(u8, self.buffer.items[self.pos .. self.pos + 7], "DOCTYPE")) {
                            self.pos += 7;
                            self.column += 7;
                            return try self.parseDoctype();
                        } else if (self.pos + 1 < self.buffer.items.len and self.buffer.items[self.pos] == '-' and self.buffer.items[self.pos + 1] == '-') {
                            self.pos += 2; // skip <!--
                            self.column += 2;
                            return try self.parseComment();
                        }
                        self.emitError(TokenizerError.InvalidToken, "Invalid character after '<'");
                        return TokenizerError.InvalidToken;
                    },
                    '?' => { // processing instruction
                        self.pos += 1;
                        self.column += 1;
                        return try self.parsePi();
                    },
                    else => return try self.parseStartTag(), // start or self closing tag
                }
            } else {
                return try self.parseText(); // text content
            }
        }

        pub fn parseStartTag(self: *Self) !Token {
            const name = try self.readName();
            if (name.len == 0) {
                self.emitError(TokenizerError.InvalidToken, "Empty tag name");
                return TokenizerError.InvalidToken;
            }
            errdefer self.allocator.free(name);
            var attributes = std.ArrayList(Self.Attribute).init(self.allocator);
            errdefer {
                for (attributes.items) |attr| {
                    self.allocator.free(attr.name);
                    self.allocator.free(attr.value);
                }
                attributes.deinit();
            }

            while (true) {
                try self.skipWhitespace();
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file while parsing start tag");
                        return TokenizerError.UnexpectedEOF;
                    }
                }
                if (self.pos >= self.buffer.items.len) {
                    self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file after reading attributes");
                    return TokenizerError.UnexpectedEOF;
                }
                const c = self.buffer.items[self.pos];
                if (c == '>') {
                    self.pos += 1;
                    self.column += 1;
                    return Token{ .start_tag = .{ .name = name, .attributes = try attributes.toOwnedSlice() } };
                } else if (c == '/' and self.pos + 1 < self.buffer.items.len and self.buffer.items[self.pos + 1] == '>') {
                    self.pos += 2;
                    self.column += 2;
                    return Token{ .self_closing_tag = .{ .name = name, .attributes = try attributes.toOwnedSlice() } };
                } else {
                    const attr_name = try self.readName();
                    if (attr_name.len == 0) {
                        self.emitError(TokenizerError.InvalidToken, "Empty attribute name");
                        return TokenizerError.InvalidToken;
                    }
                    errdefer self.allocator.free(attr_name);

                    try self.skipWhitespace();
                    if (self.pos >= self.buffer.items.len) {
                        std.log.warn("Attribute '{s}' has no value due to EOF", .{attr_name});
                        const attribute = Self.Attribute{ .name = attr_name, .value = try self.allocator.dupe(u8, "") };
                        {
                            errdefer {
                                self.allocator.free(attribute.name);
                                self.allocator.free(attribute.value);
                            }
                            try attributes.append(attribute);
                        }
                        continue;
                    }

                    const next_c = self.buffer.items[self.pos];
                    if (next_c != '=') {
                        std.log.warn("Attribute '{s}' has no value", .{attr_name});
                        const attribute = Self.Attribute{ .name = attr_name, .value = try self.allocator.dupe(u8, "") };
                        {
                            errdefer {
                                self.allocator.free(attribute.name);
                                self.allocator.free(attribute.value);
                            }
                            try attributes.append(attribute);
                        }
                        continue;
                    }
                    self.pos += 1; // skip '='
                    self.column += 1;

                    try self.skipWhitespace();
                    const string_result = try self.readAttributeValue();
                    errdefer self.allocator.free(string_result.value);
                    const value = string_result.value;
                    const attribute = Self.Attribute{ .name = attr_name, .value = value };
                    {
                        errdefer {
                            self.allocator.free(attribute.name);
                            self.allocator.free(attribute.value);
                        }
                        try attributes.append(attribute);
                    }
                }
            }
        }

        pub fn readAttributeValue(self: *Self) !struct { value: []u8, start_line: usize, start_column: usize } {
            if (self.pos >= self.buffer.items.len or (self.buffer.items[self.pos] != '"' and self.buffer.items[self.pos] != '\'')) {
                self.emitError(TokenizerError.InvalidToken, "Expected quote to start attribute value string");
                return TokenizerError.InvalidToken;
            }
            const quote = self.buffer.items[self.pos];
            self.pos += 1;
            self.column += 1;
            const start_line = self.line;
            const start_column = self.column;
            var value = std.ArrayList(u8).init(self.allocator); //collect raw utf8 bytes
            errdefer value.deinit();

            while (true) {
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.UnexpectedEOF, "Unterminated string");
                        return TokenizerError.UnexpectedEOF;
                    }
                }

                const saved_pos = self.pos; // gotta save pos to capture the bytes of the current char
                const cp = try self.decodeUtf8();

                // check if reached the closing quote
                if (cp == @as(u32, quote)) {
                    return .{ .value = try value.toOwnedSlice(), .start_line = start_line, .start_column = start_column };
                }

                const char_bytes = self.buffer.items[saved_pos..self.pos];
                try value.appendSlice(char_bytes);
                self.advancePosition(cp);
            }
        }

        pub fn parseEndTag(self: *Self) !Token {
            const name = try self.readName();
            errdefer self.allocator.free(name);
            try self.skipWhitespace();

            if (self.pos >= self.buffer.items.len) {
                const has_more_data = try self.readNextChunk();
                if (!has_more_data) {
                    self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file while parsing end tag");
                    return TokenizerError.UnexpectedEOF;
                }
            }

            if (self.buffer.items[self.pos] != '>') {
                self.emitError(TokenizerError.InvalidToken, "Expected '>' to close end tag");
                return TokenizerError.InvalidToken;
            }
            self.pos += 1;
            self.column += 1;
            return Token{ .end_tag = name };
        }

        pub fn parseText(self: *Self) !Token {
            var content = std.ArrayList(u8).init(self.allocator);
            errdefer content.deinit();

            while (true) {
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        // gotta allow empty text tokens
                        return Token{ .text = try content.toOwnedSlice() };
                    }
                }

                const saved_pos = self.pos;
                const cp = try self.decodeUtf8();

                if (cp == '<') {
                    self.pos = saved_pos; // rewind to let nexttoken handle <
                    return Token{ .text = try content.toOwnedSlice() };
                }

                try content.appendSlice(self.buffer.items[saved_pos..self.pos]);
                self.advancePosition(cp);
            }
        }

        pub fn parseComment(self: *Self) !Token {
            var content = std.ArrayList(u8).init(self.allocator);
            errdefer content.deinit();
            while (true) {
                if (self.pos + 2 >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.MalformedComment, "Unterminated comment at end of input");
                        return TokenizerError.MalformedComment;
                    }
                }

                if (self.buffer.items[self.pos] == '-' and
                    self.buffer.items[self.pos + 1] == '-' and
                    self.buffer.items[self.pos + 2] == '>')
                {
                    self.pos += 3;
                    self.column += 3;
                    return Token{ .comment = try content.toOwnedSlice() };
                }

                const saved_pos = self.pos;
                const cp = try self.decodeUtf8();
                try content.appendSlice(self.buffer.items[saved_pos..self.pos]);
                self.advancePosition(cp);
            }
        }

        pub fn parsePi(self: *Self) !Token {
            const target = try self.readName();
            errdefer self.allocator.free(target); // free target incase of error
            try self.skipWhitespace();
            var data = std.ArrayList(u8).init(self.allocator);
            errdefer data.deinit();

            while (true) {
                if (self.pos + 1 >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.MalformedProcessingInstruction, "Unterminated processing instruction at end of input");
                        return TokenizerError.MalformedProcessingInstruction;
                    }
                }

                if (self.buffer.items[self.pos] == '?' and self.buffer.items[self.pos + 1] == '>') {
                    self.pos += 2;
                    self.column += 2;
                    const data_slice = try data.toOwnedSlice();
                    if (std.mem.eql(u8, target, "xml")) {
                        // Parse XML declaration attributes
                        var version: []u8 = undefined;
                        var encoding: ?[]u8 = null;
                        var standalone: ?[]u8 = null;

                        var iter = std.mem.splitAny(u8, data_slice, " ");
                        while (iter.next()) |part| {
                            if (part.len == 0) continue;
                            var kv = std.mem.splitAny(u8, part, "=");
                            const key = kv.next() orelse continue;
                            const value = kv.next() orelse {
                                self.emitError(TokenizerError.InvalidToken, "Missing value in XML declaration attribute");
                                return TokenizerError.InvalidToken;
                            };

                            if (std.mem.eql(u8, key, "version")) {
                                version = try self.allocator.dupe(u8, std.mem.trim(u8, value, "\"'"));
                            } else if (std.mem.eql(u8, key, "encoding")) {
                                encoding = try self.allocator.dupe(u8, std.mem.trim(u8, value, "\"'"));
                            } else if (std.mem.eql(u8, key, "standalone")) {
                                standalone = try self.allocator.dupe(u8, std.mem.trim(u8, value, "\"'"));
                            }
                        }
                        self.allocator.free(data_slice);
                        self.allocator.free(target);
                        return Token{ .xml_declaration = .{ .version = version, .encoding = encoding, .standalone = standalone } };
                    } else {
                        return Token{ .pi = .{ .target = target, .data = data_slice } };
                    }
                }

                const saved_pos = self.pos;
                const cp = try self.decodeUtf8();
                try data.appendSlice(self.buffer.items[saved_pos..self.pos]);
                self.advancePosition(cp);
            }
        }

        fn parseCdata(self: *Self) !Token {
            var content = std.ArrayList(u8).init(self.allocator);
            errdefer content.deinit();

            while (true) {
                if (self.pos + 2 >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.UnterminatedCDATASection, "Unterminated CDATA section at end of input");
                        return TokenizerError.UnterminatedCDATASection;
                    }
                }

                if (self.buffer.items[self.pos] == ']' and
                    self.buffer.items[self.pos + 1] == ']' and
                    self.buffer.items[self.pos + 2] == '>')
                {
                    self.pos += 3;
                    self.column += 3; // consume "]]>"
                    return Token{ .cdata = try content.toOwnedSlice() };
                }

                const saved_pos = self.pos;
                const cp = try self.decodeUtf8();
                try content.appendSlice(self.buffer.items[saved_pos..self.pos]);
                self.advancePosition(cp);
            }
        }

        fn parseDoctype(self: *Self) !Token {
            var content = std.ArrayList(u8).init(self.allocator);
            errdefer content.deinit(); //dont forget free content in case error occurs
            var in_subset = false;

            while (true) {
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file while parsing DOCTYPE");
                        return TokenizerError.UnexpectedEOF;
                    }
                }

                const saved_pos = self.pos;
                const cp = try self.decodeUtf8();
                try content.appendSlice(self.buffer.items[saved_pos..self.pos]);
                self.advancePosition(cp);

                if (cp == '>' and !in_subset) {
                    return Token{ .doctype = try content.toOwnedSlice() };
                } else if (cp == '[') {
                    in_subset = true;
                } else if (cp == ']' and in_subset) {
                    in_subset = false;
                }
            }
        }

        const Range = struct {
            start: u32,
            end: u32,
        };

        // ranges for name start chars
        const nameStartCharRanges: []const Range = &[_]Range{
            .{ .start = 0x3A, .end = 0x3A },
            .{ .start = 0x41, .end = 0x5A },
            .{ .start = 0x5F, .end = 0x5F },
            .{ .start = 0x61, .end = 0x7A },
            .{ .start = 0xC0, .end = 0xD6 },
            .{ .start = 0xD8, .end = 0xF6 },
            .{ .start = 0xF8, .end = 0x2FF },
            .{ .start = 0x370, .end = 0x37D },
            .{ .start = 0x37F, .end = 0x1FFF },
            .{ .start = 0x200C, .end = 0x200D },
            .{ .start = 0x2070, .end = 0x218F },
            .{ .start = 0x2C00, .end = 0x2FEF },
            .{ .start = 0x3001, .end = 0xD7FF },
            .{ .start = 0xF900, .end = 0xFDCF },
            .{ .start = 0xFDF0, .end = 0xFFFD },
            .{ .start = 0x10000, .end = 0xEFFFF },
        };

        // ranges for allowed name chars
        const nameCharRanges: []const Range = &[_]Range{
            .{ .start = 0x2D, .end = 0x2D },
            .{ .start = 0x2E, .end = 0x2E },
            .{ .start = 0x30, .end = 0x39 },
            .{ .start = 0x3A, .end = 0x3A },
            .{ .start = 0x41, .end = 0x5A },
            .{ .start = 0x5F, .end = 0x5F },
            .{ .start = 0x61, .end = 0x7A },
            .{ .start = 0xB7, .end = 0xB7 },
            .{ .start = 0xC0, .end = 0xD6 },
            .{ .start = 0xD8, .end = 0xF6 },
            .{ .start = 0xF8, .end = 0x2FF },
            .{ .start = 0x300, .end = 0x36F },
            .{ .start = 0x370, .end = 0x37D },
            .{ .start = 0x37F, .end = 0x1FFF },
            .{ .start = 0x200C, .end = 0x200D },
            .{ .start = 0x2070, .end = 0x218F },
            .{ .start = 0x2C00, .end = 0x2FEF },
            .{ .start = 0x3001, .end = 0xD7FF },
            .{ .start = 0xF900, .end = 0xFDCF },
            .{ .start = 0xFDF0, .end = 0xFFFD },
            .{ .start = 0x203F, .end = 0x2040 },
            .{ .start = 0x10000, .end = 0xEFFFF },
        };

        fn isInRanges(code_point: u32, ranges: []const Range) bool {
            for (ranges) |range| {
                if (code_point >= range.start and code_point <= range.end) {
                    return true;
                }
            }
            return false;
        }

        pub fn isNameStartChar(code_point: u32) bool {
            return isInRanges(code_point, nameStartCharRanges);
        }

        pub fn isNameChar(code_point: u32) bool {
            return isInRanges(code_point, nameCharRanges);
        }

        fn decodeUtf8(self: *Self) !u32 {
            if (self.pos >= self.buffer.items.len) {
                self.emitError(TokenizerError.InvalidToken, "Unexpected end of input while decoding Utf8");
                return TokenizerError.UnexpectedEOF;
            }
            var it = std.unicode.Utf8Iterator{ .bytes = self.buffer.items, .i = self.pos };
            const cp = it.nextCodepoint() orelse return TokenizerError.InvalidUTF8;
            self.pos = it.i; // advance
            return cp;
        }

        // read tag or attribute name
        pub fn readName(self: *Self) ![]u8 {
            var name = std.ArrayList(u8).init(self.allocator);
            errdefer name.deinit();

            // ensure we have data n read first char
            while (true) {
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        self.emitError(TokenizerError.UnexpectedEOF, "Unexpected end of file while reading name");
                        return TokenizerError.UnexpectedEOF;
                    }
                }
                const first_cp_u32 = try self.decodeUtf8();
                if (first_cp_u32 > 0x10FFFF) {
                    self.emitError(TokenizerError.InvalidCharacter, "Invalid Unicode code point");
                    return TokenizerError.InvalidCharacter;
                }
                const first_cp: u21 = @intCast(first_cp_u32);
                if (!isNameStartChar(first_cp)) {
                    self.emitError(TokenizerError.InvalidToken, "Invalid starting character for XML name");
                    return TokenizerError.InvalidToken;
                }
                const first_char_len = std.unicode.utf8CodepointSequenceLength(first_cp) catch unreachable;
                try name.appendSlice(self.buffer.items[self.pos - first_char_len .. self.pos]);
                self.advancePosition(first_cp);
                break;
            }

            // read subsequent chars
            while (true) {
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data = try self.readNextChunk();
                    if (!has_more_data) {
                        break; // end of input so name is complete
                    }
                }
                const saved_pos = self.pos;
                const cp_u32 = self.decodeUtf8() catch |err| {
                    if (err == TokenizerError.InvalidUTF8) {
                        self.emitError(TokenizerError.InvalidUTF8, "Invalid UTF-8 sequence in name");
                        return TokenizerError.InvalidUTF8;
                    }
                    return err;
                };
                if (cp_u32 > 0x10FFFF) {
                    self.emitError(TokenizerError.InvalidCharacter, "Invalid Unicode code point");
                    return TokenizerError.InvalidCharacter;
                }
                const cp: u21 = @intCast(cp_u32);
                if (!isNameChar(cp)) {
                    self.pos = saved_pos; // rewind if not a name char
                    break;
                }
                try name.appendSlice(self.buffer.items[saved_pos..self.pos]);
                self.advancePosition(cp);
            }
            return try name.toOwnedSlice();
        }

        fn advancePosition(self: *Self, cp: u32) void {
            if (self.expecting_cr_lf) {
                self.expecting_cr_lf = false;
                if (cp == '\n') return;
            }
            switch (cp) {
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                },
                '\r' => {
                    self.line += 1;
                    self.column = 1;
                    self.expecting_cr_lf = true;
                },
                else => {
                    self.column += 1;
                },
            }
        }

        pub fn skipWhitespace(self: *Self) !void {
            var pending_cr = false;
            while (true) {
                if (self.pos >= self.buffer.items.len) {
                    const has_more_data_data = try self.readNextChunk();
                    if (!has_more_data_data)
                        break;
                    // if the prev chunk ended with a \r and the new chunk starts with \n skip it
                    if (pending_cr and self.buffer.items[self.pos] == '\n') {
                        self.pos += 1;
                        pending_cr = false;
                    }
                    continue;
                }
                const c = self.buffer.items[self.pos];
                if (!isXmlWhitespace(c)) { // if this char is not whitespace finalize any pending cr adjustments and exit
                    if (pending_cr) {
                        self.column = 1;
                    }
                    return;
                }
                self.pos += 1;
                switch (c) {
                    ' ', '\t' => {
                        self.column += 1;
                        pending_cr = false;
                    },
                    '\n' => {
                        // if a pending CR then this \n is part of a crlf sequence
                        if (!pending_cr) {
                            self.line += 1;
                            self.column = 1;
                        } else {
                            pending_cr = false;
                        }
                    },
                    '\r' => {
                        self.line += 1;
                        self.column = 1;
                        pending_cr = true;
                        // check if the next char in the same chunk is \n
                        if (self.pos < self.buffer.items.len and self.buffer.items[self.pos] == '\n') {
                            self.pos += 1;
                            pending_cr = false;
                        }
                    },
                    else => unreachable,
                }
            }
        }

        pub fn isXmlWhitespace(char: u8) bool {
            return switch (char) {
                ' ', '\t', '\n', '\r' => true,
                else => false,
            };
        }
    };
}
