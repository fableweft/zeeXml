const std = @import("std");

pub const parser = @import("xml_parser.zig");
pub const tokenizer = @import("xml_tokenizer.zig");
pub const event = @import("xml_event.zig");

pub const Parser = parser.xml_parser;
pub const Event = event.xml_event;
pub const Attribute = event.Attribute;
pub const Tokenizer = tokenizer.xml_tokenizer;

pub fn createParser(allocator: std.mem.Allocator, reader: anytype) !Parser(@TypeOf(reader)) {
    return Parser(@TypeOf(reader)).init(allocator, reader);
}
