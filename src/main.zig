const std = @import("std");
const zeeXml = @import("zeeXml.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = true }){};
    const allocator = gpa.allocator();

    var xml_files_dir = try std.fs.cwd().openDir("xml-files", .{});
    defer xml_files_dir.close();

    const file = try xml_files_dir.openFile("basic-structure.xml", .{});
    defer file.close();

    const reader = file.reader();

    var parser = try zeeXml.createParser(allocator, reader);
    defer parser.deinit();

    while (true) {
        const maybe_event = parser.nextEvent() catch |err| {
            if (parser.last_detailed_error) |detailed_err| {
                std.debug.print("{}", .{detailed_err});
            } else {
                std.debug.print("Error processing event: {}\n", .{err});
            }
            return err;
        };

        if (maybe_event) |event| {
            switch (event) {
                .start_element => |se| {
                    std.debug.print("Start Element: {s}, URI: {?s}\n", .{ se.name, se.uri });
                    for (se.attributes) |attr| {
                        std.debug.print("  Attribute: {s}=\"{s}\", URI: {?s}\n", .{ attr.name, attr.value, attr.uri });
                    }
                },
                .end_element => |ee| {
                    std.debug.print("End Element: {s}, URI: {?s}\n", .{ ee.name, ee.uri });
                },
                .self_closing_element => |sce| {
                    std.debug.print("Self-Closing Element: {s}, URI: {?s}\n", .{ sce.name, sce.uri });
                    for (sce.attributes) |attr| {
                        std.debug.print("  Attribute: {s}=\"{s}\", URI: {?s}\n", .{ attr.name, attr.value, attr.uri });
                    }
                },
                .text => |text| {
                    std.debug.print("Text: {s}\n", .{text.content});
                },
                .comment => |comment| {
                    std.debug.print("Comment: {s}\n", .{comment.content});
                },
                .processing_instruction => |pi| {
                    std.debug.print("Processing Instruction: target={s}, data={s}\n", .{ pi.target, pi.data });
                },
                .cdata => |cdata| {
                    std.debug.print("CDATA: {s}\n", .{cdata.content});
                },
                .doctype => |doctype| {
                    std.debug.print("DOCTYPE: {s}\n", .{doctype.declaration});
                },
                .xml_declaration => |xmldec| {
                    std.debug.print("XML Declaration: version={s}", .{xmldec.version});
                    if (xmldec.encoding) |enc| std.debug.print(", encoding={s}", .{enc});
                    if (xmldec.standalone) |sa| std.debug.print(", standalone={s}", .{sa});
                    std.debug.print("\n", .{});
                },
            }
            parser.freeEvent(event);
        } else {
            break;
        }
    }

    std.debug.print("Finished processing XML file.\n", .{});
}
