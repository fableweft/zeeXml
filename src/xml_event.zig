pub const xml_event = union(enum) {
    start_element: StartElement,
    end_element: EndElement,
    text: Text,
    comment: Comment,
    processing_instruction: ProcessingInstruction,
    self_closing_element: SelfClosingElement,
    doctype: Doctype,
    cdata: Cdata,
    xml_declaration: XmlDeclaration,
};

pub const XmlDeclaration = struct {
    version: []const u8,
    encoding: ?[]const u8,
    standalone: ?[]const u8,
};

pub const Cdata = struct {
    content: []const u8,
};

pub const Doctype = struct {
    declaration: []const u8,
};

pub const StartElement = struct {
    name: []const u8,
    uri: ?[]const u8,
    local_name: []const u8,
    attributes: []Attribute,
};

pub const EndElement = struct {
    name: []const u8,
    uri: ?[]const u8, // namespace uri
    local_name: []const u8, // local name
};

pub const Text = struct {
    content: []const u8,
};

pub const Comment = struct {
    content: []const u8,
};

pub const ProcessingInstruction = struct {
    target: []const u8,
    data: []const u8,
};

pub const SelfClosingElement = struct {
    name: []const u8,
    uri: ?[]const u8, // namespace uri
    local_name: []const u8, // local name
    attributes: []Attribute,
};

// reminder for me:  this Attribute struct in xml_event.zig file will be used by parser, tokenizer layer has its own Attribute struct locally(trynna keep implementation simple and stright forward)
pub const Attribute = struct {
    name: []const u8,
    value: []const u8,
    uri: ?[]const u8, // namespace uri (null for unprefixed attributes)
    local_name: []const u8, // local name
};
