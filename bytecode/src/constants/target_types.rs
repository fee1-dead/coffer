//! Target type values for Type Annotations.
// Needs help for the naming of these variables

/// type parameter declaration of generic class or interface, the relative `target_info` item is `type_parameter_target`
pub const TYPE_GENERIC_CLASS: u8 = 0x00;
/// type parameter declaration of generic method or constructor, the relative `target_info` item is `type_parameter_target`
pub const TYPE_GENERIC_METHOD: u8 = 0x01;
/// type in extends or implements clause of class declaration (including the direct superclass or direct superinterface of an anonymous class declaration), or in extends clause of interface declaration, the relative `target_info` item is `supertype_target`
pub const TYPE_EXTENDS: u8 = 0x10;
/// type in bound of type parameter declaration of generic class or interface, the relative `target_info` item is `type_parameter_bound_target`
pub const TYPE_GENERIC_CLASS_PARAMETER: u8 = 0x11;
/// type in bound of type parameter declaration of generic method or constructor, the relative `target_info` item is `type_parameter_bound_target`
pub const TYPE_GENERIC_METHOD_PARAMETER: u8 = 0x12;
/// type in field declaration, the relative `target_info` item is `empty_target`
pub const TYPE_FIELD: u8 = 0x13;
/// return type of method, or type of newly constructed object, the relative `target_info` item is `empty_target`
pub const TYPE_METHOD: u8 = 0x14;
/// receiver type of method or constructor, the relative `target_info` item is `empty_target`
pub const TYPE_METHOD_RECIEVER: u8 = 0x15;
/// type in formal parameter declaration of method, constructor, or lambda expression, the relative `target_info` item is `formal_parameter_target`
pub const TYPE_METHOD_PARAMETER: u8 = 0x16;
/// type in throws clause of method or constructor, the relative `target_info` item is `throws_target`
pub const TYPE_METHOD_THROWS: u8 = 0x17;
/// type in local variable declaration, the relative `target_info` item is `localvar_target`
pub const TYPE_VAR_LOCAL: u8 = 0x40;
/// type in resource variable declaration, the relative `target_info` item is `localvar_target`
pub const TYPE_VAR_RESOURCE: u8 = 0x41;
/// type in exception parameter declaration, the relative `target_info` item is `catch_target`
pub const TYPE_EXCEPTION_PARAMETER: u8 = 0x42;
/// type in instanceof expression, the relative `target_info` item is `offset_target`
pub const TYPE_INSTANCEOF: u8 = 0x43;
/// type in new expression, the relative `target_info` item is `offset_target`
pub const TYPE_NEW: u8 = 0x44;
/// type in method reference expression using ::new, the relative `target_info` item is `offset_target`
pub const TYPE_METHOD_REF_NEW: u8 = 0x45;
/// type in method reference expression using ::Identifier, the relative `target_info` item is `offset_target`
pub const TYPE_METHOD_REF_IDENTIFIER: u8 = 0x46;
/// type in cast expression, the relative `target_info` item is `type_argument_target`
pub const TYPE_CAST: u8 = 0x47;
/// type argument for generic constructor in new expression or explicit constructor invocation statement, the relative `target_info` item is `type_argument_target`
pub const TYPE_GENERIC_CONSTRUCTOR: u8 = 0x48;
/// type argument for generic method in method invocation expression, the relative `target_info` item is `type_argument_target`
pub const TYPE_GENERIC_METHOD_ARGUMENT: u8 = 0x49;
/// type argument for generic constructor in method reference expression using ::new, the relative `target_info` item is `type_argument_target`
pub const TYPE_GENERIC_CONSTRUCTOR_REF_NEW: u8 = 0x4A;
/// type argument for generic method in method reference expression using ::Identifier, the relative `target_info` item is `type_argument_target`
pub const TYPE_GENERIC_METHOD_REF_NEW: u8 = 0x4B;
