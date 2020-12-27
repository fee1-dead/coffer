use crate::insns::Type;
#[test]
fn type_method_to_str() {
    assert_eq!(
        Type::method(vec![
            Type::INT,
            Type::DOUBLE,
            Type::reference("java/lang/Thread")
        ], Type::reference("java/lang/Object").into()).to_string(),
        "(IDLjava/lang/Thread;)Ljava/lang/Object;");
}
#[test]
fn type_array_to_str() {
    assert_eq!(Type::array(10, Type::BOOLEAN).to_string(), "[[[[[[[[[[Z");
}