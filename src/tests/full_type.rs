use wtf_8::w;

use crate::prelude::Type;

#[test]
fn type_method_to_str() {
    assert_eq!(
        Type::method(
            [
                Type::Int,
                Type::Double,
                Type::reference(w!("java/lang/Thread"))
            ],
            Type::reference(w!("java/lang/Object")).into()
        )
        .to_string(),
        "(IDLjava/lang/Thread;)Ljava/lang/Object;"
    );
}
#[test]
fn type_array_to_str() {
    assert_eq!(Type::array(10, Type::Boolean).to_string(), "[[[[[[[[[[Z");
}
