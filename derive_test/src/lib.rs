use gen_id_enum_derive::many_enum_array;

// rather than derive, this should be a function-like macro inside which we define the enums

// the proc_macro requires knowledge of all enums to construct a compound array enum type

many_enum_array! {

    pub enum Size {
        Big,
        Small,
    }

    pub enum Color {
        Red,
        Green,
        Blue,
    }

    pub struct ColorSize {
        pub color: Color,
        pub size: Size,
    }

}
