use gen_id_enum_derive::impl_array_types;

impl_array_types! {

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

impl ColorSize {
    pub const fn index1(self) -> usize {
        self.color.index() * Size::LEN + self.size.index()
    }
}

#[test]
fn index_compare() {
    use iter_context::ContextualIterator;
    ColorSize::iter().for_each(|color_size| {
        assert_eq!(color_size.index(), color_size.index1());
    });
}
