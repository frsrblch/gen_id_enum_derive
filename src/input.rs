use super::*;
use syn::parse::{Parse, ParseStream};

pub struct Input {
    pub enums: Vec<ItemEnum>,
    pub structs: Vec<ItemStruct>,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut enums = vec![];
        let mut structs = vec![];

        while let Ok(item) = input.parse::<Item>() {
            match item {
                Item::Enum(e) => enums.push(e),
                Item::Struct(s) => structs.push(s),
                _ => panic!("Input must only contain enums and structs of enums",),
            }
        }

        Ok(Input { enums, structs })
    }
}

impl Input {
    pub fn get_enum(&self, ident: &Ident) -> Option<&ItemEnum> {
        self.enums.iter().find(|e| e.ident.eq(ident))
    }

    pub fn get_struct(&self, ident: &Ident) -> Option<&ItemStruct> {
        self.structs.iter().find(|e| e.ident.eq(ident))
    }

    pub fn get_length(&self, ident: &Ident) -> usize {
        self.get_enum_length_inner(ident, 10)
            .or_else(|| self.get_struct_length_inner(ident, 10))
            .unwrap_or_else(|| panic!("Ident not found: {}", ident))
    }

    fn get_enum_length_inner(&self, ident: &Ident, recursion_limit: usize) -> Option<usize> {
        if recursion_limit == 0 {
            panic!("Recursion limit reached")
        }

        let item_enum = self.get_enum(ident)?;

        let mut sum = 0;

        for variant in item_enum.variants.iter() {
            let len = get_variant_type(variant)
                .and_then(|ident| {
                    let recursion_limit = recursion_limit - 1;
                    self.get_enum_length_inner(ident, recursion_limit)
                        .or_else(|| self.get_struct_length_inner(ident, recursion_limit))
                })
                .unwrap_or(1);

            sum += len;
        }

        Some(sum)
    }

    fn get_struct_length_inner(&self, ident: &Ident, recursion_limit: usize) -> Option<usize> {
        if recursion_limit == 0 {
            panic!("Recursion limit reached")
        }

        let item_struct = self.get_struct(ident)?;

        let mut product = 1;

        for field in item_struct.fields.iter() {
            let ident = get_field_type(field);
            let recursion_limit = recursion_limit - 1;

            let len = self
                .get_enum_length_inner(ident, recursion_limit)
                .or_else(|| self.get_struct_length_inner(ident, recursion_limit))
                .unwrap_or(1);

            product *= len;
        }

        Some(product)
    }
}
