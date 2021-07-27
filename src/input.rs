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

    pub fn get_len_const(&self, ident: &Ident) -> proc_macro2::TokenStream {
        let len = self.get_length(ident);

        quote! {
            pub const LEN: usize = #len;
        }
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

    pub fn get_impl_enum(&self, item_enum: &ItemEnum) -> proc_macro2::TokenStream {
        let (impl_generics, ty_generics, where_clause) = item_enum.generics.split_for_impl();
        let ty = &item_enum.ident;

        let const_len = self.get_len_const(ty);
        let const_array = self.get_array_const(ty);
        let fn_index = self.get_enum_index_fn(item_enum);

        quote! {
            impl #impl_generics #ty #ty_generics #where_clause {
                #const_len

                #const_array

                #fn_index

                pub fn iter<'a>() -> iter_context::Iter<'a, Self, Self> {
                    iter_context::Iter::new(Self::ARRAY.iter())
                }
            }
        }
    }

    pub fn get_impl_struct(&self, item_struct: &ItemStruct) -> proc_macro2::TokenStream {
        let (impl_generics, ty_generics, where_clause) = item_struct.generics.split_for_impl();
        let ty = &item_struct.ident;

        let const_len = self.get_len_const(ty);
        let const_array = self.get_array_const(ty);
        let fn_index = self.get_struct_index_fn(item_struct);

        quote! {
            impl #impl_generics #ty #ty_generics #where_clause {
                #const_len

                #const_array

                #fn_index

                pub fn iter<'a>() -> iter_context::Iter<'a, Self, Self> {
                    iter_context::Iter::new(Self::ARRAY.iter())
                }
            }
        }
    }

    pub fn get_enum_tests(&self) -> proc_macro2::TokenStream {
        let tests = self.enums.iter().map(|e| {
            let ty = &e.ident;

            quote! {
                #ty::ARRAY
                    .iter()
                    .enumerate()
                    .for_each(|(i, v)| {
                        assert_eq!(i, v.index());
                    });
            }
        });

        quote! {
            #[cfg(test)]
            mod array_enum_tests {
                use super::*;

                #[test]
                fn enum_index_tests() {
                    #( #tests )*
                }
            }
        }
    }

    pub fn get_array_const(&self, ident: &Ident) -> proc_macro2::TokenStream {
        let variants = self.get_all_variants(ident);
        quote! {
            pub const ARRAY: [Self; Self::LEN] =
            [
                #( #variants, )*
            ];
        }
    }

    pub fn get_all_variants(&self, ident: &Ident) -> Vec<proc_macro2::TokenStream> {
        let enum_variants = self.get_enum(ident).map(|e| self.get_all_enum_variants(e));

        let get_struct_variants = || {
            self.get_struct(ident)
                .map(|s| self.get_all_struct_variants(s))
        };

        enum_variants
            .or_else(get_struct_variants)
            .unwrap_or_else(|| panic!("Ident not found: {}", ident))
    }

    pub fn get_all_enum_variants(&self, item_enum: &ItemEnum) -> Vec<proc_macro2::TokenStream> {
        let ty = &item_enum.ident;

        item_enum
            .variants
            .iter()
            .flat_map(|variant| {
                if let Some(ident) = get_variant_type(variant) {
                    let variant = &variant.ident;

                    self.get_all_variants(ident)
                        .into_iter()
                        .map(|inner| quote! { #ty :: #variant ( #inner ) })
                        .collect()
                } else {
                    vec![quote! { #ty :: #variant }]
                }
            })
            .collect()
    }

    pub fn get_all_struct_variants(
        &self,
        item_struct: &ItemStruct,
    ) -> Vec<proc_macro2::TokenStream> {
        let ty = &item_struct.ident;

        let field_variants: Vec<Vec<_>> = item_struct
            .fields
            .iter()
            .map(|ty| {
                let inner = get_field_type(ty);
                self.get_all_variants(inner)
            })
            .collect();

        let permutations = get_permutations(&field_variants);

        match &item_struct.fields {
            Fields::Named(fields) => permutations
                .iter()
                .map(|p| {
                    let fields = fields.named.iter().map(|f| &f.ident);
                    quote! { #ty { #( #fields: #p, )* } }
                })
                .collect(),
            Fields::Unnamed(_) => permutations
                .iter()
                .map(|p| {
                    quote! { #ty ( #( #p, )* ) }
                })
                .collect(),
            Fields::Unit => {
                vec![quote! { #ty }]
            }
        }
    }

    pub fn get_enum_index_fn(&self, item_enum: &ItemEnum) -> proc_macro2::TokenStream {
        let is_flat = item_enum
            .variants
            .iter()
            .all(|v| matches!(&v.fields, Fields::Unit));

        if is_flat {
            quote! {
                pub const fn index(self) -> usize {
                    self as usize
                }
            }
        } else {
            let mut index_sum = 0;

            let match_variants = item_enum.variants.iter().map(move |variant| {
                let index = index_sum;

                let variant_len = get_variant_type(variant)
                    .map(|inner| self.get_length(inner))
                    .unwrap_or(1);

                index_sum += variant_len;

                let var_ident = &variant.ident;
                if get_variant_type(variant).is_some() {
                    quote! { Self::#var_ident(inner) => #index + inner.index(), }
                } else {
                    quote! { Self::#var_ident => #index, }
                }
            });

            quote! {
                pub const fn index(self) -> usize {
                    match self {
                        #( #match_variants )*
                    }
                }
            }
        }
    }

    pub fn get_struct_index_fn(&self, item_struct: &ItemStruct) -> proc_macro2::TokenStream {
        let variants: Vec<_> = self.get_all_variants(&item_struct.ident);

        let variants = variants.iter().enumerate().map(|(i, variant)| {
            quote! { #variant => #i, }
        });

        quote! {
            pub const fn index(self) -> usize {
                match self {
                    #( #variants )*
                }
            }
        }
    }
}
