extern crate proc_macro;
use crate::input::Input;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Field, Fields, Ident, Item, ItemEnum, ItemStruct, Type, Variant};

mod input;

#[proc_macro]
pub fn many_enum_array(input: TokenStream) -> TokenStream {
    let input = &parse_macro_input!(input as Input);
    let derives = &get_derives();

    let enum_tokens = input.enums.iter().map(|ty| {
        let impl_enum = get_impl_enum(ty, input);

        let array: proc_macro2::TokenStream = get_array_struct(&ty.ident);

        quote! {
            #derives
            #ty

            #impl_enum

            #array
        }
    });

    let struct_tokens = input.structs.iter().map(|ty| {
        let impl_struct: proc_macro2::TokenStream = get_impl_struct(ty, input);

        let array: proc_macro2::TokenStream = get_array_struct(&ty.ident);

        quote! {
            #derives
            #ty

            #impl_struct

            #array
        }
    });

    let tests = get_enum_tests(input);

    let tokens = quote! {
        #( #enum_tokens )*

        #( #struct_tokens )*

        #tests
    };

    tokens.into()
}

fn get_derives() -> proc_macro2::TokenStream {
    quote! {
        #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
    }
}

fn get_impl_struct(item_struct: &ItemStruct, input: &Input) -> proc_macro2::TokenStream {
    let (impl_generics, ty_generics, where_clause) = item_struct.generics.split_for_impl();
    let ty = &item_struct.ident;

    let const_len = get_len_const(ty, input);
    let const_array = get_array_const(ty, input);
    let fn_index = get_struct_index_fn(item_struct, input);

    quote! {
        impl #impl_generics #ty #ty_generics #where_clause {
            #const_len

            #const_array

            #fn_index
        }
    }
}

fn get_array_struct(ident: &Ident) -> proc_macro2::TokenStream {
    let ty = ident;

    let array = &Ident::new(&format!("{}Array", ident), ident.span());

    quote! {
        #[derive(Debug, Default, Copy, Clone)]
        pub struct #array <T> {
            values: [T; #ty::LEN],
        }

        impl<T> #array <T> {
            #[inline]
            pub const fn new(values: [T; <#ty>::LEN]) -> Self {
                Self { values }
            }

            #[inline]
            pub fn iter(&self) -> iter_context::Iter<#ty, T> {
                iter_context::Iter::new(self.values.iter())
            }

            #[inline]
            pub fn iter_mut(&mut self) -> iter_context::IterMut<#ty, T> {
                iter_context::IterMut::new(self.values.iter_mut())
            }
        }

        impl<T> std::ops::Index<#ty> for #array <T> {
            type Output = T;

            #[inline]
            fn index(&self, index: #ty) -> &Self::Output {
                self.values.index(index.index())
            }
        }

        impl<T> std::ops::IndexMut<#ty> for #array <T> {
            #[inline]
            fn index_mut(&mut self, index: #ty) -> &mut Self::Output {
                self.values.index_mut(index.index())
            }
        }

        impl<'a, T> IntoIterator for &'a #array <T> {
            type Item = &'a T;
            type IntoIter = std::slice::Iter<'a, T>;

            fn into_iter(self) -> Self::IntoIter {
                self.values.iter()
            }
        }

        impl<'a, T> IntoIterator for &'a mut #array <T> {
            type Item = &'a mut T;
            type IntoIter = std::slice::IterMut<'a, T>;

            fn into_iter(self) -> Self::IntoIter {
                self.values.iter_mut()
            }
        }

        impl<'a, T> iter_context::ContextualIterator for &'a #array <T> {
            type Context = #ty;
        }

        impl<'a, T> iter_context::ContextualIterator for &'a mut #array <T> {
            type Context = #ty;
        }
    }
}

fn get_impl_enum(item_enum: &ItemEnum, enums: &Input) -> proc_macro2::TokenStream {
    let (impl_generics, ty_generics, where_clause) = item_enum.generics.split_for_impl();
    let ty = &item_enum.ident;

    let const_len = get_len_const(ty, enums);
    let const_array = get_array_const(ty, enums);
    let fn_index = get_enum_index_fn(item_enum, enums);

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

fn get_len_const(ident: &Ident, input: &Input) -> proc_macro2::TokenStream {
    let len = input.get_length(ident);

    quote! {
        pub const LEN: usize = #len;
    }
}

fn get_array_const(ident: &Ident, input: &Input) -> proc_macro2::TokenStream {
    let variants = get_all_variants(ident, input);
    quote! {
        pub const ARRAY: [Self; Self::LEN] =
        [
            #( #variants, )*
        ];
    }
}

fn get_all_variants(ident: &Ident, input: &Input) -> Vec<proc_macro2::TokenStream> {
    input
        .get_enum(ident)
        .map(|e| get_all_enum_variants(e, input))
        .or_else(|| {
            input
                .get_struct(ident)
                .map(|s| get_all_struct_variants(s, input))
        })
        .unwrap_or_else(|| panic!("Ident not found: {}", ident))
}

fn get_all_enum_variants(item_enum: &ItemEnum, input: &Input) -> Vec<proc_macro2::TokenStream> {
    let ty = &item_enum.ident;

    item_enum
        .variants
        .iter()
        .flat_map(move |variant| {
            if let Some(ident) = get_variant_type(variant) {
                let variant = &variant.ident;

                get_all_variants(ident, input)
                    .into_iter()
                    .map(|inner| quote! { #ty :: #variant ( #inner ) })
                    .collect()
            } else {
                vec![quote! { #ty :: #variant }]
            }
        })
        .collect()
}

fn get_all_struct_variants(
    item_struct: &ItemStruct,
    input: &Input,
) -> Vec<proc_macro2::TokenStream> {
    let ty = &item_struct.ident;

    let field_variants: Vec<Vec<_>> = item_struct
        .fields
        .iter()
        .map(|ty| {
            let inner = get_field_type(ty);
            get_all_variants(inner, input)
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

fn get_permutations<T: Clone>(sets: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    match &sets.as_slice() {
        &[] => vec![],
        &[single] => single.iter().map(|ts| vec![ts.clone()]).collect(),
        &[a, b] => a
            .iter()
            .flat_map(|a| b.iter().map(move |b| vec![a.clone(), b.clone()]))
            .collect(),
        &[a, b, c] => a
            .iter()
            .flat_map(move |a| {
                b.iter()
                    .flat_map(move |b| c.iter().map(move |c| vec![a.clone(), b.clone(), c.clone()]))
            })
            .collect(),
        _ => unimplemented!("Cannot have more than three sets"),
    }
}

#[test]
fn permutation_test() {
    let input: Vec<Vec<u32>> = vec![vec![1, 2], vec![2, 3, 5], vec![7]];
    let expected: Vec<Vec<u32>> = vec![
        vec![1, 2, 7],
        vec![1, 3, 7],
        vec![1, 5, 7],
        vec![2, 2, 7],
        vec![2, 3, 7],
        vec![2, 5, 7],
    ];
    let actual = get_permutations(&input);

    assert_eq!(expected, actual);
}

fn get_variant_type(variant: &Variant) -> Option<&Ident> {
    let fields = variant.fields.iter().take(2).collect::<Vec<_>>();

    match fields.as_slice() {
        &[] => None,
        &[field] => Some(get_field_type(field)),
        _ => panic!(
            "{}: Variants can only have zero or one fields",
            variant.ident
        ),
    }
}

fn get_field_type(field: &Field) -> &Ident {
    match &field.ty {
        Type::Path(path) => path.path.get_ident().expect("path does not have Ident"),
        _ => panic!("field type is not Type::Path"),
    }
}

fn get_enum_index_fn(item_enum: &ItemEnum, input: &Input) -> proc_macro2::TokenStream {
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
        let start_index = {
            let mut index_sum = 0;

            item_enum
                .variants
                .iter()
                .map(|variant| {
                    if let Some(inner) = get_variant_type(variant) {
                        input.get_length(inner)
                    } else {
                        1
                    }
                })
                .map(move |len| {
                    let index = index_sum;
                    index_sum += len;
                    index
                })
        };

        let ty = &item_enum.ident;
        let match_variants = item_enum
            .variants
            .iter()
            .zip(start_index)
            .map(|(variant, index)| {
                let var_ident = &variant.ident;
                if get_variant_type(variant).is_some() {
                    quote! { #ty :: #var_ident(inner) => #index + inner.index(), }
                } else {
                    quote! { #ty :: #var_ident => #index, }
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

fn get_struct_index_fn(item_struct: &ItemStruct, input: &Input) -> proc_macro2::TokenStream {
    let variants: Vec<_> = get_all_variants(&item_struct.ident, input);

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

fn get_enum_tests(input: &Input) -> proc_macro2::TokenStream {
    let tests = input.enums.iter().map(|e| {
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
        mod tests {
            use super::*;

            #[test]
            fn enum_index_tests() {
                #( #tests )*
            }
        }
    }
}
