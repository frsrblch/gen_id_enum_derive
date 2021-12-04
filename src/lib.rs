extern crate proc_macro;
use crate::input::Input;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Field, Fields, Ident, Item, ItemEnum, ItemStruct, Type, Variant};

mod input;

#[proc_macro]
pub fn multi_enum_array(input: TokenStream) -> TokenStream {
    let input = &parse_macro_input!(input as Input);

    let enum_tokens = input.enums.iter().map(|ty| {
        let impl_enum = input.get_impl_enum(ty);

        let array: proc_macro2::TokenStream = get_array_struct(&ty.ident);

        quote! {
            #ty

            #impl_enum

            #array
        }
    });

    let struct_tokens = input.structs.iter().map(|ty| {
        let impl_struct: proc_macro2::TokenStream = input.get_impl_struct(ty);

        let array: proc_macro2::TokenStream = get_array_struct(&ty.ident);

        quote! {
            #ty

            #impl_struct

            #array
        }
    });

    let tests = input.get_enum_tests();

    let tokens = quote! {
        #( #enum_tokens )*

        #( #struct_tokens )*

        #tests
    };

    tokens.into()
}

fn get_array_struct(ident: &Ident) -> proc_macro2::TokenStream {
    let ty = ident;

    let array = &Ident::new(&format!("{}Array", ident), ident.span());

    quote! {
        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
        pub struct #array <T> {
            values: [T; #ty::LEN],
        }

        impl<T> From<[T; #ty::LEN]> for #array <T> {
            fn from(array: [T; #ty::LEN]) -> Self {
                Self::new(array)
            }
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
