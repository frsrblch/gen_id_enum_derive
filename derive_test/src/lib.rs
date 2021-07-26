use gen_id_enum_derive::many_enum_array;

// rather than derive, this should be a function-like macro inside which we define the enums

// the proc_macro requires knowledge of all enums to construct a compound array enum type

many_enum_array! {

    pub enum Ore {
        Iron,
        Copper,
        Chromium,
        Aluminum,
        Titanium,
        Uranium,
    }

    pub enum Gas {
        Hydrogen,
        Helium,
        Nitrogen,
        Oxygen,
        Xenon,
    }

    pub enum Resource {
        Ore(Ore),
        Gas(Gas),
        Water,
        Oil,
    }

    pub enum Material {
        Steel,
        Copper,
        Aluminum,
        Titanium,
        Uranium,
    }

    pub enum Consumables {
        Grain,
        Produce,
        Animals,
        Narcotics,
    }

    pub enum Fuel {
        Deuterium,
        UraniumRods,
        Antimatter,
    }

    pub enum Goods {
        Res(Resource),
        Mat(Material),
        Fuel(Fuel),
        Cons(Consumables),
    }

}

// many_enum_array! {
//     pub enum EnumA {
//         Option1,
//         Option2,
//     }
//
//     pub enum Enum {
//         VariantA(EnumA),
//         VariantB,
//         VariantC,
//     }
// }
//
// #[test]
// fn enum_len() {
//     assert_eq!(4, Enum::LEN);
//     assert_eq!(2, EnumA::LEN);
// }
//
// impl Enum {
//     pub const fn index(&self) -> usize {
//         match self {
//             Enum::VariantA(enum_a) => 0 + enum_a.index(),
//             Enum::VariantB => EnumA::LEN,
//             Enum::VariantC => EnumA::LEN + 1,
//         }
//     }
// }
//
// impl EnumA {
//     pub const fn index(&self) -> usize {
//         match self {
//             EnumA::Option1 => 0,
//             EnumA::Option2 => 1,
//         }
//     }
// }

// use iter_context::*;
// use std::ops::AddAssign;

// pub enum EnumA {
//     Option1,
//     Option2,
// }
//
// impl EnumA {
//     const LEN: usize = 2;
//
//     pub fn array() -> Array<Self, { Self::LEN }> {
//         Array([EnumA::Option1, EnumA::Option2])
//     }
// }
//
// pub struct Array<T, const LEN: usize>([T; LEN]);

// pub struct EnumAArray<T>([T; EnumA::LEN]);
//
// impl<T> EnumAArray<T> {
//     pub fn iter(&self) -> Iter<EnumA, T> {
//         Iter::new(self.0.iter())
//     }
//
//     pub fn iter_mut(&mut self) -> IterMut<EnumA, T> {
//         IterMut::new(self.0.iter_mut())
//     }
// }
//
// impl<'a, T> IntoIterator for &'a EnumAArray<T> {
//     type Item = &'a T;
//     type IntoIter = std::slice::Iter<'a, T>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         self.0.iter()
//     }
// }
//
// impl<'a, T> IntoIterator for &'a mut EnumAArray<T> {
//     type Item = &'a mut T;
//     type IntoIter = std::slice::IterMut<'a, T>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         self.0.iter_mut()
//     }
// }
//
// impl<'a, T> ContextualIterator for &'a EnumAArray<T> {
//     type Context = EnumA;
// }
//
// impl<'a, T> ContextualIterator for &'a mut EnumAArray<T> {
//     type Context = EnumA;
// }
//
// pub struct EnumArray<T>([T; Enum::LEN]);
//
// impl<T> EnumArray<T> {
//     pub fn iter(&self) -> Iter<Enum, T> {
//         Iter::new(self.0.iter())
//     }
//
//     pub fn iter_mut(&mut self) -> IterMut<Enum, T> {
//         IterMut::new(self.0.iter_mut())
//     }
// }
//
// impl<'a, T> IntoIterator for &'a EnumArray<T> {
//     type Item = &'a T;
//     type IntoIter = std::slice::Iter<'a, T>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         self.0.iter()
//     }
// }
//
// impl<'a, T> IntoIterator for &'a mut EnumArray<T> {
//     type Item = &'a mut T;
//     type IntoIter = std::slice::IterMut<'a, T>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         self.0.iter_mut()
//     }
// }
//
// impl<'a, T> ContextualIterator for &'a EnumArray<T> {
//     type Context = Enum;
// }
//
// impl<'a, T> ContextualIterator for &'a mut EnumArray<T> {
//     type Context = Enum;
// }
//
// impl<T, Rhs> std::ops::AddAssign<Rhs> for EnumArray<T>
// where
//     T: AddAssign<Rhs::Item>,
//     Rhs: ContextualIterator<Context = Enum>,
// {
//     fn add_assign(&mut self, rhs: Rhs) {
//         self.iter_mut()
//             .zip(rhs)
//             .for_each(|(lhs, rhs)| lhs.add_assign(rhs))
//     }
// }
//
// impl<'a, T, U: 'a> std::ops::AddAssign<&'a EnumAArray<U>> for EnumArray<T>
// where
//     T: AddAssign<&'a U>,
// {
//     fn add_assign(&mut self, rhs: &'a EnumAArray<U>) {
//         self.0[0..2]
//             .iter_mut()
//             .zip(rhs.0.iter())
//             .for_each(|(lhs, rhs)| lhs.add_assign(rhs))
//     }
// }
//
// #[test]
// fn enum_component_add_test() {
//     use gen_id_component::Component;
//     let mut enum_comp = Component::<(), EnumArray<f32>>::from(vec![EnumArray([1., 2., 3., 4.])]);
//     let enum_a_comp = Component::<(), EnumAArray<f32>>::from(vec![EnumAArray([2., 3.])]);
//
//     enum_comp += &enum_a_comp;
//
//     let array: &EnumArray<f32> = enum_comp.iter().into_iter().next().unwrap();
//     assert_eq!([3., 5., 3., 4.], array.0);
// }
