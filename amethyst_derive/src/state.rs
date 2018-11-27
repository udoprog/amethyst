use proc_macro2::{Span, TokenStream};
use syn::{Data, DataEnum, DataStruct, DeriveInput, Fields, Ident};

/// Derive to implement the `State` trait.
pub fn impl_state(ast: &DeriveInput) -> TokenStream {
    match ast.data {
        Data::Enum(ref en) => return impl_state_enum(ast, en),
        Data::Struct(ref st) => return impl_state_struct(ast, st),
        _ => panic!("`State` attribute is only supported on enums"),
    }
}

/// Implement `State` for enums.
pub fn impl_state_enum(ast: &DeriveInput, en: &DataEnum) -> TokenStream {
    let base = &ast.ident;
    let storage = Ident::new(&format!("{}StateStorage", base), Span::call_site());

    let callback = quote!(Box<dyn amethyst::state::StateHandler<#base, E>>);

    let mut field_inits = Vec::new();
    let mut fields = Vec::new();
    let mut get_mut = Vec::new();
    let mut insert = Vec::new();
    let mut do_values = Vec::new();

    let first = en
        .variants
        .iter()
        .next()
        .expect("enum must have at least one variant");

    let default_fn = match first.fields {
        Fields::Unit => {
            let ident = &first.ident;
            quote!(#base::#ident)
        }
        Fields::Unnamed(ref unnamed) => {
            if unnamed.unnamed.len() != 1 {
                panic!("Unnamed variants must have exactly one element");
            }

            let ident = &first.ident;
            quote!(#base::#ident(Default::default()))
        }
        _ => {
            panic!("Only unit fields are supported in state enums");
        }
    };

    for (i, variant) in en.variants.iter().enumerate() {
        let field = Ident::new(&format!("f{}", i), Span::call_site());

        match variant.fields {
            Fields::Unit => {
                let var = &variant.ident;
                let m = quote!(#base::#var);

                fields.push(quote!(#field: Option<#callback>));
                field_inits.push(quote!(#field: None));
                insert.push(
                    quote!(#m => return ::std::mem::replace(&mut self.#field, Some(callback))),
                );
                get_mut.push(quote!(#m => return self.#field.as_mut()));
                do_values.push(quote! {
                    if let Some(c) = self.#field.as_mut() {
                        apply(c);
                    }
                });
            }
            Fields::Unnamed(ref unnamed) => {
                let var = &variant.ident;
                let m = quote!(#base::#var(value));
                let m_ref = quote!(#base::#var(ref value));
                let element = unnamed.unnamed.first().expect("Expected one element");

                if unnamed.unnamed.len() > 1 {
                    panic!("Unnamed variants must have exactly one element");
                }

                fields.push(
                    quote!(#field: <#element as amethyst::state::State<E, #callback>>::Storage),
                );
                field_inits.push(quote!(#field: Default::default()));
                insert.push(quote!(#m => return self.#field.insert(value, callback)));
                get_mut.push(quote!(#m_ref => return self.#field.get_mut(value)));
                do_values.push(quote!(self.#field.do_values(&mut apply);));
            }
            _ => panic!("Only unit fields are supported in state enums"),
        }
    }

    quote! {
        impl Default for #base {
            fn default() -> #base {
                #default_fn
            }
        }

        #[allow(non_camel_case_types)]
        pub struct #storage<E> {
            #(#fields,)*
        }

        impl<E> Default for #storage<E> {
            fn default() -> #storage<E> {
                #storage { #(#field_inits,)* }
            }
        }

        impl<E> amethyst::state::StateStorage<#base, #callback> for #storage<E> {
            fn insert(
                &mut self,
                state: #base,
                callback: #callback,
            ) -> Option<#callback> {
                match state {
                    #(#insert,)*
                }
            }

            fn get_mut(&mut self, value: &#base) -> Option<&mut #callback> {
                match *value {
                    #(#get_mut,)*
                }
            }

            fn do_values<F>(&mut self, mut apply: F) where F: FnMut(&mut #callback) {
                #(#do_values)*
            }
        }

        impl<E> amethyst::state::State<E> for #base {
            type Storage = #storage<E>;
        }
    }
}

/// Implement `State` for structs.
///
/// This requires the struct itself to be `Hash + PartialEq + Eq + Default`.
pub fn impl_state_struct(ast: &DeriveInput, _: &DataStruct) -> TokenStream {
    let base = &ast.ident;

    quote! {
        impl<E, T> amethyst::state::State<E, T> for #base {
            type Storage = amethyst::state::MapStateStorage<Self, T>;
        }
    }
}
