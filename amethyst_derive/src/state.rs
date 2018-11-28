use proc_macro2::{Span, TokenStream};
use syn::{Data, DeriveInput, Fields, Ident};

/// Derive to implement the State trait.
pub fn impl_state(ast: &DeriveInput) -> TokenStream {
    let base = &ast.ident;
    let storage = Ident::new(&format!("{}StateStorage", base), Span::call_site());

    let en = match ast.data {
        Data::Enum(ref en) => en,
        _ => panic!("`State` attribute is only supported on enums"),
    };

    let callback = quote!(Box<dyn amethyst::dynamic::StateCallback<#base, E>>);

    let mut fields = Vec::new();
    let mut variants = Vec::new();
    let mut names = Vec::new();

    for (i, variant) in en.variants.iter().enumerate() {
        match variant.fields {
            Fields::Unit => {}
            _ => panic!("Only unit fields are supported in state enums"),
        }

        let field = Ident::new(&format!("f{}", i), Span::call_site());
        let var = &variant.ident;

        variants.push(quote!(#base::#var => return &mut self.#field));
        fields.push(quote!(#field: Option<#callback>));
        names.push(field);
    }

    let names = &names;

    quote! {
        #[allow(non_camel_case_types)]
        pub struct #storage<E> {
            #(#fields,)*
        }

        impl<E> Default for #storage<E> {
            fn default() -> #storage<E> {
                #storage { #(#names: None,)* }
            }
        }

        impl<E> amethyst::dynamic::StateStorage<#base, E> for #storage<E> {
            fn get_mut(&mut self, value: &#base) -> &mut Option<#callback> {
                match *value { #(#variants,)* }
            }

            fn do_values<F>(&mut self, mut apply: F) where F: FnMut(&mut #callback) {
                #(
                if let Some(c) = self.#names.as_mut() {
                    apply(c);
                }
                )*
            }
        }

        impl<E> amethyst::dynamic::State<E> for #base {
            type Storage = #storage<E>;
        }
    }
}
