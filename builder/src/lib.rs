use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::*;

fn is_option(ty: &Type) -> bool {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        return segments.iter().last().is_some_and(|x| x.ident == "Option");
    }
    false
}

fn unwrap_option(ty: &Type) -> Option<&Type> {
    if let Type::Path(ref p) = ty {
        return p.path.segments.first().and_then(|f| {
            if f.ident != "Option" {
                return None;
            }

            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref args, ..
            }) = f.arguments
            {
                let arg = if let Some(arg) = args.first() {
                    arg
                } else {
                    return None;
                };

                if let GenericArgument::Type(ref t) = arg {
                    Some(t)
                } else {
                    None
                }
            } else {
                None
            }
        });
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = parse_macro_input!(input);

    println!("{:?}", ast.attrs);

    let name = ast.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), name.span());

    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let option_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if is_option(&ty) {
            quote! {
                #name: #ty
            }
        } else {
            quote! {
                #name: Option<#ty>
            }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;

        // println!("{:?}", f.attrs);

        // let builder = f.attrs.iter().find(|a| if let Attribute {} = a {});

        let ty = unwrap_option(&f.ty).unwrap_or(&f.ty);

        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let default_values = fields.iter().map(|f| {
        let name = &f.ident;

        quote! {
            #name: None
        }
    });

    let build_content = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if is_option(&ty) {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let tokens = quote! {
        pub struct #builder_name {
            #(#option_fields,)*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#default_values,)*
                }
            }
        }

        impl #builder_name {
            #(#methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#build_content,)*
                })
            }
        }
    };

    tokens.into()
}
