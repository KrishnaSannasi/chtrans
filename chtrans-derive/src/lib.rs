
extern crate proc_macro;
use proc_macro::TokenStream;
use syn::spanned::Spanned as _;
use quote::quote;

use syn::parse::{self, Parse};

#[derive(Debug)]
enum Type {
    ReprC,
    Packed
}

#[derive(Debug)]
struct Repr {
    align: Option<u32>,
    ty: Type,
}

impl Parse for Repr {
    fn parse(buf: &syn::parse::ParseBuffer<'_>) -> parse::Result<Self> {
        use syn::Token;
        let mut ty = None::<Type>;
        let mut align = None;

        let span = buf.cursor().span();

        while let Ok(meta) = buf.parse::<syn::NestedMeta>() {
            match meta {
                syn::NestedMeta::Meta(syn::Meta::Path(meta)) => {
                    if meta.is_ident("C") {
                        if ty.is_some() {
                            return Err(syn::Error::new(meta.span(), "You cannot use `repr(C)` multiple times or with `repr(packed)`"))
                        }
                        ty = Some(Type::ReprC)
                    } else if meta.is_ident("packed") {
                        if ty.is_some() {
                            return Err(syn::Error::new(meta.span(), "You cannot use `repr(packed)` multiple times or with `repr(C)`"))
                        }
                        ty = Some(Type::Packed)
                    } else {
                        return Err(syn::Error::new(meta.span(), "unknown attribute"))
                    }
                }
                syn::NestedMeta::Meta(syn::Meta::List(meta)) => {
                    if meta.path.is_ident("align") {
                        if meta.nested.len() != 1 {
                            return Err(syn::Error::new(meta.span(), "`align` should only have 1 argument, which should be a power of 2"))
                        }

                        if let syn::NestedMeta::Lit(syn::Lit::Int(ref value)) = meta.nested[0] {
                            let align_value = if let Ok(align) = value.base10_parse::<u32>() {
                                align
                            } else {
                                return Err(syn::Error::new(meta.span(), "`align` should be an integer power of 2"))
                            };

                            if !align_value.is_power_of_two() {
                                return Err(syn::Error::new(meta.span(), "`align` should be an integer power of 2"))
                            }
                            
                            align = Some(align_value);
                        }
                    } else {
                        return Err(syn::Error::new(meta.span(), "unknown attribute"))
                    }
                }
                syn::NestedMeta::Meta(syn::Meta::NameValue(meta)) => {
                    if meta.path.is_ident("align") {
                        return Err(syn::Error::new(meta.span(), "use `align` like this: align(...)"))
                    } else {
                        return Err(syn::Error::new(meta.span(), "unknown attribute"))
                    }
                }
                syn::NestedMeta::Lit(meta) => {
                    return Err(syn::Error::new(meta.span(), "unknown attribute"))
                }
            }

            if let Err(e) = buf.parse::<Token![,]>() {
                if !buf.is_empty() {
                    return Err(e)
                }

                break
            }
        }

        if !buf.is_empty() {
            return Err(syn::Error::new(span, "invalid meta on attribute"));
        }

        let ty = ty.ok_or_else(|| syn::Error::new(span, "a representation type was not specified, please pick `C` or `packed`"))?;

        Ok(Repr { ty, align })
    }
}

#[proc_macro_attribute]
pub fn repr(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = syn::parse_macro_input!(attr as Repr);
    let input = syn::parse_macro_input!(item as syn::DeriveInput);

    match input.data {
        syn::Data::Struct(data) => repr_struct(attr, input.attrs, input.vis, input.ident, input.generics, data),
        syn::Data::Union(data) => repr_union(attr, input.attrs, input.vis, input.ident, input.generics, data),
        syn::Data::Enum(_) => TokenStream::from(quote!(compile_error!{"enums are not allowed to use `chtrans::repr` attributes"}))
    }
}

struct SliceToTokens<'a, T>(&'a [T]);

impl<T: quote::ToTokens> quote::ToTokens for SliceToTokens<'_, T> {
    fn to_tokens(&self, token_stream: &mut proc_macro2::TokenStream) {
        for item in self.0 {
            quote::ToTokens::to_tokens(item, token_stream)
        }
    }
}

fn repr_struct(
    repr_attr: Repr,
    attrs: Vec<syn::Attribute>,
    vis: syn::Visibility,
    ident: syn::Ident,
    generics: syn::Generics,
    data: syn::DataStruct,
) -> TokenStream {
    let repr_ty = data.fields.iter()
    .map(|field| &field.ty)
    .fold(
        quote!{::chtrans::hlist::Nil},
        |rest, field| quote! {::chtrans::hlist::Cons<#rest, #field>},
    );

    let repr = match repr_attr.ty {
        Type::ReprC => quote!{::chtrans::repr::ReprC},
        Type::Packed => quote!{::chtrans::repr::Packed},
    };

    let attrs = SliceToTokens(&attrs);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    
    TokenStream::from(match data.fields {
        syn::Fields::Unit => quote! {
            #attrs
            #[repr(C)]
            #vis struct #ident #generics;
            unsafe impl #impl_generics ::chtrans::Type for #ident #ty_generics #where_clause {
                type Repr = ::chtrans::repr::r#struct::Struct<#repr, #repr_ty>;
            }
        },
        syn::Fields::Unnamed(fields) => quote! {
            #attrs
            #[repr(C)]
            #vis struct #ident #fields #generics;
            unsafe impl #impl_generics ::chtrans::Type for #ident #ty_generics #where_clause {
                type Repr = ::chtrans::repr::r#struct::Struct<#repr, #repr_ty>;
            }
        },
        syn::Fields::Named(fields) => quote! {
            #attrs
            #[repr(C)]
            #vis struct #ident #generics #fields
            unsafe impl #impl_generics ::chtrans::Type for #ident #ty_generics #where_clause {
                type Repr = ::chtrans::repr::r#struct::Struct<#repr, #repr_ty>;
            }
        }
    })
}

fn repr_union(
    repr_attr: Repr,
    attrs: Vec<syn::Attribute>,
    vis: syn::Visibility,
    ident: syn::Ident,
    generics: syn::Generics,
    data: syn::DataUnion,
) -> TokenStream {
    todo!("unions are not suppported yet")
}
