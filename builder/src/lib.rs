use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};
use syn::spanned::Spanned;


#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match do_expend(&input) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into()
    }
}

type FiledType = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

fn parse_fields_name(input: &DeriveInput) -> syn::Result<&FiledType> {
    // input是引用类型 那么匹配时named也需要采用引用类型
    if let syn::Data::Struct(syn::DataStruct{
        fields: syn::Fields::Named(syn::FieldsNamed{ref named,..}),
        .. }) = input.data {
        return Ok(named);
    };
    Err(syn::Error::new_spanned(input, "parse fields"))
}

fn repeat_fields(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream>{
    let fields = parse_fields_name(input)?;
    let field_names: Vec<_> = fields.iter().map(|f|&f.ident).collect();
    let field_types: Vec<_> = fields.iter().map(|f|{
       if parse_struct_attributes(&f).is_some(){
           let origin_type = &f.ty;
           quote!(#origin_type)
       } else if let Some(origin_type) = match_struct_type(&f.ty, "Option"){
           quote!(std::option::Option<#origin_type>)
       } else if parse_struct_attributes(&f).is_some() {
           let origin_type = &f.ty;
           quote!(#origin_type)
       } else {
           let origin_type = &f.ty;
           quote!(std::option::Option<#origin_type>)
        }
    }).collect();

    // 将数组进行对应展开 *表示一个或多个
    let ret = quote!(
        #(#field_names: #field_types),*
    );
    Ok(ret)
}

fn repeat_init(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream>{
    let fields = parse_fields_name(input)?;
    let field_names: Vec<_> = fields.iter().map(|f|{
        let ident = &f.ident;
        if parse_struct_attributes(&f).is_some() {
            quote!(
                #ident: std::vec::Vec::new()
            )
        } else {
            quote!(
                #ident: std::option::Option::None
            )
        }
    }).collect();
    let ret = quote!(
        #(#field_names),*
    );
    Ok(ret)
}

fn repeat_impl_func(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream>{
    let fields = parse_fields_name(input)?;
    let field_names: Vec<_> = fields.iter().map(|f|&f.ident).collect();
    let field_types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut func_tokenstream_list = proc_macro2::TokenStream::new();

    for (index, (&k, &v)) in field_names.iter().zip(field_types.iter()).enumerate() {
        let ret = if let Some(inner_type) = match_struct_type(v, "Option") {
            quote!(
                fn #k(&mut self, #k: #inner_type) -> &mut Self {
                    self.#k = std::option::Option::Some(#k);
                    self
                }
            )
        } else if let Some(ref specified_name) = parse_struct_attributes(&fields[index]){
            let inner_type = match_struct_type(v, "Vec").ok_or(syn::Error::new(fields[index].span(), "specified field must vec"))?;

            let mut tokenstram = quote!(
                fn #specified_name(&mut self, #specified_name: #inner_type) -> &mut Self {
                    self.#k.push(#specified_name);
                    self
                }
            );

            if specified_name != k.as_ref().unwrap() {
                tokenstram.extend(quote!(
                    fn #k(&mut self, #k: #v) -> &mut Self {
                        self.#k = #k.clone();
                        self
                    }
                ))
            }
            tokenstram
        } else {
            quote!(
                fn #k(&mut self, #k: #v) -> &mut Self {
                    self.#k = std::option::Option::Some(#k);
                    self
                }
            )
        };

        func_tokenstream_list.extend(ret);
    }

    Ok(func_tokenstream_list)
}

fn check_struct_fields(input: &DeriveInput) -> syn::Result<Vec<proc_macro2::TokenStream>>{
    let struct_name_indent = &input.ident;
    let fields = parse_fields_name(input)?;
    let field_names: Vec<_> = fields.iter().map(|f|&f.ident).collect();

    let mut tokenstreams = Vec::new();

    for (index, k) in field_names.iter().enumerate() {
        let ident = k.as_ref().unwrap();
        if match_struct_type(&fields[index].ty, "Option").is_none() &&
            parse_struct_attributes(&fields[index]).is_none(){
            tokenstreams.push(quote!(
                if self.#ident.is_none() {
                    let err = format!("{} is nonce", stringify!(#ident));
                    return std::result::Result::Err(err.into());
                }
            ));
        }

    }

    let mut instance_tokenstreams = Vec::new();
    for (index, v) in field_names.iter().enumerate() {
        let ident = v.as_ref().unwrap();
        if parse_struct_attributes(&fields[index]).is_some() {
            instance_tokenstreams.push(quote!(
                #ident: self.#ident.clone()
            ))
        }else if match_struct_type(&fields[index].ty, "Option").is_none() {
            instance_tokenstreams.push(quote!(
                #ident: self.#ident.clone().unwrap()
            ))
        } else {
            instance_tokenstreams.push(quote!(
            #ident: self.#ident.clone()
        ))
        }

    }

    tokenstreams.push(quote!(
        let command = #struct_name_indent {
            #(#instance_tokenstreams),*
        };
        std::result::Result::Ok(command)
    ));


    Ok(tokenstreams)
}

fn match_struct_type<'a>(ty: &'a syn::Type, outer_ident_name: &'a str) -> Option<&'a syn::Type>{
    if let syn::Type::Path(syn::TypePath{path: syn::Path{segments, ..}, ..}) = ty {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments{
                        args, ..}) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}


fn parse_struct_attributes(input: &syn::Field) -> Option<syn::Ident>{
    for attr in &input.attrs {
        if let Ok(syn::Meta::List(syn::MetaList{ref path, ref nested, ..})) = attr.parse_meta(){
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first(){
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Some(syn::Ident::new(ident_str.value().as_str(), attr.span()));
                            }
                        }
                    }
                }
            }
        }
    }

    None
}



fn do_expend(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream>{
    let struct_name = input.ident.to_string(); // 结构体名
    let new_struct_name = format!("{}Builder", struct_name);
    // 将new_struct_name构建新的语法树，span()方法是将原始的struct的span传入，方便new_struct错误能够精确定位
    let new_struct_ident = syn::Ident::new(&new_struct_name, struct_name.span());

    let struct_name_indent = &input.ident;

    let repeat_fields_tokenstream = repeat_fields(input)?;
    let init_fields_tokenstream = repeat_init(input)?;
    let repeat_func_tokenstream = repeat_impl_func(input)?;
    let check_struct_tokenstreams = check_struct_fields(input)?;

    // 生成新的代码
    let ret = quote!{
        // 模板语法
        pub struct #new_struct_ident {
            #repeat_fields_tokenstream
        }

        impl #struct_name_indent {
            pub fn builder() -> #new_struct_ident {
                #new_struct_ident {
                    #init_fields_tokenstream
                }
            }
        }

        impl #new_struct_ident {
            #repeat_func_tokenstream

            pub fn build(&mut self) -> std::result::Result<#struct_name_indent, std::boxed::Box<dyn std::error::Error>> {
                // 将列表展开
                #(#check_struct_tokenstreams)*
            }

        }


    };
    Ok(ret)
}