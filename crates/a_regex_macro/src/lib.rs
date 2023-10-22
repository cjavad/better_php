extern crate proc_macro;

fn generate_ignore_case_regex(input: String) -> String {
  let mut output = String::new();
  for c in input.chars() {
    output.push_str(&format!("[{}{}]", c.to_lowercase(), c.to_uppercase()));
  }
  output
}

#[proc_macro]
pub fn ignore_case(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = syn::parse::<syn::LitStr>(input).unwrap();
  let regex = generate_ignore_case_regex(input.value());
  quote::quote!(#regex).into()
}