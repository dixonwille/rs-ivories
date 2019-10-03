mod ast;

fn main() {
    let data = "(-3 + 2) * 5";
    println!("{:#?}", ast::parse_expression(data))
}
