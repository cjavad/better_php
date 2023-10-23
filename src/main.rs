use clap::{Args, Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(version = "1.0", author = "Your Name")]
struct Cli {
    #[command(subcommand)]
    command: Development,
}

#[derive(Args, Debug)]
struct FileTarget {
    #[arg(short = 'f')]
    file: String,
}

#[derive(Subcommand, Debug)]
enum Development {
    #[command(name = "lexer")]
    Lexer(FileTarget),
    #[command(name = "parser")]
    Parser(FileTarget),
}

fn get_file_contents_as_string(file_path: &str) -> String {
    use std::fs::File;
    use std::io::Read;

    let mut file = File::open(file_path).expect("Unable to open file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("Unable to read file");

    contents
}

fn main() {
    let cli = Cli::parse();


    match cli.command {
        Development::Lexer(file_target) => {
            let file_contents = get_file_contents_as_string(&file_target.file);
            a_parser::token::lexerize(file_contents.as_str()).for_each(|token| {
                match token {
                    Ok(token) => {
                        println!("{:?}", token);
                    }
                    Err(_) => {
                        println!("Error");
                    }
                }
            });
        }
        Development::Parser(file_target) => {
            let file_contents = get_file_contents_as_string(&file_target.file);
            let mut tokens = a_parser::token::lexerize(file_contents.as_str());
            let _ = a_parser::parser::parse(&mut tokens);
        }
    }
}
