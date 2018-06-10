use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

extern crate clap;
use clap::{App, Arg, SubCommand};

extern crate dimsum;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rand;
extern crate serde;
extern crate serde_json;

extern crate futures;
extern crate hyper;
#[macro_use]
extern crate serde_derive;
extern crate tokio_core;

mod ast;
mod error;
mod fetcher;
mod interpreter;
mod lexer;
mod parser;
mod tokenid;

use fetcher::Fetcher;
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;

fn main() {
    env_logger::init();

    let matches = App::new("retrobasic")
        .version("0.1")
        .author("Salim Alam")
        .about("BASIC interpreter")
        .subcommand(SubCommand::with_name("list").about("List of known classic BASIC programs"))
        .subcommand(
            SubCommand::with_name("play")
                .about("Play a known classic BASIC program")
                .arg(
                    Arg::with_name("NAME")
                        .short("n")
                        .help("Name of classic BASIC programs")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("run")
                .about("Run a BASIC program from a specified file")
                .arg(
                    Arg::with_name("FILENAME")
                        .short("f")
                        .help("File path of BASIC program")
                        .required(true)
                        .index(1),
                ),
        )
        .get_matches();

    if let Some(_matches) = matches.subcommand_matches("list") {
        let fetcher = Fetcher::new();
        println!("{}", fetcher);
        std::process::exit(0);
    }

    let prog = if let Some(matches) = matches.subcommand_matches("play") {
        let name = matches.value_of("NAME").unwrap();
        let fetcher = Fetcher::new();
        match fetcher.fetch(name) {
            Some(b) => b,
            None => {
                println!("Program '{}' not found!", name);
                std::process::exit(1);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let filename = matches.value_of("FILENAME").unwrap();
        let path = Path::new(&filename);
        let mut file = match File::open(&path) {
            Err(why) => {
                println!("Couldn't open {}: {}", filename, why.description());
                std::process::exit(1);
            }
            Ok(file) => file,
        };

        let mut b = String::new();
        match file.read_to_string(&mut b) {
            Err(why) => {
                println!("couldn't read {}: {}", filename, why.description());
                std::process::exit(1);
            }
            Ok(_) => {}
        };
        b
    } else {
        println!("How about a nice game of Chess?");
        std::process::exit(1);
    };

    let lexer = Lexer::new(prog);
    let ast = match Parser::new(lexer).parse() {
        Ok(ast) => ast,
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        }
    };

    let mut interpreter = Interpreter::new();
    match interpreter.run(&ast) {
        Ok(_) => (),
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        }
    };
}
