# retrobasic - A BASIC interpreter written in Rust

## Overview

retrobasic is a BASIC interpreter written in Rust. It is based on the
original BASIC '64 implementation, and modified as needed to make it more
compatible with some of the classic BASIC games.

## Features

* Supports running local BASIC programs
* Supports fetching and running classic BASIC games from the Internet

The programs that retrobasic can fetch are located at the [Vintage Basic](http://www.vintage-basic.net/games.html) web site.
Thanks to Lyle Kopnicky for the scanning and hosting these classic programs.

For example:
```
% retrobasic play hammurabi
                               HAMURABI
              CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY



TRY YOUR HAND AT GOVERNING ANCIENT SUMERIA
FOR A TEN-YEAR TERM OF OFFICE.



HAMURABI:  I BEG TO REPORT TO YOU,
IN YEAR 1 , 0 PEOPLE STARVED, 5 CAME TO THE CITY,
POPULATION IS NOW 100
THE CITY NOW OWNS  1000 ACRES.
YOU HARVESTED 3 BUSHELS PER ACRE.
THE RATS ATE 200 BUSHELS.
YOU NOW HAVE  2800 BUSHELS IN STORE.

LAND IS TRADING AT 21 BUSHELS PER ACRE.
HOW MANY ACRES DO YOU WISH TO BUY?

```

You can see a full list of the known games by issuing the `retrobasic list` command.

## Building

1. You need to have Rust installed - see the [Rust Web Site](https://www.rust-lang.org/index.html) for latest info on installing Rust.

```
cargo build
```

## Testing

Included unit tests can be run with:
```
cargo test
```


## Usage

The basic usage and options are outlined below (also available with the --help flag).

```
USAGE:
    retrobasic [SUBCOMMAND]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    help    Prints this message or the help of the given subcommand(s)
    list    List of known classic BASIC programs
    play    Play a known classic BASIC program
    run     Run a BASIC program from a specified file
```


## References

Some reference documentation:
* [BASIC Grammer](https://rosettacode.org/wiki/BNF_Grammar#BASIC/)
* [MITS Altair BASIC](https://archive.org/stream/bitsavers_mitsMITSAl_6669937/MITS_AltairBASIC_1975#page/n38/mode/1up)
* [Writing BASIC Interpreters](https://sites.google.com/site/smallbasicinterpreters/)
* [LLVM AST](http://llvm.org/docs/tutorial/LangImpl03.html)
* [BASIC Games](http://www.atariarchives.org/basicgames/)

## Author(s)

Salim Alam

## License

Source code is released under the Apache 2.0 license as follows:

Copyright 2017 Salim Alam

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
