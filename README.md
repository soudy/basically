# basically [![Build Status](https://travis-ci.org/soudy/basically.svg?branch=master)](https://travis-ci.org/soudy/basically)

A basic BASIC interpreter, basically. A modern implementation of the BASIC V2.0
language from the [Commodore 64](https://www.c64-wiki.com/wiki/C64) in Clojure.
It is _not_ a Commodore 64 emulator, so certain operations like `POKE` and
`PEEK` are not implemented.

## Installation

Run using lein

    $ lein run

Build and run standalone JAR

    $ lein uberjar
    $ java -jar ./target/uberjar/basically-VERSION-standalone.jar

Or download a jar from the [releases page](https://github.com/soudy/basically/releases).

## Usage
Start a REPL
```
$ basically
READY.
PRINT "HELLO WORLD"
HELLO WORLD

READY.
```

Run a program
```
$ basically examples/factorial.b
 10 ! = 3628800
```
For more examples, see the examples directory.

## License

Copyright © 2018 Steven Oud

Distributed under the MIT license.
