# basically [![Build Status](https://travis-ci.org/soudy/basically.svg?branch=master)](https://travis-ci.org/soudy/basically)

A basic BASIC interpreter, basically. A modern implementation of the BASIC V2.0
language from the [Commodore 64](https://www.c64-wiki.com/wiki/C64) in Clojure.
It is _not_ a Commodore 64 emulator, so certain operations like `POKE` and
`PEEK` are not implemented.

## Usage

Run using lein

    $ lein run

Build and run standalone JAR

    $ lein uberjar
    $ java -jar ./target/uberjar/basically-VERSION-standalone.jar

## Example

Calculating the factorial of 10.

```BASIC
10 REM FACTORIAL CALC USING SIMPLE LOOP
20 N=10 : F=1
30 FOR I=1 TO N
40   F = F*I
50 NEXT
60 PRINT N"! ="F
```

Executing program:

    $ basically examples/fibonacci.b
     10 ! = 3628800

## License

Copyright © 2018 Steven Oud

Distributed under the MIT license.
