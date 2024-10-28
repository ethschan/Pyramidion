# Pyramidion

An esoteric language where everything is structured as a pyramid.

## What is this language?

[Pyramidion](https://en.wikipedia.org/wiki/Pyramidion) uses pyramids to structure code, evaluating expressions from the base up to the peak. Operators at higher levels share results from lower levels, requiring careful planning of the pyramid’s structure. 

Inspired by [Kaleidoscope](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) and developed to explore LLVM and compiler optimization.

## Example Syntax

```
      +
     + +
    * 0 *
   x x y y
pyramid bar(x, y)

 bar
 + +
1 1 2
```

- The function `bar(x, y)` squares two numbers and then adds them.
- The `main` pyramid calls `bar(1 + 1, 1 + 2)`.

## Commands

| Name     | Arity | Function            |
|----------|-------|---------------------|
| `+`      | 2     | `a + b`             |
| `-`      | 2     | `a - b`             |
| `*`      | 2     | `a * b`             |
| `/`      | 2     | `a / b`             |
| pyramid  | N/A   | Function definition |

## Usage
To compile Pyramidion using `clang++` and LLVM, run:

```bash
cd src/
clang++ -g pyramidion.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o pyramidion
```

To execute a Pyramidion program:

```bash
./pyramidion <input file>
```

