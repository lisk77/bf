# bf 

A brainfuck compiler written in Haskell

## Requirements

To build the binary, `fasm` is required to be installed on the device that runs the compiler. Other than that this compiler uses standard Haskell features so no extra library installs are required.

## Usage

```
bf <file> -o <binary>       # builds a binary
bf run <file>               # uses the inbuilt interpreter to run the file
```

## Optimizations

The output of this compiler has been simply optimized by bunching incrememts, decrements as well as moves to the right and to the left together. 
It also eliminates simple things like 

```bf
>>><<<
```

will turn into nothing at all.

Something like 
```bf
++-
```

will be turned into just `+` because thats the sum change of the input. More optimizations are planned.
