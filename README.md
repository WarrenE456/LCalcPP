# LCalc++

LCalc++ is a toy language I implemented after going through the first part of 'Crafting Interpreters' (it's a great book, BTW. You should check it out). As you could probably guess by the name, LCalc++ is a typed Lambda Calculus-like language.

## How to Run
Git clone the git repo: ```git clone [repo url]``` then, cd into the directory that was created. This is a Rust project, so run `cargo run` to build and run the project (install cargo if it's not already installed).
This should run LCalc++ in prompt mode, which leads me into the next section.

## A Short Tutorial

### Variables

Creating a variable looks like this: ```let var_name : Number = 3``` Keep in mind the type annotation is optional. 

### Abstractions

The most important type in LCalc++ is the abstractions, which are written like this ```L x : String. x * 3```. They look exactly like abstractions in the Lambda Calculus, except for the fact that a Latin 'L' takes the place of the Greek lambda, as most people do not carry around a second keyboard for the Greek alphabet. 

#### Application
Application of a function (calling a function) looks like this: ```(L x: String. x * 3) "Hello"```.

#### 'in' expressions

To 'concatenate statements' (kinda), you can use 'in' expressions: `let a = 3 in a + 1`.
Note that the 'in' expressions and variable binding from before are really just syntactic sugar for abstractions ```(L a. a + 1) 3```. 

## A Strange Quirk
In the REPL, if you try inputting `let a = 3` then on the next line input `a`, you will see that you get an error message yelling at you for using an unbound variable.
This is because variables are not implemented with an environment, 
but instead beta-reduction, which makes the prompt-mode a little more clunky in exchange for truer to the original Lambda Calculus (and admittedly lazier) implementation.

### Currying
There are no multi-argument functions in the Lambda Calculus nor in LCalc++, but you can achieve the same thing (and more) by currying functions: `L a. L b. a + b`.
This is a design choice, not a limitation, because it allows you to do partial application of functions, which I won't go into. 


### Data types
The only types are the Number, String, and abstraction type, so you may be wondering where Booleans, structs, and enums are.
Really, these things are not missing because abstractions take their place. On a side note, you really don't even need Numbers or Strings either. Search the church encoding if you are interested.
You can implement booleans using abstractions because, really, they are just functions that take two things and return either the first or the second.

### Other: Type definitions and Print Statements
I'll take this opportunity to introduce type definitions and print statements: ```type Bool = Any -> Any -> Any in let TRUE = L a. L b. a in let FALSE = L a. L b. b in PRINT ((TRUE "True, " "False, ") + (FALSE "True" "False"))``` If you are clever enough you can
even implement all the logical operators (I'd recommend you look at some introduction to Lambda Calculus videos because it's really fascinating). Along with 'PRINT,' the other built-in functions are 'EQUAL' and 'GREATER,' which are both self-explanatory enough.

### Running Files

If you want to write a program and run it instead of using the REPL, just do `cargo run -- <filename>`. The language is quite small, so I've covered a lot of it, but the rest I am sure you can figure out by playing around in the language and looking at the examples. Good luck!
