# LCalc++
LCalc++ is a toy language I implemended after going through the first part of 'Crafting Interpreters' (it's a great book BTW you should check it out). As you could probably guess by the name, LCalc++ is a typed Lambda Calculus-like language.
# How to Run
Git clone the git repo: ```git clone [repo url]``` then, cd into the directory that was created. This is a rust project so run ```cargo run``` to build and run the project (install cargo if it's not already installed).
This should run LCalc++ in prompt-mode, which leads me into the next section.
# A Short Tutorial
Creating a variable looks like this: ```let var_name : Number = 3``` Keep in mind the type annotation is optional. To 'conncatonate statements' (kinda) you can use 'in' expressions:
```let a = 3 in a + 1```. The most important type in LCalc++ are the abstractions which are written like this ```L x : String. x * 3```. They look exactly like abstractions in the Labda Calulus except for the fact that a latin 'L' takes the place of the greek lambda
as most people do not carry around a second keyboard for the greek alphabet. Application of a function (calling a function) looks like this: ```(L x: String. x * 3) "Hello"```.
Note that the 'in' expressions and variable binding from before are really just syntatic sugar for abstractions ```(L a. a + 1) 3```. In the prompt-mode, if you try inputing ```let a = 3``` then on the next line input ```a```
you will see that you get an error message yelling at you for using an unbound variable. This is because variables are not implemented with an environment, but instead beta-reduction which makes the prompt-mode a little more clunky
in exchance for truer to the original Lambda Calculus (and admittedly lazier) implementation. There are no multi-argument functions in the Lambda Calculus nor in LCalc++, but you can achieve the same thing by 'currying' functions:
```L a. L b. a + b```. This is a design choice, not a limitation, because it allows you to do partial application of functions, which I won't go into. The only types are Numbers, String, and abstractions so you may be wondering where Booleans, structs, and enums are.
Really these thigns are not mising because abstractions take their place (you really don't even need Numbers or String which was a shock to me the first time I heard it). Booleans are really just functions that take two things and return either the first or the second.
I'll take this opportunity to introduce type definitions and print statements: ```type Bool = Any -> Any -> Any in let TRUE = L a. L b. a in let FALSE = L a. L b. b in PRINT ((TRUE "True, " "False, ") + (FALSE "True" "False"))``` If you are clever enough you can
even implement all the logical operators (I'd recommend you look at some introduction to Lambda Calculus videos because it's really fasinating). Along with 'PRINT' other built in 'EQUAL' and 'GREATER,' which I think are self-explanatory enough. If you want to write
a program then run it instead of using prompt mode just do ```cargo run -- <filename>```. The language is quite small, so I've covered a lot of it, but the rest I am sure you can figure out bying playing around in the language and looking at the examples.
