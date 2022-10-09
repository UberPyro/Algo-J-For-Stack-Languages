(disclaimer: I haven't tested this implementation thoroughly yet, so it might not be perfect)

# Algorithm J for Stack Languages

This repo contains an extremely minimal implementation of a typechecker for a concatenative language written in OCaml. It is intended for learning purposes, so that others who are interested in statically-typed stack languages have something to follow. While this implementation is derivative of a description from an [academic paper](https://www2.ccs.neu.edu/racket/pubs/dissertation-kleffner.pdf) (the nature of which some may find daunting), the actual concepts at play are actually *very* simple and should be accessible to anyone. Indeed, as this minimal implementation forgoes scoped variables and nested let bindings, and with concatenation as the only relevant inference rule, it is, in many respects, vastly simpler to implement than traditional HM type systems. 

## An Important Dependency

As this is an Algorithm J style implementation, we depend heavily on the union-find data structure. We use the [module here](https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatUref.html) from [Batteries](https://ocaml-batteries-team.github.io/batteries-included/hdoc2/index.html) as it is my personal preference, but any implementation should work. If you are writing a typechecker in another language, I recommend using a similar implementation - rolling your own data structure is possible, but tricky, and very involved to make one with optimal time complexity. Understanding how the union-find data structure works is very unnecessary for this part of the process - rather, I encourage gaining a basic understanding of what the provided methods do. 

The implementation from Batteries is provided as unifiable references. `uref` and `uget` work like `ref` and `(!)` respectively. `unite` is the important operation. `unite` takes two operations and makes them the same, where you provide the function that figures out what the new quantity is. After uniting, `uget`ing *either* reference will yield that new quantity; additionally, if either reference is `uset` or `unite`d again, changing the underlying quantity, it will be that newly set quantity again that is yielded from `uget`ing *either* reference. From an imperative point of view, unified references are simply pointing at the same address. 

This turns out to be very useful for typechecking languages with generics. Expressions can be of some polymorphic type (e.g. `A`) or some concrete type `int`. Unifying polymorphic types make them the same - for example, the identity function must have its input and output the same. Unifying polymorphic types with a concrete type must yield the concrete type, however, as that is the more specific type. 

## Stack Type Inference

Union-find / unifiable references are very good at modelling polymorphic variables, as they can be unified together until they are unified with concrete types and eventually solved. However, stack languages have two kinds of polymorphic variables - type variables, and *type stacks* (also called sequence types). Type stacks reprsent stack polymorphism - the property of a function that it is invariant over some stack. Take for instance the following examples: 

```
(+) : 0 int int -- 0 int
dup : 0 A -- 0 A A
swap : 0 B A -- 0 A B
call : 0 [0 -- 1] -- 1
unit : 0 A -- 0 [1 -- 1 A]
dip : 0 A [0 -- 1] -- 1 A
```

Here, we use lowercase to represent concrete types, uppercase to represent polymorphic types, numbers to represent stacks, `--` to represent stack effects, and `[]` to represent quotation. The key is that stack variables represent the rest of the stack. For example: 

- in `2 1 (+)`, the rest of the stack (`0`) over the `(+)` operation is the empty stack. 
- in `2 1 3 (+)`, the type stack `0` is `int` during `(+)`. 
- in `4 1 6 5 swap`, the type stack `0` is `4 1` during the swap

It turns out that stack types can be modelled very well by linked lists made out of unifiable references, so both the `data` ("this element") field and the `next` field are unifiable. Unifying two stacks means that we recursively pop the heads off of the two lists and unify them (the `data` fields) until we get to a nil (one list is empty). We then unify the bottoms (the `next` fields), making the stacks the same, and choosing the longer stack as the new underlying value. This simple process extends HM type inference for stacks. 

## Additional Nodes on this Implementation

I've tried to keep this implementation relatively basic as I know that OCaml might be unfamiliar to some interested people. With that said, I do use some of the operations from OCaml and Batteries (for convenience) that might not be widely known. 

- `unique` is a function from batteries which generates a new integer whenever it is called. This is important for generating fresh type variables. 
- `(@@)` is a slow infix operator for application, like `$` in Haskell or `<|` in F#. 
- `(%)` is composition from Batteries, like `.` in Haskell or `<<` in `F#`. 
