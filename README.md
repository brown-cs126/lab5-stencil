# Lab: Implementing tail recursion modulo `pair`

We've talked in class about tail calls. A compiler with proper support for tail
calls lets us write tail-recursive functions without worrying about overflowing
the stack; rather than allocating a new stack frame for each call we re-use the
existing stack frame.

There's another class of functions that it's a bit hard to express naturally
using tail recursion. `range` is an example:

```
(define (range from to)
  (if (= from to)
    ()
    (pair from (range (add1 from) to))))
```

This function is definitely not tail-recursive; in the recursive case it calls
itself and then builds a `pair` out of the result. But it is "almost" tail
recursive--the only thing we have left to do once we call the function is to
build the `pair` and return it.

Here's another of these "almost" tail-recursive functions:

```
(define (add1s l)
  (if (empty? l) 
    l
    (pair (add1 (left l)) (add1s (right l)))))
```

(This function is sort of like a specialized version of `map`). Once again,
there's not much work to be done after the recursive call--we just need to build
the pair.

The calls in these functions are *tail-calls modulo `pair`*. A tail call modulo
`pair` is a call to `pair` in tail position where the second operand is a function
call. We're going to use a trick (sometimes called "tail call optimization
modulo cons" after the traditional name for the `pair` operation) that lets us
re-use stack frames in this common situation.

Before you move on, think for a minute about how you would do this. As a hint:
think about when you could build the `pair`.

## The trick

When we see a function in tail position modulo `pair`, we're going to call a
special "modulo pair mode" version of the function. Instead of evaluating its
body and returning it, this version of the function is going to evaluate its
body and then *write* that value to a location in memory, which we'll call the
`cell pointer`, given as an additional parameter. Since the end goal is to build
a pair where the `right` element is the result of this function call, the cell
pointer should correspond to the right half of a freshly-allocated pair. Then
the function will return a value passed in as another additional parameter; this
parameter, which we'll call the `return parameter`, should be the
correctly-tagged pointer for the pair.

If a function called in modulo pair mode *itself* contains a tail call, it
should call the function in question in modulo pair mode, keeping the cell
pointer and the return parameter the same.

If a function called in modulo pair mode contains a tail call modulo pair (as is
the case in both of the functions above), it should call itself modulo pair. The
return parameter should remain the same--at the end of the day, we still want to
return the outermost pair. The cell pointer, though, will need to change. We'll
need to allocate a new pair, write its tagged pointer into the cell pointer, and
then move its right half into the cell pointer for the tail call.

## The implementation

We've provided a version of the Homework 6 stencil with regular tail calls
implemented as in class.

(TAs do the next part in class)

First, change compile_defn to produce a version of each function "mod pair":

```ocaml
let rec compile_expr_mod_pair (defns : defn list) (tab : symtab)
    (stack_index : int) : s_exp -> directive list = function
  | e ->
      compile_expr defns tab stack_index false e
      @ [ Mov (Reg R8, cell_pointer)
        ; Mov (MemOffset (Reg R8, Imm 0), Reg Rax)
        ; Mov (Reg Rax, return_param) ]


let compile_defn (defns : defn list) defn : directive list =
  let ftab =
    defn.args |> List.mapi (fun i arg -> (arg, (i + 1) * -8)) |> Symtab.of_list
  in
  let ftab_mod_pair = ftab |> Symtab.map (fun i -> i - 16) in
  [Label (function_label defn.name)]
  @ compile_expr defns ftab ((List.length defn.args + 1) * -8) true defn.body
  @ [Ret]
  @ [Label (function_label_mod_pair defn.name)]
  @ compile_expr_mod_pair defns ftab_mod_pair
      ((List.length defn.args + 3) * -8)
      defn.body
  @ [Ret]
```

Right now, all this modulo pair mode function does is to write its return value
into the cell pointer and return the return param.

(TAs do this in class)

Next, call this new mode of the function from the right place in `compile_expr`:

```ocaml
  | Lst [Sym "pair"; e; Lst (Sym f :: args)] when is_defn defns f && is_tail ->
      let defn = get_defn defns f in
      if List.length args = List.length defn.args then
        let compiled_left = compile_expr defns tab stack_index false e in
        let pair_args =
          [ Mov (MemOffset (Reg Rdi, Imm 0), Reg Rax)
          ; Mov (Reg Rax, Reg Rdi)
          ; Or (Reg Rax, Imm pair_tag)
          ; Mov (stack_address stack_index, Reg Rax)
          ; Mov (Reg R8, Reg Rdi)
          ; Add (Reg R8, Imm 8)
          ; Mov (stack_address (stack_index - 8), Reg R8)
          ; Add (Reg Rdi, Imm 16) ]
        in
        let compiled_args =
          args
          |> List.mapi (fun i arg ->
                 compile_expr defns tab (stack_index - (8 * (i + 2))) false arg
                 @ [Mov (stack_address (stack_index - (8 * (i + 2))), Reg Rax)])
          |> List.concat
        in
        let moved_args =
          List.range 0 (List.length args + 1)
          |> List.map (fun i ->
                 [ Mov (Reg R8, stack_address (stack_index - (8 * i)))
                 ; Mov (stack_address ((i + 1) * -8), Reg R8) ])
          |> List.concat
        in
        compiled_left @ pair_args @ compiled_args @ moved_args
        @ [Jmp (function_label_mod_pair f)]
      else raise (Error.Stuck e)

```

This code builds a pair, puts the left value in the left cell, then writes the
return parameter and the cell pointer. These get moved to the bottom of the
stack along with the other tail-call arguments.

Now, implement the other cases of `compile_expr_mod_pair`. You should have cases
for `if`, `let`, function calls, and function calls mod pair (`(pair e (f e ...))`).
