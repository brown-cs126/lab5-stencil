open Core
open Csci1260
open Asm

let command =
  Command.basic ~summary:"Compile the given file to an executable"
    Command.Let_syntax.(
      let%map_open filename = anon ("filename" %: Filename.arg_type)
      and directory = anon ("directory" %: Filename.arg_type)
      and run = flag "-r" no_arg ~doc:"run the binary" in
      fun () ->
        try
          let ast = S_exp.parse_file_many filename in
          let instrs = Compile.compile ast in
          let filename = Filename.basename filename in
          if run then
            Assemble.eval directory Runtime.runtime filename [] instrs
            |> function
            | Ok output ->
                printf "%s\n" output
            | Error (Expected error | Unexpected error) ->
                eprintf "%s\n" error
          else
            Assemble.build directory Runtime.runtime filename instrs |> ignore
        with Error.Stuck _ as e ->
          Printf.eprintf "Error: %s\n" (Exn.to_string e))

let () = Command.run ~version:"1.0" command
