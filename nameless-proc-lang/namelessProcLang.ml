open Base

type pgm = exp

and exp =
  | Int of int
  | Idx of idx
  | Add of exp * exp
  | Sub of exp * exp
  | IsZero of exp
  | If of exp * exp * exp
  | Let of exp * exp
  | Proc of exp
  | AppProc of exp * exp

and value = VInt of int | VBool of bool | VProc of exp * env
and env = value list
and idx = int

module Env : sig
  type t = env

  val empty : t
  val extend : value -> t -> t
  val lookup : idx -> t -> (value, [> `EmptyEnv ]) Result.t
end = struct
  type t = env

  let empty = []
  let extend v rho = v :: rho

  let lookup i rho =
    match List.nth rho i with Some v -> Ok v | None -> Error `EmptyEnv
end

let rec eval : Env.t -> exp -> (value, [> `EmptyEnv ]) Result.t =
 fun rho exp ->
  let open Result in
  match exp with
  | Int n -> Ok (VInt n)
  | Idx i -> Env.lookup i rho
  | Add (e1, e2) -> bin_op ~f:( + ) rho e1 e2
  | Sub (e1, e2) -> bin_op ~f:( - ) rho e1 e2
  | IsZero e -> (
      eval rho e >>= function
      | VInt 0 -> Ok (VBool true)
      | VInt _ -> Ok (VBool false)
      | _ -> Error `TypeErrorIsZero)
  | If (e, te, ee) -> (
      eval rho e >>= function
      | VBool true -> eval rho te
      | VBool false -> eval rho ee
      | _ -> Error `TypeErrorIfCond)
  | Let (e1, e2) ->
      eval rho e1 >>= (fun v -> Ok (Env.extend v rho)) >>= Fn.flip eval e2
  | Proc e -> Ok (VProc (e, rho))
  | AppProc (e1, e2) ->
      let func =
        eval rho e1 >>= function
        | VProc (e, rho') -> Ok (e, rho')
        | _ -> Error `TypeErrorAppProc
      in
      let v = eval rho e2 in
      combine func v
        ~ok:(fun (e, rho') a -> eval (Env.extend a rho') e)
        ~err:Fn.const
      |> join

and bin_op :
    f:(int -> int -> int) ->
    Env.t ->
    exp ->
    exp ->
    (value, [> `TypeErrorBinOp ]) Result.t =
 fun ~f rho e1 e2 ->
  let open Result in
  let ensure_vint = function VInt n -> Ok n | _ -> Error `TypeErrorBinOp in
  let n1 = eval rho e1 >>= ensure_vint in
  let n2 = eval rho e2 >>= ensure_vint in
  combine n1 n2 ~ok:(fun n1 n2 -> VInt (f n1 n2)) ~err:Fn.const

let string_of_value : value -> string = function
  | VInt n -> Int.to_string n
  | VBool b -> Bool.to_string b
  | VProc (_, _) -> "<proc>"

let string_of_error : [> `EmptyEnv ] -> string = function
  | `EmptyEnv -> "unbound variable"
  | `TypeErrorIsZero -> "Type Error: isZero expects an integer"
  | `TypeErrorIfCond -> "Type Error: if condition must be boolean"
  | `TypeErrorAppProc -> "Type error: expected function in application"
  | `TypeErrorBinOp -> "Type error: binary operator expected int arguments"

let () =
  let open Stdio.Out_channel in
  let sample : pgm =
    AppProc
      (Let (Int 37, Proc (Let (Sub (Idx 0, Idx 1), Sub (Idx 2, Idx 1)))), Int 10)
  in
  match eval Env.empty sample with
  | Ok x -> string_of_value x |> print_endline
  | Error e -> string_of_error e |> print_endline
