(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_byte b = Printf.sprintf "%d is not a byte" b

(* Bytes *)

type t = char

let unsafe_of_byte = Astring_unsafe.char_unsafe_of_byte

let of_byte b =
  if b < 0 || b > 255 then invalid_arg (err_byte b) else unsafe_of_byte b

let of_int b =
  if b < 0 || b > 255 then None else (Some (unsafe_of_byte b))

let to_int = Astring_unsafe.char_to_byte

let hash c = Hashtbl.hash c

(* Predicates *)

let equal : t -> t -> bool = fun c0 c1 -> c0 = c1
let compare : t -> t -> int = fun c0 c1 -> Pervasives.compare c0 c1

(* Bytes as US-ASCII characters *)

module Ascii = struct
  let max_ascii = '\x7F'

  let is_valid : t -> bool = fun c -> c <= max_ascii

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

  let is_upper = function 'A' .. 'Z' -> true | _ -> false

  let is_lower = function 'a' .. 'z' -> true | _ -> false

  let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

  let is_alphanum = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

  let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false

  let is_blank = function ' ' | '\t' -> true | _ -> false

  let is_graphic = function '!' .. '~' -> true | _ -> false

  let is_print = function ' ' .. '~' -> true | _ -> false

  let is_control = function '\x00' .. '\x1F' | '\x7F' -> true | _ -> false

  let uppercase = function
  | 'a' .. 'z' as c -> unsafe_of_byte @@ to_int c - 0x20
  | c -> c

  let lowercase = function
  | 'A' .. 'Z' as c -> unsafe_of_byte @@ to_int c + 0x20
  | c -> c

  (* Escaping *)

  let escape = Astring_escape.char_escape
  let escape_char = Astring_escape.char_escape_char
end

(* Pretty printing *)

let pp = Format.pp_print_char
let pp_char ppf c =
  Format.pp_print_char ppf '\'';
  Format.pp_print_string ppf (Ascii.escape_char c);
  Format.pp_print_char ppf '\'';
  ()

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
