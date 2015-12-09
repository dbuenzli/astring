(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring_unsafe

let sunsafe_get = string_unsafe_get

(* Errors *)

let strf = Format.asprintf
let err_base = "not on the same physical base"
let err_empty_sub pos = strf "empty substring [%d;%d]" pos pos

(* From strings *)

let make_sub s ~start ~stop = (s, start, stop)

let of_string_with_pos_range ?start ?stop s =
  Astring_base.with_pos_range make_sub ?start ?stop s

let of_string_with_pos_len ?start ?len s =
  Astring_base.with_pos_len make_sub ?start ?len s

let of_string_with_index_range ?first ?last s =
  Astring_base.with_index_range make_sub ?first ?last s

(* Substrings *)

type t = string * int * int

let empty = (Astring_base.empty, 0, 0)
let v = of_string_with_pos_range
let start_pos (_, start, _) = start
let stop_pos (_, _, stop) = stop
let base_string (s, _, _) = s
let length (_, start, stop) = stop - start
let get (s, start, _) i = string_safe_get s (start + i)
let get_byte s i = char_to_byte (get s i)
let unsafe_get (s, start, _) i = string_unsafe_get s (start + i)
let unsafe_get_byte s i = char_to_byte (unsafe_get s i)

let head ?(rev = false) (s, start, stop) =
  if start = stop then None else
  Some (string_unsafe_get s (if rev then stop - 1 else start))

let get_head ?(rev = false) (s, start, stop) =
  if start = stop then invalid_arg (err_empty_sub start) else
  string_unsafe_get s (if rev then stop - 1 else start)

let of_string s = v s
let to_string (s, start, stop) =
  if start = stop then Astring_base.empty else
  if start = 0 && stop = string_length s then s else
  unsafe_string_sub s start (stop - start)

let rebase (_, start, stop as sub) = (to_string sub, 0, stop - start)
let hash s = Hashtbl.hash s

(* Stretching substrings *)

let start (s, start, _) = (s, start, start)
let stop (s, _, stop) = (s, stop, stop)

let tail ?(rev = false) (s, start, stop as sub) =
  if start = stop then sub else
  if rev then (s, start, stop - 1) else (s, start + 1, stop)

let base (s, _, _) = (s, 0, string_length s)

let extend ?(rev = false) ?max ?(sat = (fun _ -> true)) (s, start, stop) =
  if rev then begin
    let min_idx = match max with
    | None -> 0
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max -> let i = start - max in if i < 0 then 0 else i
    in
    let rec loop i =
      if i < min_idx then (s, min_idx, stop) else
      if sat (string_unsafe_get s i) then loop (i - 1) else
      (s, i + 1, stop)
    in
    loop (start - 1)
  end else begin
    let max_idx = string_length s - 1 in
    let max_idx = match max with
    | None -> max_idx
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max -> let i = stop + max - 1 in if i > max_idx then max_idx else i
    in
    let rec loop i =
      if i > max_idx then (s, start, i) else
      if sat (string_unsafe_get s i) then loop (i + 1) else
      (s, start, i)
    in
    loop stop
  end

let reduce
    ?(rev = false) ?max ?(sat = (fun _ -> true)) (s, start, stop as sub) =
  if start = stop then sub else
  if rev then begin
    let min_idx = match max with
    | None -> start
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max -> let i = stop - max in if i < start then start else i
    in
    let rec loop i =
      if i < min_idx then (s, start, min_idx) else
      if sat (string_unsafe_get s i) then loop (i - 1) else
      (s, start, i + 1)
    in
    loop (stop - 1)
  end else begin
    let max_idx = stop - 1 in
    let max_idx = match max with
    | None -> max_idx
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max -> let i = start + max - 1 in if i > max_idx then max_idx else i
    in
    let rec loop i =
      if i > max_idx then (s, i, stop) else
      if sat (string_unsafe_get s i) then loop (i + 1) else
      (s, i, stop)
    in
    loop start
  end

let extent (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  let start = if start0 < start1 then start0 else start1 in
  let stop = if stop0 < stop1 then stop1 else stop0 in
  (s0, start, stop)

let overlap (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  if not (start0 <= stop1 && start1 <= stop0) then None else
  let start = if start0 < start1 then start1 else start0 in
  let stop = if stop0 < stop1 then stop0 else stop1 in
  Some (s0, start, stop)

(* Appending substrings *)

let append (s0, start0, _ as sub0) (s1, start1, _ as sub1) =
  let l0 = length sub0 in
  if l0 = 0 then rebase sub1 else
  let l1 = length sub1 in
  if l1 = 0 then rebase sub0 else
  let len = l0 + l1 in
  let b = Bytes.create len in
  bytes_unsafe_blit_string s0 start0 b 0 l0;
  bytes_unsafe_blit_string s1 start1 b l0 l1;
  (bytes_unsafe_to_string b, 0, len)

let concat ?sep:(sep, sep_start, _ as sep_sub = empty) = function
| [] -> empty
| [s] -> rebase s
| (s, start, _ as sub) :: ss ->
    let sub_len = length sub in
    let sep_len = length sep_sub in
    let rec cat_len l = function
    | [] -> l
    | h :: t -> cat_len (l + sep_len + length h) t
    in
    let cat_len = cat_len sub_len ss in
    let b = Bytes.create cat_len in
    bytes_unsafe_blit_string s start b 0 sub_len;
    let rec loop i = function
    | [] -> bytes_unsafe_to_string b
    | (str, str_start, _ as str_sub) :: ss ->
        let sep_pos = i in
        let str_pos = i + sep_len in
        let str_len = length str_sub in
        bytes_unsafe_blit_string sep sep_start b sep_pos sep_len;
        bytes_unsafe_blit_string str str_start b str_pos str_len;
        loop (str_pos + str_len) ss
    in
    (loop sub_len ss, 0, cat_len)

(* Predicates *)

let is_empty (_, start, stop) = stop - start = 0

let is_prefix ~affix:(affix, astart, _ as affix_sub) (s, sstart, _ as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx (* zero based idx *) = len_a - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if sunsafe_get affix (astart + i) <> sunsafe_get s (sstart + i)
    then false
    else loop (i + 1)
  in
  loop 0

let is_infix ~affix:(affix, astart, _ as affix_sub) (s, sstart, _ as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx_a (* zero based idx *) = len_a - 1 in
  let max_zidx_s (* zero based idx *) = len_s - len_a  in
  let rec loop i k =
    if i > max_zidx_s then false else
    if k > max_zidx_a then true else
    if k > 0 then
      if sunsafe_get affix (astart + k) = sunsafe_get s (sstart + i + k)
      then loop i (k + 1)
      else loop (i + 1) 0
    else if sunsafe_get affix astart = sunsafe_get s (sstart + i)
    then loop i 1
    else loop (i + 1) 0
  in
  loop 0 0

let is_suffix ~affix:(affix, _, astop as affix_sub) (s, _, sstop as s_sub) =
  let len_a = length affix_sub in
  let len_s = length s_sub in
  if len_a > len_s then false else
  let max_zidx (* zero based idx *) = len_a - 1 in
  let max_idx_a = astop - 1 in
  let max_idx_s = sstop - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if sunsafe_get affix (max_idx_a - i) <> sunsafe_get s (max_idx_s - i)
    then false
    else loop (i + 1)
  in
  loop 0

let for_all sat (s, start, stop) = Astring_base.for_all sat s ~start ~stop
let exists sat (s, start, stop) = Astring_base.exists sat s ~start ~stop

let equal_base (s0, _, _) (s1, _, _) = s0 == s1

let equal_bytes (s0, start0, stop0) (s1, start1, stop1) =
  if s0 == s1 && start0 = start1 && stop0 = stop1 then true else
  let len0 = stop0 - start0 in
  let len1 = stop1 - start1 in
  if len0 <> len1 then false else
  let max_zidx = len0 - 1 in
  let rec loop i =
    if i > max_zidx then true else
    if sunsafe_get s0 (start0 + i) <> sunsafe_get s1 (start1 + i)
    then false
    else loop (i + 1)
  in
  loop 0

let compare_bytes (s0, start0, stop0) (s1, start1, stop1) =
  if s0 == s1 && start0 = start1 && stop0 = stop1 then 0 else
  let len0 = stop0 - start0 in
  let len1 = stop1 - start1 in
  let min_len = if len0 < len1 then len0 else len1 in
  let max_i = min_len - 1 in
  let rec loop i =
    if i > max_i then Pervasives.compare len0 len1 else
    let c0 = sunsafe_get s0 (start0 + i) in
    let c1 = sunsafe_get s1 (start1 + i) in
    let cmp = Pervasives.compare c0 c1 in
    if cmp <> 0 then cmp else
    loop (i + 1)
  in
  loop 0

let eq_pos : int -> int -> bool = fun p0 p1 -> p0 = p1
let equal (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  eq_pos start0 start1 && eq_pos stop0 stop1

let compare_pos : int -> int -> int = Pervasives.compare
let compare (s0, start0, stop0) (s1, start1, stop1) =
  if s0 != s1 then invalid_arg err_base else
  let c = compare_pos start0 start1 in
  if c <> 0 then c else
  compare_pos stop0 stop1

(* Finding and parsing bytes *)

let find ?(rev = false) sat (s, start, stop) =
  if rev then begin
    let rec loop i =
      if i < start then None else
      if sat (sunsafe_get s i) then Some (s, i, i + 1) else loop (i - 1)
    in
    loop (stop - 1)
  end else begin
    let max_idx = stop - 1 in
    let rec loop i =
      if i > max_idx then None else
      if sat (sunsafe_get s i) then Some (s, i, i + 1) else loop (i + 1)
    in
    loop start
  end

let find_sub ?(rev = false) ~sub:(sub, sub_start, sub_stop) (s, start, stop) =
  let len_sub = sub_stop - sub_start in
  let len_s = stop - start in
  if len_sub > len_s then None else
  let max_zidx_sub = len_sub - 1 in
  if rev then begin
    let rec loop i k =
      if i < start then None else
      if k > max_zidx_sub then Some (s, i, i + len_sub) else
      if k > 0 then
        if sunsafe_get sub (sub_start + k) = sunsafe_get s (i + k)
        then loop i (k + 1)
        else loop (i - 1) 0
      else if sunsafe_get sub sub_start = sunsafe_get s i then loop i 1 else
      loop (i - 1) 0
    in
    loop (stop - len_sub) 0
  end else begin
    let max_idx_s = start + len_s - len_sub in
    let rec loop i k =
      if i > max_idx_s then None else
      if k > max_zidx_sub then Some (s, i, i + len_sub) else
      if k > 0 then
        if sunsafe_get sub (sub_start + k) = sunsafe_get s (i + k)
        then loop i (k + 1)
        else loop (i + 1) 0
      else if sunsafe_get sub sub_start = sunsafe_get s i then loop i 1 else
      loop (i + 1) 0
    in
    loop start 0
  end

let span ?(rev = false) ?max ?(sat = fun _ -> true) (s, start, stop as sub) =
  let max_idx = stop - 1 in
  if rev then begin
    let min_idx = match max with
    | None -> start
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max -> let i = stop - max in if i < start then start else i
    in
    let rec loop i =
      if i < min_idx then (s, start, i + 1), (s, i + 1, stop) else
      if sat (sunsafe_get s i) then loop (i - 1) else
      if i = max_idx then sub, (s, stop, stop) else
      (s, start, i + 1), (s, i + 1, stop)
    in
    loop max_idx
  end else begin
    let max_idx = match max with
    | None -> max_idx
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max -> let i = start + max - 1 in if i > max_idx then max_idx else i
    in
    let rec loop i =
      if i > max_idx then (s, start, i), (s, i, stop) else
      if sat (sunsafe_get s i) then loop (i + 1) else
      if i = start then (s, start, start), sub else
      (s, start, i), (s, i, stop)
    in
    loop start
  end

let min_span ?(rev = false) ~min ?max ?(sat = fun _ -> true) (s, start, stop) =
  if min < 0 then invalid_arg (Astring_base.err_neg_min min) else
  let max_idx = stop - 1 in
  if rev then begin
    let min_idx = match max with
    | None -> start
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max when min > max -> invalid_arg (Astring_base.err_min_max min max)
    | Some max -> let i = stop - max in if i < start then start else i
    in
    let need_idx = stop - min - 1 in
    let rec loop i =
      if i < min_idx
      then (if i > need_idx then None else
            Some ((s, start, i + 1), (s, i + 1, stop)))
      else if sat (sunsafe_get s i) then loop (i - 1)
      else if i > need_idx then None
      else Some ((s, start, i + 1), (s, i + 1, stop))
    in
    loop max_idx
  end else begin
    let max_idx = match max with
    | None -> max_idx
    | Some max when max < 0 -> invalid_arg (Astring_base.err_neg_max max)
    | Some max when min > max -> invalid_arg (Astring_base.err_min_max min max)
    | Some max -> let i = start + max - 1 in if i > max_idx then max_idx else i
    in
    let need_idx = start + min in
    let rec loop i =
      if i > max_idx
      then (if i < need_idx then None else Some ((s, start, i), (s, i, stop)))
      else if sat (sunsafe_get s i) then loop (i + 1)
      else if i < need_idx then None
      else Some ((s, start, i), (s, i, stop))
    in
    loop start
  end

let drop ?(rev = false) ?max ?sat s =
  if rev then fst (span ~rev ?max ?sat s) else snd (span ~rev ?max ?sat s)

(* Extracting substrings *)

let with_pos_range ?(start = 0) ?stop (base, sub_start, sub_stop) =
  let sub_len = sub_stop - sub_start in
  let stop = match stop with None -> sub_len | Some stop -> stop in
  if start < 0 || stop > sub_len || stop < start
  then invalid_arg (Astring_base.err_pos_range start stop sub_len)
  else (base, sub_start + start, sub_start + stop)

let with_pos_len ?(start = 0) ?len (base, sub_start, sub_stop) =
  let sub_len = sub_stop - sub_start in
  let len = match len with None -> sub_len - start | Some l -> l in
  let stop = start + len in
  if start < 0 || stop > sub_len || stop < start
  then invalid_arg (Astring_base.err_pos_len start len sub_len)
  else (base, sub_start + start, sub_start + stop)

let with_index_range ?(first = 0) ?last (base, sub_start, sub_stop) =
  let sub_len = sub_stop - sub_start in
  let last = match last with None -> sub_len - 1 | Some l -> l in
  if first < 0 || last > sub_len - 1 || last < first
  then invalid_arg (Astring_base.err_index_range first last sub_len)
  else (base, sub_start + first, sub_start + last + 1)

let slice ?(start = 0) ?stop (base, sub_start, sub_stop as sub) =
  let max_pos = sub_stop - sub_start in
  let clip_pos p = if p < 0 then 0 else if p > max_pos then max_pos else p in
  let start = clip_pos (if start < 0 then max_pos + start else start) in
  let stop = match stop with None -> max_pos | Some stop -> stop in
  let stop = clip_pos (if stop < 0 then max_pos + stop else stop) in
  if start > stop then (base, sub_start, sub_start) else
  if start = 0 && stop = max_pos then sub else
  (base, sub_start + start, sub_start + stop)

let trim ?(drop = Astring_char.Ascii.is_white) (s, start, stop as sub) =
  let len = stop - start in
  if len = 0 then sub else
  let max_pos = stop in
  let max_idx = stop - 1 in
  let rec left_pos i =
    if i > max_idx then max_pos else
    if drop (sunsafe_get s i) then left_pos (i + 1) else i
  in
  let rec right_pos i =
    if i < start then start else
    if drop (sunsafe_get s i) then right_pos (i - 1) else (i + 1)
  in
  let left = left_pos start in
  if left = max_pos then (s, (start + stop) / 2, (start + stop) / 2)  else
  let right = right_pos max_idx in
  if left = start && right = max_pos then sub else
  (s, left, right)

let fcut ~sep:(sep, sep_start, sep_stop) (s, start, stop) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - sep_len in
  let rec check_sep i k =
    if k > max_sep_zidx then Some ((s, start, i), (s, i + sep_len, stop))
    else if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
    then check_sep i (k + 1)
    else scan (i + 1)
  and scan i =
    if i > max_s_idx then None else
    if sunsafe_get s i = sunsafe_get sep sep_start
    then check_sep i 1
    else scan (i + 1)
  in
  scan start

let rcut ~sep:(sep, sep_start, sep_stop) (s, start, stop) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - 1 in
  let rec check_sep i k =
    if k > max_sep_zidx then Some ((s, start, i), (s, i + sep_len, stop))
    else if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
    then check_sep i (k + 1)
    else rscan (i - 1)
  and rscan i =
    if i < start then None else
    if sunsafe_get s i = sunsafe_get sep sep_start
    then check_sep i 1
    else rscan (i - 1)
  in
  rscan (max_s_idx - max_sep_zidx)

let cut ?(rev = false) ~sep s = if rev then rcut ~sep s else fcut ~sep s

let add_sub ~no_empty s ~start ~stop acc =
  if start = stop then (if no_empty then acc else (s, start, start) :: acc) else
  (s, start, stop) :: acc

let fcuts ~no_empty ~sep:(sep, sep_start, sep_stop) (s, start, stop as sub) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let s_len = stop - start in
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - sep_len in
  let rec check_sep sstart i k acc =
    if k > max_sep_zidx then
      let new_start = i + sep_len in
      scan new_start new_start (add_sub ~no_empty s ~start:sstart ~stop:i acc)
    else if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
    then check_sep sstart i (k + 1) acc
    else scan sstart (i + 1) acc
  and scan sstart i acc =
    if i > max_s_idx then
      if sstart = start then (if no_empty && s_len = 0 then [] else [sub]) else
      List.rev (add_sub ~no_empty s ~start:sstart ~stop acc)
    else if sunsafe_get s i = sunsafe_get sep sep_start
    then check_sep sstart i 1 acc
    else scan sstart (i + 1) acc
  in
  scan start start []

let rcuts ~no_empty ~sep:(sep, sep_start, sep_stop) (s, start, stop as sub) =
  let sep_len = sep_stop - sep_start in
  if sep_len = 0 then invalid_arg Astring_base.err_empty_sep else
  let s_len = stop - start in
  let max_sep_zidx = sep_len - 1 in
  let max_s_idx = stop - 1 in
  let rec check_sep sstop i k acc =
    if k > max_sep_zidx then
      let start = i + sep_len in
      rscan i (i - sep_len) (add_sub ~no_empty s ~start ~stop:sstop acc)
    else if sunsafe_get s (i + k) = sunsafe_get sep (sep_start + k)
    then check_sep sstop i (k + 1) acc
    else rscan sstop (i - 1) acc
  and rscan sstop i acc =
    if i < start then
      if sstop = stop then (if no_empty && s_len = 0 then [] else [sub]) else
      add_sub ~no_empty s ~start ~stop:sstop acc
    else if sunsafe_get s i = sunsafe_get sep sep_start
    then check_sep sstop i 1 acc
    else rscan sstop (i - 1) acc
  in
  rscan stop (max_s_idx - max_sep_zidx) []

let cuts ?(rev = false) ?(empty = true) ~sep s =
  let no_empty = not empty in
  if rev then rcuts ~no_empty ~sep  s else fcuts ~no_empty ~sep  s

let fields
    ?(empty = false) ?(is_sep = Astring_char.Ascii.is_white)
    (s, start, stop as sub)
  =
  let no_empty = not empty in
  let max_pos = stop in
  let rec loop i end_pos acc =
    if i < start then begin
      if end_pos = max_pos
      then (if no_empty && max_pos = start then [] else [sub])
      else add_sub ~no_empty s ~start ~stop:end_pos acc
    end else begin
      if not (is_sep (sunsafe_get s i)) then loop (i - 1) end_pos acc else
      loop (i - 1) i (add_sub ~no_empty s ~start:(i + 1) ~stop:end_pos acc)
    end
  in
  loop (max_pos - 1) max_pos []

(* Traversing *)

let iter f (s, start, stop) =
  for i = start to stop - 1 do f (sunsafe_get s i) done

let iteri f (s, start, stop)  =
  for i = start to stop - 1 do f (i - start) (sunsafe_get s i) done

let map f (s, start, stop) =
  let len = stop - start in
  if len = 0 then empty else
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    bytes_unsafe_set b i (f (sunsafe_get s (start + i)))
  done;
  (bytes_unsafe_to_string b, 0, len)

let mapi f (s, start, stop) =
  let len = stop - start in
  if len = 0 then empty else
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    bytes_unsafe_set b i (f i (sunsafe_get s (start + i)))
  done;
  (bytes_unsafe_to_string b, 0, len)

let fold_left f acc (s, start, stop) =
  Astring_base.fold_left f acc s ~start ~stop

let fold_right f (s, start, stop) acc =
  Astring_base.fold_right f s acc ~start ~stop

(* Pretty printing *)

let pp ppf s =
  Format.pp_print_string ppf (to_string s)

let dump ppf s =
  Format.pp_print_char ppf '"';
  Format.pp_print_string ppf (Astring_escape.escape_string (to_string s));
  Format.pp_print_char ppf '"';
  ()

let dump_raw ppf (s, start, stop) =
  Format.fprintf ppf "@[<1>(@[<1>(base@ \"%s\")@]@ @[<1>(start@ %d)@]@ \
                         @[(stop@ %d)@])@]"
    (Astring_escape.escape_string s) start stop

(* OCaml base type conversions *)

let of_char c = v (Astring_base.of_char c)
let to_char s = Astring_base.to_char (to_string s)
let of_bool b = v (Astring_base.of_bool b)
let to_bool s = Astring_base.to_bool (to_string s)
let of_int i = v (Astring_base.of_int i)
let to_int s = Astring_base.to_int (to_string s)
let of_nativeint i = v (Astring_base.of_nativeint i)
let to_nativeint s = Astring_base.to_nativeint (to_string s)
let of_int32 i = v (Astring_base.of_int32 i)
let to_int32 s = Astring_base.to_int32 (to_string s)
let of_int64 i = v (Astring_base.of_int64 i)
let to_int64 s = Astring_base.to_int64 (to_string s)
let of_float f = v (Astring_base.of_float f)
let to_float s = Astring_base.to_float (to_string s)

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
