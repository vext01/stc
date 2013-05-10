open String
open List
open Printf

let rec split_string str ch =
    let pos = try Some (index str ch) with Not_found -> None in
    match pos with
    | None -> [str]
    | Some pos' -> let remain_len = (String.length str) - pos' - 1 in
        let remain_str = String.sub str (pos' + 1) remain_len in
        let elem = String.sub str 0 pos' in
        [ elem ] @ split_string remain_str ch;;
