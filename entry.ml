(*
 * Copyright 2017 Yoshihiro Tanaka
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Author: Yoshihiro Tanaka <contact@cordea.jp>
 * date  : 2017-05-12
 *)

module Entry : sig

    val runnable : unit -> bool

    val url : unit -> string

    val entry_options : (string * Arg.spec * string) list

    val parse : string -> string list

end =
struct
    open Core

    type category = All | Social | Economics | Life | Knowledge | It | Entertainment | Game | Fun | Video

    type entry = { mutable count : int; mutable category: category; }

    let usage = "
Usage: hb entry [options]

Options:

  -c    Number of entries (default 10)
  -t    Entry type (default all)
"

    let is_help = ref false

    let runnable () =
        !is_help = false

    let category_string cat =
        match cat with
        | All -> "all"
        | Social -> "social"
        | Economics -> "economics"
        | Life -> "life"
        | Knowledge -> "knowledge"
        | It -> "it"
        | Entertainment -> "entertainment"
        | Game -> "game"
        | Fun -> "fun"
        | Video -> "video"

    let entry_base_url cat =
        Printf.sprintf "http://b.hatena.ne.jp/hotentry/%s.rss" cat

    let entry_base_all_url =
        "http://feeds.feedburner.com/hatena/b/hotentry"

    let new_entry () =
        { count = 10; category = All; }

    let entry = new_entry ()

    let url () =
        if entry.category = All then
            entry_base_all_url
        else
            let cat_str = category_string (entry.category) in
            entry_base_url cat_str

    let entry_options = [
        ("-c",
            Arg.Int (fun v -> entry.count <- v),
            "");
        ("-t",
            Arg.String (function
                | "all" -> entry.category <- All
                | "social" -> entry.category <- Social
                | "economics" -> entry.category <- Economics
                | "life" -> entry.category <- Life
                | "knowledge" -> entry.category <- Knowledge
                | "it" -> entry.category <- It
                | "entertainment" -> entry.category <- Entertainment
                | "game" -> entry.category <- Game
                | "fun" -> entry.category <- Fun
                | "video" -> entry.category <- Video
                | _ -> raise (Arg.Bad "")),
            "");
        ("-help",
            Arg.Unit (fun () ->
                is_help := true;
                print_endline usage),
            "");
        ("--help",
            Arg.Unit (fun () ->
                is_help := true;
                print_endline usage),
            "");
    ]

    let val_by_key node key =
        node
            |> List.filter (fun x -> Xml.tag x = key)
            |> List.hd
            |> Xml.children
            |> List.hd
            |> Xml.pcdata

    let parse response =
        let x = Xml.parse_string response in
        Xml.children x
            |> List.filter (fun x -> Xml.tag x = "item")
            |> List.map (fun x ->
                    let child = Xml.children x in
                    let title = val_by_key child "title"
                    and url = val_by_key child "link" in
                    Printf.sprintf "%s\n\t%s" title url)
            |> fun x -> Core.Std.List.take x entry.count

end
