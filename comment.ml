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

module Comment : sig

    val url : unit -> string

    val request_url : string -> unit

    val comment_options : (string * Arg.spec * string) list

    val parse : string -> string list

end =
struct
    open Core
    open Yojson.Basic.Util

    type comment = { mutable url : string; mutable count : int; }

    let comment_base_url =
        "http://b.hatena.ne.jp/entry/jsonlite/"

    let new_comment () =
        { url = ""; count = 10 }

    let comment = new_comment ()

    let request_url url =
        comment.url <- url

    let url () =
        let encoded_url = Uri.pct_encode comment.url in
        Printf.sprintf "%s?url=%s" comment_base_url encoded_url

    let comment_options = [
            ("-c",
                Arg.Int (function v -> comment.count <- v),
                "");
        ]

    let parse_comment node =
        node |> member "comment" |> to_string

    let parse response =
        [Yojson.Basic.from_string response]
            |> filter_member "bookmarks"
            |> flatten
            |> List.filter (fun x -> String.length (parse_comment x) > 0)
            |> List.map (fun x ->
                    let comment = parse_comment x
                    and user = x |> member "user" |> to_string in
                    Printf.sprintf "%s - %s" comment user)
            |> fun x -> Core.Std.List.take x comment.count

end
