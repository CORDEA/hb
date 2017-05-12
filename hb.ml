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

open Lwt
open Cohttp
open Cohttp_lwt_unix
open Comment
open Entry

type request_type = None | Entry | Comment

let request_type = ref None

let options = ref []

let usage = ""

let request url =
    let headers = Cohttp.Header.of_list [
        "User-agent", "ocaml-hb/0.0.1"
    ] in
    Client.call ~headers `GET (Uri.of_string url) >>= fun (resp, body) ->
        body |> Cohttp_lwt_body.to_string >|= fun body ->
            body

let parse arg =
    if !Arg.current = 1 then
        match arg with
        | "comment" ->
                request_type := Comment;
                options := Comment.comment_options
        | "entry" ->
                request_type := Entry;
                options := Entry.entry_options
        | _ -> raise (Arg.Bad "")
    else
        if !request_type = Comment then
            Comment.request_url arg

let () =
    Arg.parse_dynamic options parse usage;
    match !request_type with
    | Comment ->
            Lwt_main.run (request (Comment.url ()))
                |> Comment.parse
                |> List.iter (print_endline)
    | Entry ->
            Lwt_main.run (request (Entry.url ()))
                |> Entry.parse
                |> List.iter (print_endline)
    | None ->
            raise (Arg.Bad "")
;;
