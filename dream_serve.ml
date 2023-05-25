(* This file is part of dream-serve, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/dream-serve.

   Copyright 2021 Anton Bachin *)



module Dream =
struct
  include Dream

  let path = path [@ocaml.warning "-3"]
  let with_path = with_path [@ocaml.warning "-3"]
end



(* Parse command-line arguments. *)

let path =
  ref "."

let port =
  ref 8080

let md =
  ref true

let () =
  Arg.parse (Arg.align [
    "-p", Arg.Set_int port, " Port number";
    "--no-md", Arg.Clear md, " Don't convert Markdown files to HTML";
  ]) ((:=) path) "serve [OPTIONS] [PATH]"



(* Set up a recursive watcher on the whole directory tree.
   Switch to libuv for file watcher support. *)

let () =
  Lwt_engine.set (new Serve_lwt_luv.Lwt_luv.engine)

let watch_directory path notify =
  let watcher = Luv.FS_event.init () |> Result.get_ok in
  Luv.FS_event.start watcher path begin function
    | Error _ -> ()
    | Ok (_, _) -> notify ()
  end

let rec watch_tree path notify =
  watch_directory path notify;

  Sys.readdir path
  |> Array.iter begin fun entry ->
    let entry = Filename.concat path entry in
    if Sys.is_directory entry then
      watch_tree entry notify
    else
      ()
  end



(* Accumulate multiple events and emit one after a delay. *)

let debounce delay f =
  let had_recent = ref false in
  fun () ->
    if !had_recent then
      ()
    else begin
      had_recent := true;
      Lwt.on_success (Lwt_unix.sleep delay) (fun () ->
        had_recent := false;
        f ())
    end



(* Maintain a set of connected WebSockets to notify when there is an update. *)

let websockets =
  Hashtbl.create 256

let last_key = ref 0

let add websocket =
  incr last_key;
  let key = !last_key in
  Hashtbl.replace websockets key websocket;
  key

let remove key =
  Hashtbl.remove websockets key

let () =
  watch_tree !path @@ debounce 0.25 @@ (fun () ->
    websockets |> Hashtbl.iter (fun key websocket ->
      Lwt.async (fun () ->
        Lwt.catch
          (fun () -> Dream.send websocket "refresh")
          (fun _ -> remove key; Lwt.return_unit))))



(* Intercept HTML responses and insert script. *)

let script = {js|
var _monitoring_socket =
  new WebSocket("ws://" + location.host + "/_monitoring_websocket");

_monitoring_socket.onmessage = function (e) {
  location.reload(true);
}
|js}

let inject_script next_handler request =
  let%lwt response = next_handler request in

  match Dream.header response "Content-Type" with
  | Some ("text/html" | "text/html; charset=utf-8") ->
    let%lwt body = Dream.body response in

    let soup =
      Markup.string body
      |> Markup.parse_html ~context:`Document
      |> Markup.signals
      |> Soup.from_signals
    in

    let open Soup.Infix in

    begin match soup $? "head" with
    | None -> Lwt.return response
    | Some head ->
      Soup.create_element "script" ~inner_text:script
      |> Soup.append_child head;
      Dream.set_body response (Soup.to_string soup);
      Lwt.return response
    end

  | _ ->
    Lwt.return response



(* Redirect requests for directories to index.html. *)

let index_html next_handler request =
  let rec is_directory path =
    match path with
    | [""] -> true
    | _::suffix -> is_directory suffix
    | _ -> false
  in

  let path = Dream.path request in

  if is_directory path then
    Dream.redirect request (String.concat "/" (path @ ["index.html"]))
  else
    next_handler request



(* Optionally rewrite requests to foo.html to foo.md. *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::more -> last more

let rec replace_last x = function
  | [] -> assert false
  | [_] -> [x]
  | x'::more -> x'::(replace_last x more)

(* Based on Quick Start in the Cmarkit docs
   (https://erratique.ch/software/cmarkit/doc/). *)
let cmark_to_html md =
  let doc = Cmarkit.Doc.of_string md in
  Cmarkit_html.of_doc ~safe:false doc

let try_markdown next_handler request =
  let%lwt response = next_handler request in

  if Dream.status response <> `Not_Found then
    Lwt.return response

  else begin
    let path = Dream.path request in
    let (ends_with_html, file) =
      match last path with
      | Some file when Filename.extension file = ".html" -> true, file
      | _ -> false, ""
    in
    if not ends_with_html then
      Lwt.return response

    else begin
      let original_response = response in

      let path = replace_last ((Filename.remove_extension file) ^ ".md") path in
      let request = Dream.with_path path request in

      let%lwt response = next_handler request in

      if Dream.status response <> `OK then
        Lwt.return original_response

      else begin
        Dream.set_header response "Content-Type" Dream.text_html;
        let%lwt body = Dream.body response in
        Dream.set_body response (cmark_to_html body);
        Lwt.return response
      end
    end
  end



(* Suppress caching, as this server runs only locally. *)

let suppress_caching next_handler request =
  let%lwt response = next_handler request in
  begin match Dream.header response "Content-Type" with
  | Some "text/html; charset=utf-8" ->
    Dream.set_header response "Cache-Control" "no-store";
  | _ ->
    ()
  end;
  Lwt.return response



(* Run the web server. *)

let () =
  Dream.log "Running at http://localhost:%i" !port;
  Dream.log "Type Ctrl+C to stop";

  Lwt_main.run
  @@ Dream.serve ~port:!port
  @@ Dream.logger
  @@ suppress_caching
  @@ index_html
  @@ inject_script
  @@ (if !md then try_markdown else Dream.no_middleware)
  @@ Dream.router [

    (* Upon a request to /_monitoring_websocket, add the WebSocket to the
       WebSocket set. Then, wait for any event at all on it, whether end of
       input or a message, because we don't expect any messages from clients.
       Close the WebSocket in response to the event. *)
    Dream.get "/_monitoring_websocket" (fun _ ->
      Dream.websocket (fun socket ->
        let key = add socket in
        Lwt.bind
          (Dream.receive socket) (fun _ ->
          remove key;
          Dream.close_websocket socket)));

    (* All other requests are served from the file system. *)
    Dream.get "**" (Dream.static !path);

  ]
