(* This file is part of dream-serve, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/dream-serve.

   Copyright 2021 Anton Bachin *)



(* Parse command-line arguments. *)

let path =
  ref "."

let port =
  ref 8080

let () =
  Arg.parse (Arg.align [
    "-p", Arg.Set_int port, " Port number";
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

  match Dream.header "Content-Type" response with
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
      Lwt.return (Dream.with_body (Soup.to_string soup) response)
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



(* Run the web server. *)

let () =
  Dream.run ~debug:true ~port:!port
  @@ Dream.logger
  @@ index_html
  @@ inject_script
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
  @@ Dream.not_found
