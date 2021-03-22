let path =
  ref "."

let port =
  ref 8080

(* Parse command-line arguments. *)
let () =
  Arg.parse (Arg.align [
    "-p", Arg.Set_int port, " Port number";
  ]) ((:=) path) "serve [OPTIONS] [PATH]"

(* Switch to libuv for file watcher support. *)
let () =
  Lwt_engine.set (new Serve_lwt_luv.Lwt_luv.engine)

(* Set up a recursive watcher on the whole directory tree. *)
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
          (fun () -> Dream.send "refresh" websocket)
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
  let open Lwt.Infix in

  next_handler request
  >>= fun response ->

  match Dream.header "Content-Type" response with
  | Some "text/html" ->
    Dream.body response
    >>= fun body ->

    let soup = Soup.parse body in

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

(* Rewrites requests for directories to requests for index.html. *)
let index_html next_handler request =
  let rec is_directory path =
    match path with
    | [""] -> true
    | _::suffix -> is_directory suffix
    | _ -> false
  in

  let path = Dream__pure.Inmost.internal_path (Obj.magic request) in

  if is_directory path then
    let location = Dream.path request ^ "index.html" in
    Dream.respond ~status:`See_Other ~headers:["Location", location] ""
  else
    next_handler request

(* Run the web server. *)
let () =
  Dream.run ~debug:true ~port:!port
  @@ Dream.logger
  @@ index_html
  @@ inject_script
  @@ Dream.router [

    (* Upon a request to /_monitoring_websocket, create add the websocket to the
       websocket set to notify. Then wait for any message on it, or closing. If
       any is received, remove the socket for the set and close it on the
       server's end. *)
    Dream.get "/_monitoring_websocket" (fun _ ->
      Dream.websocket (fun socket ->
        let key = add socket in
        Lwt.bind
          (Dream.receive socket) (fun _ ->
          remove key;
          Dream.close socket)));

    (* All other requests are served from the file system. *)
    Dream.get "*" (Dream.static ".");

  ]
  @@ fun _ -> Dream.respond ~status:`Not_Found ""
