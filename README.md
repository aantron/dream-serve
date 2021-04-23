# dream-serve

**dream-serve** is a [very simple][source] server for static sites. It reloads
your browser when the site changes.

<br>

<p align="center">
<img src="https://raw.githubusercontent.com/aantron/dream-serve/master/reload.gif"></img>
</p>

<br>

It's good for [developing docs][odoc], streamlining [coverage reports][bisect],
and so forth. It works by injecting a script into HTML, which opens a WebSocket
back to the server. The server uses the WebSocket to tell the browser when to
reload.

<br>

## Usage

Until dream-serve is released to opam and npm:

```
git clone https://github.com/aantron/dream-serve.git
cd dream-serve
_build/install/default/bin/dream-serve /my/site
```

After npm release:

```
npx dream-serve ./my/site
```

After opam release:

```
opam install dream-serve
dream-serve ./my/site
```

<br>

## Based on

dream-serve is implemented as [one small OCaml file][source]. It is based on Web
framework [Dream][dream], [libuv][libuv] binding [Luv][luv], and HTML rewriter
[Lambda Soup][soup].

Special thanks to [Ulrik Strid][ulrikstrid] for writing the
[integration][lwt_luv] between Lwt and Luv/libuv that makes it possible to
easily run Dream over libuv.

[source]: https://github.com/aantron/dream-serve/blob/master/dream_serve.ml
[dream]: https://github.com/aantron/dream#readme
[luv]: https://github.com/aantron/luv#readme
[soup]: https://github.com/aantron/lambdasoup#readme
[ulrikstrid]: https://github.com/ulrikstrid
[lwt_luv]: https://github.com/ocsigen/lwt/pull/811
[bisect]: https://github.com/aantron/bisect_ppx#readme
[libuv]: https://github.com/libuv/libuv
[odoc]: https://github.com/ocaml/odoc

<br>

## Roadmap

- [ ] Factor out the client-handling and HTML-rewriting logic into a library
  that can be used with dynamic apps, so that they trigger a reload when coming
  back up.
