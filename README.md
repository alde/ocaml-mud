# OCaml MUD server

Trying to learn some OCaml by writing a (very simple) MUD server.

## To run:

(only tested on MacOS)

```bash
dune build
dune exec ./bin/main.exe
```

Connect with

```bash
nc localhost 4000
```

Supports `look`, movement (`north` | `south` | `east` | `west` | `up` | `down`), `say`.
