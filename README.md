# process-backend-uiop

UIOP backend for [`process-protocol`](https://github.com/egao1980/process-protocol) (≥ 0.3.0; option semantics are documented there).

```lisp
(asdf:load-system "process-backend-uiop")
(stack-process:run '("uname" "-s"))
```

## How it maps the protocol (0.2.0)

- **`run`** launches the child with stdin / stdout / stderr on temp files and reads them back as bytes: output is exact (`:octets`, default) or decoded only when `:output :string` asks. Input strings are encoded with `:external-format` (`:utf-8`); octet input is written as is.
- **`:env`** is merged into the inherited environment (`sb-ext:posix-environ` on SBCL, then `:environment`). `:env-mode :replace` passes `:env` alone. Program lookup uses the parent's `PATH`. Other Unix Lisps get the same semantics through `/usr/bin/env [-i] [-u NAME] NAME=VALUE … cmd`; on Windows non-SBCL Lisps `:env` signals `process-error`.
- **`launch :element-type :octets`** asks UIOP for `(unsigned-byte 8)` pipes. SBCL's pipes are bivalent: `read-byte` / `read-sequence` on octet vectors work, `stream-element-type` still says `character`.
- **`:timeout`** polls the child. On expiry: `process-timeout-error` with a `continue` restart (wait another `:timeout`). `run` kills (`SIGKILL` / `TerminateProcess`) and reaps the child on any other exit; `wait` leaves it running.

OCI: `ghcr.io/egao1980/cl-systems/process-backend-uiop:0.2.0`

## License

MIT
