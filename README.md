# process-backend-uiop

UIOP backend for [`process-protocol`](https://github.com/egao1980/process-protocol) (≥ 0.4.0; option semantics are documented there).

```lisp
(asdf:load-system "process-backend-uiop")
(stack-process:run '("uname" "-s"))
```

## How it maps the protocol (0.3.0)

- **`run`** launches the child with stdin / stdout / stderr on temp files and reads them back as bytes: output is exact (`:octets`, default) or decoded only when `:output :string` asks. Input strings are encoded with `:external-format` (`:utf-8`); octet input is written as is.
- **`:env`** is merged into the inherited environment (`sb-ext:posix-environ` on SBCL, then `:environment`). `:env-mode :replace` passes `:env` alone. Program lookup uses the parent's `PATH`. Other Unix Lisps get the same semantics through `/usr/bin/env [-i] [-u NAME] NAME=VALUE … cmd`; on Windows non-SBCL Lisps `:env` signals `process-error`.
- **`launch :element-type :octets`** asks UIOP for `(unsigned-byte 8)` pipes. SBCL's pipes are bivalent: `read-byte` / `read-sequence` on octet vectors work, `stream-element-type` still says `character`.
- **`:timeout`** polls the child. On expiry: `process-timeout-error` with a `continue` restart (wait another `:timeout`). `run` kills (`SIGKILL` / `TerminateProcess`) and reaps the child on any other exit; `wait` leaves it running.

- **`:process-group t`** (`launch`, `run`): Unix (SBCL) — SBCL's `spawn()` already makes a child whose stdin is not inherited a process-group leader; the backend refuses `:input :interactive` and checks `getpgid(pid) = pid` after the launch, signalling `process-group-error` otherwise. Other Unix Lisps: `supports-p :process-group` is NIL and the option signals `process-group-error`. Windows (SBCL): the child is put in a new job object right after `CreateProcess` (kernel32 `CreateJobObjectW` / `AssignProcessToJobObject` through `sb-alien`, no native library); its descendants inherit the job.
- **`kill :tree t`**: Unix `killpg(pid, SIGTERM)` (`SIGKILL` with `:force t`), also after the shell itself exited. Windows (SBCL): `TerminateJobObject` (always forced, exit code 1), also after the shell itself exited. `taskkill /T` is not used there: an MSYS / Cygwin fork+exec (Git for Windows `sh`) leaves the exec'd program with a Windows parent pid that has already exited, so a parent-pid walk misses it ([#13](https://github.com/egao1980/process-backend-uiop/issues/13)). Limit: a grandchild the child starts before the backend assigns the job (right after `CreateProcess` returns) escapes it; an MSYS `sh` takes far longer than that to start (UIOP cannot start the child suspended). Other Windows Lisps, or a failed assignment: `taskkill /T /F /PID pid` while the child runs, with the MSYS gap above. A handle launched without `:process-group t` gets `process-group-error` with a `continue` restart (kill the direct child only).
- **`run :process-group t :timeout n`**: leaving `run` early (timeout, unwind) SIGKILLs the group / terminates the job (`taskkill /T` fallback), then reaps the child.
- **`pid`**: `uiop:process-info-pid`, cached at launch. `supports-p`: `:pid` always, `:process-group` on SBCL and on Windows.

OCI: `ghcr.io/egao1980/cl-systems/process-backend-uiop:0.3.1`

## License

MIT
