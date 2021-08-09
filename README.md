# portmidi

PortMidi bindings for Common Lisp using CFFI

Loadable as "portmidi" via ASDF / Quicklisp (as a local project)

## Status

The library is usable so far, almost all functions are implemented (except for filtering).
The interface is pretty basic and close to portmidi, and it hasn't been updated in quite a while,
so contributions for a more high-level interface are welcome.

I'm not using this library anymore and won't do any major work on it,
but I'll try to respond to issues and merge pull-requests.
If you are using this library and would like to become its maintainer, let me know!

## Example:

```lisp
(ql:quickload "portmidi")
(defvar *midi-out* (pm:open-output (pm:get-default-output-device-id) 1024 0))
(pm:write-short-midi *midi-out* 0 (pm:note-on 0 80))
(pm:write-short-midi *midi-out* 0 (pm:note-off 0 80))
(pm:close-midi *midi-out*)
```
