# OCaml-Wlc

Experimental OCaml binding for Wlc (http://github.com/Cloudef/wlc).

## How to build

After installing [Wlc](http://github.com/Cloudef/wlc), run:

```
opam pin add wlc https://github.com/Armael/ocaml-wlc.git
```

## Example

See `example/example.ml`.

## Is it up-to-date?

The hash of the Wlc commit corresponding to the current state of the
binding can be found in the file WLC_REV.

If it's not the latest commit:

- If Wlc's API (include/wlc/*.h) has not changed in between,
  then it's probably fine;
- If it changed, please ping me via github so I update the bindings :-).

## Implementation details

- *_user_data functions are not bound. It could be done, but
   there is no need to bloat the binding since we can have hashtables
   {view/output/...} -> anything in OCaml.

- some zeroed handles may not be handled (i.e. OCaml functions taking or
  returning a `t` instead of a `option t`) probably causing an error in
  the zero case. Report it if it happens

- I tried to order the various functions in submodules, causing some renamings
  for module dependencies reasons. Names of functions and variants is not
  written in stone and can be discussed.
