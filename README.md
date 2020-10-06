# chat-test

Simple one-to-one chat that runs in a terminal

## Installation and running

1. Install [opam](https://opam.ocaml.org/doc/Install.html)
2. ```sh 
   opam install . --deps-only 
   eval $(opam config env)
   dune build
   ```
3. Run ```_build/default/bin/server.exe``` and ```_build/default/bin/client.exe``` in separate terminal windows
