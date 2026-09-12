#!/usr/bin/env bash
# Build the MicroHs runtime as JavaScript (rts.js) and run a hello-world
# combinator file with node:  node rts.js hello-world.comb
set -euo pipefail
cd "$(dirname "$0")"

# 1. Native compiler (needed to produce .comb files)
if [ ! -x bin/mhs ]; then
  nix-shell -p gcc --run 'make bin/mhs'
fi

# 2. Runtime compiled to JavaScript
nix-shell -p emscripten --run 'make rts.js'

# 3. Hello world -> combinator file
cat > HelloWorld.hs <<'HS'
module HelloWorld(main) where
main :: IO ()
main = putStrLn "hello world"
HS
bin/mhs -ilib HelloWorld -ohello-world.comb
rm -f HelloWorld.hs

# 4. Run it with node
nix-shell -p nodejs --run 'node rts.js hello-world.comb'
