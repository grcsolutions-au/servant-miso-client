.PHONY: build build-js serve serve-js clean

CLIENT = exe:servant-multipart-miso-drop-in-client-test-client
GHCJS = --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

build:
	wasm32-wasi-cabal build $(CLIENT)
	rm -rf public
	cp -r $(MISO)/tests/static public
	cp $(MISO)/js/miso.js public/
	sed -i 's#</head>#<script src="miso.js"></script></head>#' public/index.html
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin $(CLIENT) | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/component-tests.wasm

build-js:
	cabal build $(CLIENT) $(GHCJS)

serve:
	http-server public

serve-js:
	jsexe=$$(cabal list-bin $(CLIENT) $(GHCJS) | tail -n 1); \
	if test -d "$$jsexe"; then dir="$$jsexe"; else dir=$$(dirname "$$jsexe"); fi; \
	http-server "$$dir"

clean:
	rm -rf public
