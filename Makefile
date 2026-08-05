compiler:
	cd minic && dune build

runtime:
	cd minir && cargo build

clean:
	cd minic && dune clean
	cd minir && cargo clean
