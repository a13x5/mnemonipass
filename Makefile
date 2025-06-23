output-filename := mnemonipass
main-filename := main.scm

.PHONY : install guix-build

build : $(main-filename)
	gsc -postlude "(main)" -o ${output-filename} -exe -ld-options '-static' -nopreload $^
install : build
	mkdir -p ${PREFIX}/bin
	cp ${output-filename} ${PREFIX}/bin/${output-filename}
nix-build :
	nix build
