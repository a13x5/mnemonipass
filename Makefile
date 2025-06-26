output-filename := mnemonipass
main-filename := main.scm
dicts-dir := dicts
dicts-scm := $(dicts-dir)/dicts.scm

.PHONY : install nix-build

build : $(dicts-scm)
	gsc -postlude "(main)" -o ${output-filename} -exe -ld-options '-static' -nopreload $(main-filename)
install :
	mkdir -p ${PREFIX}/bin
	cp ${output-filename} ${PREFIX}/bin/${output-filename}
nix-build :
	nix build
$(dicts-scm) :
	cd $(dicts-dir); \
	gsi build-dicts.scm
