output-filename := mnemonipass
main-filename := main.scm
package-filename := package.scm

.PHONY : install

build : $(main-filename)
	gsc -postlude "(main)" -o ${output-filename} -exe -ld-options '-static' -nopreload $^
install : build
	mkdir -p ${PREFIX}/bin
	cp ${output-filename} ${PREFIX}/bin/${output-filename}
guix-build :
	guix build -f ${package-filename}
