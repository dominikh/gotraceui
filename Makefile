.PHONY: doc/manual/version.tex doc/manual/manual.pdf all

all: gotraceui.pdf

doc/manual/version.tex:
	git log --format="%h" -n1 >doc/manual/version.tex
	test -n "$$(git status -u no --porcelain)" && echo "(dirty)" >>doc/manual/version.tex || true

gotraceui.pdf: doc/manual/manual.pdf
	cp doc/manual/manual.pdf gotraceui.pdf

# latexmk is like a LaTeX-specific make. It tracks dependencies and only rebuilds when necessary.
# This is superior to having to manually extract all of our dependencies.
doc/manual/manual.pdf: doc/manual/version.tex
	latexmk -cd -f -pdf -lualatex -interaction=nonstopmode -use-make -bibtex-cond1 doc/manual/manual.tex
