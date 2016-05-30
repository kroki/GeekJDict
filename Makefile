check-grammar:
	@sed -n '/^g\|^[ 	]*\(#\|$$\)/p' grammar.test \
	| COLUMNS=1000 ./geekjdict --no-color \
	| sed '$$d; s/^geekjdict>> //' \
	| diff --unified grammar.test -
