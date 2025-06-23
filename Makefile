run:
	guix shell -m manifest.scm -- \
		guile -L ./src -c '((@(guile-show-hub main) main))' 

fmt:
	find . -maxdepth 8 -name "*.scm" -type f -exec guix style -f {} \;

# Development REPL
repl:
	guix shell -m manifest.scm -- \
		guile \
		-L ./src \
		-c '((@ (ares server) run-nrepl-server))'

