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


# Deploy generated assets to jointhefreeworld
deploy:
	CI=true make run
	rm -rfv /srv/http/jointhefreeworld.org/guile-show-hub
	mkdir -p /srv/http/jointhefreeworld.org/guile-show-hub
	mv -v index.html /srv/http/jointhefreeworld.org/guile-show-hub/
