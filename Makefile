# guile-show-hub

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# guile-show-hub is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# guile-show-hub is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with guile-show-hub.  If not, see <https://www.gnu.org/licenses/>.

run:
	guix shell -m manifest.scm -- \
		guile -L ./src -c '((@(guile-show-hub main) main))'

format:
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
	mv -v ./dist/index.html /srv/http/jointhefreeworld.org/guile-show-hub/

serve:
	darkhttpd ./dist --port 50074


dev: format
		watchexec -i "dist/**" -r -e scm,css,js -- "make run && make serve"
