all:
	racket make.rkt

clean:
	(cd docs; rm *.html; rm *.pdf; rm *.css; rm *.js; rm *.png; rm *.svg)
