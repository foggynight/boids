.PHONY: all
all:
	sed -e '/(STYLE)/{r template.css' -e 'd}' template.html >boids.html
	sed -i -e '/(SCRIPT)/{r boids.scm' -e 'd}' boids.html
