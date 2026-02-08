.PHONY: test
test:
	./test.sh


TAGS: *.py
	ctags -e --languages=Python --exclude=.venv --exclude=__pycache__ -R .
