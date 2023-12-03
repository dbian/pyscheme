deploy:
	python -m build --wheel
	python -m twine upload ./dist/*

t:
	python -m unittest

tt:
	python -m unittest discover -p "test_t*.py"