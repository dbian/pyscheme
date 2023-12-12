deploy:
	python -m build --wheel
	python -m twine upload ./dist/*

t:
	python -m unittest -f

tp:
	python -m unittest discover -p "test_p*.py" -f --locals