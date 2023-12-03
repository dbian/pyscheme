deploy:
	.env\Scripts\python.exe -m build --wheel
	.env\Scripts\python.exe -m twine upload .\dist\*

t:
	python -m unittest

tt:
	python -m unittest discover -p "test_t*.py"