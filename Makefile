deploy:
	.env\Scripts\python.exe -m build --wheel
	.env\Scripts\python.exe -m twine upload .\dist\*