deploy:
	py -m build --wheel
	twine upload dist/*