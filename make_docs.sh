rm -r docs
make html
mv html docs
echo "This entire folder is deleted and auto-generated via `make html`" > docs/WARNING
rm -r html
