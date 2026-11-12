# README - `./scripts`

`./scripts` is where all code scripts, regardless of language (e.g. R, python), are to be stored.


## Python environment and modules

We use a python environment for download scripts

```sh
# from the repository root
python3 -m venv .venv
source .venv/bin/activate
pip install --upgrade pip
pip install requests pyprojroot dotenv
```

The `requirements.txt` file has a list of the installed modules and versions

```sh
pip freeze > requirements.txt
```

And we can use it to install the same modules in another environment:

```sh
pip install -r requirements.txt
```