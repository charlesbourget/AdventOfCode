# Helper scripts

Wanted to try common lisp so wrote this little script to add completed stars to the main README.

## How to run

You will need:
- [sbcl](https://www.sbcl.org/)
- [quicklisp](https://www.quicklisp.org/beta/)
- Make

Compile the script to a binary executable:

```bash
make
```

Run the script:

```bash
./bin/stars -f ../README.md
```