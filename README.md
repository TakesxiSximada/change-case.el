# change-case.el

Transform a string between camelCase, PascalCase, snake_case, kebab-case, doted.case and others by Emacs Lisp. Inspired by https://github.com/blakeembrey/change-case/ .
#### Screenshot

![example](https://media.giphy.com/media/fW4lOLLIfCk7OleiyW/source.gif "example")

## Supported case

- [x] dotted.case
- [x] path/case
- [x] snake_case
- [x] kebab-case
- [x] PascalCase
- [x] camelCase

## Dependencies

- dash
- s
- ert

## Install

**This package is not yet melpa packaging.**

1. Download [change-case.el](https://gist.githubusercontent.com/sximada/819e066481b57f8ea6e5a8ec92fb9c27/raw/9bbd7c116540133b945bc70e9fb38912fa8a72ff/change-case.el).
2. M-x package-install-file RET /PATH/TO/DOWNLOADED/change-case.el RET

## Testing

Using ert.

1. Open change-case.el file.
2. Run M-x ert on change-case.el buffer.

Output example::
```
Selector: t
Passed:  13
Failed:  0
Skipped: 0
Total:   13/13

Started at:   2020-04-29 15:28:31+0900
Finished.
Finished at:  2020-04-29 15:28:31+0900

.............
```

## Bug report and Contributing

We haven't prepared those flows yet. For the time being, please write the steps
to reproduce the bug in this gist comment, or attach the patch file in diff format.
Welcome them.
