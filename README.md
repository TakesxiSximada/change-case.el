# change-case.el

Transform a string between camelCase, PascalCase, snake_case, kebab-case, doted.case and others by Emacs Lisp. Inspired by https://www.npmjs.com/package/change-case.

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

1. Download change-case.el.
2. M-x package-install-file RET /PATH/TO/DOWNLOADED/change-case.el RET

## Screenshot

![example](https://media.giphy.com/media/kaOE3qOXzoZQ0zxpea/source.gif "example")


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