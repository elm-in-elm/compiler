# Dev progress page

A nice page inspired by [NetSurf](http://www.netsurf-browser.org/documentation/progress.html), generated from a [`data.txt`](https://github.com/elm-in-elm/compiler/blob/master/dev-progress/data.txt) file, automatically pushed to `gh-pages` branch via GitHub Action.

## Syntax

* lines starting with `//` get filtered out (comments!)
* lines starting with `#` are section names 
* other lines are basically CSV separated by `|`. The columns are title, status,
  notes.
* status can be one of:
  * `notstarted`
  * `juststarted`
  * `inprogress`
  * `nearlydone`
  * `complete`

