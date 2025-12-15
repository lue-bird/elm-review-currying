## (unreleased) elm-review-currying

The [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule [`Review.Currying.forbid`](Review-Currying#forbid) checks that functions are not applied only partially, like `justs = List.filterMap identity`.

```elm
module ReviewConfig exposing (config)

import Review.Currying
import Review.Rule

config : List Review.Rule.Rule
config =
    [ Review.Currying.forbid
    ]
```
works well in combination with [`elm-review-pipeline-styles`](https://dark.elm.dmy.fr/packages/SiriusStarr/elm-review-pipeline-styles/latest/) which allows you to also ban `<<` and `>>` as these are pretty much useless without currying

## try it

```bash
elm-review --template lue-bird/elm-review-currying/preview
```

## this is not (yet?) published because...
- when counting expected parameters, function type aliases are not respected
  so you might end up with false positives
- application of e.g. references in `case of` like
  ```elm
  (case ... of ... -> List.map) identity
  ```
  is not caught, making them false negatives.
  In general, it is impossible to not have false negatives without type inference
- the fix to a lambda is not yet implemented. Requires avoiding overlap with potential existing bindings which is fairly annoying to ensure
