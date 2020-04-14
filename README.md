# purescript-undefined-or

A purescript library for more convenient interfacing with foreign code.

A lot of JS functions accept and return records where some fields may be
missing. For missing fields in argument records you can use `Data.Options`
module. For missing fields in return records you can either use `Foreign`
and write parsers, or this module and trust the foreign code.

A simple example of usage:
```
// Test.js
exports.typicalApi = function() {
  if (Math.random() > 0.5) {
    return {"numberData": 9431, "stringData": "atad"};
  } else {
    return {"numberData": 8752}
  }
}

-- Test.purs
type ApiRet = {numberData :: Boolean, stringData :: UndefinedOr String}

foreign import typicalApi :: Effect ApiRet

...

result <- typicalApi
case fromUndefined result.stringData of
    Just data -> Console.log $ "got a string: " <> data
    Nothing   -> Console.log "no string"
```

You should avoid using this library whenever possible, as it's a shortcut:
it lifts the neccessity of data validation from you, the ffi-bindings
writer, to the user of your api. Also 75% of the use cases can be covered
by clever use of typeclasses.

## License

This library is licensed under [MIT license](./LICENSE)

## Documentation

Module documentations is
[published on Pursuit](https://pursuit.purescript.org/packages/purescript-undefined-or/docs/Data.UndefinedOr).
