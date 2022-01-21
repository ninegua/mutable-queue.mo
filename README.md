# Mutable Queue

A [motoko] module of a mutable queue data structure supporting both `pushFront` and `pushBack` but only `popFront`.
It can be conveniently used for stable variables to hold a collection of values if you don't mind O(n) lookups.

Internally it is just a linked list with both `first` and `last` pointers.

You can use this library with the [vessel] package manager.

## Development

Documentation is non-existent but functions should be self-explanatory given their types.

If you have installed a [nix] environment, you can run the tests like this:

```
nix-shell
cd test
make
```

[motoko]: https://github.com/dfinity/motoko
[vessel]: https://github.com/dfinity/vessel
