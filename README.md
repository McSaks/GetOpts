# Read command-line options in Wolfram Language (a.k.a. Mathematica) #

## Documentation

`GetOps[argv, spec]` extracts values of `argv` list according to `spec` list.
`argv` is a list of strings representing command-line arguments (excluding the program name).

Supported formats of an option with a value
both for long and short options are: `-o val`, `-o=val`, and `-o:val`.
No terse form like `-n10` is supported.
Option bundling (`-abc` as `-a -b -c`) may be used only for flag or switch single-letter options.

If several same-key options appear, the last one wins (_count_ option behaves differently, see below).

Returns list of two lists `{ {positionalArgs, ...}, {key -> value, ...} }`.

`spec` is a list or association of option specifications,
each being of the form `name -> type` where `name` is either

  * string option like one-letter `"o"` or long `"-option"` for `-o` or `--option`, or

  * list of strings, which means either of them works.
    The first element in considered as a key.

`type` specifies the option type and default value. Supported types are:

  - With a value:

    * __string__: `type` is either `GOMandatory` for mandatory option (yes, oxymoron, but supported),
      or any value declaring the default value. The string passed is returned as is.

    * __converted__ value: `type` is either `func -> GOMandatory` or `func -> value`.
      Like previous, but given string is passed to `func` converter to get the final value.
      The default value given is not converted and is returned as is.
      (__Yet to be implemented.__)

  - Without a value:

    * __flag__: `type` is `GOFlag`. Returns `True` if the option appears at least once.
      Once set, it is not unsetteble. See _switch_ below.

    * __switch__: `type` is either `False` or `True`. Switches on if used as `++option`, `+-option`, or `+o`.
      `+abc` makes `a`, `b`, and `c` all be `True`. Switches off on `--option` or `-o`.
      The rightmost conflicting option wins.

    * __count__: `type` is `GOCount`. Returns number of appearances of the option.
      `-oo --option` gives `-option -> 3`.

`GetOpts` may take one option `Orderless`. If `False` (default), first non-option found stops scanning options;
all to th eright is considered positional arguments. If `True`, options and positional arguments may be interleaved.
Scanning options stops at `--` in either case.


## Sample usage

Load the package:
```mma
Needs["GetOps`"];
```

Set option specification:
```mma
spec = {
  {"-mand", "m"} -> GOMandatory,
  {"-opt", "o"} -> "some default value",
  {"-count", "c", "n"} -> GOCount,
  "-flag" -> GOFlag,
  "-switch" -> False
};
```

Get command-line options:
```mma
argv = Rest[$CommandLine];
(* or *)
argv = {
  one,  two,
  --mand=0,
  --cnt,  -c,  -n,
  ++switch,
  three,
  -o, val,
  -m:1,
  --, (* no more arguments interpreted as keyword options *)
  --some--numbered--option--
};
```

Get result:
```mma
GetOpts[argv, spec]  ⟶  {
  {"one", "two", "three", "--some--numbered--option--"},
  {"-mand" -> "1",
   "-opt" -> "val",
   "-count" -> 3,
   "-flag" -> False,
   "-switch" -> True}
}
```
