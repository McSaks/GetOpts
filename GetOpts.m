(* ::Package:: *)


BeginPackage["GetOpts`"];

Unprotect /@ Names["GetOpts`*"];
Unprotect /@ Names["GetOpts`Private`*"];

GetOpts::usage="GetOpts[\!\(\*
StyleBox[\"options\", \"TI\"]\), \!\(\*
StyleBox[\"speclist\", \"TI\"]\)] \
extracts values of \!\(\*
StyleBox[\"options\", \"TI\"]\) list according to \!\(\*
StyleBox[\"speclist\", \"TI\"]\) list.
\!\(\*
StyleBox[\"speclist\", \"TI\"]\) elements must have a form \!\(\*StyleBox[\"key\", \"TI\"]\) \[Rule] \!\(\*
StyleBox[\"spec\", \"TI\"]\), where \!\(\*
StyleBox[\"spec\", \"TI\"]\) is one of:
  \[Bullet]  \!\(\*StyleBox[\"GOMandatory\", \"MR\"]\): option with a parameter; mandatory.
  \[Bullet]  \!\(\*StyleBox[\"defaultValue\", \"TI\"]\): option with a parameter; optional, set to \!\(\*
    StyleBox[\"defaultValue\", \"TI\"]\) if missing.
  \[Bullet]  \!\(\*StyleBox[\"function\", \"TI\"] \[Rule] \*StyleBox[\"defaultValue\", \"TI\"]\): \
like above, but the string is passed to \!\(\*StyleBox[\"function\", \"TI\"]\). \
\!\(\*StyleBox[\"defaultValue\", \"TI\"]\) is used as is.
  \[Bullet]  \!\(\*StyleBox[\"GOFlag\", \"MR\"]\): option without parameters; set to \!\(\*
    StyleBox[\"True\", \"MR\"]\) if present.
  \[Bullet]  \!\(\*StyleBox[\"True|False\", \"MR\"]\): option without parameters; set to \!\(\*
    StyleBox[\"True\", \"MR\"]\) if used as \!\(\*StyleBox[\"+\", \"MR\"]\*StyleBox[\"key\", \"TI\"]\) \
and to False if as \!\(\*StyleBox[\"-\", \"MR\"]\*StyleBox[\"key\", \"TI\"]\); optional, \
set to the specified default value if missing.
  \[Bullet]  \!\(\*StyleBox[\"GOCount\", \"MR\"]\): counts occurances of the option.
Sets the last value in case of repeating \!\(\*StyleBox[\"key\", \"TI\"]\)s (if not \!\(\*StyleBox[\"GOCount\", \"MR\"]\)).
Returns numbered and keywods options as \!\(\*StyleBox[\({
	{\*StyleBox[\"arg1\", \"TI\"], \*StyleBox[\"arg2\", \"TI\"], \*StyleBox[\"...\", \"TI\"]},
	{\*StyleBox[\"key1\", \"TI\"] \[Rule] \*StyleBox[\"val1\", \"TI\"],
	 \*StyleBox[\"key2\", \"TI\"] \[Rule] \*StyleBox[\"val2\", \"TI\"], \*StyleBox[\"...\", \"TI\"]}
}\), \"MR\"]\).
Option \!\(\*StyleBox[\(Orderless \[Rule] True|False\), \"MR\"]\) controls whether first non-option \
stops scanning options. Default is \!\(\*StyleBox[\"False\", \"MR\"]\) which means it does.

Example of usage:\n\!\(\*StyleBox[\"
spec = {\\n
  {\\\"mand\\\", \\\"m\\\"} \[Rule] GOMandatory,\\n
  {\\\"opt\\\", \\\"o\\\"} \[Rule] \\\"some default value\\\",\\n
  {\\\"count\\\", \\\"c\\\", \\\"n\\\"} \[Rule] GOCount,\\n
  \\\"flag\\\" \[Rule] GOFlag,\\n
  \\\"switch\\\" \[Rule] False\\n
};\\n
argv = {\\n
  \\\"one\\\",  \\\"two\\\",\\n
  \\\"--mand=0\\\",\\n
  \\\"--cnt\\\",  \\\"-c\\\",  \\\"-n\\\",\\n
  \\\"++switch\\\",\\n
  \\\"three\\\",\\n
  \\\"-o\\\", \\\"val\\\",\\n
  \\\"-m:1\\\",\\n
  \\\"--\\\", (* no more arguments interpreted as keyword options *)\\n
  \\\"--some--numbered--option--\\\"\\n
};\\n
GetOpts[argv, spec] \[LongRightArrow] {\\n
  {\\\"one\\\", \\\"two\\\", \\\"three\\\", \\\"--some--numbered--option--\\\"},\\n
  {\\\"mand\\\" \[Rule] \\\"1\\\",\\n
   \\\"opt\\\" \[Rule] \\\"val\\\",\\n
   \\\"count\\\" \[Rule] 3,\\n
   \\\"flag\\\" \[Rule] False,\\n
   \\\"switch\\\" \[Rule] True}\\n
}\",\"MR\", ShowStringCharacters->True, AutoSpacing->False]\)";
GOMandatory::usage = "GOMandatory is a specification for GetOpts for a mandatory option.";
GOFlag::usage = "GOFlag is a specification for GetOpts for a flag option.";
GOCount::usage = "GOCount is a specification for GetOpts for a counting option.";


SyntaxInformation[GetOpts] = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};
Options[GetOpts] = {Orderless -> False};


Begin["`Private`"];




(* Print message and throw *)

SetAttributes[fail, HoldAll];
If[$VersionNumber >= 10,
  fail[msg_MessageName, args___] := (
    Message[msg, args];
    Throw[Failure[HoldForm[msg], Association[
        "MessageTemplate" :> msg,
        "MessageParameters" -> {args}]],
      HoldForm[msg]]),
  (* no Failure in WL < 10 *)
  fail[msg_MessageName, args___] := fail[$Failed, msg, args]
];
fail[val_, msg_MessageName, args___] := (Message[msg, args]; Throw[val, HoldForm[msg]]);



(* Boolean value of the first sign in an option *)

plusminus["+"] = True;
plusminus["-"] = False;



(* Normalize one option *)
(* argspec is {{optsWithArg, ...}, {optsWoArg, ...}}.
 * Starts from n-th element of argv.
 * Returns opt -> norm, newN} or {bundle[opt -> norm, ...], newN} or nonoption,
 *   where bundle is used for groupped short options,
 *   stops parsing at newN.
 * Normal form is:
 *   - for argumentless option: True if ++opt, False if --opt,
 *   - for argumented option: provided argument.
 *)

SetAttributes[bundle, Flat];
normalizeOne[argv_, argspec_, n_] /; n > Length[argv] = nonoption; (* all scanned *)
normalizeOne[argv_, argspec_, n_] := With[{str = argv[[n]]},
  Which[
    
    (* only arguments left *)
    str === "--",
      nonoption,
    
    (* long option *)
    MemberQ[{"--", "+-", "++"}, Quiet@StringTake[str, 2]],
      normalizeLong[str, argv, argspec, plusminus[StringTake[str, 1]], n],
    
    (* short option *)
    MemberQ[{"-", "+"}, Quiet@StringTake[str, 1]],
      normalizeShort[str, argv, argspec, plusminus[StringTake[str, 1]], n],
    
    (* not an option *)
    True,
      nonoption
  ]
];



(* Long option *)

GetOpts::unknopt = "Unknown option: `1`.";
normalizeLong[str_, argv_, argspec: {_List, _List}, pm_, n_] := With[{
    opt = StringReplace[str,
      StartOfString ~~ ("--" | "+-" | "++" | "-" | "+") ~~ o__ :> o],
    warg = First @ argspec,
    woarg = Last @ argspec
  },
  With[{optpair = StringSplit[opt, "=" | ":", 2]},
    Which[
      
      (* in 'long' list *)
      MemberQ[warg, First @ optpair],
        normalizeLongArg[opt, argv, optpair, n],
      
      (* in 'short' list *)
      MemberQ[woarg, opt],
        normalizeLongNoArg[opt, pm, n],
      
      (* not listed *)
      True,
         fail[GetOpts::unknopt, opt];
    ]
  ]
];

  (* Requires argument *)
  GetOpts::noarg = "No argument provided for option `1`.";
  normalizeLongArg[opt_, argv_, optpair_, n_] :=
    If[ Length[optpair] > 1,
      {Rule @@ optpair, n},
    (* else *)
      If[n + 1 > Length[argv],
        fail[GetOpts::noarg, opt],
      (* else *)
        {opt -> argv[[n + 1]], n + 1}
      ]
    ];

  (* No argument required *)
  normalizeLongNoArg[opt_, pm_, n_] := {opt -> pm, n};



(* Short option *)

normalizeShort[str_, argv_, argspec_, pm_, n_] := With[{
    opt = StringDrop[str, 1],
    warg = First@argspec,
    woarg = Last@argspec
  },
  With[{
      noarg = bundle @@
        (# -> pm & /@ TakeWhile[Characters[opt], MemberQ[woarg, #] &])
    },
    With[{numnoarg = Length[noarg]},
      If[numnoarg == StringLength[opt],
        {noarg, n},
      (* else *)
       With[{ optlett = StringTake[opt, {numnoarg + 1}] },
        If[!MemberQ[warg, optlett],
          fail[GetOpts::unknopt, optlett] ];
        Append[
          noarg,
          normalizeLongArg[ (* parse rest as if it were long *)
            optlett,
            argv,
            StringSplit[
              StringReplace[StringDrop[opt, numnoarg],
                StartOfString ~~ o_ ~~ r: Except["=" | ":"] ~~ rest___ :>
                  o <> "=" <> r <> rest
              ],
              "=" | ":",
              2 ],
            n
          ]
        ]
       ]
      ] ~Replace~ ( bundle[most___, {rule_, num_}] :> {bundle[most, rule], num} )
    ]
  ]
];



(* Recursively normalize whole argv *)

normalize[argv_, argspec_, n_] :=
  With[{ n1 = normalizeOne[argv, argspec, n] },
    If[n1 === nonoption,
      bundle[n],
    (* else *)
      Append[bundle[First @ n1], normalize[argv, argspec, Last @ n1 + 1]]
    ]
  ];



(* bundle[rules, ..., n]  ⟶  {{rules, ...}, n} *)

norm2rules[b_bundle] := {List @@ Most[b], Last[b]};
rules[argv_, argspec_] := norm2rules[normalize[argv, argspec, 1]];



(* Prepare to retrieve passed option values *)

optQ[opt_String] := StringMatchQ[opt, ("-" | "+") ~~ __];
optQ[opt_] := (Message[optQ::string, 1, HoldForm[optQ[opt]]]; False);

position[list_, test_] := With[{i = Position[list, _?test, 1, 1, Heads -> False]},
  If[i === {}, 0, First@First@i]];

pregetargs[argv_, argspec_, Orderless -> False] :=
  With[{ r = rules[argv, argspec] },
    {First @ r, Drop[argv, Last @ r - 1]}
  ];

pregetargs[argv_, argspec_, Orderless -> True] :=
  With[{ firstgroup = pregetargs[argv, argspec, Orderless -> False] },
    With[{
        opts = First @ firstgroup,
        args = Last @ firstgroup
      },
      With[{ firstopt = position[args, optQ] },
        If[ firstopt == 0 || args[[firstopt]] === "--", (* no more options *)
          firstgroup,
        (* else *)
          With[{
              then = pregetargs[Drop[args, firstopt - 1], argspec, Orderless -> True]
            },
            MapThread[Join, {{opts, Take[args, firstopt - 1]}, then}]
          ]
        ]
      ]
    ]
  ];



(* Get {warg, woarg} list *)

expandOptspec[optspec: {(_ -> _) ...}] :=
  Replace[optspec,
    (opts_List -> spec_) :> flat @@ (# -> spec & /@ opts), 1
  ] /. flat -> Sequence;

argSpec[optspec: {(_ -> _) ...}] :=
  With[{expandedoptspec = expandOptspec[optspec]},
    With[{
        woarg = Cases[expandedoptspec,
          (opt_ -> (GOFlag | GOCount | True | False)) :> opt]
      },
      {Complement[First /@ expandedoptspec, woarg], woarg}
    ]
  ];



(* Finally, get options *)

getargs[argv_, optspec_, Orderless -> ol_] :=
  finalproc[
    pregetargs[argv, argSpec[optspec], Orderless -> ol],
    optspec
  ];

finalproc[r_, optspec_] := {
  processopts[First@r, optspec],   (* options *)
  DeleteCases[Last@r, "--", 1, 1]  (* positionals *)
};



(* Interpret given options *)

processopts[opts_List, optspec_List] := processopt[opts] /@ optspec;

(* matchspec[{"long", "l"}]["l" -> 42] mathes *)
matchspec[opt_][opt_ -> _] := True;
matchspec[{___, opt_, ___}][opt_ -> _] := True;
matchspec[_][opt_ -> _] := False;

(* use first specified option as a key *)
optkey[{key_, ___}] := key;
optkey[key_] := key;

processopt[opts_List][spec: (opt_ -> thespec_)] :=
  With[{
      thisopts = Select[opts, matchspec[opt]],
      key = optkey[opt]
    },
    key -> resolveopt[Last /@ thisopts, thespec, key]
  ];



(* Interpret eack type of option *)

GetOpts::mandatory = "Option `1` is mandatory";

(* boolean *)
resolveopt[values_List, default: (False | True), _] := Last[Prepend[values, default]];

(* flag *)
resolveopt[{}, GOFlag, _] = False;
resolveopt[{__}, GOFlag, _] = True;

(* counter *)
resolveopt[values_List, GOCount, _] := Length[values];

(* mandatory with value *)
resolveopt[{}, GOMandatory, key_] := fail[GetOpts::mandatory, key];
resolveopt[values: {__}, GOMandatory, key_] := resolveopt[values, None, key];

(* optional with value *)
resolveopt[values_List, default_, _] := Last[Prepend[values, default]];
resolveopt[{}, func_ -> default_, _] := default;
resolveopt[values: {__}, func_ -> default_, _] := func[Last[values]];



(* Remove dashes *)

undash[spec: ((key: _String | {__String}) -> val_)] := undashKey[key] -> val;
undashKey[spec_String] := StringReplace[spec, StartOfString ~~ ("--"|"+-"|"++"|"-"|"+") -> ""];
undashKey[spec: {__String}] := undashKey /@ spec;



(* Interface *)

(* <| association |> mey be used *)
GetOpts[argv: {___String}, spec_Association, opts___] := GetOpts[argv, Normal[spec], opts];

GetOpts[argv: {___String}, spec: {((_String | {__String}) -> _)...}, OptionsPattern[]] :=
  getargs[argv, undash /@ spec, Orderless -> OptionValue[Orderless]]



(* Check erroneous usage *)

(* wrong option value *)
GetOpts[argv : {___String}, spec: {((_String | {__String}) -> _) ...}, Orderless -> v: Except[False|True]] /;
  Message[GetOpts::opttf, Orderless, v] = $Failed;

(* wrong argv *)
e: GetOpts[NOTargv_, spec_, OptionsPattern[]] /;
  Message[GetOpts::strlist, HoldForm[e], 1] = $Failed;

(* wrong spec *)
GetOpts::strlistrules = "Keys in `1` must be strings or non-empty list of strings.";
GetOpts[argv: {___String}, NOTspec : {(_ -> _) ...}, OptionsPattern[]] /;
  Message[GetOpts::strlistrules, NOTspec] = $Failed;

(* completely wrong spec (not even association-like) *)
GetOpts[argv : {___String}, NOTspec : Except[{(_ -> _) ...}], OptionsPattern[]] /;
  Message[GetOpts::invru, NOTspec] = $Failed;

(* wrong number of arguments *)
GetOpts[] /; Message[GetOpts::argrx, GetOpts, 0, 2] = $Failed;
GetOpts[_] /; Message[GetOpts::argr, GetOpts, 2] = $Failed;
GetOpts[_, _, rest__] /;
  Message[GetOpts::argrx, GetOpts, 3 + Length[{rest}], 2] = $Failed;


End[];

Protect /@ Names["GetOpts`*"];
Protect /@ Names["GetOpts`Private`*"];

EndPackage[];
