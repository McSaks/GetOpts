(* ::Package:: *)

(* ::Program:: *)
(*
 spec = {
   {"-mand", "m"} -> GOMandatory,
   {"-opt", "o"} -> "some default value",
   {"-count", "c", "n"} -> GOCount,
   "-flag" -> GOFlag,
   "-switch" -> False
 };
 argv = {
   "one",  "two",
   "--mand=0",
   "--count",  "-c",  "-n",
   "++switch",
   "three",
   "-o", "val",
   "-m:1",
   "--", (* no more arguments interpreted as keyword options *)
   "--some--numbered--option--"
 };
 GetOpts[argv, spec] -> {
   {"one", "two", "three", "--some--numbered--option--"},
   {"-mand" -> "1",
    "-opt" -> "val",
    "-count" -> 3,
    "-flag" -> False,
    "switch" -> True}
 }
*)


BeginPackage["GetOpts`"];


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
  {\\\"-mand\\\", \\\"m\\\"} \[Rule] GOMandatory,\\n
  {\\\"-opt\\\", \\\"o\\\"} \[Rule] \\\"some default value\\\",\\n
  {\\\"-count\\\", \\\"c\\\", \\\"n\\\"} \[Rule] GOCount,\\n
  \\\"-flag\\\" \[Rule] GOFlag,\\n
  \\\"-switch\\\" \[Rule] False\\n
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
  {\\\"-mand\\\" \[Rule] \\\"1\\\",\\n
   \\\"-opt\\\" \[Rule] \\\"val\\\",\\n
   \\\"-count\\\" \[Rule] 3,\\n
   \\\"-flag\\\" \[Rule] False,\\n
   \\\"switch\\\" \[Rule] True}\\n
}\",\"MR\", ShowStringCharacters->True, AutoSpacing->False]\)";
GOMandatory::usage = "";
GOFlag::usage = "";
GOCount::usage = "";


GetOpts::unknownopt="Unknown option `1`.";
GetOpts::mandatory="Option -`1` is mandatory.";
GetOpts::novalue="Value for option -`1` is missing.";
GetOpts::nonswitch="Option -`1` is not a switch and cannot be used like +`1`.";


Begin["`Private`"];


(* ::OldCode:: *)
(*GetOpts[opts_List, spec: {_Rule...}] := Catch[ Module[*)
(*	{*)
(*		iarg = 1,*)
(*		nargs = Length @ opts,*)
(*		sign,*)
(*		opt,*)
(*		keys = First /@ spec,*)
(*		key, val,*)
(*		result = {}, numbered = {},*)
(*		thespec, keyspec,*)
(*		equalsgn = "="|":", hasequalsgn,*)
(*		onlynumbered = False*)
(*	},*)
(*	While[ iarg <= nargs,*)
(*		opt = opts[[iarg]];*)
(*		If[opt === "--", onlynumbered = True; ++iarg; Continue[];];*)
(*		sign = StringTake[opt, 1];*)
(*		If[ (sign === "-" || sign === "+") && !onlynumbered,*)
(*			key = StringDrop[opt, 1];*)
(*			hasequalsgn = !StringFreeQ[key, equalsgn];*)
(*			If[hasequalsgn,*)
(*				key = StringSplit[key, equalsgn, 2];*)
(*				val = key[[2]];*)
(*				key = key[[1]]; ];*)
(*			keyspec = KeySpec[key, spec];*)
(*			If[ keyspec === $Failed,*)
(*				Message[GetOpts::unknownopt, opt];*)
(*				Throw[$Failed, GetOpts] ];*)
(*			{key, thespec} = List @@ keyspec;*)
(*			If[ sign === "+" && thespec =!= True && thespec =!= False,*)
(*				Message[GetOpts::nonswitch, key];*)
(*				Throw[$Failed, GetOpts]; ];*)
(*			Switch[ thespec,*)
(*				GOFlag, (* -key -> True, missing -> False *)*)
(*					PrependTo[result, key -> True],*)
(*				True | False, (* +key -> True, -key -> False, missing -> default *)*)
(*					PrependTo[result, key -> (sign === "+")],*)
(*				GOCount,*)
(*					If[ MemberQ[result, key -> _],*)
(*						result = Replace[result,*)
(*							(key -> v_) :> (key -> v + 1),*)
(*							{1}]*)
(*					(* Else *),*)
(*						 PrependTo[result, key -> 1]; ],*)
(*				GOMandatory | _, (* -key val -> val, missing -> default *)*)
(*					If[!hasequalsgn && iarg == nargs,*)
(*						Message[GetOpts::novalue, key];*)
(*						Throw[$Failed, GetOpts]; ];*)
(*					If[!hasequalsgn, val = opts[[++iarg]];];*)
(*					PrependTo[result, key -> val]*)
(*			]; (* end switch *)*)
(*		(* Else, not [-+] *),*)
(*			AppendTo[numbered, opt];*)
(*			(*Message[GetOpts::unknownopt, opt];*)
(*			Throw[$Failed, GetOpts];*)*)
(*		];*)
(*		++iarg;*)
(*	]; (* end while *)*)
(*	{numbered, Replace[#,*)
(*		(k_ -> _) :>*)
(*		(FirstKey @ k ->*)
(*			If[ MemberQ[FirstKey /@ First /@ result, FirstKey @ k],*)
(*				Replace[FirstKey @ k, result],*)
(*			(* Else *)*)
(*				thespec = Replace[k, spec];*)
(*				Switch[ thespec,*)
(*					GOFlag,*)
(*						False,*)
(*					GOMandatory,*)
(*						Message[GetOpts::mandatory, k];*)
(*						Throw[$Failed, GetOpts],*)
(*					GOCount,*)
(*						0,*)
(*					_,*)
(*						thespec*)
(*				]*)
(*			]*)
(*		)*)
(*	]& /@ spec}*)
(*], GetOpts];*)

Options[GetOpts] = {Orderless -> False};
GetOpts[optsOrig_List, spec_Association, rest___] := GetOpts[optsOrig, Normal[spec], rest];
GetOpts[optsOrig_List, spec: {_Rule...}, OptionsPattern[]] := Catch[ Module[
	{
		opts = optsOrig,
		iarg = 1,
		nargs,
		sign,
		opt,
		keys = First /@ spec,
		key, val,
		result = {}, numbered = {},
		thespec, keyspec,
		pmsgn = "-"|"+",
		equalsgn = "="|":", hasequalsgn,
		onlynumbered = False,
		oneletter,
    orderless = OptionValue[Orderless]
	},
	nargs := Length @ opts;
	oneletter = Flatten @ Cases[spec,
		(k : _List | (ks_ /; StringLength@ks == 1) ->
		 GOFlag | GOCount | True | False) :>
			If[ Head @ k === List,
				Select[k, StringLength@# == 1 &], k] ];
	While[ iarg <= nargs,
		opt = opts[[iarg]];
		If[opt === "--", onlynumbered = True; ++iarg; Continue[];];
		sign = StringTake[opt, 1];
		If[ MatchQ[sign, pmsgn] && !onlynumbered,
			key = StringDrop[opt, 1];
			If[ StringLength@key > 1 && !MatchQ[key, pmsgn],
				If[ StringMatchQ[key, oneletter..],
					opts = Flatten @ Insert[opts,
						sign <> # & /@ Characters@key,
						iarg + 1];
					++iarg
					Continue[];
				(* Else *),
					Null; ];
			];
			hasequalsgn = !StringFreeQ[key, equalsgn];
			If[ hasequalsgn,
				key = StringSplit[key, equalsgn, 2];
				val = key[[2]];
				key = key[[1]]; ];
			keyspec = KeySpec[key, spec];
			If[ keyspec === $Failed,
				Message[GetOpts::unknownopt, opt];
				Throw[$Failed, GetOpts]; ];
			{key, thespec} = List @@ keyspec;
			If[ sign === "+" && thespec =!= True && thespec =!= False,
				Message[GetOpts::nonswitch, MinusToPlus@key];
				Throw[$Failed, GetOpts]; ];
			Switch[ thespec,
				GOFlag, (* -key -> True, missing -> False *)
					PrependTo[result, key -> True],
				True | False, (* +key -> True, -key -> False, missing -> default *)
					PrependTo[result, key -> (sign === "+")],
				GOCount,
					If[ MemberQ[result, key -> _],
						result = Replace[result,
							(key -> v_) :> (key -> v + 1),
							{1}]
					(* Else *),
						 PrependTo[result, key -> 1]; ],
				GOMandatory | _, (* -key val -> val, missing -> default *)
					If[ !hasequalsgn && iarg == nargs,
						Message[GetOpts::novalue, key];
						Throw[$Failed, GetOpts]; ];
					If[ !hasequalsgn,
						val = opts[[++iarg]]; ];
					PrependTo[result, key -> val]
			]; (* end switch *)
		(* Else, not [-+] *),
			AppendTo[numbered, opt];
      onlynumbered = onlynumbered || !orderless;
			(*Message[GetOpts::unknownopt, opt];
			Throw[$Failed, GetOpts];*)
		];
		++iarg;
	]; (* end while *)
	{numbered, Replace[#,
		(k_ -> _) :>
		(FirstKey @ k ->
			If[ MemberQ[FirstKey /@ First /@ result, FirstKey @ k],
				Replace[FirstKey @ k, result],
			(* Else *)
				thespec = Replace[k, spec];
				Switch[ thespec,
					GOFlag,
						False,
					GOMandatory,
						Message[GetOpts::mandatory, First @ k];
						Throw[$Failed, GetOpts],
					GOCount,
						0,
					_,
						thespec
				] (* end switch *)
			] (* end if *)
		)
	]& /@ spec}
], GetOpts];


FirstOrFail @ l_List := First @ l;
FirstOrFail @ {} = $Failed;
FirstKey[s_String] := s;
FirstKey[s_List] := First @ s;
FirstKey[{}] = $Failed;
PlusToMinus[s_String] :=
	Module[{p = StringPosition[s, Except["+"|"-"], 1]},
		If[p =!= {}, p = p[[1,1]] - 1;
			StringJoin[ConstantArray["-", p], StringDrop[s, p]],
			StringJoin[ConstantArray["-", StringLength@s]] ] ];
PlusToMinus[s_List] := PlusToMinus /@ s;
MinusToPlus[s_String] :=
	Module[{p = StringPosition[s, Except["+"|"-"], 1]},
		If[p =!= {}, p = p[[1,1]] - 1;
			StringJoin[ConstantArray["+", p], StringDrop[s, p]],
			StringJoin[ConstantArray["+", StringLength@s]] ] ];
EachKeySpec[k_, s_String -> sp_] /; PlusToMinus@k === PlusToMinus@s  :=  k -> sp;
EachKeySpec[k_, s_List -> sp_] /; MemberQ[PlusToMinus@s, PlusToMinus@k]  :=  First[s] -> sp;
EachKeySpec[k_, s_ -> sp_] = False;
KeySpec[k_, s_] := FirstOrFail @ Cases[EachKeySpec[k, #]& /@ s, Except@False, 1];


End[];


EndPackage[];
