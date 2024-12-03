(* ::Package:: *)

(*\:6e05\:7a7a\:4e0d\:7b49\:5f0f\:7ea6\:675f\:7684\:68c0\:67e5\:7ed3\:679c*)
check={};

(*\:5e26\:5165\:521d\:59cb\:731c\:6d4b\:503c*)
guess=Join[Table[{t[i],initialGuessT[[i]]},{i,1,Length[vars]}],Table[{lambda[i],initialGuessLambda[[i]]},{i,1,Length[varLambda]}]];
(*\:6c42\:89e3*)
result=Check[FindRoot[eq,guess],"error"];

If[Not[TrueQ[result=="error"]],

(*\:53d7\:5230\:6c42\:89e3\:7684\:6d6e\:52a8\:8bef\:5dee\:5f71\:54cd\:ff0c\:9700\:8981\:5c06\:975e\:5e38\:63a5\:8fd10\:7684\:7ed3\:679c\:8fd1\:4f3c\:4e3a0*)
(threshold=10^-10;
allVarswithValues=result/. x_Real:>If[Abs[x]<threshold,0,x];
(*\:68c0\:67e5\:4e0d\:7b49\:5f0f\:7ea6\:675f\:662f\:5426\:6ee1\:8db3*)
check=ineq/.allVarswithValues;),
Null];



