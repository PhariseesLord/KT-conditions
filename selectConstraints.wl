(* ::Package:: *)

(*\:521d\:59cb\:5316*)
(*\:8fd9\:4e2a\:6570\:7ec4\:4e2d0\:8868\:793a\:7684\:662fx=0\:ff0c1\:8868\:793ax>0*)
eqIndex=Flatten[Position[arr,0]];
(*\:521d\:59cb\:5316\:7528\:4e8e\:50a8\:5b58\:7b49\:5f0f\:3001\:4e0d\:7b49\:5f0f\:7ea6\:675f\:7684\:6570\:7ec4*)
eq=ConstantArray[0,Length[allVars]];
ineq=ConstantArray[0,Length[allVars]];


(*\:7b5b\:9009\:51fa\:5bf9\:5e94\:7684\:7b49\:5f0f\:4e0e\:4e0d\:7b49\:5f0f\:7ea6\:675f*)
Do[
(*\:5982\:679c\:7b2ci\:4e2a\:53d8\:91cf=0*)
If[MemberQ[eqIndex,i],

(eq[[i]]=allVars[[i]]==0;(*\:5219\:4ed6\:80af\:5b9a\:5c5e\:4e8e\:7b49\:5f0f\:7ea6\:675f*)

If[i<=Length[vars],(*\:5982\:679c\:8fd9\:4e2a\:53d8\:91cf\:662f\:4e00\:4e2ax\:53d8\:91cf*)
ineq[[i]]=constraints[[i]]<=0,(*\:6839\:636e\:677e\:5f1b\:4e92\:8865\:ff0c\:5bf9\:5e94\:7684\:5bfc\:6570<=0*)
ineq[[i]]=constraints[[i]]>=0](*\:5982\:679c\:8fd9\:4e2a\:53d8\:91cf\:662f\:4e00\:4e2a\[Lambda]\:53d8\:91cf\:ff0c\:6839\:636e\:677e\:5f1b\:4e92\:8865\:ff0c\:5bf9\:5e94\:5bfc\:6570>=0*)
;),

(*\:5982\:679c\:7b2ci\:4e2a\:53d8\:91cf>0*)
(eq[[i]]=constraints[[i]]==0;(*\:6839\:636e\:677e\:5f1b\:4e92\:8865\:ff0c\:5bf9\:5e94\:7684\:5bfc\:6570=0*)
ineq[[i]]=allVars[[i]]>=0;)(*\:5219\:4ed6\:80af\:5b9a\:5c5e\:4e8e\:4e0d\:7b49\:5f0f\:7ea6\:675f*)
],
{i,1,Length[allVars]}]
