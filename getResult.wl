(* ::Package:: *)

(*\:5206\:522b\:622a\:53d6\:4f18\:5316\:53d8\:91cf\:548c\[Lambda]\:4e58\:5b50\:7684\:5bf9\:5e94\:89c4\:5219*)
varswithValues=Take[allVarswithValues,Length[vars]];
varLambdawithValues=Drop[allVarswithValues,Length[vars]];

(*\:521d\:59cb\:5316\:53d8\:91cf\:ff0c\:7528\:4e8e\:50a8\:5b58Ti,t1,t2*)
varT=Array[T,n+2];(*\:50a8\:5b58\:5f62\:5f0f\:53d8\:91cf*)
TwithValues=ConstantArray[0,Length[vars]];(*\:50a8\:5b58\:5bf9\:5e94\:89c4\:5219*)

(*\:8ba1\:7b97Ti\:5217\:8868*)
expr=Join[Table[Sum[t[j],{j,1,i}],{i,1,n}],{t[n+1]},{t[n+2]+Sum[t[j],{j,1,n-1}]}];(*Ti\:7684\:8868\:8fbe\:5f0f*)
temp=expr/.varswithValues;(*\:5e26\:5165\:8868\:8fbe\:5f0f\:5f97\:5230Ti\:7684\:503c\:5217\:8868*)

(*\:5199\:5165\:5bf9\:5e94\:89c4\:5219*)
Do[TwithValues[[i]]=T[i]->temp[[i]],{i,1,Length[vars]}];

(*\:8ba1\:7b97\:76ee\:6807\:51fd\:6570*)
objExpr=w*c2/r*(1-Exp[-r*T[n+1]])-v*Exp[-r*T[n-1]]*intgminusc1[t[n+2],t[n]]+p*Sum[Exp[-r*T[i]]*V[t[i]],{i,1,n}]+Exp[-r*T[n]]*piF;(*\:76ee\:6807\:51fd\:6570\:7684\:8868\:8fbe\:5f0f*)
obj=objExpr/.Join[varswithValues,TwithValues];(*\:5e26\:5165\:8868\:8fbe\:5f0f\:5f97\:5230\:51fd\:6570\:503c*)
