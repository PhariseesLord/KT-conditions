(* ::Package:: *)

(* ::Section:: *)
(*\:521d\:59cb\:5316*)


(* ::Subsection:: *)
(*\:53d8\:91cf\:521d\:59cb\:5316*)


Clear[allVarswithValues,resultT,resultLambda,maximalNumber]

n=5;(*DTi\:7684\:6570\:91cf*)
extra=2;(*\:989d\:5916\:53d8\:91cf,\:5982t1,Dt2\:7684\:6570\:91cf*)
m=3;(*lambda\:7684\:6570\:91cf*)
resultObj=0;(*\:6700\:7ec8\:7684\:76ee\:6807\:51fd\:6570\:503c*)

(*\:521d\:59cb\:731c\:6d4b\:503c*)
initialGuessT={6,6,6,6,5,5,5};
initialGuessLambda={1,400,400};
(*\:521d\:59cb\:5316\:53d8\:91cf*)
vars=Array[t,n+extra];(*\:7528\:4e8e\:50a8\:5b58DTi,t1,Dt2*)
varLambda=Array[lambda,m];(*\:7528\:4e8e\:50a8\:5b58lambda1\:ff0clambda2\:ff0clambda3*)
allVars=Join[vars,varLambda];(*\:7528\:4e8e\:50a8\:5b58\:6240\:6709\:7684\:53d8\:91cf*)

(* \:83b7\:53d6\:5f53\:524d notebook \:7684\:76ee\:5f55 *)
currentDir = NotebookDirectory[];


(* ::Subsection::Closed:: *)
(*\:51fd\:6570\:521d\:59cb\:5316*)


V[t_]:=-6.333806818182641` +89.16295770202204` t+-65.5877130681829` t^2+18.642913510101252` t^3+-1.4757339015151698` t^4;
g[t_] :=2*(89.16295770202204` -131.1754261363658` t+55.928740530303756` t^2-5.902935606060679` t^3);
s0=2000;
r=0.08;
p=40;
c1=30;
c2=4;
w=30;
v=40;
alpha=0.75;
e=20;
rho=0.5;

(*some pre-specified functions*)
intg[t0_,t1_] :=Integrate[g[t],{t,t0,t1}];
intVPrime[x_]:=Integrate[V'[t]*Exp[-r*t],{t,0,x}];
intV[x_]=Integrate[V[t]*Exp[-r*t],{t,0,x}];
intgPrime[t1_,t2_]=Integrate[g'[t]*Exp[-r*t],{t,t1,t2}];
intgminusc1[t1_,t2_]=Integrate[(g[t]-c1)*Exp[-r*t],{t,t1,t2}];

tF=T /. FindRoot[{(1-Exp[-r*T])*(p*(V'[T]-r*V[T])-v*(g[T]-c1))-r*(p*V[T]*Exp[-r*T]-v*intgminusc1[0,T])==0},{T,3}];
jF[T_] := (p*V[T]*Exp[-r*T]-v*intgminusc1[0,T])/(1-Exp[-r*T]);
piF=jF[tF];
Plot[jF[t],{t,0,10}]


(* ::Section::Closed:: *)
(*\:5b9a\:4e49\:7ea6\:675f\:8868\:8fbe\:5f0f*)


(*L\:5bf9DTi\:6c42\:5bfc\:ff0ci\[LessEqual]N-1\:7684\:60c5\:51b5*)
LTi=Table[
p*Exp[-r*Sum[t[j],{j,1,i}]]*(V'[t[i]]-r*Sum[Exp[-r*Sum[t[l],{l,i+1,j}]]*V[t[j]],{j,i,n}])-r*Exp[-r*Sum[t[j],{j,1,n}]]*piF+v*r*Exp[-r*Sum[t[j],{j,1,n-1}]]*intgminusc1[t[n+2],t[n]]-lambda[1]*(g[t[i]]-c1)+lambda[3],
{i,1,n-1}];
(*L\:5bf9DTN\:6c42\:5bfc*)
LTn=p*Exp[-r*Sum[t[i],{i,1,n}]]*(V'[t[n]]-r*V[t[n]])-r*Exp[-r*Sum[t[i],{i,1,n}]]*piF-v*Exp[-r*Sum[t[i],{i,1,n}]]*(g[t[n]]-c1)+lambda[2]+lambda[3];
(*L\:5bf9t1\:6c42\:5bfc*)
Lt1=w*c2*Exp[-r*t[n+1]]-lambda[1]*c1-lambda[3];
(*L\:5bf9Dt2\:6c42\:5bfc*)
LDt2=v*Exp[-r*(Sum[t[i],{i,1,n-1}]+t[n+2])]*(g[t[n+2]]-c1)-lambda[1]*(g[t[n+2]]-c1)-lambda[2];
(*\:517b\:5206\:7ea6\:675f*)
ST=s0+c1*(Sum[t[i],{i,1,n-1}]+t[n+2]-t[n+1])-Sum[intg[0,t[i]],{i,1,n-1}]-intg[0,t[n+2]];
(*L\:5bf9t1\:6c42\:5bfc*)
constraintt1=Sum[t[j],{j,1,n}]-t[n+1];
(*L\:5bf9Dt2\:6c42\:5bfc*)
constraintDt2=t[n]-t[n+2];

(*\:5c06\:6240\:6709\:7ea6\:675f\:653e\:5230\:4e00\:8d77*)
constraints=Flatten[{LTi,LTn,Lt1,LDt2,ST,constraintt1,constraintDt2,allVars}];


(* ::Section::Closed:: *)
(*\:904d\:5386\:6240\:6709\:53ef\:80fd\:7684\:60c5\:51b5*)


list=Tuples[{0,1},n+extra+m];

Do[
(arr=list[[k]];
(*\:7b5b\:9009\:51fa\:5bf9\:5e94\:7684\:7b49\:5f0f\:4e0e\:4e0d\:7b49\:5f0f\:7ea6\:675f*)
Get[FileNameJoin[{currentDir, "selectConstraints.wl"}]];

(*\:6c42\:89e3*)
Get[FileNameJoin[{currentDir, "solve.wl"}]];

(*\:68c0\:6d4b\:662f\:5426\:901a\:8fc7\:5168\:90e8\:4e0d\:7b49\:5f0f\:68c0\:9a8c*)
If[(*\:5c06check\:4e2d\:7684\:6240\:6709\:5143\:7d20\:62c9\:5e73\:5230\:4e00\:4e2a\:5217\:8868\:91cc\:ff0c\:7136\:540e\:5bf9\:5176\:4e2d\:6240\:6709\:5143\:7d20\:53d6\:5e76\:8fd0\:7b97\:ff0c\:53ea\:6709\:5f53check\:4e2d\:6240\:6709\:5143\:7d20\:90fd\:4e3atrue\:7684\:65f6\:5019\:ff0c\:7ed3\:679c\:624d\:4e3atrue\:ff0c\:5373\:901a\:8fc7\:68c0\:6d4b*)
And[And @@ check,Not[TrueQ[result=="error"]]] ,


(*\:5982\:679c\:901a\:8fc7\:4e86\:6240\:6709\:7684\:4e0d\:7b49\:5f0f\:68c0\:9a8c\:ff0c\:5219\:5f97\:5230\:6700\:7ec8\:7ed3\:679c\:5e76\:8ba1\:7b97\:76ee\:6807\:51fd\:6570*)
(Get[FileNameJoin[{currentDir, "getResult.wl"}]]

(*\:5224\:65ad\:662f\:5426\:4ea7\:751f\:4e86\:66f4\:9ad8\:7684\:76ee\:6807\:503c\:ff0c\:5982\:679c\:662f\:ff0c\:90a3\:4e48\:5c06\:8ba1\:7b97\:7ed3\:679c\:50a8\:5b58\:8d77\:6765*)
If[obj>resultObj,
(resultT=TwithValues;resultLambda=varLambdawithValues;resultObj=obj;
maximalNumber=k;(*\:7528\:4e8e\:68c0\:6d4b\:5728\:7b2c\:51e0\:6b21\:8fbe\:5230\:4e86\:6700\:5927\:76ee\:6807\:503c*)
),
Null];

(*\:68c0\:67e5\:662f\:5426\:6240\:6709\:7684\:60c5\:51b5\:4e0b\:90fd\:65e0\:89e3\:ff0ccaseCheck\:9ed8\:8ba4\:4e3aFalse\:ff0c\:4f46\:53ea\:8981\:6709\:4e00\:79cd\:60c5\:51b5\:4e0b\:901a\:8fc7\:4fbf\:4f1a\:88ab\:8c03\:6574\:4e3aTrue*)
caseCheck=True;),

Null];),

{k,1,Length[list]}]


(* ::Section::Closed:: *)
(*\:5448\:73b0\:7ed3\:679c*)


resultT
resultLambda
resultObj
maximalNumber
list[[maximalNumber]]




