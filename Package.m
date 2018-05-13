(* ::Package:: *)

(* ::Code::Initialization::Plain:: *)
(* PACKAGE.M
 * Progetto d'esame di Matematica Computazionale + Calcolo Numerico e Software Didattico
 * Corsi di laurea magistrale in Informatica e Matematica
 * Anno accademico 2016/2017
 * 
 * Autori:
 *   Federico Giubaldo, Cristina Stamegna, Domenica Stilo, Mattia Venturini
 *
 * Versione di sviluppo e testing: Wolfram Mathematica 11.1
 *)

BeginPackage[ "Progetto`"];

Unprotect["Progetto`*"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["Progetto`*"];

(* Usage prima del Private *)

(* Fase 1 *)
drawSystem::usage = "disegna un sistema di 2 equazioni (non gestisce casi particolari)";

DrawDis::usage = "ShowLine[] Mostra una retta, con la possibilit\[AGrave] di modificare i parametri della forma implicita";
GraphicalMethodExample::usage = "Mostra un grafico per una disequazione di secondo grado dove i parametri possono essere modificati";

(* servono ad evitare problemi nella print delle equazioni *)
x::usage = "";
y::usage = "";

Begin["`Private`"]; (* Comincia spazio privato *)

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

(* 
 * disegna un sistema di 2 equazioni (non gestisce casi particolari)
 * @param eq1, eq2: equazioni come espressioni booleane, con variabili libere x e y
 *)
drawSystem[eq1_,eq2_]:=
	Module[{sol,intersections},
		sol =Solve[Rationalize[eq1&&eq2],{x,y},Reals]; (* trova le intersezioni delle 2 equazioni *)
		intersections={Red,PointSize[Large],Point[{x,y}/.sol]}; (* crea i punti che rappresentano le intersezioni *)
		Show[ (* disegno il grafico *)
			Plot[y/.Solve[eq1],{x,-20,20},PlotRange->{{-10,10},{-10,10}},ImageSize->Large,AspectRatio->1,PlotStyle->{Purple,Thick}], (* disegno eq1 *)
			Plot[y/.Solve[eq2],{x,-20,20},PlotRange->{{-10,10},{-10,10}},ImageSize->Large, AspectRatio->1,PlotStyle->{Blue,Thick}], (* disegno eq2 *)
			Graphics[{intersections}]  (* disegno le intersezioni *)
		]
	];

GenericQuiz[diseq_, equation_, op1_, op2_, op3_, op4_, risp_]:= 
	DynamicModule[{r1, r2}, 
		Labeled[elenco = {RadioButtonBar[Dynamic[r1],{op1, op2, op3, op4}]};
		Show[
			Plot[equation, {x, -2,5}, PlotRange->{{-2,5},{-10,10}}],
			RegionPlot[{diseq}, {x, -2,5},{y, -10,10}, PlotLegends->LineLegend[{Blue}, {diseq}]]
		], 
			Column[Flatten[{{Text["Seleziona le funzione corretta:"], elenco}, Graphics[{
			Dynamic[If[r2, Green, Red, Blue]], Rectangle[{0,0}]}, ImageSize->{20,20}], 
				Dynamic[If[r2, "Giusto!", "Sbagliato!", ""]], Button["Controlla", r2=(r1===risp)]}]], Right]];

GenericQuiz2[equation_, op1_, op2_, op3_, op4_, risp_, fill_, limit_]:= 
	DynamicModule[{r1,r2}, 
		Labeled[elenco = {RadioButtonBar[Dynamic[r1], {op1, op2, op3, op4}]};
		Plot[equation, {x, limit, 5}, Filling->fill, PlotLegends->LineLegend[{Blue}, {equation}]], 
			Column[Flatten[{{Text["Seleziona le funzione corretta:"], elenco}, Graphics[{
			Dynamic[If[r2, Green, Red, Blue]], Rectangle[{0,0}]}, ImageSize->{20,20}], 
				Dynamic[If[r2, "Giusto!", "Sbagliato!", ""]], Button["Controlla", r2=(r1===risp)]}]], Right]];
				
				
DrawDis[a_, b_, c_ , dis_] := (
   l = a x^2 + b x + c == 0;
   If[ dis == ">",
    lgh = a x^2 + b x + c > 0;
    ];
   If[ dis == "<",
    lgh = a x^2 + b x + c < 0;
    ];
   Show[
    Plot[l, {x, -10, 10}, PlotRange -> Automatic, ImageSize -> Full, 
     Epilog -> {Black, 
       Text[Style[lgh, 14, Bold], Scaled[{.30, .95}]]}    ],
    RegionPlot[lgh, {x, -15, 15}, {y, -10000, 10000}]
    ]
   );
   
GraphicalMethodExample[] := (
   Manipulate[
    DrawDis[a, b, c, dis],
    Style[
     "Metodo Grafico per disequazione di secondo grado ax^2 + bx + c",
      Bold, 24], 
    {a, 0.1, 2}, 
    {b, 4, 20}, 
    {c, -20 , 2},
    Style["Seleziona Maggiore o Minore", Bold, 
     12],  {dis, {">", "<"}}, 
    ContentSize -> {1000, 700},
    Initialization :> (a = 1) 
    ]
   );

(* FASE 2 --------------------------------------------------------------------------------- *)

(* 
 * verifica le condizioni di esistenza per una figura
 * @param figura: stringa che indica il tipo di figura da verificare (Line, Parabole, Ellipse, Circle, Hyperbole o None)
 * @param a,b,c: coefficenti della figura
 * Ritorna al chiamante una stringa con l'errore riscontrato o la stringa "OK" in caso di successo
 *)
checkCondition[figura_,a_,b_,c_]:= (
	If[figura=="None", (* figura non definita: \[EGrave] un errore in quanto vogliamo avere 2 figure nel sistema *)
		Return["Inserire due equazioni"]
	];
	If[a == Null || b==Null || c==Null, (* errore generico: campi dei coefficenti vuoti *)
		Return["Errore: hai lasciato alcuni campi vuoti"]
	];
	If[figura == "Line"&&a==0&&b==0, (* Retta: almeno uno tra a e b deve essere diverso da 0 *)
		Return["Retta: valori dei coefficienti non validi"]
	];
	If[figura == "Parabola"&&a==0, (* Parabola: a \[NotEqual] 0 *)
		Return["Parabola: valore 'a' deve essere diverso da 0, altrimenti ottieni una retta"]
	];
	If[figura == "Ellipse"&&(a<=0||b<=0), (* Ellisse: coefficenti non negativi *)
		Return["Ellisse: i valori dei coefficienti devono essere positivi"]
	];
	If[figura == "Ellipse"&&(a==b), (* Ellisse: coefficenti diversi tra loro (senn\[OGrave] collassa a circonferenza) *)
		Return["Ellisse: devi avere 'a' diverso da 'b', altrimenti ottieni una circonferenza"]
	];
	If[figura == "Circle"&&(a==0&&b==0&&c==0), (* Circonferenza: almeno uno dei coefficenti deve essere non nullo *)
		Return["Circonferenza: valori non ammessi, l'equazione rappresenta le rette bisettrici"]
	];
	If[figura=="Circle" && (-a/2)^2+(-b/2)^2-c<0, (* equazione di esistenza della Circonferenza *)
		Return["Circonferenza: valori non ammessi, bisogna rispettare la condizione descritta nella teoria"]
	];
	If[figura == "Hyperbole"&&(a<=0||b<=0)|| figura == "Ellisse"&&a<=b, (* Iperbole: coefficienti non negativi *)
		Return["Iperbole: i valori dei coefficienti devono essere positivi"]
	];
	Return ["OK"]; (* Nessun errore *)
);

End[]; (* Fine spazio privato *)
Protect["Progetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)
