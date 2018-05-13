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

DrawDis::usage = "ShowLine[] Mostra una retta, con la possibilit\[AGrave] di modificare i parametri della forma implicita";
GraphicalMethodExample::usage = "Mostra un grafico per una disequazione di secondo grado dove i parametri possono essere modificati";
MakePolynomial::usage = "Usata per generare un polinomio random di grado n, con x come simbolo";
MakeInequation::usage = "Usata per generare una disequazione polinomiale di grado random (da 1 a 6)";
MakeSystem::usage = "Usata per generare un sistema di disequazioni. numg \[EGrave] una variabile opzionale per settare il numero di disequazioni";
SystemQuiz::usage = "Usata per generare graficamente un quiz contenente sistemi di disequazioni";
InequationQuiz::usage = "Usata per generare graficamente un quiz basato su una disequazione";

(* servono ad evitare problemi nella print delle equazioni *)
x::usage = "";
y::usage = "";

Begin["`Private`"]; (* Comincia spazio privato *)

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

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
   
   
DrawTwoEquationsSystem[] := (
   Manipulate[
    
    With[{p = 
       v x^8 + u x^7 + s x^6 + a x^5 + b x^4 + c x^3 + d x^2 + e x + 
        f, z = w x^8 + m x^7 + n x^6 + o x^5 + i x^4 + r x^3 + 
        h x^2 + t x + j},
     	TwoEquationsSystem[dis1, dis2, v, u, s, a, b, c, d, e, f, w, m, 
      n, o, i, r, h, t, j]],
    		Style["Prima equazione", Bold, 14],
    		{dis1, {">", "<", ">=0", "<=0"}},
    		{{v, 0, "Coefficiente x^8"}, InputField, ImageSize -> Small},
    		{{u, 0, "Coefficiente x^7"}, InputField, ImageSize -> Small},
    		{{s, 0, "Coefficiente x^6"}, InputField, ImageSize -> Small},
    		{{a, 0, "Coefficiente x^5"}, InputField, ImageSize -> Small},
    		{{b, 0, "Coefficiente x^4"}, InputField, ImageSize -> Small},
    		{{c, 4, "Coefficiente x^3"}, InputField, ImageSize -> Small},
    		{{d, 0, "Coefficiente x^2"}, InputField, ImageSize -> Small},
    		{{e, 0, "Coefficiente x"}, InputField, ImageSize -> Small},
    		{{f, 0, "Termine noto"}, InputField, ImageSize -> Small},
    		Style["Seconda equazione", Bold, 14],
    		{dis2 , {">", "<", ">=0", "<=0"}},
    		{{w, 0, "Coefficiente x^8"}, InputField, ImageSize -> Small},
    		{{m, 0, "Coefficiente x^7"}, InputField, ImageSize -> Small},
    		{{n, 0, "Coefficiente x^6"}, InputField, ImageSize -> Small},
    		{{o, 0, "Coefficiente x^5"}, InputField, ImageSize -> Small},
    		{{i, 1, "Coefficiente x^4"}, InputField, ImageSize -> Small},
    		{{r, 0, "Coefficiente x^3"}, InputField, ImageSize -> Small},
    		{{h, -9, "Coefficiente x^2"}, InputField, ImageSize -> Small},
    		{{t, 0, "Coefficiente x"}, InputField, ImageSize -> Small},
    		{{j, 0, "Termine noto"}, InputField, ImageSize -> Small}]);
    		
TwoEquationsSystem[dis1_, dis2_ , v_, u_, s_, a_, b_, c_, d_, e_, f_, 
   w_, m_, n_, o_, i_, r_, h_, t_, j_] := (
   If[dis1 == ">", 
    p = v x^8 + u x^7 + s x^6 + a x^5 + b x^4 + c x^3 + d x^2 + e x + 
        f > 0;];
   If[dis1 == "<", 
    p = v x^8 + u x^7 + s x^6 + a x^5 + b x^4 + c x^3 + d x^2 + e x + 
        f < 0;];
   If[dis1 == ">=0", 
    p = v x^8 + u x^7 + s x^6 + a x^5 + b x^4 + c x^3 + d x^2 + e x + 
        f >= 0;];
   If[dis1 == "<=0", 
    p = v x^8 + u x^7 + s x^6 + a x^5 + b x^4 + c x^3 + d x^2 + e x + 
        f <= 0;];
   If[dis2 == ">", 
    z = w x^8 + m x^7 + n x^6 + o x^5 + i x^4 + r x^3 + h x^2 + t x + 
        j > 0;];
   If[dis2 == ">=0", 
    z = w x^8 + m x^7 + n x^6 + o x^5 + i x^4 + r x^3 + h x^2 + t x + 
        j >= 0;];
   If[dis2 == "<", 
    z = w x^8 + m x^7 + n x^6 + o x^5 + i x^4 + r x^3 + h x^2 + t x + 
        j < 0;];
   If[dis2 == "<=0", 
    z = w x^8 + m x^7 + n x^6 + o x^5 + i x^4 + r x^3 + h x^2 + t x + 
        j <= 0;];
   Show[
    		Plot[{v x^8 + u x^7 + s x^6 + a x^5 + b x^4 + c x^3 + d x^2 + 
       e x + f, 
      w x^8 + m x^7 + n x^6 + o x^5 + i x^4 + r x^3 + h x^2 + t x + 
       j}, {x, -20, 20}, ImageSize -> Large, 
     PlotRange -> {{-20, 20}, {-20, 20}}, 
     PlotLegends -> Placed[{p , z}, Above]], 
    		RegionPlot[p, {x, -20, 20}, {y, -20, 20}, ImageSize -> Large],
    		RegionPlot[z, {x, -20, 20}, {y, -20, 20}, ImageSize -> Large, 
     PlotStyle -> {Orange, Opacity[0.2]}]
    	]
   );
   
realFactorization[poly_, x_] := 
 Module[{n, nreal}, n = Exponent[poly, x];
  nreal = CountRoots[poly, x];
  Times @@ 
   Join[Table[(x - ToRadicals@Root[poly & /. x -> #, i]), {i, nreal}],
     Table[With[{r1 = ToRadicals@Root[poly & /. x -> #, i], 
       r2 = ToRadicals@Root[poly & /. x -> #, i + 1]}, (x^2 - 
        Expand[(r1 + r2)] x + Expand[r1 r2])], {i, nreal + 1, n, 2}]]]
	
polyFact[] := 
  Manipulate[
   Pane[Text[
      Style[HoldForm[#] == Factor[#, Extension -> Sqrt[2]] &[p], 
       14]], {500, 80}, ImageMargins -> 20, 
     Alignment -> {Left, Center}] // TraditionalForm, 
   Style["Fattorizzatore equazioni di grado n", Bold, 
    18], {{p, x^4 - 2, "Inserisci l'equazione da fattorizzare:"}}];
    
(* Esercizi *)

(*Equations: usata per mostrare a schermo un sistema con la parentesi graffa*)
Equations /: MakeBoxes[Equations[eqs_], TraditionalForm] := 
  RowBox[{"\[Piecewise]", 
      GridBox[{MakeBoxes[#, TraditionalForm]} & /@ {##} & @@ eqs]}]

(* MakePolynomial: usata per generare un polinomio random di grado n, con x come simbolo *)

MakePolynomial[n_Integer, x_Symbol] :=
  Module[
    {z, c},
    z = RandomChoice[{-1, 1}] RandomInteger[{1, 20}];
    c = Table[RandomInteger[{-10, 10}], {n}];
    FromDigits[
      Reverse[
        AppendTo[c, z]
        ], x
      ]
    ]

(* MakeInequation: usata per generare una disequazione polinomiale di grado random (da 1 a 6) *)
MakeInequation[] := (
    (* Inizializzazione variabili *)
    flagTmp := True;
    eqTmp = MakePolynomial[RandomInteger[{1, 6}], x]; 
  disTmp = eqTmp > 0;
  
    (* Loop finchè non viene generata una disequazione con soluzione esistente in R *)
    While[flagTmp,
      Clear[eqTmp]; Clear[disTmp];
      eqTmp = MakePolynomial[RandomInteger[{1, 6}], x];
      If[ 
    Resolve[Exists[x, eqTmp == 0], 
     Reals],   (*Se ha soluzione in R*)
        
    If[RandomInteger[{1, 2}] == 1, 
          disTmp = eqTmp > 0,
          disTmp = eqTmp < 0
          ];
        flagTmp := False;
        ]
      ];
    Return[List[eqTmp, disTmp]];
    )

(* MakeSystem: usata per generare un sistema di disequazioni. numg è una variabile opzionale per settare il numero di disequazioni *)

MakeSystem[numg_: - 1] := (
    (* Inizializzazione variabili *)
    equs  = {}; 
  disq = {x > -5 , x < 5}; systemOkFlag := True;
    (* Check numero di disequazioni *)
    
  numDis = IntegerPart[numg];
    If[numDis < 1,
      flagRandom := True;,
      flagRandom := False;
      ];
  
    (* Loop finchè non viene generato un sistema di disequazioni con soluzione esistente in R *)
    While[systemOkFlag,
      If[flagRandom,
        numDis = RandomInteger[{2, 5}];
        ];
      Clear[equs]; Clear[disq];
      equs  = {}; disq = {x > -5 , x < 5};
      (* Loop per generare ogni singola disequazione*)
      i = 0;
      While[i < numDis,
        eqDisTmp = MakeInequation[];
        equs = Append[equs, First[eqDisTmp]];
        disq = Append[disq, Last[eqDisTmp]];
        i++;
        ];
      If[ ToString[Head[Reduce[disq, Reals]]] == "Inequality", (* 
    Se sistema ha soluzione esistente in R *)
        
    systemOkFlag := False
        ];
      ];
    Clear[numDis];
    Return[List[equs, disq]]; 
  )

(* SystemQuiz: usata per costruire un intero esercizio sui sistemi *)

SystemQuiz[] := 
    DynamicModule[
      {r1, r2, equations, first, second, third, fourth, risp, 
        rightSystem, SystemExercise}, 
      
      SystemExercise[] := (
          Clear[systemsTmp];
          systemsTmp = List[MakeSystem[]];	(* 
     Genera il sistema considerato nell'esercizio *)
          
     sqmi = 0;
          While[sqmi < 3,
            systemsTmp = Append[systemsTmp,  MakeSystem[1]]; (* 
      Genera altri 3 sistemi per le risposte sbagliate *)
            
      sqmi++;
            ];
          permut = RandomSample[{1, 2, 3, 4}];	(* 
     Permutazione delle risposte *)
          
     tmpList = Last[Extract[systemsTmp, 1]];
          (* Stampa il sistema *)
          
     rightSystem = Take[tmpList, {3, Length[tmpList]}];
          
          (* Genera il quiz graficamente *)
          
     equations = First[Extract[systemsTmp, 1]];
          
     first =  Reduce[Last[Extract[systemsTmp, Extract[permut, 1]]]];
          
     second =  Reduce[Last[Extract[systemsTmp, Extract[permut, 2]]]];
          
     third = Reduce[Last[Extract[systemsTmp, Extract[permut, 3]]]];
          
     fourth = Reduce[Last[Extract[systemsTmp, Extract[permut, 4]]]];
          risp = ToString[First[First[Position[permut, 1]]]];  
          );
      
      SystemExercise[];
      
      Column [{
          Row[{
              Dynamic[
            	(* Mostra i 4 Plot *)
        Grid[{{
                   Show[
                      
                      Plot[equations, {x, -100, 100}, 
                        PlotRange -> {{-3, 3}, {-10, 20}}, 
                        PlotLabel -> 
                          Style[ 
                             StringReplace[
                               
                StringReplace[ToString[N[first]], "-5. < " -> ""], 
                               " < 5." -> ""], 24, Orange] , 
                        ImageSize ->  Large],
                      
                      RegionPlot[first, {x, -5, 5}, {y, -100, 100}, 
                        PlotStyle -> {Green, Opacity[0.25]}, 
                        BoundaryStyle -> None]
                      ],
                    Show[
                      
                      Plot[equations, {x, -100, 100}, 
                        PlotRange -> {{-3, 3}, {-10, 20}}, 
                        PlotLabel -> 
                          Style[
                             StringReplace[
                               
                StringReplace[ToString[N[second]], "-5. < " -> ""], 
                               " < 5." -> ""], 24, Black] , 
                        ImageSize -> Large],
                      
                      RegionPlot[second, {x, -5, 5}, {y, -100, 100}, 
                        PlotStyle -> {Green, Opacity[0.25]}, 
                        BoundaryStyle -> None]
                      ]},
          
                   {Show[
                      
                      Plot[equations, {x, -100, 100}, 
                        PlotRange -> {{-3, 3}, {-10, 20}}, 
                        PlotLabel -> 
                          Style[
                             StringReplace[
                               
                StringReplace[ToString[N[third]], "-5. < " -> ""], 
                               " < 5." -> ""], 24, Green] , 
                        ImageSize -> Large],
                      
                      RegionPlot[third, {x, -5, 5}, {y, -100, 100}, 
                        PlotStyle -> {Green, Opacity[0.25]}, 
                        BoundaryStyle -> None]
                      ],
                    Show[
                      
                      Plot[equations, {x, -100, 100}, 
                        PlotRange -> {{-3, 3}, {-10, 20}}, 
                        PlotLabel -> 
                          Style[
                             StringReplace[
                               
                StringReplace[ToString[N[fourth]], "-5. < " -> ""], 
                               " < 5." -> ""], 24, Red] , 
                        ImageSize -> Large],
                      
                      RegionPlot[fourth, {x, -5, 5}, {y, -100, 100}, 
                        PlotStyle -> {Green, Opacity[0.25]}, 
                        BoundaryStyle -> None]
                      ]}
          }]
                ],
              
              "\t",
              
              Dynamic[
                (* Elenco risposte *)
                elenco = { 
                    Table[RadioButtonBar[Dynamic[r1], {
                          Style["1", 24, Orange],
                          Style["2", 24, Black],
                          Style["3", 24, Green],
                          Style["4", 24, Red]
                          }, Appearance -> a], {a, {"Vertical"}}]
                    };
        elencoDis = {};
        iTmp = 1;
        While[iTmp <= Length[rightSystem], 
         elencoDis = 
          Append[elencoDis, 
           Style[Extract[rightSystem, iTmp], 26, 
            Extract[ColorData[97, "ColorList"], iTmp++]]]
         ];
                
                (* Colonna delle risposte *)
                Column[
                  Flatten[{
                      Style["Sistema:", 24, Black], elencoDis,
                      {Text[
                          
             Style["\nSeleziona la soluzione corretta:", 24, 
                            Black]], 
                        elenco}, 
                      
                      
           Graphics[{ If[r2, Green, Red, Blue], Rectangle[{0, 0}]}, 
                        ImageSize -> {20, 20}], 
                      If[r2, "Giusto!", "Sbagliato!", ""], 
                      Button["Controlla",
                        r2 = (ToString[r1] === risp);
                        If[r2,
                          (* Then *)
                          
             CreateDialog[
                            Column[{
                                Style["  Esatto!  ", 32],
                                DefaultButton[Style["Continua", 20],
                                  Clear[r2];
                                  SystemExercise[];
                                  DialogReturn[];
                                  ],
                                DefaultButton[Style["Esci", 20],
                                  DialogReturn[];
                                  ]
                                }, ItemSize -> 20],
                            
                            Modal -> 
                              
               True,(*indica di aprire il dialog come schermata modale (pop-up)*)
                            
                            
              NotebookEventActions -> {"WindowClose" :> (SystemExercise[])}]
                          ]
                        ]}
                    ]
                  ]
                ]
              
              }]
          }]
      ];

(* InequationQuiz: usata per costruire un intero esercizio su una disequazione *)
InequationQuiz[ ] := 
    DynamicModule[
      {r1, r2, inequation, first, second, third, fourth, risp, 
        InequationExercise}, 
      
      (*Genera la disequazione*)
      InequationExercise[] := (
          Clear[inequationTmp];
          inequationTmp = MakeSystem[1]; (* 
     Genera la disequazione considerata nell'esercizio *)
          
     sols = List[N[Reduce[Last[inequationTmp]], 2]];
          iei = 0;
          While[iei < 3,
            sols = Append[sols,  N[Reduce[Last[MakeSystem[1]]], 2]];	(* 
      Genera le altre risposte *)
            iei++;
            ];
          permut2 = RandomSample[{1, 2, 3, 4}];	(* 
     Permutazione delle risposte *)
          
          inequation = inequationTmp; 
          first = ToString[Extract[sols, Extract[permut2, 1]]]; 
          second = ToString[Extract[sols, Extract[permut2, 2]]]; 
          third =  ToString[Extract[sols, Extract[permut2, 3]]] ;   
          fourth = ToString[Extract[sols, Extract[permut2, 4]]]; 
          risp = ToString[Extract[sols, 1]] ; 
          );
      
      InequationExercise[];
      
      (* Genera il quiz graficamente *)
      Column [{
          Row[{
              Dynamic[
            	(* Mostra il Plot *)
                Show[
                  Plot[First[inequation], {x, -100, 100}, 
                    PlotRange -> {{-3, 3}, {-20, 20}}, 
                    
          PlotLabel -> Style[Last[Last[inequation]], 24, Orange] , 
                    PlotLegends -> Placed["Expressions", Below], 
                    ImageSize -> Large],
                  RegionPlot[
                    
          Reduce[Last[inequation]], {x, -5, 5}, {y, -100, 100}, 
                    PlotStyle -> {Green, Opacity[0.25]}, 
          BoundaryStyle -> None]
                  ]
                ],
              
              "\t",
              
              Dynamic[
           		(* Elenco risposte *)
                elenco = { 
                    Table[
                      RadioButtonBar[Dynamic[r1],
                        {Style[
                            
              StringReplace[StringReplace[first, "-5.0 < " -> ""], 
                              " < 5.0" -> ""], 24, Black],
                          
                          Style[StringReplace[
                              
               StringReplace[second, "-5.0 < " -> ""], 
                              " < 5.0" -> ""], 24, Black],
                          
                          Style[StringReplace[
                              StringReplace[third, "-5.0 < " -> ""], 
               " < 5.0" -> ""],
                             24, Black],
                          
                          Style[StringReplace[
                              
               StringReplace[fourth, "-5.0 < " -> ""], 
                              " < 5.0" -> ""], 24, Black]
                          }, Appearance -> a],
                      {a, {"Vertical"}}
                      ]};
                
            	(* Colonna delle risposte *)
                Column[
                  Flatten[{
                      {Text[
                          
             Style["Seleziona l'intervallo corretto:", 24, Black]], 
                        elenco}, 
                      
                      
           Graphics[{ If[r2, Green, Red, Blue], Rectangle[{0, 0}]}, 
                        ImageSize -> {20, 20}], 
                      If[r2, "Giusto!", "Sbagliato!", ""], 
                      Button["Controlla",
                        
                        r2 = (ToString[r1] === 
                              
               StringReplace[StringReplace[risp, "-5.0 < " -> ""], 
                                " < 5.0" -> ""]);
                        If[r2,
                          (* Then *)
                          
             
             CreateDialog[	(*Finestra di Dialogo*)
                        Column[{
                            Style["  Esatto!  ", 32],
                            DefaultButton[Style["Continua", 20],
                              Clear[r2];
                              InequationExercise[];
                              DialogReturn[];
                              ],
                            DefaultButton[Style["Esci", 20],
                              DialogReturn[];
                              ]
                            }, ItemSize -> 20],
                        
                        Modal -> True,(*indica di aprire il dialog come schermata modale (pop-up)*)
                            
                            
              NotebookEventActions -> {"WindowClose" :> (InequationExercise[])}]
                          ]
                        ]}
                    ]
                  ]
                ]
              
              }]
          }]
      ];
   
End[]; (* Fine spazio privato *)
Protect["Progetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)



