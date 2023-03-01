(* ::Package:: *)

ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  ChristoffelSymbols[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], False, True, 
    True] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   newCoordinates_List] := ChristoffelSymbols[ResourceFunction["MetricTensor"][
     matrixRepresentation /. Thread[coordinates -> newCoordinates], newCoordinates, index1, index2], False, True, 
    True] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   newCoordinates_List, index1_, index2_, index3_] := 
  ChristoffelSymbols[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2, index3] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["TensorRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True && index3 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*christoffelSymbols[[
                 #1,index[[2]],index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === False && index2 === False && index3 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[index[[1]],#1[[1]],
                  #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === True && index2 === False && index3 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                  christoffelSymbols[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]], 
        If[index1 === False && index2 === True && index3 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*
                   christoffelSymbols[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === False && index2 === False && index3 === True, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                    christoffelSymbols[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === True && index2 === True && index3 === False, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[#1[[1]],index[[2]],
                      #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === True && index2 === False && 
             index3 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                       #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                       index[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]], If[index1 === False && index2 === True && index3 === True, 
             christoffelSymbols, Indeterminate]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["ReducedTensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True && index3 === True, 
      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*
                 christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 3]]]], If[index1 === False && index2 === False && index3 === False, 
       FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],
                   #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[index[[1]],#1[[1]],
                   #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 3]]]], If[index1 === True && index2 === False && index3 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                   Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[3]]]]*christoffelSymbols[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                  Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]], 
        If[index1 === False && index2 === True && index3 === False, FullSimplify[
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*
                    christoffelSymbols[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 3]]]], If[index1 === False && index2 === False && 
           index3 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> 
                 Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*christoffelSymbols[[index[[1]],#1,
                      index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 3]]]], If[index1 === True && index2 === True && index3 === False, 
           FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                       #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[#1[[1]],
                       index[[2]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]]], If[index1 === True && index2 === False && index3 === True, 
            FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                        #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                        index[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 3]]]], If[index1 === False && index2 === True && 
              index3 === True, FullSimplify[christoffelSymbols], Indeterminate]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["SymbolicTensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                  Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                  Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     If[index1 === True && index2 === True && index3 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*christoffelSymbols[[
                 #1,index[[2]],index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === False && index2 === False && index3 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[index[[1]],#1[[1]],
                  #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === True && index2 === False && index3 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                  christoffelSymbols[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]], 
        If[index1 === False && index2 === True && index3 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*
                   christoffelSymbols[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === False && index2 === False && index3 === True, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                    christoffelSymbols[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === True && index2 === True && index3 === False, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[#1[[1]],index[[2]],
                      #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 3]]], If[index1 === True && index2 === False && 
             index3 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                       #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                       index[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]], If[index1 === False && index2 === True && index3 === True, 
             christoffelSymbols, Indeterminate]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["MetricTensor"] := ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
    metricIndex1, metricIndex2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["Coordinates"] := coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["Indices"] := {index1, index2, index3} /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["CovariantQ"] := If[index1 === True && index2 === True && index3 === True, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["ContravariantQ"] := If[index1 === False && index2 === False && index3 === False, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["MixedQ"] := 
  If[ !((index1 === True && index2 === True && index3 === True) || (index1 === False && index2 === False && 
       index3 === False)), True, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index2]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["Symbol"] := If[index1 === True && index2 === True && index3 === True, 
    Subscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && index3 === False, 
     Superscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && index3 === False, 
      Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === True && index3 === False, 
       Subsuperscript["\[FormalCapitalGamma]", "\[FormalMu]", "\[FormalRho]\[FormalNu]"], If[index1 === False && index2 === False && index3 === True, 
        Subsuperscript["\[FormalCapitalGamma]", "\[FormalNu]", "\[FormalRho]\[FormalMu]"], If[index1 === True && index2 === True && index3 === False, 
         Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalMu]", "\[FormalNu]"], If[index1 === True && index2 === False && index3 === True, 
          Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalNu]", "\[FormalMu]"], If[index1 === False && index2 === True && index3 === True, 
           Subsuperscript["\[FormalCapitalGamma]", "\[FormalMu]\[FormalNu]", "\[FormalRho]"], Indeterminate]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["VanishingChristoffelQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, fieldEquations}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; fieldEquations = FullSimplify[Thread[Catenate[Catenate[christoffelSymbols]] == 
         Catenate[Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation], 
             Length[matrixRepresentation]}]]]]]; If[fieldEquations === True, True, 
      If[fieldEquations === False, False, If[Length[Select[fieldEquations, #1 === True & ]] == 
         Length[matrixRepresentation]*Length[matrixRepresentation]*Length[matrixRepresentation], True, False]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["VanishingChristoffelConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, fieldEquations}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; fieldEquations = FullSimplify[Thread[Catenate[Catenate[christoffelSymbols]] == 
         Catenate[Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation], 
             Length[matrixRepresentation]}]]]]]; If[fieldEquations === True, {}, If[fieldEquations === False, 
       Indeterminate, If[Length[Select[fieldEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[fieldEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["IndexContractions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, newChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[(index1 === True && index2 === True && index3 === True) || 
       (index1 === False && index2 === False && index3 === False), Association[], 
      If[index1 === True && index2 === False && index3 === False, 
       newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*
                   Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                    #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> 
          (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalMu]\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,
                 #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
       If[index1 === False && index2 === True && index3 === False, 
        newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                 (Inverse[matrixRepresentation][[#1,index[[3]]]]*christoffelSymbols[[index[[1]],index[[2]],#1]] & ) /@ 
                  Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
         Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> 
           (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[
                  matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
          Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,
                  #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
        If[index1 === False && index2 === False && index3 === True, 
         newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (Inverse[matrixRepresentation][[index[[2]],#1]]*christoffelSymbols[[index[[1]],#1,index[[3]]]] & ) /@ 
                   Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
          Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalMu]"] -> (Module[{index = #1}, Total[
                (newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Range[Length[matrixRepresentation]], Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> 
            (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,#1]] & ) /@ Range[Length[
                   matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
         If[index1 === True && index2 === True && index3 === False, newChristoffelSymbols = 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[#1[[1]],index[[2]],
                       #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalMu]", "\[FormalSigma]"] -> 
             (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[
                    matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
            Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,
                    #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
          If[index1 === True && index2 === False && index3 === True, newChristoffelSymbols = 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                       Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                        index[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> 
              (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[
                     matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
             Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,
                     #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
           If[index1 === False && index2 === True && index3 === True, newChristoffelSymbols = christoffelSymbols; 
             Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[
                   (newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                Range[Length[matrixRepresentation]], Subsuperscript["\[FormalCapitalGamma]", "\[FormalMu]\[FormalSigma]", "\[FormalSigma]"] -> 
               (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[
                      matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], Indeterminate]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["ReducedIndexContractions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, newChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; If[(index1 === True && index2 === True && index3 === True) || 
       (index1 === False && index2 === False && index3 === False), Association[], 
      If[index1 === True && index2 === False && index3 === False, 
       newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*
                   Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                    #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> 
          FullSimplify[(Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ 
                Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
         Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalMu]\[FormalSigma]"] -> FullSimplify[
           (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[
                  matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]], 
       If[index1 === False && index2 === True && index3 === False, 
        newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                 (Inverse[matrixRepresentation][[#1,index[[3]]]]*christoffelSymbols[[index[[1]],index[[2]],#1]] & ) /@ 
                  Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
         Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> FullSimplify[
            (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[
                   matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
          Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, Total[
                (newChristoffelSymbols[[index,#1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Range[Length[matrixRepresentation]]]], If[index1 === False && index2 === False && index3 === True, 
         newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (Inverse[matrixRepresentation][[index[[2]],#1]]*christoffelSymbols[[index[[1]],#1,index[[3]]]] & ) /@ 
                   Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
          Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalMu]"] -> FullSimplify[(Module[{index = #1}, 
                Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
              Range[Length[matrixRepresentation]]], Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> 
            FullSimplify[(Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,#1]] & ) /@ 
                  Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]], 
         If[index1 === True && index2 === True && index3 === False, newChristoffelSymbols = 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[#1[[1]],index[[2]],
                       #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalMu]", "\[FormalSigma]"] -> 
             FullSimplify[(Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ 
                   Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
            Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, 
                 Total[(newChristoffelSymbols[[index,#1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[
                Length[matrixRepresentation]]]], If[index1 === True && index2 === False && index3 === True, 
           newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                    (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*
                       christoffelSymbols[[#1[[1]],#1[[2]],index[[3]]]] & ) /@ Tuples[Range[Length[
                        matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
            Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, 
                  Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                Range[Length[matrixRepresentation]]], Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> 
              FullSimplify[(Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,#1]] & ) /@ 
                    Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]], 
           If[index1 === False && index2 === True && index3 === True, newChristoffelSymbols = christoffelSymbols; 
             Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, 
                   Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                 Range[Length[matrixRepresentation]]], Subsuperscript["\[FormalCapitalGamma]", "\[FormalMu]\[FormalSigma]", "\[FormalSigma]"] -> FullSimplify[
                (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[
                       matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]], Indeterminate]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["SymbolicIndexContractions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, newChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                  Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                  Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     If[(index1 === True && index2 === True && index3 === True) || (index1 === False && index2 === False && 
        index3 === False), Association[], If[index1 === True && index2 === False && index3 === False, 
       newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*
                   Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                    #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> 
          (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalMu]\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,
                 #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
       If[index1 === False && index2 === True && index3 === False, 
        newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                 (Inverse[matrixRepresentation][[#1,index[[3]]]]*christoffelSymbols[[index[[1]],index[[2]],#1]] & ) /@ 
                  Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
         Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> 
           (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[
                  matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
          Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,
                  #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
        If[index1 === False && index2 === False && index3 === True, 
         newChristoffelSymbols = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (Inverse[matrixRepresentation][[index[[2]],#1]]*christoffelSymbols[[index[[1]],#1,index[[3]]]] & ) /@ 
                   Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
          Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalSigma]\[FormalMu]"] -> (Module[{index = #1}, Total[
                (newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Range[Length[matrixRepresentation]], Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> 
            (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,#1]] & ) /@ Range[Length[
                   matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
         If[index1 === True && index2 === True && index3 === False, newChristoffelSymbols = 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[#1[[1]],index[[2]],
                       #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalMu]", "\[FormalSigma]"] -> 
             (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[
                    matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
            Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,
                    #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
          If[index1 === True && index2 === False && index3 === True, newChristoffelSymbols = 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                       Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                        index[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> 
              (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[
                     matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
             Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[(newChristoffelSymbols[[index,#1,
                     #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], 
           If[index1 === False && index2 === True && index3 === True, newChristoffelSymbols = christoffelSymbols; 
             Association[Subsuperscript["\[FormalCapitalGamma]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[
                   (newChristoffelSymbols[[#1,#1,index]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                Range[Length[matrixRepresentation]], Subsuperscript["\[FormalCapitalGamma]", "\[FormalMu]\[FormalSigma]", "\[FormalSigma]"] -> 
               (Module[{index = #1}, Total[(newChristoffelSymbols[[#1,index,#1]] & ) /@ Range[Length[
                      matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]], Indeterminate]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["Dimensions"] := Length[matrixRepresentation] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["Signature"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["RiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], True, False], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["PseudoRiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], False, True], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["LorentzianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["RiemannianConditions"] := Module[{eigenvalues, riemannianConditions}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; 
     If[riemannianConditions === True, {}, If[riemannianConditions === False, Indeterminate, 
       If[Length[Select[riemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["PseudoRiemannianConditions"] := 
  Module[{eigenvalues, pseudoRiemannianConditions}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     pseudoRiemannianConditions = FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["LorentzianConditions"] := 
  Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinate, lorentzianConditions}, 
    eigensystem = Eigensystem[matrixRepresentation]; eigenvalues = First[eigensystem]; eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinate, eigenvalues[[#1]] < 0, eigenvalues[[#1]] > 0] & ) /@ 
          Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, If[lorentzianConditions === False, 
         Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, Indeterminate, 
          DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["ConnectionSingularities"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
            Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[Catenate[Catenate[FullSimplify[
                 christoffelSymbols]]], newCoordinates] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["CovariantChristoffelSymbols"] := 
  ChristoffelSymbols[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    True, True, True] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["ContravariantChristoffelSymbols"] := 
  ChristoffelSymbols[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    False, False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], index1_, index2_, index3_], newCoordinates_List] := 
  ChristoffelSymbols[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2, index3] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], index1_, index2_, index3_], newIndex1_, newIndex2_, newIndex3_] := 
  ChristoffelSymbols[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    newIndex1, newIndex2, newIndex3] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[newIndex1] && BooleanQ[newIndex2] && BooleanQ[newIndex3]
ChristoffelSymbols[ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], index1_, index2_, index3_], newCoordinates_List, newIndex1_, newIndex2_, newIndex3_] := 
  ChristoffelSymbols[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], newIndex1, newIndex2, newIndex3] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[newIndex1] && BooleanQ[newIndex2] && BooleanQ[newIndex3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_]["Properties"] := {"TensorRepresentation", "ReducedTensorRepresentation", 
    "SymbolicTensorRepresentation", "MetricTensor", "Coordinates", "CoordinateOneForms", "Indices", "CovariantQ", 
    "ContravariantQ", "MixedQ", "Symbol", "VanishingChristoffelQ", "VanishingChristoffelConditions", "IndexContractions", 
    "ReducedIndexContractions", "SymbolicIndexContractions", "Dimensions", "Signature", "RiemannianQ", 
    "PseudoRiemannianQ", "LorentzianQ", "RiemannianConditions", "PseudoRiemannianConditions", "LorentzianConditions", 
    "ConnectionSingularities", "CovariantChristoffelSymbols", "ContravariantChristoffelSymbols", "Properties"} /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols /: MakeBoxes[christoffelSymbols:ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, 
       coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, index3_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, tensorRepresentation, newTensorRepresentation, type, symbol, 
      dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, signature, icon}, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                  (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                     index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                    newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[coordinates, StringQ]; If[index1 === True && index2 === True && index3 === True, 
       newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (matrixRepresentation[[index[[1]],#1]]*tensorRepresentation[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
        type = "Covariant"; symbol = Subscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && 
         index3 === False, newTensorRepresentation = Normal[SparseArray[
            (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                    Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*tensorRepresentation[[index[[1]],#1[[1]],
                     #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 3]]]; type = "Contravariant"; 
         symbol = Superscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && index3 === False, 
         newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*
                     Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*tensorRepresentation[[#1[[1]],#1[[2]],
                      #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 3]]]; type = "Mixed"; 
          symbol = Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === True && index3 === False, 
          newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                   (Inverse[matrixRepresentation][[#1,index[[3]]]]*tensorRepresentation[[index[[1]],index[[2]],
                       #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalGamma]", "\[FormalMu]", 
             "\[FormalRho]\[FormalNu]"], If[index1 === False && index2 === False && index3 === True, 
           newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                    (Inverse[matrixRepresentation][[index[[2]],#1]]*tensorRepresentation[[index[[1]],#1,index[[
                         3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                 Range[Length[matrixRepresentation]], 3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalGamma]", "\[FormalNu]", 
              "\[FormalRho]\[FormalMu]"], If[index1 === True && index2 === True && index3 === False, 
            newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                     (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*
                        tensorRepresentation[[#1[[1]],index[[2]],#1[[2]]]] & ) /@ Tuples[Range[Length[
                         matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]]; 
             type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalMu]", "\[FormalNu]"], If[index1 === True && index2 === False && 
              index3 === True, newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                          index[[2]]]]*tensorRepresentation[[#1[[1]],#1[[2]],index[[3]]]] & ) /@ Tuples[
                        Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                   3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalGamma]", "\[FormalRho]\[FormalNu]", "\[FormalMu]"], 
             If[index1 === False && index2 === True && index3 === True, newTensorRepresentation = tensorRepresentation; 
               type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalGamma]", "\[FormalMu]\[FormalNu]", "\[FormalRho]"], newTensorRepresentation = 
                ConstantArray[Indeterminate, {Length[matrixRepresentation], Length[matrixRepresentation], 
                  Length[matrixRepresentation]}]; type = Indeterminate; symbol = Indeterminate]]]]]]]]; 
      dimensions = Length[matrixRepresentation]; eigenvalues = Eigenvalues[matrixRepresentation]; 
      positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
          Length[matrixRepresentation], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[Total[newTensorRepresentation], 
        ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], 
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["ChristoffelSymbols", christoffelSymbols, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
ChristoffelSymbols[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_][symbol_Integer, row_Integer, column_Integer] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True && index3 === True, 
      Normal[SparseArray[(Module[{index - #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*christoffelSymbols[[
                  #1,index[[2]],index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 3]]][[symbol,row,column]], 
      If[index1 === False && index2 === False && index3 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[index[[1]],#1[[1]],
                   #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 3]]][[symbol,row,column]], 
       If[index1 === True && index2 === False && index3 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                   Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[3]]]]*christoffelSymbols[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                  Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]][[
         symbol,row,column]], If[index1 === False && index2 === True && index3 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*
                    christoffelSymbols[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 3]]][[symbol,row,column]], 
         If[index1 === False && index2 === False && index3 === True, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                     christoffelSymbols[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 3]]][[symbol,row,column]], 
          If[index1 === True && index2 === True && index3 === False, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*christoffelSymbols[[#1[[1]],index[[2]],
                       #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 3]]][[symbol,row,column]], 
           If[index1 === True && index2 === False && index3 === True, 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                       Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*christoffelSymbols[[#1[[1]],#1[[2]],
                        index[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 3]]][[symbol,row,column]], 
            If[index1 === False && index2 === True && index3 === True, christoffelSymbols[[symbol,row,column]], 
             Indeterminate]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
