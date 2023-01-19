(* ::Package:: *)

RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], True, 
    True] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   newCoordinates_List] := RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. 
      Thread[coordinates -> newCoordinates], newCoordinates, metricIndex1, metricIndex2], True, True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   newCoordinates_List, index1_, index2_] := 
  RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "MatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; If[index1 === True && index2 === True, ricciTensor, 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
       If[index1 === True && index2 === False, Normal[SparseArray[
          (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*ricciTensor[[First[index],
                   #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
            2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedMatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; If[index1 === True && index2 === True, 
      FullSimplify[ricciTensor], If[index1 === False && index2 === False, 
       FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                   First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]]], If[index1 === True && index2 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                    Last[index]]]*ricciTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]]], If[index1 === False && index2 === True, 
         FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                     First[index],#1]]*ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymbolicMatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     If[index1 === True && index2 === True, ricciTensor, If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
       If[index1 === True && index2 === False, Normal[SparseArray[
          (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*ricciTensor[[First[index],
                   #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
            2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "RicciScalar"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedRicciScalar"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymbolicRicciScalar"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "MetricTensor"] := ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Coordinates"] := coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Indices"] := {index1, index2} /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "CovariantQ"] := If[index1 === True && index2 === True, True, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ContravariantQ"] := If[index1 === False && index2 === False, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "MixedQ"] := If[(index1 === True && index2 === False) || (index1 === False && index2 === True), True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Symbol"] := If[index1 === True && index2 === True, Subscript["\[FormalCapitalR]", "\[FormalMu]\[FormalNu]"], 
    If[index1 === False && index2 === False, Superscript["\[FormalCapitalR]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
      Subsuperscript["\[FormalCapitalR]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, Subsuperscript["\[FormalCapitalR]", "\[FormalNu]", "\[FormalMu]"], 
       Indeterminate]]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "RicciFlatQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, 
     fieldEquations}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; fieldEquations = 
      FullSimplify[Thread[Catenate[ricciTensor] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
            Length[matrixRepresentation]}]]]]; If[fieldEquations === True, True, If[fieldEquations === False, False, 
       If[Length[Select[fieldEquations, #1 === True & ]] == Length[matrixRepresentation]*Length[matrixRepresentation], 
        True, False]]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "VanishingRicciScalarQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]] == 0] === True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "RicciFlatConditions"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, fieldEquations}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; fieldEquations = 
      FullSimplify[Thread[Catenate[ricciTensor] == Catenate[ConstantArray[0, {Length[newMatrixRepresentation], 
            Length[newMatrixRepresentation]}]]]]; If[fieldEquations === True, {}, 
      If[fieldEquations === False, Indeterminate, If[Length[Select[fieldEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[fieldEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "VanishingRicciScalarCondition"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     riemannTensor, ricciTensor, fieldEquation}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; fieldEquation = 
      FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]] == 0]; If[fieldEquation === False, Indeterminate, 
      fieldEquation]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "CovariantDerivatives"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, newRicciTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; If[index1 === True && index2 === True, 
      newRicciTensor = ricciTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                  newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[
                    First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + Total[
                (christoffelSymbols[[index[[2]],index[[1]],#1]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                   newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,
                     Last[index]]]*ricciTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRicciTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    newRicciTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      First[index],#1]]*ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[
                     newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedCovariantDerivatives"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, newRicciTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; If[index1 === True && index2 === True, 
      newRicciTensor = ricciTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             FullSimplify[D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - Total[
                (christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                   newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[
                    First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> FullSimplify[D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                    newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,
                     Last[index]]]*ricciTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> FullSimplify[
                D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRicciTensor[[index[[2]],#1]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                     newRicciTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      First[index],#1]]*ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[
                     newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                FullSimplify[D[newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                  Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                    Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                      newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymbolicCovariantDerivatives"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, newRicciTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; If[index1 === True && index2 === True, 
      newRicciTensor = ricciTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             Inactive[D][newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                  newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[
                    First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> Inactive[D][newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
               Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                   newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,
                     Last[index]]]*ricciTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               Inactive[D][newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRicciTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    newRicciTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newRicciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      First[index],#1]]*ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[
                     newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                Inactive[D][newRicciTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRicciTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     newRicciTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "BianchiIdentities"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, mixedRicciTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; mixedRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, D[mixedRicciTensor[[nestedIndex,index]], 
                newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                   mixedRicciTensor[[#1,index]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[#1,nestedIndex,index]]*mixedRicciTensor[[nestedIndex,#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]] == 
          (1/2)*D[ricciScalar, newCoordinates[[index]]]] & ) /@ Range[Length[newMatrixRepresentation]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymbolicBianchiIdentities"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, mixedRicciTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; mixedRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, Inactive[D][mixedRicciTensor[[nestedIndex,index]], 
                newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                   mixedRicciTensor[[#1,index]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[#1,nestedIndex,index]]*mixedRicciTensor[[nestedIndex,#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]] == 
          (1/2)*Inactive[D][ricciScalar, newCoordinates[[index]]]] & ) /@ Range[Length[newMatrixRepresentation]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Dimensions"] := Length[matrixRepresentation] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymmetricQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     SymmetricMatrixQ[Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "DiagonalQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     DiagonalMatrixQ[Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Signature"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "RiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], True, False], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "PseudoRiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], False, True], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "LorentzianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "RiemannianConditions"] := Module[{eigenvalues, riemannianConditions}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; 
     If[riemannianConditions === True, {}, If[riemannianConditions === False, Indeterminate, 
       If[Length[Select[riemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "PseudoRiemannianConditions"] := Module[{eigenvalues, pseudoRiemannianConditions}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; pseudoRiemannianConditions = 
      FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "LorentzianConditions"] := Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinate, lorentzianConditions}, 
    eigensystem = Eigensystem[matrixRepresentation]; eigenvalues = First[eigensystem]; eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinate, eigenvalues[[#1]] < 0, eigenvalues[[#1]] > 0] & ) /@ 
          Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, If[lorentzianConditions === False, 
         Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, Indeterminate, 
          DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "CurvatureSingularities"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
            Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[Catenate[FullSimplify[ricciTensor]], 
              newCoordinates] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ScalarCurvatureSingularities"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
            Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[FullSimplify[ricciScalar], 
              newCoordinates] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Determinant"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     Det[Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedDeterminant"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     FullSimplify[Det[Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                 Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymbolicDeterminant"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; 
     Det[Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Eigenvalues"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     Eigenvalues[Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedEigenvalues"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     FullSimplify[Eigenvalues[Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Eigenvectors"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     Eigenvectors[Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedEigenvectors"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     FullSimplify[Eigenvectors[Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "CovariantRicciTensor"] := RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ContravariantRicciTensor"] := RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ScalarVolumeExpansion"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; "\[FormalCapitalV]"["\[FormalEpsilon]"]/"\[FormalMu]"["\[FormalEpsilon]"] == 
      1 - (ricciScalar/(6*(Length[matrixRepresentation] + 2)))*"\[FormalEpsilon]"^2 + "\[FormalCapitalO]"["\[FormalEpsilon]"]^3] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedScalarVolumeExpansion"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; FullSimplify["\[FormalCapitalV]"["\[FormalEpsilon]"]/"\[FormalMu]"["\[FormalEpsilon]"] == 
       1 - (ricciScalar/(6*(Length[matrixRepresentation] + 2)))*"\[FormalEpsilon]"^2 + "\[FormalCapitalO]"["\[FormalEpsilon]"]^3]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymbolicScalarVolumeExpansion"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     riemannTensor, ricciTensor, ricciScalar}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; "\[FormalCapitalV]"["\[FormalEpsilon]"]/"\[FormalMu]"["\[FormalEpsilon]"] == 
      1 - (ricciScalar/(6*(Length[matrixRepresentation] + 2)))*"\[FormalEpsilon]"^2 + "\[FormalCapitalO]"["\[FormalEpsilon]"]^3] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "VolumeFormExpansion"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, coordinateOneForms}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; coordinateOneForms = 
      (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
         If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
          StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates; "\[FormalD]\[FormalCapitalV]"/"\[FormalD]\[FormalMu]" == 
      1 - (1/6)*Total[(ricciTensor[[First[#1],Last[#1]]]*coordinateOneForms[[First[#1]]]*coordinateOneForms[[
             Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]] + 
       "\[FormalCapitalO]"[Abs[Sqrt[Total[(matrixRepresentation[[First[#1],Last[#1]]]*coordinateOneForms[[First[#1]]]*
               coordinateOneForms[[Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]^3]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedVolumeFormExpansion"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, coordinateOneForms}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; coordinateOneForms = 
      (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
         If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
          StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates; 
     FullSimplify["\[FormalD]\[FormalCapitalV]"/"\[FormalD]\[FormalMu]" == 1 - (1/6)*Total[(ricciTensor[[First[#1],Last[#1]]]*coordinateOneForms[[First[#1]]]*
             coordinateOneForms[[Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]] + 
        "\[FormalCapitalO]"[Abs[Sqrt[Total[(matrixRepresentation[[First[#1],Last[#1]]]*coordinateOneForms[[First[#1]]]*
                coordinateOneForms[[Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]^3]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "SymbolicVolumeFormExpansion"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, coordinateOneForms}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     coordinateOneForms = (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], 
          ToString[Last[#1]]], If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], 
           ToString[Last[#1]]], StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates; 
     "\[FormalD]\[FormalCapitalV]"/"\[FormalD]\[FormalMu]" == 1 - (1/6)*Total[(ricciTensor[[First[#1],Last[#1]]]*coordinateOneForms[[First[#1]]]*
            coordinateOneForms[[Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]] + 
       "\[FormalCapitalO]"[Abs[Sqrt[Total[(matrixRepresentation[[First[#1],Last[#1]]]*coordinateOneForms[[First[#1]]]*
               coordinateOneForms[[Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]^3]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "Properties"] := {"MatrixRepresentation", "ReducedMatrixRepresentation", "SymbolicMatrixRepresentation", 
    "RicciScalar", "ReducedRicciScalar", "SymbolicRicciScalar", "MetricTensor", "Coordinates", "CoordinateOneForms", 
    "Indices", "CovariantQ", "ContravariantQ", "MixedQ", "Symbol", "RicciFlatQ", "VanishingRicciScalarQ", 
    "RicciFlatConditions", "VanishingRicciScalarCondition", "CovariantDerivatives", "ReducedCovariantDerivatives", 
    "SymbolicCovariantDerivatives", "BianchiIdentities", "SymbolicBianchiIdentities", "Dimensions", "SymmetricQ", 
    "DiagonalQ", "Signature", "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", "RiemannianConditions", 
    "PseudoRiemannianConditions", "LorentzianConditions", "CurvatureSingularities", "ScalarCurvatureSingularities", 
    "Determinant", "ReducedDeterminant", "SymbolicDeterminant", "Eigenvalues", "ReducedEigenvalues", "Eigenvectors", 
    "ReducedEigenvectors", "CovariantRicciTensor", "ContravariantRicciTensor", "ScalarVolumeExpansion", 
    "ReducedScalarVolumeExpansion", "SymbolicScalarVolumeExpansion", "VolumeFormExpansion", "ReducedVolumeFormExpansion", 
    "SymbolicVolumeFormExpansion", "Properties"} /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
RicciTensor[RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    index1_, index2_], newCoordinates_List] := 
  RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    index1_, index2_], newIndex1_, newIndex2_] := 
  RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], newIndex1, 
    newIndex2] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[newIndex1] && BooleanQ[newIndex2]
RicciTensor[RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    index1_, index2_], newCoordinates_List, newIndex1_, newIndex2_] := 
  RicciTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], newIndex1, newIndex2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[newIndex1] && BooleanQ[newIndex2]
RicciTensor /: MakeBoxes[ricciTensor:RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
       metricIndex1_, metricIndex2_], index1_, index2_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, tensorRepresentation, matrixForm, 
      type, symbol, dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, signature, icon}, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
               Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
      If[index1 === True && index2 === True, matrixForm = tensorRepresentation; type = "Covariant"; 
        symbol = Subscript["\[FormalCapitalR]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
        matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                     First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                     Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Contravariant"; 
         symbol = Superscript["\[FormalCapitalR]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
         matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                      Last[index]]]*tensorRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
          symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, 
          matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                       First[index],#1]]*tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                      matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
           symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalNu]", "\[FormalMu]"], matrixForm = ConstantArray[Indeterminate, 
             {Length[matrixRepresentation], Length[matrixRepresentation]}]; type = Indeterminate; 
           symbol = Indeterminate]]]]; dimensions = Length[matrixRepresentation]; 
      eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
          Length[matrixRepresentation], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[matrixForm, ImageSize -> 
         Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], Frame -> False, 
        FrameTicks -> None]; BoxForm`ArrangeSummaryBox["RicciTensor", ricciTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   row_Integer, column_Integer] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; If[index1 === True && index2 === True, 
      ricciTensor[[row,column]], If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]][[
        row,column]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                   ricciTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]][[row,column]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]][[row,column]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   row_Integer, All] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; If[index1 === True && index2 === True, ricciTensor[[row,All]], 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]][[
        row,All]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                   ricciTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]][[row,All]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]][[row,All]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
RicciTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   All, column_Integer] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; If[index1 === True && index2 === True, 
      Transpose[{ricciTensor[[All,column]]}], If[index1 === False && index2 === False, 
       Transpose[{Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                     First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*ricciTensor[[First[#1],
                     Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]][[All,column]]}], If[index1 === True && index2 === False, 
        Transpose[{Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                      Last[index]]]*ricciTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 2]]][[All,column]]}], If[index1 === False && index2 === True, 
         Transpose[{Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                       #1]]*ricciTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]][[All,column]]}], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
