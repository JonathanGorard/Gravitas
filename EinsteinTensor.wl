(* ::Package:: *)

EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  EinsteinTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], True, True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], newCoordinates_List] := 
  EinsteinTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, index1, index2], True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   newCoordinates_List, index1_, index2_] := 
  EinsteinTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["MatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     riemannTensor, ricciTensor, ricciScalar, einsteinTensor}, 
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
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; If[index1 === True && index2 === True, einsteinTensor, 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
       If[index1 === True && index2 === False, Normal[SparseArray[
          (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*einsteinTensor[[
                   First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   einsteinTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["ReducedMatrixRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     einsteinTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; If[index1 === True && index2 === True, 
      FullSimplify[einsteinTensor], If[index1 === False && index2 === False, 
       FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                   First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]]], If[index1 === True && index2 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                    Last[index]]]*einsteinTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]]], If[index1 === False && index2 === True, 
         FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                     First[index],#1]]*einsteinTensor[[#1,Last[index]]] & ) /@ Range[Length[
                    matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
         Indeterminate]]]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["SymbolicMatrixRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     einsteinTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; If[index1 === True && index2 === True, einsteinTensor, 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
       If[index1 === True && index2 === False, Normal[SparseArray[
          (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*einsteinTensor[[
                   First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   einsteinTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["Trace"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, 
     ricciScalar, einsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
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
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; 
     Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["ReducedTrace"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, einsteinTensor}, 
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
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; 
     FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["SymbolicTrace"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, einsteinTensor}, 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; 
     Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["MetricTensor"] := ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, 
    metricIndex2] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["Coordinates"] := coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["Indices"] := {index1, index2} /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["CovariantQ"] := If[index1 === True && index2 === True, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["ContravariantQ"] := If[index1 === False && index2 === False, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["MixedQ"] := If[(index1 === True && index2 === False) || (index1 === False && index2 === True), True, 
    False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["Symbol"] := If[index1 === True && index2 === True, Subscript["\[FormalCapitalG]", "\[FormalMu]\[FormalNu]"], 
    If[index1 === False && index2 === False, Superscript["\[FormalCapitalG]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
      Subsuperscript["\[FormalCapitalG]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, Subsuperscript["\[FormalCapitalG]", "\[FormalNu]", "\[FormalMu]"], 
       Indeterminate]]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["EinsteinFlatQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, einsteinTensor, fieldEquations}, 
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
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; fieldEquations = 
      FullSimplify[Thread[Catenate[einsteinTensor] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
            Length[matrixRepresentation]}]]]]; If[fieldEquations === True, True, If[fieldEquations === False, False, 
       If[Length[Select[fieldEquations, #1 === True & ]] == Length[matrixRepresentation]*Length[matrixRepresentation], 
        True, False]]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["VanishingEinsteinTraceQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     riemannTensor, ricciTensor, ricciScalar, einsteinTensor}, 
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
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; 
     FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]] == 0] === True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["EinsteinFlatConditions"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     riemannTensor, ricciTensor, ricciScalar, einsteinTensor, fieldEquations}, 
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
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; fieldEquations = 
      FullSimplify[Thread[Catenate[einsteinTensor] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
            Length[matrixRepresentation]}]]]]; If[fieldEquations === True, {}, If[fieldEquations === False, 
       Indeterminate, If[Length[Select[fieldEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[fieldEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["VanishingEinsteinTraceCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     einsteinTensor, fieldEquation}, newMatrixRepresentation = matrixRepresentation /. 
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
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*matrixRepresentation; fieldEquation = 
      FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]] == 0]; If[fieldEquation === False, Indeterminate, 
      fieldEquation]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["CovariantDerivatives"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     riemannTensor, ricciTensor, ricciScalar, einsteinTensor, newEinsteinTensor}, 
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
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*newMatrixRepresentation; If[index1 === True && index2 === True, 
      newEinsteinTensor = einsteinTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalG]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                  newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[
                    First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalG]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + Total[
                (christoffelSymbols[[index[[2]],index[[1]],#1]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                   newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,
                     Last[index]]]*einsteinTensor[[First[index],#1]] & ) /@ Range[Length[
                    newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalG]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newEinsteinTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    newEinsteinTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      First[index],#1]]*einsteinTensor[[#1,Last[index]]] & ) /@ Range[Length[
                     newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalG]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["ReducedCovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     einsteinTensor, newEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
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
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*newMatrixRepresentation; If[index1 === True && index2 === True, 
      newEinsteinTensor = einsteinTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalG]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             FullSimplify[D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - Total[
                (christoffelSymbols[[#1,index[[1]],index[[2]]]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                   newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[
                    First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalG]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> FullSimplify[D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[
                  index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newEinsteinTensor[[#1,
                     index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                 (christoffelSymbols[[index[[3]],index[[1]],#1]]*newEinsteinTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3] /. 
          (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,
                     Last[index]]]*einsteinTensor[[First[index],#1]] & ) /@ Range[Length[
                    newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalG]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> FullSimplify[
                D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newEinsteinTensor[[index[[2]],#1]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                     newEinsteinTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      First[index],#1]]*einsteinTensor[[#1,Last[index]]] & ) /@ Range[Length[
                     newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalG]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                FullSimplify[D[newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                  Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                    Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                      newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["SymbolicCovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     einsteinTensor, newEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
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
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*newMatrixRepresentation; If[index1 === True && index2 === True, 
      newEinsteinTensor = einsteinTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalG]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             Inactive[D][newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                  newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[
                    First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalG]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> Inactive[D][newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
               Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                   newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,
                     Last[index]]]*einsteinTensor[[First[index],#1]] & ) /@ Range[Length[
                    newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalG]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               Inactive[D][newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newEinsteinTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    newEinsteinTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newEinsteinTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      First[index],#1]]*einsteinTensor[[#1,Last[index]]] & ) /@ Range[Length[
                     newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalG]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                Inactive[D][newEinsteinTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newEinsteinTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     newEinsteinTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["BianchiIdentities"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, einsteinTensor, contravariantEinsteinTensor}, 
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
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*newMatrixRepresentation; contravariantEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, D[contravariantEinsteinTensor[[nestedIndex,index]], 
                newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                   contravariantEinsteinTensor[[#1,index]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index,nestedIndex,#1]]*contravariantEinsteinTensor[[nestedIndex,#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]] == 0] & ) /@ 
       Range[Length[newMatrixRepresentation]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_]["SymbolicBianchiIdentities"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     einsteinTensor, contravariantEinsteinTensor}, 
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
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; einsteinTensor = 
      ricciTensor - (1/2)*ricciScalar*newMatrixRepresentation; contravariantEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*einsteinTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, Inactive[D][contravariantEinsteinTensor[[nestedIndex,
                 index]], newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                   contravariantEinsteinTensor[[#1,index]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index,nestedIndex,#1]]*contravariantEinsteinTensor[[nestedIndex,#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]] == 0] & ) /@ 
       Range[Length[newMatrixRepresentation]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinTensor /: MakeBoxes[einsteinTensor:EinsteinTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
       metricIndex1_, metricIndex2_], index1_, index2_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
      tensorRepresentation, matrixForm, type, symbol, dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, 
      signature, icon}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; christoffelSymbols = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
      ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                 Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]; ricciScalar = 
       Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]; tensorRepresentation = 
       ricciTensor - (1/2)*ricciScalar*matrixRepresentation; If[index1 === True && index2 === True, 
       matrixForm = tensorRepresentation; type = "Covariant"; symbol = Subscript["\[FormalCapitalG]", "\[FormalMu]\[FormalNu]"], 
       If[index1 === False && index2 === False, 
        matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                     First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                     Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Contravariant"; 
         symbol = Superscript["\[FormalCapitalG]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
         matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                      Last[index]]]*tensorRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
          symbol = Subsuperscript["\[FormalCapitalG]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, 
          matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                       First[index],#1]]*tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                      matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
           symbol = Subsuperscript["\[FormalCapitalG]", "\[FormalNu]", "\[FormalMu]"], matrixForm = ConstantArray[Indeterminate, 
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
        FrameTicks -> None]; BoxForm`ArrangeSummaryBox["EinsteinTensor", einsteinTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     BooleanQ[index1] && BooleanQ[index2]
