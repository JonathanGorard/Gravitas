(* ::Package:: *)

SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_]] := 
  EinsteinSolution[ResourceFunction["StressEnergyTensor"][ResourceFunction["MetricTensor"][metricMatrixRepresentation, 
      coordinates, metricIndex1, metricIndex2], matrixRepresentation, index1, index2], "\[FormalCapitalLambda]"] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], 
   (metricTensor_)[newMetricMatrixRepresentation_List, newCoordinates_List, newMetricIndex1_, newMetricIndex2_]] := 
  EinsteinSolution[ResourceFunction["StressEnergyTensor"][ResourceFunction["MetricTensor"][newMetricMatrixRepresentation, 
      newCoordinates, newMetricIndex1, newMetricIndex2], matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     index1, index2], "\[FormalCapitalLambda]"] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[Dimensions[newMetricMatrixRepresentation]] == 2 && 
    Length[newCoordinates] == Length[newMetricMatrixRepresentation] && BooleanQ[newMetricIndex1] && 
    BooleanQ[newMetricIndex2] && Length[newCoordinates] == Length[matrixRepresentation]
SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_] := 
  EinsteinSolution[ResourceFunction["StressEnergyTensor"][ResourceFunction["MetricTensor"][metricMatrixRepresentation, 
      coordinates, metricIndex1, metricIndex2], matrixRepresentation, index1, index2], cosmologicalConstant] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], 
   (metricTensor_)[newMetricMatrixRepresentation_List, newCoordinates_List, newMetricIndex1_, newMetricIndex2_], 
   cosmologicalConstant_] := EinsteinSolution[ResourceFunction["StressEnergyTensor"][
     ResourceFunction["MetricTensor"][newMetricMatrixRepresentation, newCoordinates, newMetricIndex1, newMetricIndex2], 
     matrixRepresentation /. Thread[coordinates -> newCoordinates], index1, index2], cosmologicalConstant] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[Dimensions[newMetricMatrixRepresentation]] == 2 && Length[newCoordinates] == 
     Length[newMetricMatrixRepresentation] && BooleanQ[newMetricIndex1] && BooleanQ[newMetricIndex2] && 
    Length[newCoordinates] == Length[matrixRepresentation]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["FieldEquations"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     tensorRepresentation, einsteinEquations}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; riemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
               newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], newCoordinates[[
                index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[metricMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     tensorRepresentation = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; einsteinEquations = 
      FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + 
           cosmologicalConstant*metricMatrixRepresentation] == Catenate[(8*Pi)*tensorRepresentation]]]; 
     If[einsteinEquations === True, {}, If[einsteinEquations === False, Indeterminate, 
       If[Length[Select[einsteinEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["EinsteinEquations"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     tensorRepresentation}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; riemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
               newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], newCoordinates[[
                index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[metricMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     tensorRepresentation = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     Thread[Catenate[ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + cosmologicalConstant*
          metricMatrixRepresentation] == Catenate[(8*Pi)*tensorRepresentation]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["ReducedEinsteinEquations"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     tensorRepresentation}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; riemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
               newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], newCoordinates[[
                index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[metricMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     tensorRepresentation = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + 
          cosmologicalConstant*metricMatrixRepresentation] == Catenate[(8*Pi)*tensorRepresentation]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_][
   "SymbolicEinsteinEquations"] := Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols, 
     riemannTensor, ricciTensor, ricciScalar, tensorRepresentation}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                 #1]]*(Inactive[D][newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; riemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],index[[2]],
                index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[metricMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     tensorRepresentation = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     Thread[Catenate[ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + cosmologicalConstant*
          metricMatrixRepresentation] == Catenate[(8*Pi)*tensorRepresentation]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["ContinuityEquations"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; (Module[{index = #1}, D[newMatrixRepresentation[[First[index],Last[index]]], 
            newCoordinates[[Last[index]]]] + Total[(christoffelSymbols[[First[index],Last[index],#1]]*
               newMatrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
           Total[(christoffelSymbols[[Last[index],Last[index],#1]]*newMatrixRepresentation[[First[index],#1]] & ) /@ 
             Range[Length[newMatrixRepresentation]]] == 0] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_][
   "ReducedContinuityEquations"] := Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, 
     christoffelSymbols}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; FullSimplify[(Module[{index = #1}, D[newMatrixRepresentation[[First[index],Last[index]]], 
             newCoordinates[[Last[index]]]] + Total[(christoffelSymbols[[First[index],Last[index],#1]]*
                newMatrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
            Total[(christoffelSymbols[[Last[index],Last[index],#1]]*newMatrixRepresentation[[First[index],#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] == 0] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_][
   "SymbolicContinuityEquations"] := Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, 
     christoffelSymbols}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; (Module[{index = #1}, Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], 
            newCoordinates[[Last[index]]]] + Total[(christoffelSymbols[[First[index],Last[index],#1]]*
               newMatrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
           Total[(christoffelSymbols[[Last[index],Last[index],#1]]*newMatrixRepresentation[[First[index],#1]] & ) /@ 
             Range[Length[newMatrixRepresentation]]] == 0] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["MetricTensor"] := 
  ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["StressEnergyTensor"] := 
  ResourceFunction["StressEnergyTensor"][ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], matrixRepresentation, index1, index2] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["Coordinates"] := 
  coordinates /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["SolutionQ"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     tensorRepresentation, einsteinEquations}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; riemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
               newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], newCoordinates[[
                index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[metricMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     tensorRepresentation = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; einsteinEquations = 
      FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + 
           cosmologicalConstant*metricMatrixRepresentation] == Catenate[(8*Pi)*tensorRepresentation]]]; 
     If[einsteinEquations === True, True, If[einsteinEquations === False, False, 
       If[Length[Select[einsteinEquations, #1 === False & ]] > 0, False, True]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["ExactSolutionQ"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
     tensorRepresentation, einsteinEquations}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; riemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
               newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], newCoordinates[[
                index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; ricciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[metricMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     tensorRepresentation = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; einsteinEquations = 
      FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + 
           cosmologicalConstant*metricMatrixRepresentation] == Catenate[(8*Pi)*tensorRepresentation]]]; 
     If[einsteinEquations === True, True, If[einsteinEquations === False, False, 
       If[Length[Select[einsteinEquations, #1 === False & ]] > 0, False, 
        If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]] == 0, True, 
         False]]]]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["CosmologicalConstant"] := 
  cosmologicalConstant /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["Dimensions"] := 
  Length[metricMatrixRepresentation] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["Signature"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["RiemannianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[metricMatrixRepresentation] || Length[negativeEigenvalues] == 
         Length[metricMatrixRepresentation], True, False], Indeterminate]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["PseudoRiemannianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[metricMatrixRepresentation] || Length[negativeEigenvalues] == 
         Length[metricMatrixRepresentation], False, True], Indeterminate]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["LorentzianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["RiemannianConditions"] := 
  Module[{eigenvalues, riemannianConditions}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; If[riemannianConditions === True, {}, 
      If[riemannianConditions === False, Indeterminate, If[Length[Select[riemannianConditions, #1 === False & ]] > 0, 
        Indeterminate, DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_][
   "PseudoRiemannianConditions"] := Module[{eigenvalues, pseudoRiemannianConditions}, 
    eigenvalues = Eigenvalues[metricMatrixRepresentation]; pseudoRiemannianConditions = 
      FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["LorentzianConditions"] := 
  Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinate, lorentzianConditions}, 
    eigensystem = Eigensystem[metricMatrixRepresentation]; eigenvalues = First[eigensystem]; 
     eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinate, eigenvalues[[#1]] < 0, eigenvalues[[#1]] > 0] & ) /@ 
          Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, If[lorentzianConditions === False, 
         Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, Indeterminate, 
          DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_]["Properties"] := 
  {"FieldEquations", "EinsteinEquations", "ReducedEinsteinEquations", "SymbolicEinsteinEquations", "ContinuityEquations", 
    "ReducedContinuityEquations", "SymbolicContinuityEquations", "MetricTensor", "StressEnergyTensor", "Coordinates", 
    "CoordinateOneForms", "SolutionQ", "ExactSolutionQ", "CosmologicalConstant", "Dimensions", "Signature", 
    "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", "RiemannianConditions", "PseudoRiemannianConditions", 
    "LorentzianConditions", "Properties"} /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
EinsteinSolution /: 
  MakeBoxes[einsteinSolution:EinsteinSolution[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, 
        coordinates_List, metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], 
      cosmologicalConstant_], format_] := Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols, 
      riemannTensor, ricciTensor, ricciScalar, matrixForm, tensorRepresentation, einsteinEquations, solution, 
      exactSolution, fieldEquations, icon}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
        (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                  D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                  D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ Range[
                Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
           3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[coordinates, StringQ]; ricciTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
               Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
      ricciScalar = Total[(Inverse[metricMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
      matrixForm = ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + cosmologicalConstant*
         metricMatrixRepresentation; tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                 metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
               Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; einsteinEquations = 
       FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*metricMatrixRepresentation + 
            cosmologicalConstant*metricMatrixRepresentation] == Catenate[(8*Pi)*tensorRepresentation]]]; 
      If[einsteinEquations === True, solution = True; exactSolution = True; fieldEquations = 0, 
       If[einsteinEquations === False, solution = False; exactSolution = False; fieldEquations = Indeterminate, 
        If[Length[Select[einsteinEquations, #1 === False & ]] > 0, solution = False; exactSolution = False; 
          fieldEquations = Indeterminate, If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, 
                #1 =!= True & ]]] == 0, solution = True; exactSolution = True; fieldEquations = 0, 
          solution = True; exactSolution = False; fieldEquations = Length[DeleteDuplicates[Reverse /@ Sort /@ 
                Select[einsteinEquations, #1 =!= True & ]]]]]]]; 
      icon = MatrixPlot[matrixForm, ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification])}], Frame -> False, FrameTicks -> None]; 
      BoxForm`ArrangeSummaryBox["EinsteinSolution", einsteinSolution, icon, 
       {{BoxForm`SummaryItem[{"Solution: ", solution}], BoxForm`SummaryItem[{"Exact Solution: ", exactSolution}]}, 
        {BoxForm`SummaryItem[{"Field Equations: ", fieldEquations}], BoxForm`SummaryItem[{"Cosmological Constant: ", 
           cosmologicalConstant}]}}, {{}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
     BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
