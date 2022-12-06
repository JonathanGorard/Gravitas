(* ::Package:: *)

SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_]] := 
  EinsteinSolution[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], 
     matrixRepresentation, index1, index2], "\[FormalCapitalLambda]"] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], 
   (metricTensor_)[newMetricMatrixRepresentation_List, newCoordinates_List, newMetricIndex1_, newMetricIndex2_]] := 
  EinsteinSolution[StressEnergyTensor[MetricTensor[newMetricMatrixRepresentation, newCoordinates, newMetricIndex1, 
      newMetricIndex2], matrixRepresentation, index1, index2], "\[FormalCapitalLambda]"] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[Dimensions[newMetricMatrixRepresentation]] == 2 && Length[newCoordinates] == 
     Length[newMetricMatrixRepresentation] && BooleanQ[newMetricIndex1] && BooleanQ[newMetricIndex2] && 
    Length[newCoordinates] == Length[matrixRepresentation]
SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], cosmologicalConstant_] := 
  EinsteinSolution[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], 
     matrixRepresentation, index1, index2], cosmologicalConstant] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
SolveEinsteinEquations[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], 
   (metricTensor_)[newMetricMatrixRepresentation_List, newCoordinates_List, newMetricIndex1_, newMetricIndex2_], 
   cosmologicalConstant_] := EinsteinSolution[StressEnergyTensor[MetricTensor[newMetricMatrixRepresentation, 
      newCoordinates, newMetricIndex1, newMetricIndex2], matrixRepresentation, index1, index2], cosmologicalConstant] /; 
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
