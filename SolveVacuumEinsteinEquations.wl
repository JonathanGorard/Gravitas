(* ::Package:: *)

SolveVacuumEinsteinEquations[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  VacuumSolution[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], "\[FormalCapitalLambda]"] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
SolveVacuumEinsteinEquations[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   cosmologicalConstant_] := VacuumSolution[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, 
     index2], cosmologicalConstant] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "FieldEquations"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, 
     ricciScalar, einsteinEquations}, newMatrixRepresentation = matrixRepresentation /. 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinEquations = 
      FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
           cosmologicalConstant*matrixRepresentation] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
            Length[matrixRepresentation]}]]]]; If[einsteinEquations === True, {}, If[einsteinEquations === False, 
       Indeterminate, If[Length[Select[einsteinEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "EinsteinEquations"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + cosmologicalConstant*matrixRepresentation] == 
       Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation]}]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "ReducedEinsteinEquations"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
          cosmologicalConstant*matrixRepresentation] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
           Length[matrixRepresentation]}]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "SymbolicEinsteinEquations"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + cosmologicalConstant*matrixRepresentation] == 
       Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation]}]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "MetricTensor"] := ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "Coordinates"] := coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "SolutionQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, 
     ricciScalar, einsteinEquations}, newMatrixRepresentation = matrixRepresentation /. 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinEquations = 
      FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
           cosmologicalConstant*matrixRepresentation] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
            Length[matrixRepresentation]}]]]]; If[einsteinEquations === True, True, 
      If[einsteinEquations === False, False, If[Length[Select[einsteinEquations, #1 === False & ]] > 0, False, 
        True]]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "ExactSolutionQ"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, 
     ricciScalar, einsteinEquations}, newMatrixRepresentation = matrixRepresentation /. 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; einsteinEquations = 
      FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
           cosmologicalConstant*matrixRepresentation] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
            Length[matrixRepresentation]}]]]]; If[einsteinEquations === True, True, 
      If[einsteinEquations === False, False, If[Length[Select[einsteinEquations, #1 === False & ]] > 0, False, 
        If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]] == 0, True, 
         False]]]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "CosmologicalConstant"] := cosmologicalConstant /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "Dimensions"] := Length[matrixRepresentation] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "Signature"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "RiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], True, False], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "PseudoRiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], False, True], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "LorentzianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "RiemannianConditions"] := Module[{eigenvalues, riemannianConditions}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; 
     If[riemannianConditions === True, {}, If[riemannianConditions === False, Indeterminate, 
       If[Length[Select[riemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "PseudoRiemannianConditions"] := Module[{eigenvalues, pseudoRiemannianConditions}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; pseudoRiemannianConditions = 
      FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "LorentzianConditions"] := Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinate, lorentzianConditions}, 
    eigensystem = Eigensystem[matrixRepresentation]; eigenvalues = First[eigensystem]; eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinate, eigenvalues[[#1]] < 0, eigenvalues[[#1]] > 0] & ) /@ 
          Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, If[lorentzianConditions === False, 
         Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, Indeterminate, 
          DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], cosmologicalConstant_][
   "Properties"] := {"FieldEquations", "EinsteinEquations", "ReducedEinsteinEquations", "SymbolicEinsteinEquations", 
    "MetricTensor", "Coordinates", "CoordinateOneForms", "SolutionQ", "ExactSolutionQ", "CosmologicalConstant", 
    "Dimensions", "Signature", "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", "RiemannianConditions", 
    "PseudoRiemannianConditions", "LorentzianConditions", "Properties"} /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
VacuumSolution /: MakeBoxes[vacuumSolution:VacuumSolution[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
       index1_, index2_], cosmologicalConstant_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, 
      matrixForm, einsteinEquations, solution, exactSolution, fieldEquations, icon}, 
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
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
      ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                 Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]; ricciScalar = 
       Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]; 
      matrixForm = ricciTensor - (1/2)*ricciScalar*matrixRepresentation + cosmologicalConstant*matrixRepresentation; 
      einsteinEquations = FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
            cosmologicalConstant*matrixRepresentation] == Catenate[ConstantArray[0, {Length[matrixRepresentation], 
             Length[matrixRepresentation]}]]]]; If[einsteinEquations === True, solution = True; exactSolution = True; 
        fieldEquations = 0, If[einsteinEquations === False, solution = False; exactSolution = False; 
         fieldEquations = Indeterminate, If[Length[Select[einsteinEquations, #1 === False & ]] > 0, 
         solution = False; exactSolution = False; fieldEquations = Indeterminate, 
         If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]] == 0, 
          solution = True; exactSolution = True; fieldEquations = 0, solution = True; exactSolution = False; 
           fieldEquations = Length[DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]]]]]]; 
      icon = MatrixPlot[matrixForm, ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification])}], Frame -> False, FrameTicks -> None]; 
      BoxForm`ArrangeSummaryBox["VacuumSolution", vacuumSolution, icon, 
       {{BoxForm`SummaryItem[{"Solution: ", solution}], BoxForm`SummaryItem[{"Exact Solution: ", exactSolution}]}, 
        {BoxForm`SummaryItem[{"Field Equations: ", fieldEquations}], BoxForm`SummaryItem[{"Cosmological Constant: ", 
           cosmologicalConstant}]}}, {{}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
