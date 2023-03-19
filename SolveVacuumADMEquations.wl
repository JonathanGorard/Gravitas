(* ::Package:: *)

SolveVacuumADMEquations[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, 
     index2_], timeCoordinate_, lapseFunction_, shiftVector_List]] := 
  VacuumADMSolution[ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], 
     timeCoordinate, lapseFunction, shiftVector], "\[FormalCapitalLambda]"] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
SolveVacuumADMEquations[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, 
     index2_], timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_] := 
  VacuumADMSolution[ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], 
     timeCoordinate, lapseFunction, shiftVector], cosmologicalConstant] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["SolutionQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, leftHandSide, 
     rightHandSide, evolutionEquations}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]])*Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + 
                   Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*
                  (D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + Total[
                    (spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             newLapseFunction*Total[(((2*cosmologicalConstant)/(Length[newMatrixRepresentation] - 1))*
                  spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*Inverse[newMatrixRepresentation][[#1,
                   First[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; evolutionEquations = 
      FullSimplify[Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]]; If[evolutionEquations === True, True, 
      If[evolutionEquations === False, False, If[Length[Select[evolutionEquations, #1 === False & ]] > 0, False, 
        True]]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["ExactSolutionQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, leftHandSide, 
     rightHandSide, evolutionEquations}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]])*Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + 
                   Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*
                  (D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + Total[
                    (spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             newLapseFunction*Total[(((2*cosmologicalConstant)/(Length[newMatrixRepresentation] - 1))*
                  spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*Inverse[newMatrixRepresentation][[#1,
                   First[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; evolutionEquations = 
      FullSimplify[Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]]; If[evolutionEquations === True, True, 
      If[evolutionEquations === False, False, If[Length[Select[evolutionEquations, #1 === False & ]] > 0, False, 
        If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[evolutionEquations, #1 =!= True & ]]] == 0, True, 
         False]]]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["FieldEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, leftHandSide, 
     rightHandSide, evolutionEquations}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]])*Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + 
                   Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*
                  (D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + Total[
                    (spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             newLapseFunction*Total[(((2*cosmologicalConstant)/(Length[newMatrixRepresentation] - 1))*
                  spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*Inverse[newMatrixRepresentation][[#1,
                   First[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; evolutionEquations = 
      FullSimplify[Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]]; If[evolutionEquations === True, {}, 
      If[evolutionEquations === False, Indeterminate, If[Length[Select[evolutionEquations, #1 === False & ]] > 0, 
        Indeterminate, DeleteDuplicates[Reverse /@ Sort /@ Select[evolutionEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["EvolutionEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, leftHandSide, 
     rightHandSide}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]])*Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + 
                   Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*
                  (D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + Total[
                    (spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             newLapseFunction*Total[(((2*cosmologicalConstant)/(Length[newMatrixRepresentation] - 1))*
                  spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*Inverse[newMatrixRepresentation][[#1,
                   First[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["ReducedEvolutionEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, leftHandSide, 
     rightHandSide}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]])*Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + 
                   Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*
                  (D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + Total[
                    (spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             newLapseFunction*Total[(((2*cosmologicalConstant)/(Length[newMatrixRepresentation] - 1))*
                  spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*Inverse[newMatrixRepresentation][[#1,
                   First[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     FullSimplify[Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["SymbolicEvolutionEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, leftHandSide, 
     rightHandSide}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][spatialChristoffelSymbols[[index[[1]],
               index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                  index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     mixedSpatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*spatialRicciTensor[[#1,
                 Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][mixedExtrinsicCurvatureTensor[[
              First[index],Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (Inactive[D][Inactive[D][newLapseFunction, 
                     newCoordinates[[Last[index]]]], newCoordinates[[nestedIndex]]] - Total[
                    (spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*Inactive[D][newLapseFunction, 
                        newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]])*
                  Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(Inactive[D][mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(Inactive[D][newShiftVector[[nestedIndex]], newCoordinates[[
                     Last[index]]]] + Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[
                        #1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   nestedIndex,Last[index]]]*(Inactive[D][newShiftVector[[First[index]]], newCoordinates[[
                     nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[
                        #1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] - newLapseFunction*Total[(((2*cosmologicalConstant)/
                   (Length[newMatrixRepresentation] - 1))*spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*
                  Inverse[newMatrixRepresentation][[#1,First[index]]] & ) /@ Range[Length[
                  newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["HamiltonianConstraintSatisfiedQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spatialRicciScalar = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     FullSimplify[((spatialRicciScalar + extrinsicCurvatureTrace^2 - 
           Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]) - 
          2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[Join[coordinates, {timeCoordinate}], StringQ]) == 0] === True] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["HamiltonianConstraint"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spatialRicciScalar = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     (spatialRicciScalar + extrinsicCurvatureTrace^2 - Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
            mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]) - 2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["ReducedHamiltonianConstraint"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spatialRicciScalar = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     FullSimplify[(spatialRicciScalar + extrinsicCurvatureTrace^2 - 
         Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[Last[#1],
              First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]) - 
        2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["SymbolicHamiltonianConstraint"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][spatialChristoffelSymbols[[index[[1]],
               index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                  index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     spatialRicciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     (spatialRicciScalar + extrinsicCurvatureTrace^2 - Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
            mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]) - 2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["MomentumConstraintsSatisfiedQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor, fieldEquations}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; fieldEquations = 
      FullSimplify[Thread[(Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[
                      index,#1]], newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                 Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                      Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
                 Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                      First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
                  newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ 
             Range[Length[newMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[Join[coordinates, {timeCoordinate}], StringQ]) == ConstantArray[0, Length[matrixRepresentation]]]]; 
     If[fieldEquations === True, True, If[fieldEquations === False, False, 
       If[Length[Select[fieldEquations, #1 === True & ]] == Length[matrixRepresentation], True, False]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["MomentumConstraints"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[index,#1]], 
                 newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
             Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                  Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
             Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                  First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
              newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ 
         Range[Length[newMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["ReducedMomentumConstraints"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[index,#1]], 
                  newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
              Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                   Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
              Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                   First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
               newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ 
          Range[Length[newMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
     timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_]["SymbolicMomentumConstraints"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeEinsteinTensor = 
      -(cosmologicalConstant*spacetimeMetricTensor); mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inactive[D][mixedExtrinsicCurvatureTensor[[index,#1]], 
                 newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
             Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                  Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
             Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                  First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
             Inactive[D][extrinsicCurvatureTrace, newCoordinates[[index]]] - newLapseFunction*
              mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ Range[Length[newMatrixRepresentation]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
VacuumADMSolution /: 
  MakeBoxes[vacuumADMSolution:VacuumADMSolution[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, 
        coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], cosmologicalConstant_], 
    format_] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
      shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, 
      extrinsicCurvatureTrace, spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, 
      leftHandSide, rightHandSide, evolutionEquations, solution, exactSolution, fieldEquations, 
      spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, spacetimeRicciScalar, matrixForm, icon}, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
       coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
      newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
          StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
       shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
      shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]]]]; spatialChristoffelSymbols = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
                newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                   shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
                newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                   shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                 First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]]; mixedExtrinsicCurvatureTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                 extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
       Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
      spatialRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                  newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
      spatialRicciTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
      mixedSpatialRicciTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*spatialRicciTensor[[#1,
                  Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
       Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
          (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
          (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
          ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
      leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],Last[
                index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
      rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[
                First[index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                       Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                         Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                        newMatrixRepresentation]]])*Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
               mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                  newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                     newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                        mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                    Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                         First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                Range[Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, 
                  mixedExtrinsicCurvatureTensor[[First[index],nestedIndex]]*(D[newShiftVector[[nestedIndex]], 
                     newCoordinates[[Last[index]]]] + Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*
                        newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(Module[{nestedIndex = #1}, 
                  mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*(D[newShiftVector[[First[index]]], 
                     newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                        newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - newLapseFunction*Total[(((2*cosmologicalConstant)/
                    (Length[newMatrixRepresentation] - 1))*spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*
                   Inverse[newMatrixRepresentation][[#1,First[index]]] & ) /@ Range[Length[
                   newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
      evolutionEquations = FullSimplify[Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. 
         (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]; 
      If[evolutionEquations === True, solution = True; exactSolution = True; fieldEquations = 0, 
       If[evolutionEquations === False, solution = False; exactSolution = False; fieldEquations = Indeterminate, 
        If[Length[Select[evolutionEquations, #1 === False & ]] > 0, solution = False; exactSolution = False; 
          fieldEquations = Indeterminate, If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[evolutionEquations, 
                #1 =!= True & ]]] == 0, solution = True; exactSolution = True; fieldEquations = 0, 
          solution = True; exactSolution = False; fieldEquations = Length[DeleteDuplicates[Reverse /@ Sort /@ 
                Select[evolutionEquations, #1 =!= True & ]]]]]]]; spacetimeChristoffelSymbols = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                 (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                  D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                  D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                    #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
          Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[
                index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
              Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - Total[(spacetimeChristoffelSymbols[[
                   index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; 
      spacetimeRicciTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
      spacetimeRicciScalar = Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]; 
      matrixForm = spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; 
      icon = MatrixPlot[matrixForm, ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification])}], Frame -> False, FrameTicks -> None]; 
      BoxForm`ArrangeSummaryBox["VacuumADMSolution", vacuumADMSolution, icon, 
       {{BoxForm`SummaryItem[{"Solution: ", solution}], BoxForm`SummaryItem[{"Exact Solution: ", exactSolution}]}, 
        {BoxForm`SummaryItem[{"Field Equations: ", fieldEquations}], BoxForm`SummaryItem[{"Cosmological Constant: ", 
           cosmologicalConstant}]}}, {{}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
     BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
