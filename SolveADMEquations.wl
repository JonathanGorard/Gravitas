(* ::Package:: *)

SolveADMEquations[(admStressEnergyDecomposition_)[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, 
      coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], 
    matrixRepresentation_List]] := 
  ADMSolution[ADMStressEnergyDecomposition[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
       metricMatrixRepresentation, coordinates, index1, index2], timeCoordinate, lapseFunction, shiftVector], 
     matrixRepresentation], "\[FormalCapitalLambda]"] /; SymbolName[admStressEnergyDecomposition] === "ADMStressEnergyDecomposition" && 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
SolveADMEquations[(admStressEnergyDecomposition_)[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, 
      coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], 
    matrixRepresentation_List], cosmologicalConstant_] := 
  ADMSolution[ADMStressEnergyDecomposition[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
       metricMatrixRepresentation, coordinates, index1, index2], timeCoordinate, lapseFunction, shiftVector], 
     matrixRepresentation], cosmologicalConstant] /; SymbolName[admStressEnergyDecomposition] === 
     "ADMStressEnergyDecomposition" && SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[metricMatrixRepresentation] && Length[coordinates] + 1 == Length[matrixRepresentation]
ADMSolution[(admStressEnergyDecomposition_)[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, 
       coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], 
     matrixRepresentation_List], cosmologicalConstant_]["SolutionQ"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, 
     newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, 
     extrinsicCurvatureTrace, spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, 
     stressEnergyTrace, leftHandSide, rightHandSide, evolutionEquations}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spatialChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                 #1]]*(D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + D[shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              D[newMetricMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     spatialRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     stressEnergyTrace = Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*
          newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMetricMatrixRepresentation]]])*Inverse[newMetricMatrixRepresentation][[nestedIndex,
                   First[index]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + newLapseFunction*
              extrinsicCurvatureTrace*mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + 
             Total[(Module[{nestedIndex = #1}, newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[
                     First[index],Last[index]]], newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[
                        First[index],nestedIndex,#1]]*mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*mixedExtrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[
                       newMetricMatrixRepresentation]]])] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[First[index],nestedIndex]]*
                  (D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + Total[
                    (spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]])] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   nestedIndex,Last[index]]]*(D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + 
                   Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]])] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - newLapseFunction*(Total[(Module[{nestedIndex = #1}, 
                   8*Pi*newMatrixRepresentation[[nestedIndex + 1,Last[index] + 1]]*Inverse[
                      newMetricMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]] - 4*Pi*stressEnergyTrace*KroneckerDelta[First[index], 
                 Last[index]]) - newLapseFunction*Total[(((2*cosmologicalConstant)/
                   (Length[newMetricMatrixRepresentation] - 1))*spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*
                  Inverse[newMetricMatrixRepresentation][[#1,First[index]]] & ) /@ Range[Length[
                  newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     evolutionEquations = FullSimplify[Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]; 
     If[evolutionEquations === True, True, If[evolutionEquations === False, False, 
       If[Length[Select[evolutionEquations, #1 === False & ]] > 0, False, True]]]] /; 
   SymbolName[admStressEnergyDecomposition] === "ADMStressEnergyDecomposition" && 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMSolution[(admStressEnergyDecomposition_)[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, 
       coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], 
     matrixRepresentation_List], cosmologicalConstant_]["ExactSolutionQ"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, 
     newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, 
     extrinsicCurvatureTrace, spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, 
     stressEnergyTrace, leftHandSide, rightHandSide, evolutionEquations}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spatialChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                 #1]]*(D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + D[shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              D[newMetricMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     spatialRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     stressEnergyTrace = Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*
          newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMetricMatrixRepresentation]]])*Inverse[newMetricMatrixRepresentation][[nestedIndex,
                   First[index]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + newLapseFunction*
              extrinsicCurvatureTrace*mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + 
             Total[(Module[{nestedIndex = #1}, newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[
                     First[index],Last[index]]], newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[
                        First[index],nestedIndex,#1]]*mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*mixedExtrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[
                       newMetricMatrixRepresentation]]])] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[First[index],nestedIndex]]*
                  (D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + Total[
                    (spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]])] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   nestedIndex,Last[index]]]*(D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + 
                   Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]])] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - newLapseFunction*(Total[(Module[{nestedIndex = #1}, 
                   8*Pi*newMatrixRepresentation[[nestedIndex + 1,Last[index] + 1]]*Inverse[
                      newMetricMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]] - 4*Pi*stressEnergyTrace*KroneckerDelta[First[index], 
                 Last[index]]) - newLapseFunction*Total[(((2*cosmologicalConstant)/
                   (Length[newMetricMatrixRepresentation] - 1))*spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*
                  Inverse[newMetricMatrixRepresentation][[#1,First[index]]] & ) /@ Range[Length[
                  newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     evolutionEquations = FullSimplify[Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]; 
     If[evolutionEquations === True, True, If[evolutionEquations === False, False, 
       If[Length[Select[evolutionEquations, #1 === False & ]] > 0, False, 
        If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[evolutionEquations, #1 =!= True & ]]] == 0, True, 
         False]]]]] /; SymbolName[admStressEnergyDecomposition] === "ADMStressEnergyDecomposition" && 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMSolution[(admStressEnergyDecomposition_)[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, 
       coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], 
     matrixRepresentation_List], cosmologicalConstant_]["EvolutionEquations"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, 
     newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, 
     extrinsicCurvatureTrace, spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, 
     stressEnergyTrace, leftHandSide, rightHandSide}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spatialChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                 #1]]*(D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + D[shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              D[newMetricMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     spatialRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     stressEnergyTrace = Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*
          newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMetricMatrixRepresentation]]])*Inverse[newMetricMatrixRepresentation][[nestedIndex,
                   First[index]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + newLapseFunction*
              extrinsicCurvatureTrace*mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + 
             Total[(Module[{nestedIndex = #1}, newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[
                     First[index],Last[index]]], newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[
                        First[index],nestedIndex,#1]]*mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*mixedExtrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[
                       newMetricMatrixRepresentation]]])] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[First[index],nestedIndex]]*
                  (D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + Total[
                    (spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]])] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   nestedIndex,Last[index]]]*(D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + 
                   Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMetricMatrixRepresentation]]])] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - newLapseFunction*(Total[(Module[{nestedIndex = #1}, 
                   8*Pi*newMatrixRepresentation[[nestedIndex + 1,Last[index] + 1]]*Inverse[
                      newMetricMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]] - 4*Pi*stressEnergyTrace*KroneckerDelta[First[index], 
                 Last[index]]) - newLapseFunction*Total[(((2*cosmologicalConstant)/
                   (Length[newMetricMatrixRepresentation] - 1))*spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*
                  Inverse[newMetricMatrixRepresentation][[#1,First[index]]] & ) /@ Range[Length[
                  newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admStressEnergyDecomposition] === "ADMStressEnergyDecomposition" && 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMSolution[(admStressEnergyDecomposition_)[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, 
       coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], 
     matrixRepresentation_List], cosmologicalConstant_]["HamiltonianConstraint"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, 
     newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, 
     extrinsicCurvatureTrace, spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, 
     spacetimeEinsteinTensor, contravariantSpacetimeEinsteinTensor}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spatialChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                 #1]]*(D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + D[shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              D[newMetricMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     spatialRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          2]]]; spatialRicciScalar = Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*
          spatialRicciTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[
                newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     spacetimeEinsteinTensor = 8*Pi*newMatrixRepresentation - cosmologicalConstant*spacetimeMetricTensor; 
     contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     (spatialRicciScalar + extrinsicCurvatureTrace^2 - Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
            mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]) - 2*newLapseFunction^2*
        contravariantSpacetimeEinsteinTensor[[1,1]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admStressEnergyDecomposition] === "ADMStressEnergyDecomposition" && 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMSolution[(admStressEnergyDecomposition_)[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, 
       coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], 
     matrixRepresentation_List], cosmologicalConstant_]["MomentumConstraints"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, 
     newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, 
     extrinsicCurvatureTrace, spacetimeMetricTensor, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spatialChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                 #1]]*(D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + D[shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              D[newMetricMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[
                newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     spacetimeEinsteinTensor = 8*Pi*newMatrixRepresentation - cosmologicalConstant*spacetimeMetricTensor; 
     mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[index,#1]], 
                 newCoordinates[[#1]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
             Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                  Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] - 
             Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                  First[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] - 
             D[extrinsicCurvatureTrace, newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[index + 
                1,1]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admStressEnergyDecomposition] === "ADMStressEnergyDecomposition" && 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMSolution /: MakeBoxes[admSolution:ADMSolution[(admStressEnergyDecomposition_)[
       (admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, index1_, index2_], 
        timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List], cosmologicalConstant_], 
    format_] := Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
      newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, 
      mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, spatialRiemannTensor, spatialRicciTensor, 
      mixedSpatialRicciTensor, spacetimeMetricTensor, stressEnergyTrace, leftHandSide, rightHandSide, evolutionEquations, 
      solution, exactSolution, fieldEquations, spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, 
      spacetimeRicciScalar, matrixForm, icon}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
        (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
      newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
       coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
      newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
          StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
       shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
      shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]]]]; spatialChristoffelSymbols = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                  #1]]*(D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                  D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                  D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ Range[
                Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
           3]]]; extrinsicCurvatureTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
                newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                   shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + D[shiftCovector[[
                 Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                    Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - D[
                newMetricMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                 extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
       Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
      spatialRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,
                   index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4]]]; spatialRicciTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                 Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedSpatialRicciTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                 spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
       Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[
                Length[newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
          (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                   #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
           Range[Length[newMetricMatrixRepresentation]], 
          (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                   #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
           Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
              First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
      stressEnergyTrace = Total[(Inverse[newMetricMatrixRepresentation][[First[#1],Last[#1]]]*
           newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]; 
      leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],Last[
                index]]], newTimeCoordinatee]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
      rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[
                First[index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                       Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                         Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                        newMetricMatrixRepresentation]]])*Inverse[newMetricMatrixRepresentation][[nestedIndex,
                    First[index]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + newLapseFunction*
               extrinsicCurvatureTrace*mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + 
              Total[(Module[{nestedIndex = #1}, newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[
                      First[index],Last[index]]], newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[
                         First[index],nestedIndex,#1]]*mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ 
                      Range[Length[newMetricMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                         Last[index]]]*mixedExtrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[
                        newMetricMatrixRepresentation]]])] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
              Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[First[index],nestedIndex]]*
                   (D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + Total[(spatialChristoffelSymbols[[
                         nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ Range[Length[
                        newMetricMatrixRepresentation]]])] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*
                   (D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + Total[
                     (spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                      Range[Length[newMetricMatrixRepresentation]]])] & ) /@ Range[Length[
                  newMetricMatrixRepresentation]]] - newLapseFunction*(Total[(Module[{nestedIndex = #1}, 
                    8*Pi*newMatrixRepresentation[[nestedIndex + 1,Last[index] + 1]]*Inverse[
                       newMetricMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ Range[Length[
                    newMetricMatrixRepresentation]]] - 4*Pi*stressEnergyTrace*KroneckerDelta[First[index], 
                  Last[index]]) - newLapseFunction*Total[(((2*cosmologicalConstant)/(Length[
                      newMetricMatrixRepresentation] - 1))*spacetimeMetricTensor[[#1 + 1,Last[index] + 1]]*
                   Inverse[newMetricMatrixRepresentation][[#1,First[index]]] & ) /@ Range[Length[
                   newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
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
      BoxForm`ArrangeSummaryBox["ADMSolution", admSolution, icon, {{BoxForm`SummaryItem[{"Solution: ", solution}], 
         BoxForm`SummaryItem[{"Exact Solution: ", exactSolution}]}, 
        {BoxForm`SummaryItem[{"Field Equations: ", fieldEquations}], BoxForm`SummaryItem[{"Cosmological Constant: ", 
           cosmologicalConstant}]}}, {{}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[admStressEnergyDecomposition] === "ADMStressEnergyDecomposition" && 
     SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
     BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
     Length[coordinates] + 1 == Length[matrixRepresentation]
