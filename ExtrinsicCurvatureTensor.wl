(* ::Package:: *)

ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, 
     index2_], timeCoordinate_, lapseFunction_, shiftVector_List]] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, 
      coordinates, index1, index2], timeCoordinate, lapseFunction, shiftVector], True, True] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, 
     index2_], timeCoordinate_, lapseFunction_, shiftVector_List], newTimeCoordinate_] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
      matrixRepresentation /. timeCoordinate -> newTimeCoordinate, coordinates, index1, index2], newTimeCoordinate, 
     lapseFunction /. timeCoordinate -> newTimeCoordinate, shiftVector /. timeCoordinate -> newTimeCoordinate], True, 
    True] /; SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], newTimeCoordinate_, index1_, index2_] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
      matrixRepresentation /. timeCoordinate -> newTimeCoordinate, coordinates, metricIndex1, metricIndex2], 
     newTimeCoordinate, lapseFunction /. timeCoordinate -> newTimeCoordinate, 
     shiftVector /. timeCoordinate -> newTimeCoordinate], index1, index2] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, 
     index2_], timeCoordinate_, lapseFunction_, shiftVector_List], newLapseFunction_, newShiftVector_List] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, 
      coordinates, index1, index2], timeCoordinate, newLapseFunction, newShiftVector], True, True] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation] && 
    Length[newShiftVector] == Length[matrixRepresentation]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], newLapseFunction_, newShiftVector_, index1_, 
   index2_] := ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
      matrixRepresentation, coordinates, metricIndex1, metricIndex2], timeCoordinate, newLapseFunction, newShiftVector], 
    index1, index2] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && Length[newShiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, 
     index2_], timeCoordinate_, lapseFunction_, shiftVector_List], newTimeCoordinate_, newLapseFunction_, 
   newShiftVector_List] := ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][
     ResourceFunction["MetricTensor"][matrixRepresentation /. timeCoordinate -> newTimeCoordinate, coordinates, index1, 
      index2], newTimeCoordinate, newLapseFunction, newShiftVector], True, True] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation] && 
    Length[newShiftVector] == Length[matrixRepresentation]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], newTimeCoordinate_, newLapseFunction_, 
   newShiftVector_List, index1_, index2_] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
      matrixRepresentation /. timeCoordinate -> newTimeCoordinate, coordinates, metricIndex1, metricIndex2], 
     newTimeCoordinate, newLapseFunction, newShiftVector], index1, index2] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    Length[newShiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["MatrixRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     If[index1 === True && index2 === True, extrinsicCurvatureTensor, If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                  extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[admDecomposition] == "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "ReducedMatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     If[index1 === True && index2 === True, FullSimplify[extrinsicCurvatureTensor], 
      If[index1 === False && index2 === False, FullSimplify[
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]]], If[index1 === True && index2 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                    Last[index]]]*extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[
                   matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
        If[index1 === False && index2 === True, FullSimplify[
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]]], Indeterminate]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "SymbolicMatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
     extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[
                 First[index]]], newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],
                    First[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][
                shiftCovector[[Last[index]]], newCoordinates[[First[index]]]] - Total[
                (spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] - Inactive[D][newMatrixRepresentation[[First[index],
                 Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     If[index1 === True && index2 === True, extrinsicCurvatureTensor, If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                  extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Trace"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ReducedTrace"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SymbolicTrace"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
     extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[
                 First[index]]], newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],
                    First[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][
                shiftCovector[[Last[index]]], newCoordinates[[First[index]]]] - Total[
                (spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] - Inactive[D][newMatrixRepresentation[[First[index],
                 Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ADMDecomposition"] := 
  ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, 
     metricIndex2], timeCoordinate, lapseFunction, shiftVector] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SpatialMetricTensor"] := 
  ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SpacetimeMetricTensor"] := 
  Module[{shiftCovector}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; ResourceFunction["MetricTensor"][
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]], Join[{timeCoordinate}, coordinates], True, True]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["NormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,
                  #1]]*D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ReducedNormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     FullSimplify[
      Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,
                   #1]]*D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ 
                Range[Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SymbolicNormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,
                  #1]]*Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["TimeVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, normalVector}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*normalVector[[index]] + 
             Join[{0}, newShiftVector][[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SymbolicTimeVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, normalVector}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*normalVector[[index]] + 
             Join[{0}, newShiftVector][[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["TimeCoordinate"] := 
  timeCoordinate /; SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SpatialCoordinates"] := 
  coordinates /; SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ Join[{timeCoordinate}, coordinates] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["LapseFunction"] := 
  lapseFunction /; SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ShiftVector"] := 
  shiftVector /; SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Indices"] := 
  {index1, index2} /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["CovariantQ"] := 
  If[index1 === True && index2 === True, True, False] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ContravariantQ"] := 
  If[index1 === False && index2 === False, True, False] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["MixedQ"] := 
  If[(index1 === True && index2 === False) || (index1 === False && index2 === True), True, False] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Symbol"] := 
  If[index1 === True && index2 === True, Subscript["\[FormalCapitalK]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
     Superscript["\[FormalCapitalK]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, Subsuperscript["\[FormalCapitalK]", "\[FormalMu]", "\[FormalNu]"], 
      If[index1 === False && index2 === True, Subsuperscript["\[FormalCapitalK]", "\[FormalNu]", "\[FormalMu]"], Indeterminate]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ExtrinsicallyFlatQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, fieldEquations}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     fieldEquations = FullSimplify[Thread[Catenate[extrinsicCurvatureTensor] == 
         Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation]}]]]]; 
     If[fieldEquations === True, True, If[fieldEquations === False, False, 
       If[Length[Select[fieldEquations, #1 === True & ]] == Length[matrixRepresentation]*Length[matrixRepresentation], 
        True, False]]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "VanishingExtrinsicTraceQ"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, 
     newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],
             Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]] == 0] === True] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "ExtrinsicallyFlatConditions"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, 
     fieldEquations}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     fieldEquations = FullSimplify[Thread[Catenate[extrinsicCurvatureTensor] == 
         Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation]}]]]]; 
     If[fieldEquations === True, {}, If[fieldEquations === False, Indeterminate, 
       If[Length[Select[fieldEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[fieldEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "VanishingExtrinsicTraceCondition"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, 
     fieldEquation}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     fieldEquation = FullSimplify[Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*
            extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]] == 0]; 
     If[fieldEquation === False, Indeterminate, fieldEquation]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["CovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, newExtrinsicCurvatureTensor}, 
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
          2]]]; If[index1 === True && index2 === True, newExtrinsicCurvatureTensor = extrinsicCurvatureTensor; 
       Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                 StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalK]", StringJoin[ToString[newCoordinates[[
                   index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             D[newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(spatialChristoffelSymbols[[#1,index[[1]],index[[2]]]]*newExtrinsicCurvatureTensor[[#1,
                   index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(spatialChristoffelSymbols[[#1,index[[1]],index[[3]]]]*newExtrinsicCurvatureTensor[[index[[2]],
                   #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[Join[coordinates, {timeCoordinate}], StringQ]], If[index1 === False && index2 === False, 
       newExtrinsicCurvatureTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (Inverse[newMatrixRepresentation][[First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],
                    Last[index]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
             2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                   index[[1]]]], StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalK]", StringJoin[
                  ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                   StandardForm]]], StandardForm]] -> D[newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], 
                newCoordinates[[index[[1]]]]] + Total[(spatialChristoffelSymbols[[index[[2]],index[[1]],#1]]*
                   newExtrinsicCurvatureTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (spatialChristoffelSymbols[[index[[3]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[index[[2]],
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[Join[coordinates, {timeCoordinate}], StringQ]], If[index1 === True && index2 === False, 
        newExtrinsicCurvatureTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*extrinsicCurvatureTensor[[First[index],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalK]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               D[newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(spatialChristoffelSymbols[[index[[3]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[index[[2]],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,
                     index[[1]],index[[2]]]]*newExtrinsicCurvatureTensor[[#1,index[[3]]]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3] /. 
           (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]], 
        If[index1 === False && index2 === True, newExtrinsicCurvatureTensor = 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                     extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
           (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalK]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                D[newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(spatialChristoffelSymbols[[index[[2]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[#1,
                      index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                  (spatialChristoffelSymbols[[#1,index[[1]],index[[3]]]]*newExtrinsicCurvatureTensor[[index[[2]],
                      #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[Join[coordinates, {timeCoordinate}], StringQ]]]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "ReducedCovariantDerivatives"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, 
     newExtrinsicCurvatureTensor}, newMatrixRepresentation = matrixRepresentation /. 
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
          2]]]; If[index1 === True && index2 === True, newExtrinsicCurvatureTensor = extrinsicCurvatureTensor; 
       Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                 StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalK]", StringJoin[ToString[newCoordinates[[
                   index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             FullSimplify[D[newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - Total[
                (spatialChristoffelSymbols[[#1,index[[1]],index[[2]]]]*newExtrinsicCurvatureTensor[[#1,
                    index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (spatialChristoffelSymbols[[#1,index[[1]],index[[3]]]]*newExtrinsicCurvatureTensor[[index[[2]],
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[Join[coordinates, {timeCoordinate}], StringQ]], If[index1 === False && index2 === False, 
       newExtrinsicCurvatureTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (Inverse[newMatrixRepresentation][[First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],
                    Last[index]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
             2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                   index[[1]]]], StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalK]", StringJoin[
                  ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                   StandardForm]]], StandardForm]] -> FullSimplify[D[newExtrinsicCurvatureTensor[[index[[2]],
                  index[[3]]]], newCoordinates[[index[[1]]]]] + Total[(spatialChristoffelSymbols[[index[[2]],index[[1]],
                     #1]]*newExtrinsicCurvatureTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                Total[(spatialChristoffelSymbols[[index[[3]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[index[[2]],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[Join[coordinates, {timeCoordinate}], StringQ]], If[index1 === True && index2 === False, 
        newExtrinsicCurvatureTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*extrinsicCurvatureTensor[[First[index],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalK]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> FullSimplify[
                D[newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(spatialChristoffelSymbols[[index[[3]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[index[[2]],
                      #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,
                      index[[1]],index[[2]]]]*newExtrinsicCurvatureTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3] /. 
           (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]], 
        If[index1 === False && index2 === True, newExtrinsicCurvatureTensor = 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                     extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
           (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalK]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                FullSimplify[D[newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                  Total[(spatialChristoffelSymbols[[index[[2]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[#1,
                       index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                   (spatialChristoffelSymbols[[#1,index[[1]],index[[3]]]]*newExtrinsicCurvatureTensor[[index[[2]],
                       #1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[Join[coordinates, {timeCoordinate}], StringQ]]]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "SymbolicCovariantDerivatives"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor, 
     newExtrinsicCurvatureTensor}, newMatrixRepresentation = matrixRepresentation /. 
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
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; If[index1 === True && index2 === True, 
      newExtrinsicCurvatureTensor = extrinsicCurvatureTensor; 
       Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                 StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalK]", StringJoin[ToString[newCoordinates[[
                   index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             Inactive[D][newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(spatialChristoffelSymbols[[#1,index[[1]],index[[2]]]]*newExtrinsicCurvatureTensor[[#1,
                   index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(spatialChristoffelSymbols[[#1,index[[1]],index[[3]]]]*newExtrinsicCurvatureTensor[[index[[2]],
                   #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[Join[coordinates, {timeCoordinate}], StringQ]], If[index1 === False && index2 === False, 
       newExtrinsicCurvatureTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (Inverse[newMatrixRepresentation][[First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],
                    Last[index]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
             2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                   index[[1]]]], StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalK]", StringJoin[
                  ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                   StandardForm]]], StandardForm]] -> Inactive[D][newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], 
                newCoordinates[[index[[1]]]]] + Total[(spatialChristoffelSymbols[[index[[2]],index[[1]],#1]]*
                   newExtrinsicCurvatureTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (spatialChristoffelSymbols[[index[[3]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[index[[2]],
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[Join[coordinates, {timeCoordinate}], StringQ]], If[index1 === True && index2 === False, 
        newExtrinsicCurvatureTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*extrinsicCurvatureTensor[[First[index],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalK]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               Inactive[D][newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(spatialChristoffelSymbols[[index[[3]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[index[[2]],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,
                     index[[1]],index[[2]]]]*newExtrinsicCurvatureTensor[[#1,index[[3]]]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3] /. 
           (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]], 
        If[index1 === False && index2 === True, newExtrinsicCurvatureTensor = 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                     extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Association[
           (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalK]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                Inactive[D][newExtrinsicCurvatureTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(spatialChristoffelSymbols[[index[[2]],index[[1]],#1]]*newExtrinsicCurvatureTensor[[#1,
                      index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                  (spatialChristoffelSymbols[[#1,index[[1]],index[[3]]]]*newExtrinsicCurvatureTensor[[index[[2]],
                      #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[Join[coordinates, {timeCoordinate}], StringQ]]]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Dimensions"] := 
  Length[matrixRepresentation] + 1 /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SymmetricQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; SymmetricMatrixQ[
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
                newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                   shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
                newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                   shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                 First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["DiagonalQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     DiagonalMatrixQ[Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*
              (D[shiftCovector[[First[index]]], newCoordinates[[Last[index]]]] - Total[
                (spatialChristoffelSymbols[[#1,Last[index],First[index]]]*shiftCovector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], newCoordinates[[
                 First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[First[index],
                 Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Signature"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["RiemannianQ"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      If[Length[positiveEigenvalues] == Length[spacetimeMetricTensor] || Length[negativeEigenvalues] == 
         Length[spacetimeMetricTensor], True, False], Indeterminate]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["PseudoRiemannianQ"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      If[Length[positiveEigenvalues] == Length[spacetimeMetricTensor] || Length[negativeEigenvalues] == 
         Length[spacetimeMetricTensor], False, True], Indeterminate]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["LorentzianQ"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["RiemannianConditions"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, riemannianConditions}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; If[riemannianConditions === True, {}, 
      If[riemannianConditions === False, Indeterminate, If[Length[Select[riemannianConditions, #1 === False & ]] > 0, 
        Indeterminate, DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "PseudoRiemannianConditions"] := Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, 
     pseudoRiemannianConditions}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     pseudoRiemannianConditions = FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["LorentzianConditions"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigensystem, eigenvalues, eigenvectors, newTimeCoordinate, 
     lorentzianConditions}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigensystem = Eigensystem[spacetimeMetricTensor]; 
     eigenvalues = First[eigensystem]; eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[spacetimeMetricTensor] - 1]]]] > 0, 
      newTimeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[spacetimeMetricTensor] - 
              1]]]]]; lorentzianConditions = FullSimplify[(If[#1 == newTimeCoordinate, eigenvalues[[#1]] < 0, 
            eigenvalues[[#1]] > 0] & ) /@ Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, 
        If[lorentzianConditions === False, Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, 
          Indeterminate, DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["CurvatureSingularities"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
          2]]]; Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, Join[newCoordinates, {newTimeCoordinate}]]] === Solve, 
            {{#1}}, Solve[#1, Join[newCoordinates, {newTimeCoordinate}]]] & ) /@ 
          Flatten[{FunctionSingularities[Catenate[FullSimplify[extrinsicCurvatureTensor]], Join[newCoordinates, {
                newTimeCoordinate}]] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[shiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["TraceSingularities"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, extrinsicCurvatureTrace}, 
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
          2]]]; extrinsicCurvatureTrace = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*
          extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
            Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[FullSimplify[extrinsicCurvatureTrace], 
              newCoordinates] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Determinant"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     Det[Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
                newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                   shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
                newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                   shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                 First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ReducedDeterminant"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     FullSimplify[Det[Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(
                D[shiftCovector[[First[index]]], newCoordinates[[Last[index]]]] - Total[
                 (spatialChristoffelSymbols[[#1,Last[index],First[index]]]*shiftCovector[[#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], newCoordinates[[
                  First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[First[index],
                  Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["SymbolicDeterminant"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
     Det[Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[
                 First[index]]], newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],
                    First[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][
                shiftCovector[[Last[index]]], newCoordinates[[First[index]]]] - Total[
                (spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] - Inactive[D][newMatrixRepresentation[[First[index],
                 Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Eigenvalues"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     Eigenvalues[Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*
              (D[shiftCovector[[First[index]]], newCoordinates[[Last[index]]]] - Total[
                (spatialChristoffelSymbols[[#1,Last[index],First[index]]]*shiftCovector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], newCoordinates[[
                 First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[First[index],
                 Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ReducedEigenvalues"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     FullSimplify[Eigenvalues[Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(
                D[shiftCovector[[First[index]]], newCoordinates[[Last[index]]]] - Total[
                 (spatialChristoffelSymbols[[#1,Last[index],First[index]]]*shiftCovector[[#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], newCoordinates[[
                  First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[First[index],
                  Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Eigenvectors"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     Eigenvectors[Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*
              (D[shiftCovector[[First[index]]], newCoordinates[[Last[index]]]] - Total[
                (spatialChristoffelSymbols[[#1,Last[index],First[index]]]*shiftCovector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], newCoordinates[[
                 First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*shiftCovector[[
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[First[index],
                 Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["ReducedEigenvectors"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
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
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     FullSimplify[Eigenvectors[
       Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
                 newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                    shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
                 newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                    shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                  First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
            2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "CovariantExtrinsicCurvatureTensor"] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, 
      coordinates, metricIndex1, metricIndex2], timeCoordinate, lapseFunction, shiftVector], True, True] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][
   "ContravariantExtrinsicCurvatureTensor"] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, 
      coordinates, metricIndex1, metricIndex2], timeCoordinate, lapseFunction, shiftVector], False, False] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, 
      coordinates_List, metricIndex1_, metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, 
    index2_], newTimeCoordinate_] := ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][
     ResourceFunction["MetricTensor"][matrixRepresentation /. timeCoordinate -> newTimeCoordinate, coordinates, 
      metricIndex1, metricIndex2], newTimeCoordinate, lapseFunction, shiftVector], index1, index2] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, 
      coordinates_List, metricIndex1_, metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, 
    index2_], newLapseFunction_, newShiftVector_List] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, 
      coordinates, metricIndex1, metricIndex2], timeCoordinate, newLapseFunction, newShiftVector], index1, index2] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    Length[newShiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, 
      coordinates_List, metricIndex1_, metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, 
    index2_], newTimeCoordinate_, newLapseFunction_, newShiftVector_List] := 
  ExtrinsicCurvatureTensor[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
      matrixRepresentation /. timeCoordinate -> newTimeCoordinate, coordinates, metricIndex1, metricIndex2], 
     newTimeCoordinate, newLapseFunction, newShiftVector], index1, index2] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    Length[newShiftVector] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_]["Properties"] := 
  {"MatrixRepresentation", "ReducedMatrixRepresentation", "SymbolicMatrixRepresentation", "Trace", "ReducedTrace", 
    "SymbolicTrace", "ADMDecomposition", "SpatialMetricTensor", "SpacetimeMetricTensor", "NormalVector", 
    "ReducedNormalVector", "TimeVector", "SymbolicTimeVector", "TimeCoordinate", "SpatialCoordinates", 
    "CoordinateOneForms", "LapseFunction", "ShiftVector", "Indices", "CovariantQ", "ContravariantQ", "MixedQ", "Symbol", 
    "ExtrinsicallyFlatQ", "VanishingExtrinsicTraceQ", "ExtrinsicallyFlatConditions", "VanishingExtrinsicTraceCondition", 
    "CovariantDerivatives", "ReducedCovariantDerivatives", "SymbolicCovariantDerivatives", "Dimensions", "SymmetricQ", 
    "DiagonalQ", "Signature", "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", "RiemannianConditions", 
    "PseudoRiemannianConditions", "LorentzianConditions", "CurvatureSingularities", "TraceSingularities", "Determinant", 
    "ReducedDeterminant", "SymbolicDeterminant", "Eigenvalues", "ReducedEigenvalues", "Eigenvectors", 
    "ReducedEigenvectors", "CovariantExtrinsicCurvatureTensor", "ContravariantExtrinsicCurvatureTensor", "Properties"} /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor /: 
  MakeBoxes[extrinsicCurvatureTensor:ExtrinsicCurvatureTensor[(admDecomposition_)[
       (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], timeCoordinate_, 
       lapseFunction_, shiftVector_List], index1_, index2_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
      spatialChristoffelSymbols, tensorRepresentation, matrixForm, type, symbol, spacetimeMetricTensor, dimensions, 
      eigenvalues, positiveEigenvalues, negativeEigenvalues, signature, icon}, 
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
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
                 newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                    shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
                 newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                    shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                  First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
            2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
      If[index1 === True && index2 === True, matrixForm = tensorRepresentation; type = "Covariant"; 
        symbol = Subscript["\[FormalCapitalK]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
        matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                     First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                     Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Contravariant"; 
         symbol = Superscript["\[FormalCapitalK]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
         matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                      Last[index]]]*tensorRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
          symbol = Subsuperscript["\[FormalCapitalK]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, 
          matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                       First[index],#1]]*tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                      matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
           symbol = Subsuperscript["\[FormalCapitalK]", "\[FormalNu]", "\[FormalMu]"], matrixForm = ConstantArray[Indeterminate, 
             {Length[matrixRepresentation], Length[matrixRepresentation]}]; type = Indeterminate; 
           symbol = Indeterminate]]]]; spacetimeMetricTensor = 
       Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
           (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
           (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
           ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 2]]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]; dimensions = Length[matrixRepresentation]; 
      eigenvalues = Eigenvalues[spacetimeMetricTensor]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
       If[Length[positiveEigenvalues] == Length[spacetimeMetricTensor] || Length[negativeEigenvalues] == 
          Length[spacetimeMetricTensor], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[matrixForm, ImageSize -> 
         Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], Frame -> False, 
        FrameTicks -> None]; BoxForm`ArrangeSummaryBox["ExtrinsicCurvatureTensor", extrinsicCurvatureTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Time Coordinate: ", timeCoordinate}], BoxForm`SummaryItem[
          {"Spatial Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
     BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
     BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][row_Integer, 
   column_Integer] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, 
     newShiftVector, shiftCovector, spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     If[index1 === True && index2 === True, extrinsicCurvatureTensor[[row,column]], 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]][[row,column]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                   extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]][[row,column]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]][[row,column]], Indeterminate]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][row_Integer, All] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     If[index1 === True && index2 === True, extrinsicCurvatureTensor[[row,All]], If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]][[row,All]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                   extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]][[row,All]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]][[row,All]], Indeterminate]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ExtrinsicCurvatureTensor[(admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
      metricIndex2_], timeCoordinate_, lapseFunction_, shiftVector_List], index1_, index2_][All, column_Integer] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor}, 
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
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     If[index1 === True && index2 === True, Transpose[{extrinsicCurvatureTensor[[All,column]]}], 
      If[index1 === False && index2 === False, Transpose[
        {Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                    Inverse[matrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                     Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]][[All,column]]}], If[index1 === True && index2 === False, 
        Transpose[{Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                      Last[index]]]*extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[
                     matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]][[All,column]]}], 
        If[index1 === False && index2 === True, Transpose[
          {Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                      extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
               Tuples[Range[Length[matrixRepresentation]], 2]]][[All,column]]}], Indeterminate]]]]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[shiftVector] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
