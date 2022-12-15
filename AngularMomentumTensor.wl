(* ::Package:: *)

AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_]] := 
  AngularMomentumTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, 
      metricIndex2], matrixRepresentation, index1, index2], (Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ 
     Range[Length[coordinates]], "\[PartialD]\[FormalCapitalOmega]", (Subscript["\[FormalD]\[FormalCapitalSigma]", ToString[coordinates[[#1]], StandardForm]] & ) /@ 
     Range[Length[coordinates]], False, False] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], index1_, 
   index2_] := AngularMomentumTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, 
      metricIndex1, metricIndex2], matrixRepresentation, stressEnergyIndex1, stressEnergyIndex2], 
    (Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ Range[Length[coordinates]], "\[PartialD]\[FormalCapitalOmega]", 
    (Subscript["\[FormalD]\[FormalCapitalSigma]", ToString[coordinates[[#1]], StandardForm]] & ) /@ Range[Length[coordinates]], index1, index2] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], positionVector_List] := 
  AngularMomentumTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, 
      metricIndex2], matrixRepresentation, index1, index2], positionVector, "\[PartialD]\[FormalCapitalOmega]", 
    (Subscript["\[FormalD]\[FormalCapitalSigma]", ToString[coordinates[[#1]], StandardForm]] & ) /@ Range[Length[coordinates]], False, False] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[positionVector]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
   positionVector_List, index1_, index2_] := 
  AngularMomentumTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, 
      metricIndex2], matrixRepresentation, stressEnergyIndex1, stressEnergyIndex2], positionVector, "\[PartialD]\[FormalCapitalOmega]", 
    (Subscript["\[FormalD]\[FormalCapitalSigma]", ToString[coordinates[[#1]], StandardForm]] & ) /@ Range[Length[coordinates]], index1, index2] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], positionVector_List, 
   spacetimeBoundary_] := AngularMomentumTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, 
      metricIndex1, metricIndex2], matrixRepresentation, index1, index2], positionVector, spacetimeBoundary, 
    (Subscript["\[FormalD]\[FormalCapitalSigma]", ToString[coordinates[[#1]], StandardForm]] & ) /@ Range[Length[coordinates]], False, False] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[positionVector]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
   positionVector_List, spacetimeBoundary_, index1_, index2_] := 
  AngularMomentumTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, 
      metricIndex2], matrixRepresentation, stressEnergyIndex1, stressEnergyIndex2], positionVector, spacetimeBoundary, 
    (Subscript["\[FormalD]\[FormalCapitalSigma]", ToString[coordinates[[#1]], StandardForm]] & ) /@ Range[Length[coordinates]], index1, index2] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], positionVector_List, 
   spacetimeBoundary_, volumeOneForm_List] := 
  AngularMomentumTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, 
      metricIndex2], matrixRepresentation, index1, index2], positionVector, spacetimeBoundary, volumeOneForm, False, 
    False] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[positionVector]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, spacetimeBoundary_, volumeOneForm_List, index1_, index2_]["MatrixRepresentation"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, newMatrixRepresentation, angularMomentumDensityTensor, 
     angularMomentumTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (newCoordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              newMatrixRepresentation[[index[[2]],index[[3]]]] - (newCoordinates[[index[[2]]]] - positionVector[[
                index[[2]]]])*newMatrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; angularMomentumTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Quiet[Integrate[Total[(angularMomentumDensityTensor[[index[[1]],
                    index[[2]],#1]]*volumeOneForm[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]], Element[
                newCoordinates, spacetimeBoundary]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]] /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; If[index1 === True && index2 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*angularMomentumTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]], If[index1 === False && index2 === False, 
       angularMomentumTensor, If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],#1]]*
                  angularMomentumTensor[[#1,Last[index]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                   angularMomentumTensor[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    Length[coordinates] == Length[volumeOneForm] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, spacetimeBoundary_, volumeOneForm_List, index1_, index2_]["ReducedMatrixRepresentation"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, newMatrixRepresentation, angularMomentumDensityTensor, 
     angularMomentumTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (newCoordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              newMatrixRepresentation[[index[[2]],index[[3]]]] - (newCoordinates[[index[[2]]]] - positionVector[[
                index[[2]]]])*newMatrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; angularMomentumTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Quiet[Integrate[Total[(angularMomentumDensityTensor[[index[[1]],
                    index[[2]],#1]]*volumeOneForm[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]], Element[
                newCoordinates, spacetimeBoundary]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]] /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; If[index1 === True && index2 === True, 
      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],
                  First[#1]]]*metricMatrixRepresentation[[Last[#1],Last[index]]]*angularMomentumTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], If[index1 === False && index2 === False, 
       FullSimplify[angularMomentumTensor], If[index1 === True && index2 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],
                    #1]]*angularMomentumTensor[[#1,Last[index]]] & ) /@ Range[Length[
                   metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], 
        If[index1 === False && index2 === True, FullSimplify[
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                    angularMomentumTensor[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], Indeterminate]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    Length[coordinates] == Length[volumeOneForm] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, spacetimeBoundary_, volumeOneForm_List, index1_, index2_]["SymbolicMatrixRepresentation"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, newMatrixRepresentation, angularMomentumDensityTensor, 
     angularMomentumTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (newCoordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              newMatrixRepresentation[[index[[2]],index[[3]]]] - (newCoordinates[[index[[2]]]] - positionVector[[
                index[[2]]]])*newMatrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; angularMomentumTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Quiet[Inactive[Integrate][Total[
                (angularMomentumDensityTensor[[index[[1]],index[[2]],#1]]*volumeOneForm[[#1]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]], Element[newCoordinates, spacetimeBoundary]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]] /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*angularMomentumTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]], If[index1 === False && index2 === False, 
       angularMomentumTensor, If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],#1]]*
                  angularMomentumTensor[[#1,Last[index]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                   angularMomentumTensor[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    Length[coordinates] == Length[volumeOneForm] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, spacetimeBoundary_, volumeOneForm_List, index1_, index2_]["Symbol"] := 
  If[index1 === True && index2 === True, Subscript["\[FormalCapitalM]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
     Superscript["\[FormalCapitalM]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, Subsuperscript["\[FormalCapitalM]", "\[FormalMu]", "\[FormalNu]"], 
      If[index1 === False && index2 === True, Subsuperscript["\[FormalCapitalM]", "\[FormalNu]", "\[FormalMu]"], Indeterminate]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    Length[coordinates] == Length[volumeOneForm] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumTensor /: 
  MakeBoxes[angularMomentumTensor:AngularMomentumTensor[(stressEnergyTensor_)[
       (metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
       matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], positionVector_List, spacetimeBoundary_, 
      volumeOneForm_List, index1_, index2_], format_] := 
   Module[{newMetricMatrixRepresentation, newCoordinates, newMatrixRepresentation, angularMomentumDensityTensor, 
      tensorRepresentation, matrixForm, type, symbol, dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, 
      signature, icon}, newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; newMatrixRepresentation = matrixRepresentation /. 
        (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; angularMomentumDensityTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> (newCoordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
               newMatrixRepresentation[[index[[2]],index[[3]]]] - (newCoordinates[[index[[2]]]] - positionVector[[
                 index[[2]]]])*newMatrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> Quiet[Integrate[Total[(angularMomentumDensityTensor[[index[[1]],
                     index[[2]],#1]]*volumeOneForm[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]], 
                Element[newCoordinates, spacetimeBoundary]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
            2]]] /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; If[index1 === True && index2 === True, 
       matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],
                    First[#1]]]*metricMatrixRepresentation[[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; type = "Covariant"; 
        symbol = Subscript["\[FormalCapitalM]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, matrixForm = tensorRepresentation; 
         type = "Contravariant"; symbol = Superscript["\[FormalCapitalM]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
         matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],
                      #1]]*tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                     metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
          type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, 
          matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,
                       Last[index]]]*tensorRepresentation[[First[index],#1]] & ) /@ Range[Length[
                      metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
           type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalNu]", "\[FormalMu]"], 
          matrixForm = ConstantArray[Indeterminate, {Length[metricMatrixRepresentation], 
              Length[metricMatrixRepresentation]}]; type = Indeterminate; symbol = Indeterminate]]]]; 
      dimensions = Length[metricMatrixRepresentation]; eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
      positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[metricMatrixRepresentation] || Length[negativeEigenvalues] == 
          Length[metricMatrixRepresentation], signature = "Riemannian", 
        If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, signature = "Lorentzian", 
         signature = "Pseudo-Riemannian"]], signature = Indeterminate]; 
      icon = MatrixPlot[matrixForm, ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification])}], Frame -> False, FrameTicks -> None]; 
      BoxForm`ArrangeSummaryBox["AngularMomentumTensor", angularMomentumTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
     BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
     BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
     Length[coordinates] == Length[volumeOneForm] && BooleanQ[index1] && BooleanQ[index2]
