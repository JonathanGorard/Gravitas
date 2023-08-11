(* ::Package:: *)

SolveElectrovacuumEinsteinEquations[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_]] := 
  ElectrovacuumSolution[ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
      metricIndex1, metricIndex2], electromagneticPotential, vacuumPermeability, index1, index2], "\[FormalCapitalLambda]"] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
SolveElectrovacuumEinsteinEquations[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
   (metricTensor_)[newMatrixRepresentation_List, newCoordinates_List, newMetricIndex1_, newMetricIndex2_]] := 
  ElectrovacuumSolution[ElectromagneticTensor[ResourceFunction["MetricTensor"][newMatrixRepresentation, newCoordinates, 
      newMetricIndex1, newMetricIndex2], electromagneticPotential /. Thread[coordinates -> newCoordinates], 
     vacuumPermeability /. Thread[coordinates -> newCoordinates], index1, index2], "\[FormalCapitalLambda]"] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[Dimensions[newMatrixRepresentation]] == 2 && Length[newCoordinates] == Length[newMatrixRepresentation] && 
    BooleanQ[newMetricIndex1] && BooleanQ[newMetricIndex2] && Length[electromagneticPotential] == 
     Length[newMatrixRepresentation]
SolveElectrovacuumEinsteinEquations[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
   cosmologicalConstant_] := ElectrovacuumSolution[ElectromagneticTensor[ResourceFunction["MetricTensor"][
      matrixRepresentation, coordinates, metricIndex1, metricIndex2], electromagneticPotential, vacuumPermeability, 
     index1, index2], cosmologicalConstant] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
SolveElectrovacuumEinsteinEquations[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
   (metricTensor_)[newMatrixRepresentation_List, newCoordinates_List, newMetricIndex1_, newMetricIndex2_], 
   cosmologicalConstant_] := ElectrovacuumSolution[ElectromagneticTensor[ResourceFunction["MetricTensor"][
      newMatrixRepresentation, newCoordinates, newMetricIndex1, newMetricIndex2], 
     electromagneticPotential /. Thread[coordinates -> newCoordinates], vacuumPermeability /. 
      Thread[coordinates -> newCoordinates], index1, index2], cosmologicalConstant] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[Dimensions[newMatrixRepresentation]] == 2 && Length[newCoordinates] == Length[newMatrixRepresentation] && 
    BooleanQ[newMetricIndex1] && BooleanQ[newMetricIndex2] && Length[electromagneticPotential] == 
     Length[newMatrixRepresentation]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["FieldEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, covariantElectromagneticPotential, tensorRepresentation, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, covariantStressEnergyTensor, 
     einsteinEquations}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[First[index],First[#1]]]*
                matrixRepresentation[[Last[#1],Last[index]]]*stressEnergyTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     einsteinEquations = FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
           cosmologicalConstant*matrixRepresentation] == Catenate[(8*Pi)*covariantStressEnergyTensor]]]; 
     If[einsteinEquations === True, {}, If[einsteinEquations === False, Indeterminate, 
       If[Length[Select[einsteinEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["EinsteinEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, covariantElectromagneticPotential, tensorRepresentation, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, covariantStressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[First[index],First[#1]]]*
                matrixRepresentation[[Last[#1],Last[index]]]*stressEnergyTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + cosmologicalConstant*matrixRepresentation] == 
       Catenate[(8*Pi)*covariantStressEnergyTensor]]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ReducedEinsteinEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, covariantElectromagneticPotential, tensorRepresentation, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, covariantStressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[First[index],First[#1]]]*
                matrixRepresentation[[Last[#1],Last[index]]]*stressEnergyTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
          cosmologicalConstant*matrixRepresentation] == Catenate[(8*Pi)*covariantStressEnergyTensor]]]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["SymbolicEinsteinEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, covariantElectromagneticPotential, tensorRepresentation, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, covariantStressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[First[index],First[#1]]]*
                matrixRepresentation[[Last[#1],Last[index]]]*stressEnergyTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + cosmologicalConstant*matrixRepresentation] == 
       Catenate[(8*Pi)*covariantStressEnergyTensor]]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ContinuityEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, tensorRepresentation, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[newElectromagneticPotential]]] - 
              (1/4)*Total[(Inverse[newMatrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, D[stressEnergyTensor[[nestedIndex,index]], 
                newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                   stressEnergyTensor[[#1,index]] & ) /@ Range[Length[newElectromagneticPotential]]] + Total[
                (christoffelSymbols[[index,nestedIndex,#1]]*stressEnergyTensor[[nestedIndex,#1]] & ) /@ 
                 Range[Length[newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 
          0] & ) /@ Range[Length[newElectromagneticPotential]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ReducedContinuityEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, tensorRepresentation, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[newElectromagneticPotential]]] - 
              (1/4)*Total[(Inverse[newMatrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     FullSimplify[(Module[{index = #1}, Total[(Module[{nestedIndex = #1}, D[stressEnergyTensor[[nestedIndex,index]], 
                 newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                    stressEnergyTensor[[#1,index]] & ) /@ Range[Length[newElectromagneticPotential]]] + 
                Total[(christoffelSymbols[[index,nestedIndex,#1]]*stressEnergyTensor[[nestedIndex,#1]] & ) /@ 
                  Range[Length[newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 
           0] & ) /@ Range[Length[newElectromagneticPotential]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["SymbolicContinuityEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, tensorRepresentation, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[newElectromagneticPotential]]] - 
              (1/4)*Total[(Inverse[newMatrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, Inactive[D][stressEnergyTensor[[nestedIndex,index]], 
                newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                   stressEnergyTensor[[#1,index]] & ) /@ Range[Length[newElectromagneticPotential]]] + Total[
                (christoffelSymbols[[index,nestedIndex,#1]]*stressEnergyTensor[[nestedIndex,#1]] & ) /@ 
                 Range[Length[newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 
          0] & ) /@ Range[Length[newElectromagneticPotential]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["InhomogeneousMaxwellEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, tensorRepresentation, contravariantElectromagneticTensor, 
     electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*contravariantElectromagneticTensor; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index,#1]], newCoordinates[[
                 #1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, D[contravariantElectromagneticTensor[[index,nestedIndex]], 
                newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[index,nestedIndex,#1]]*
                   contravariantElectromagneticTensor[[#1,nestedIndex]] & ) /@ Range[Length[
                   newElectromagneticPotential]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                   contravariantElectromagneticTensor[[index,#1]] & ) /@ Range[Length[
                   newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 
          vacuumPermeability*currentDensity[[index]]] & ) /@ Range[Length[newElectromagneticPotential]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ReducedInhomogeneousMaxwellEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, tensorRepresentation, contravariantElectromagneticTensor, 
     electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*contravariantElectromagneticTensor; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index,#1]], newCoordinates[[
                 #1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     FullSimplify[(Module[{index = #1}, Total[(Module[{nestedIndex = #1}, D[contravariantElectromagneticTensor[[index,
                  nestedIndex]], newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[index,nestedIndex,#1]]*
                    contravariantElectromagneticTensor[[#1,nestedIndex]] & ) /@ Range[Length[
                    newElectromagneticPotential]]] + Total[(christoffelSymbols[[nestedIndex,nestedIndex,#1]]*
                    contravariantElectromagneticTensor[[index,#1]] & ) /@ Range[Length[
                    newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 
           vacuumPermeability*currentDensity[[index]]] & ) /@ Range[Length[newElectromagneticPotential]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ChargeConservationEquation"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, tensorRepresentation, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index,#1]], newCoordinates[[
                 #1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     Total[(Module[{index = #1}, D[currentDensity[[index]], newCoordinates[[index]]] + 
            Total[(christoffelSymbols[[index,index,#1]]*currentDensity[[#1]] & ) /@ Range[Length[
                newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 0 /. 
      (ToExpression[#1] & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ReducedChargeConservationEquation"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, tensorRepresentation, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index,#1]], newCoordinates[[
                 #1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     FullSimplify[Total[(Module[{index = #1}, D[currentDensity[[index]], newCoordinates[[index]]] + 
             Total[(christoffelSymbols[[index,index,#1]]*currentDensity[[#1]] & ) /@ Range[
                Length[newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 0 /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["MetricTensor"] := ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
    metricIndex1, metricIndex2] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ElectromagneticTensor"] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    electromagneticPotential, vacuumPermeability, index1, index2] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ElectromagneticStressEnergyTensor"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     tensorRepresentation, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; ResourceFunction["StressEnergyTensor"][
      ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      stressEnergyTensor, False, False]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["Coordinates"] := 
  coordinates /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeablity_, index1_, index2_], 
    cosmologicalConstant_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["SolutionQ"] := Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, 
     christoffelSymbols, riemannTensor, ricciTensor, ricciScalar, covariantElectromagneticPotential, 
     tensorRepresentation, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, 
     covariantStressEnergyTensor, einsteinEquations}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[First[index],First[#1]]]*
                matrixRepresentation[[Last[#1],Last[index]]]*stressEnergyTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     einsteinEquations = FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
           cosmologicalConstant*matrixRepresentation] == Catenate[(8*Pi)*covariantStressEnergyTensor]]]; 
     If[einsteinEquations === True, True, If[einsteinEquations === False, False, 
       If[Length[Select[einsteinEquations, #1 === False & ]] > 0, False, True]]]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["ExactSolutionQ"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, riemannTensor, 
     ricciTensor, ricciScalar, covariantElectromagneticPotential, tensorRepresentation, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, covariantStressEnergyTensor, 
     einsteinEquations}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
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
        Tuples[Range[Length[matrixRepresentation]], 2]]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[First[index],First[#1]]]*
                matrixRepresentation[[Last[#1],Last[index]]]*stressEnergyTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     einsteinEquations = FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
           cosmologicalConstant*matrixRepresentation] == Catenate[(8*Pi)*covariantStressEnergyTensor]]]; 
     If[einsteinEquations === True, True, If[einsteinEquations === False, False, 
       If[Length[Select[einsteinEquations, #1 === False & ]] > 0, False, 
        If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, #1 =!= True & ]]] == 0, True, 
         False]]]]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["CosmologicalConstant"] := cosmologicalConstant /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeablity_, index1_, index2_], 
    cosmologicalConstant_]["Dimensions"] := Length[matrixRepresentation] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["Signature"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[electromagneticTensor] === "ElectromagneticTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution[(electromagneticTensor_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], 
    cosmologicalConstant_]["RiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], True, False], Indeterminate]] /; 
   SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectrovacuumSolution /: 
  MakeBoxes[electrovacuumSolution:ElectrovacuumSolution[(electromagneticTensor_)[
       (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
       electromagneticPotential_List, vacuumPermeability_, index1_, index2_], cosmologicalConstant_], format_] := 
   Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, riemannTensor, 
      ricciTensor, ricciScalar, matrixForm, covariantElectromagneticPotential, tensorRepresentation, 
      contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, covariantStressEnergyTensor, 
      einsteinEquations, solution, exactSolution, fieldEquations, icon}, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; christoffelSymbols = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
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
      covariantElectromagneticPotential = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newElectromagneticPotential[[
                  #1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
          Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
       Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
              D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
           Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],Last[#1]]] & ) /@ 
               Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; mixedElectromagneticTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                 tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
       (1/vacuumPermeability)*Normal[SparseArray[
          (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                   mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - (1/4)*
                Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*tensorRepresentation[[First[#1],
                     Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                  Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
           Tuples[Range[Length[electromagneticPotential]], 2]]]; covariantStressEnergyTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[First[index],First[#1]]]*
                 matrixRepresentation[[Last[#1],Last[index]]]*stressEnergyTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
      einsteinEquations = FullSimplify[Thread[Catenate[ricciTensor - (1/2)*ricciScalar*matrixRepresentation + 
            cosmologicalConstant*matrixRepresentation] == Catenate[(8*Pi)*covariantStressEnergyTensor]]]; 
      If[einsteinEquations === True, solution = True; exactSolution = True; fieldEquations = 0, 
       If[einsteinEquations === False, solution = False; exactSolution = False; fieldEquations = Indeterminate, 
        If[Length[Select[einsteinEquations, #1 === False & ]] > 0, solution = False; exactSolution = False; 
          fieldEquations = Indeterminate, If[Length[DeleteDuplicates[Reverse /@ Sort /@ Select[einsteinEquations, 
                #1 =!= True & ]]] == 0, solution = True; exactSolution = True; fieldEquations = 0, 
          solution = True; exactSolution = False; fieldEquations = Length[DeleteDuplicates[Reverse /@ Sort /@ 
                Select[einsteinEquations, #1 =!= True & ]]]]]]]; 
      icon = MatrixPlot[matrixForm, ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification])}], Frame -> False, FrameTicks -> None]; 
      BoxForm`ArrangeSummaryBox["ElectrovacuumSolution", electrovacuumSolution, icon, 
       {{BoxForm`SummaryItem[{"Solution: ", solution}], BoxForm`SummaryItem[{"Exact Solution: ", exactSolution}]}, 
        {BoxForm`SummaryItem[{"Field Equations: ", fieldEquations}], BoxForm`SummaryItem[{"Cosmological Constant: ", 
           cosmologicalConstant}]}}, {{}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[electromagneticTensor] === "ElectromagneticTensor" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
     BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
      Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
