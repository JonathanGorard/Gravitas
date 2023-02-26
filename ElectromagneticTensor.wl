(* ::Package:: *)

ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Join[{"\[FormalCapitalPhi]"}, (Superscript["\[FormalCapitalA]", ToString[#1]] & ) /@ Range[Length[matrixRepresentation] - 1]], 
    Subscript["\[FormalMu]", "0"], True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   index1_, index2_] := ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], Join[{"\[FormalCapitalPhi]"}, (Superscript["\[FormalCapitalA]", ToString[#1]] & ) /@ 
      Range[Length[matrixRepresentation] - 1]], Subscript["\[FormalMu]", "0"], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   electromagneticPotential_List] := ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], electromagneticPotential, Subscript["\[FormalMu]", "0"], True, True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   electromagneticPotential_List, index1_, index2_] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    electromagneticPotential, Subscript["\[FormalMu]", "0"], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   electromagneticPotential_List, vacuumPermeability_] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    electromagneticPotential, vacuumPermeability, True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == Length[matrixRepresentation]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["MatrixRepresentation"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
             D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True, electromagneticTensor, 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
               Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                  electromagneticTensor[[First[index],#1]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
           Tuples[Range[Length[electromagneticPotential]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   electromagneticTensor[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
            Tuples[Range[Length[electromagneticPotential]], 2]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedMatrixRepresentation"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
             D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True, FullSimplify[electromagneticTensor], 
      If[index1 === False && index2 === False, FullSimplify[
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
           Tuples[Range[Length[electromagneticPotential]], 2]]]], If[index1 === True && index2 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                    Last[index]]]*electromagneticTensor[[First[index],#1]] & ) /@ Range[Length[
                   electromagneticPotential]]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]]], 
        If[index1 === False && index2 === True, FullSimplify[
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    electromagneticTensor[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
             Tuples[Range[Length[electromagneticPotential]], 2]]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicMatrixRepresentation"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True, electromagneticTensor, 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
               Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                  electromagneticTensor[[First[index],#1]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
           Tuples[Range[Length[electromagneticPotential]], 2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   electromagneticTensor[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
            Tuples[Range[Length[electromagneticPotential]], 2]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ElectricField"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, covariantElectricField}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
             D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; covariantElectricField = 
      Normal[SparseArray[(#1 -> electromagneticTensor[[1,#1 + 1]] & ) /@ Range[Length[electromagneticPotential] - 1]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index + 1,#1 + 1]]*
               covariantElectricField[[#1]] & ) /@ Range[Length[electromagneticPotential] - 1]]] & ) /@ 
        Range[Length[electromagneticPotential] - 1]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["MagneticField"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, leviCivitaTensor, covariantMagneticField}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
             D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; leviCivitaTensor = Normal[LeviCivitaTensor[3]]; covariantMagneticField = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((-(1/2))*(leviCivitaTensor[[index,First[#1],Last[#1]]]*
                 contravariantElectromagneticTensor[[First[#1] + 1,Last[#1] + 1]]) & ) /@ Tuples[Range[
                Length[electromagneticPotential] - 1], 2]]] & ) /@ Range[Length[electromagneticPotential] - 1]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index + 1,#1 + 1]]*
               covariantMagneticField[[#1]] & ) /@ Range[Length[electromagneticPotential] - 1]]] & ) /@ 
        Range[Length[electromagneticPotential] - 1]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ChargeDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
            D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     Total[(D[electromagneticDisplacementTensor[[1,#1]], newCoordinates[[#1]]] & ) /@ 
        Range[Length[newElectromagneticPotential]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["CurrentDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
            D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index + 1,#1]], 
                newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential] - 1]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SpacetimeCurrentDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
            D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index,#1]], 
                newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ElectromagneticStressEnergyTensor"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
             D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; mixedElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                electromagneticTensor[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]]; stressEnergyTensor = 
      (1/vacuumPermeability)*Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[
      ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      stressEnergyTensor, False, False]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["InhomogeneousMaxwellEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
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
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
            D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
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
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ChargeConservationEquation"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
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
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
            D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index,#1]], newCoordinates[[
                 #1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     Total[(Module[{index = #1}, D[currentDensity[[index]], newCoordinates[[index]]] + 
            Total[(christoffelSymbols[[index,index,#1]]*currentDensity[[#1]] & ) /@ Range[Length[
                newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 0 /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["HomogeneousMaxwellEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor}, 
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
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
            D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     (Module[{index = #1}, (D[electromagneticTensor[[index[[1]],index[[2]]]], newCoordinates[[index[[3]]]]] - 
            Total[(christoffelSymbols[[#1,index[[3]],index[[1]]]]*electromagneticTensor[[#1,index[[2]]]] & ) /@ 
              Range[Length[newElectromagneticPotential]]] - Total[(christoffelSymbols[[#1,index[[3]],index[[2]]]]*
                electromagneticTensor[[index[[1]],#1]] & ) /@ Range[Length[newElectromagneticPotential]]]) + 
           (D[electromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
            Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*electromagneticTensor[[#1,index[[3]]]] & ) /@ 
              Range[Length[newElectromagneticPotential]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                electromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newElectromagneticPotential]]]) + 
           (D[electromagneticTensor[[index[[3]],index[[1]]]], newCoordinates[[index[[2]]]]] - 
            Total[(christoffelSymbols[[#1,index[[2]],index[[3]]]]*electromagneticTensor[[#1,index[[1]]]] & ) /@ 
              Range[Length[newElectromagneticPotential]]] - Total[(christoffelSymbols[[#1,index[[2]],index[[1]]]]*
                electromagneticTensor[[index[[3]],#1]] & ) /@ Range[Length[newElectromagneticPotential]]]) == 0] & ) /@ 
       Tuples[Range[Length[newElectromagneticPotential]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor /: MakeBoxes[electromagneticTensor:ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, 
       coordinates_List, metricIndex1_, metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, 
      index2_], format_] := Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, 
      covariantElectromagneticPotential, tensorRepresentation, matrixForm, type, symbol, dimensions, eigenvalues, 
      positiveEigenvalues, negativeEigenvalues, signature, icon}, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
         Select[coordinates, StringQ]; covariantElectromagneticPotential = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                 newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
          Range[Length[newElectromagneticPotential]]]]; tensorRepresentation = 
       Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] - 
              D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] & ) /@ 
           Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[coordinates, StringQ]; If[index1 === True && index2 === True, matrixForm = tensorRepresentation; 
        type = "Covariant"; symbol = Subscript["\[FormalCapitalF]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
        matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                     First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                     Last[#1]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
             Tuples[Range[Length[electromagneticPotential]], 2]]]; type = "Contravariant"; 
         symbol = Superscript["\[FormalCapitalF]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
         matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                      Last[index]]]*tensorRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     electromagneticPotential]]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]]; 
          type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalF]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, 
          matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                       First[index],#1]]*tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                      electromagneticPotential]]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]]; 
           type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalF]", "\[FormalNu]", "\[FormalMu]"], 
          matrixForm = ConstantArray[Indeterminate, {Length[matrixRepresentation], Length[matrixRepresentation]}]; 
           type = Indeterminate; symbol = Indeterminate]]]]; dimensions = Length[matrixRepresentation]; 
      eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
          Length[matrixRepresentation], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[matrixForm, ImageSize -> 
         Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], Frame -> False, 
        FrameTicks -> None]; BoxForm`ArrangeSummaryBox["ElectromagneticTensor", electromagneticTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]