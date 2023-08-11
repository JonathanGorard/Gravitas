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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
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
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["MetricTensor"] := 
  ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["Coordinates"] := 
  coordinates /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["Indices"] := 
  {index1, index2} /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["CovariantQ"] := 
  If[index1 === True && index2 === True, True, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ContravariantQ"] := 
  If[index1 === False && index2 === False, True, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["MixedQ"] := 
  If[(index1 === True && index2 === False) || (index1 === False && index2 === True), True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["Symbol"] := 
  If[index1 === True && index2 === True, Subscript["\[FormalCapitalF]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
     Superscript["\[FormalCapitalF]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, Subsuperscript["\[FormalCapitalF]", "\[FormalMu]", "\[FormalNu]"], 
      If[index1 === False && index2 === True, Subsuperscript["\[FormalCapitalF]", "\[FormalNu]", "\[FormalMu]"], Indeterminate]]]] /; 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedElectricField"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, covariantElectricField}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; covariantElectricField = 
      Normal[SparseArray[(#1 -> electromagneticTensor[[1,#1 + 1]] & ) /@ Range[Length[electromagneticPotential] - 1]]]; 
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index + 1,
                 #1 + 1]]*covariantElectricField[[#1]] & ) /@ Range[Length[electromagneticPotential] - 1]]] & ) /@ 
         Range[Length[electromagneticPotential] - 1]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicElectricField"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, covariantElectricField}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedMagneticField"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, leviCivitaTensor, covariantMagneticField}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
          2]]]; leviCivitaTensor = Normal[LeviCivitaTensor[3]]; covariantMagneticField = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((-(1/2))*(leviCivitaTensor[[index,First[#1],Last[#1]]]*
                 contravariantElectromagneticTensor[[First[#1] + 1,Last[#1] + 1]]) & ) /@ Tuples[Range[
                Length[electromagneticPotential] - 1], 2]]] & ) /@ Range[Length[electromagneticPotential] - 1]]]; 
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index + 1,
                 #1 + 1]]*covariantMagneticField[[#1]] & ) /@ Range[Length[electromagneticPotential] - 1]]] & ) /@ 
         Range[Length[electromagneticPotential] - 1]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicMagneticField"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, leviCivitaTensor, covariantMagneticField}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedChargeDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     FullSimplify[Total[(D[electromagneticDisplacementTensor[[1,#1]], newCoordinates[[#1]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicChargeDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     Total[(Inactive[D][electromagneticDisplacementTensor[[1,#1]], newCoordinates[[#1]]] & ) /@ 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedCurrentDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index + 1,
                  #1]], newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
          Range[Length[newElectromagneticPotential] - 1]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicCurrentDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inactive[D][electromagneticDisplacementTensor[[index + 1,
                 #1]], newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedSpacetimeCurrentDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[electromagneticDisplacementTensor[[index,
                  #1]], newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
          Range[Length[newElectromagneticPotential]]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicSpacetimeCurrentDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inactive[D][electromagneticDisplacementTensor[[index,#1]], 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
          Tuples[Range[Length[electromagneticPotential]], 2]]]; ResourceFunction["StressEnergyTensor"][
      ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      stressEnergyTensor, False, False]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["PoyntingVector"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, 
     poyntingCovector}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
          Tuples[Range[Length[electromagneticPotential]], 2]]]; poyntingCovector = 
      ((1/2)*(stressEnergyTensor[[1,#1 + 1]] + stressEnergyTensor[[#1 + 1,1]]) & ) /@ 
       Range[Length[stressEnergyTensor] - 1]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index + 1,#1 + 1]]*
               poyntingCovector[[#1]] & ) /@ Range[Length[stressEnergyTensor] - 1]]] & ) /@ 
        Range[Length[stressEnergyTensor] - 1]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedPoyntingVector"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, 
     poyntingCovector}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
          Tuples[Range[Length[electromagneticPotential]], 2]]]; poyntingCovector = 
      ((1/2)*(stressEnergyTensor[[1,#1 + 1]] + stressEnergyTensor[[#1 + 1,1]]) & ) /@ 
       Range[Length[stressEnergyTensor] - 1]; FullSimplify[
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index + 1,#1 + 1]]*
                poyntingCovector[[#1]] & ) /@ Range[Length[stressEnergyTensor] - 1]]] & ) /@ 
         Range[Length[stressEnergyTensor] - 1]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicPoyntingVector"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor, 
     poyntingCovector}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
          Tuples[Range[Length[electromagneticPotential]], 2]]]; poyntingCovector = 
      ((1/2)*(stressEnergyTensor[[1,#1 + 1]] + stressEnergyTensor[[#1 + 1,1]]) & ) /@ 
       Range[Length[stressEnergyTensor] - 1]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index + 1,#1 + 1]]*
               poyntingCovector[[#1]] & ) /@ Range[Length[stressEnergyTensor] - 1]]] & ) /@ 
        Range[Length[stressEnergyTensor] - 1]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["MaxwellStressTensor"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
          Tuples[Range[Length[electromagneticPotential]], 2]]]; 
     -Normal[SparseArray[(#1 -> stressEnergyTensor[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential] - 1], 2]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedMaxwellStressTensor"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
          Tuples[Range[Length[electromagneticPotential]], 2]]]; 
     FullSimplify[-Normal[SparseArray[(#1 -> stressEnergyTensor[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential] - 1], 2]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicMaxwellStressTensor"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor, stressEnergyTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
          Tuples[Range[Length[electromagneticPotential]], 2]]]; 
     -Normal[SparseArray[(#1 -> stressEnergyTensor[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential] - 1], 2]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["LagrangianDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
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
     (Sqrt[-Det[newMatrixRepresentation]]/(4*vacuumPermeability))*
        Total[(electromagneticTensor[[First[#1],Last[#1]]]*contravariantElectromagneticTensor[[First[#1],
             Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]] + 
       Total[(covariantElectromagneticPotential[[#1]]*currentDensity[[#1]] & ) /@ 
         Range[Length[newElectromagneticPotential]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedLagrangianDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
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
     FullSimplify[(Sqrt[-Det[newMatrixRepresentation]]/(4*vacuumPermeability))*
         Total[(electromagneticTensor[[First[#1],Last[#1]]]*contravariantElectromagneticTensor[[First[#1],
              Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]] + 
        Total[(covariantElectromagneticPotential[[#1]]*currentDensity[[#1]] & ) /@ 
          Range[Length[newElectromagneticPotential]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicLagrangianDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, contravariantElectromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inactive[D][electromagneticDisplacementTensor[[index,#1]], 
                newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     (Sqrt[-Det[newMatrixRepresentation]]/(4*vacuumPermeability))*
        Total[(electromagneticTensor[[First[#1],Last[#1]]]*contravariantElectromagneticTensor[[First[#1],
             Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]] + 
       Total[(covariantElectromagneticPotential[[#1]]*currentDensity[[#1]] & ) /@ 
         Range[Length[newElectromagneticPotential]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["LorentzForceDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(electromagneticTensor[[index,#1]]*currentDensity[[
                 #1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedLorentzForceDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(electromagneticTensor[[index,#1]]*
                 currentDensity[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
          Range[Length[newElectromagneticPotential]]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicLorentzForceDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newElectromagneticPotential = electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inactive[D][electromagneticDisplacementTensor[[index,#1]], 
                newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(electromagneticTensor[[index,#1]]*currentDensity[[
                 #1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ElectromagneticDisplacementDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedElectromagneticDisplacementDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     FullSimplify[(Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                   First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
           Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_][
   "SymbolicElectromagneticDisplacementDensity"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["CovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor, newElectromagneticTensor}, 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; If[index1 === True && index2 === True, 
      newElectromagneticTensor = electromagneticTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalF]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             D[newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newElectromagneticTensor[[#1,index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                  newElectromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (Inverse[newMatrixRepresentation][[First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],
                    Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
             2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                   index[[1]]]], StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalF]", StringJoin[
                  ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                   StandardForm]]], StandardForm]] -> D[newElectromagneticTensor[[index[[2]],index[[3]]]], 
                newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                   newElectromagneticTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[3]],index[[1]],#1]]*newElectromagneticTensor[[index[[2]],#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3] /. 
          (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*electromagneticTensor[[First[index],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalF]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               D[newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newElectromagneticTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    newElectromagneticTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (Inverse[newMatrixRepresentation][[First[index],#1]]*electromagneticTensor[[#1,Last[index]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalF]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                D[newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newElectromagneticTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     newElectromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedCovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor, newElectromagneticTensor}, 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; If[index1 === True && index2 === True, 
      newElectromagneticTensor = electromagneticTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalF]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             FullSimplify[D[newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - Total[
                (christoffelSymbols[[#1,index[[1]],index[[2]]]]*newElectromagneticTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                   newElectromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (Inverse[newMatrixRepresentation][[First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],
                    Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
             2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                   index[[1]]]], StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalF]", StringJoin[
                  ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                   StandardForm]]], StandardForm]] -> FullSimplify[D[newElectromagneticTensor[[index[[2]],index[[3]]]], 
                 newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                    newElectromagneticTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newElectromagneticTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3] /. 
          (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*electromagneticTensor[[First[index],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalF]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> FullSimplify[
                D[newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newElectromagneticTensor[[index[[2]],#1]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                     newElectromagneticTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (Inverse[newMatrixRepresentation][[First[index],#1]]*electromagneticTensor[[#1,Last[index]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalF]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                FullSimplify[D[newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                  Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newElectromagneticTensor[[#1,index[[3]]]] & ) /@ 
                    Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                      newElectromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicCovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor, newElectromagneticTensor}, 
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
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; If[index1 === True && index2 === True, 
      newElectromagneticTensor = electromagneticTensor; Association[
        (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
               StandardForm], ToString[Subscript["\[FormalCapitalF]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                 ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
             Inactive[D][newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newElectromagneticTensor[[#1,index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                  newElectromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (Inverse[newMatrixRepresentation][[First[index],First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],
                    Last[index]]]*electromagneticTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
             2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                   index[[1]]]], StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalF]", StringJoin[
                  ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                   StandardForm]]], StandardForm]] -> Inactive[D][newElectromagneticTensor[[index[[2]],index[[3]]]], 
                newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                   newElectromagneticTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[3]],index[[1]],#1]]*newElectromagneticTensor[[index[[2]],#1]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3] /. 
          (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*electromagneticTensor[[First[index],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalF]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               Inactive[D][newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newElectromagneticTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    newElectromagneticTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         newElectromagneticTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (Inverse[newMatrixRepresentation][[First[index],#1]]*electromagneticTensor[[#1,Last[index]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalF]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                Inactive[D][newElectromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newElectromagneticTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     newElectromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedInhomogeneousMaxwellEquations"] := 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
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
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicInhomogeneousMaxwellEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
     electromagneticDisplacementTensor, currentDensity}, 
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
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; contravariantElectromagneticTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*contravariantElectromagneticTensor; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inactive[D][electromagneticDisplacementTensor[[index,#1]], 
                newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     (Module[{index = #1}, Total[(Module[{nestedIndex = #1}, Inactive[D][contravariantElectromagneticTensor[[index,
                 nestedIndex]], newCoordinates[[nestedIndex]]] + Total[(christoffelSymbols[[index,nestedIndex,#1]]*
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedChargeConservationEquation"] := 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
     FullSimplify[Total[(Module[{index = #1}, D[currentDensity[[index]], newCoordinates[[index]]] + 
             Total[(christoffelSymbols[[index,index,#1]]*currentDensity[[#1]] & ) /@ Range[
                Length[newElectromagneticPotential]]]] & ) /@ Range[Length[newElectromagneticPotential]]] == 0 /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicChargeConservationEquation"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor, electromagneticDisplacementTensor, currentDensity}, 
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
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; electromagneticDisplacementTensor = 
      (Sqrt[-Det[newMatrixRepresentation]]/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],
                  First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                  Last[#1]]] & ) /@ Tuples[Range[Length[newElectromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     currentDensity = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inactive[D][electromagneticDisplacementTensor[[index,#1]], 
                newCoordinates[[#1]]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; 
     Total[(Module[{index = #1}, Inactive[D][currentDensity[[index]], newCoordinates[[index]]] + 
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
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicHomogeneousMaxwellEquations"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, christoffelSymbols, 
     covariantElectromagneticPotential, electromagneticTensor}, 
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
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
            Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
         Tuples[Range[Length[newElectromagneticPotential]], 2]]]; 
     (Module[{index = #1}, (Inactive[D][electromagneticTensor[[index[[1]],index[[2]]]], newCoordinates[[index[[3]]]]] - 
            Total[(christoffelSymbols[[#1,index[[3]],index[[1]]]]*electromagneticTensor[[#1,index[[2]]]] & ) /@ 
              Range[Length[newElectromagneticPotential]]] - Total[(christoffelSymbols[[#1,index[[3]],index[[2]]]]*
                electromagneticTensor[[index[[1]],#1]] & ) /@ Range[Length[newElectromagneticPotential]]]) + 
           (Inactive[D][electromagneticTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
            Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*electromagneticTensor[[#1,index[[3]]]] & ) /@ 
              Range[Length[newElectromagneticPotential]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                electromagneticTensor[[index[[2]],#1]] & ) /@ Range[Length[newElectromagneticPotential]]]) + 
           (Inactive[D][electromagneticTensor[[index[[3]],index[[1]]]], newCoordinates[[index[[2]]]]] - 
            Total[(christoffelSymbols[[#1,index[[2]],index[[3]]]]*electromagneticTensor[[#1,index[[1]]]] & ) /@ 
              Range[Length[newElectromagneticPotential]]] - Total[(christoffelSymbols[[#1,index[[2]],index[[1]]]]*
                electromagneticTensor[[index[[3]],#1]] & ) /@ Range[Length[newElectromagneticPotential]]]) == 0] & ) /@ 
       Tuples[Range[Length[newElectromagneticPotential]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["Signature"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeablity_, index1_, index2_]["RiemannianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], True, False], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["PseudoRiemannianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], False, True], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["LorentzianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["RiemannianConditions"] := 
  Module[{eigenvalues, riemannianConditions}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; If[riemannianConditions === True, {}, 
      If[riemannianConditions === False, Indeterminate, If[Length[Select[riemannianConditions, #1 === False & ]] > 0, 
        Indeterminate, DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["PseudoRiemannianConditions"] := 
  Module[{eigenvalues, pseudoRiemannianConditions}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     pseudoRiemannianConditions = FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["LorentzianConditions"] := 
  Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinate, lorentzianConditions}, 
    eigensystem = Eigensystem[matrixRepresentation]; eigenvalues = First[eigensystem]; eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinate, eigenvalues[[#1]] < 0, eigenvalues[[#1]] > 0] & ) /@ 
          Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, If[lorentzianConditions === False, 
         Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, Indeterminate, 
          DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ElectromagneticSingularities"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; 
     Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, 
                  StringQ]]] === Solve, {{#1}}, Solve[#1, coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[
                coordinates, StringQ]]] & ) /@ Flatten[{FunctionSingularities[Catenate[FullSimplify[
                 electromagneticTensor] /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]], 
              coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]] /. Or -> List}]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["Determinant"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; Det[electromagneticTensor]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedDeterminant"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; FullSimplify[Det[electromagneticTensor]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["SymbolicDeterminant"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> Inactive[D][covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             Inactive[D][covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; Det[electromagneticTensor]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeablity_, index1_, index2_]["Eigenvalues"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; Eigenvalues[electromagneticTensor]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ReducedEigenvalues"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; FullSimplify[Eigenvalues[electromagneticTensor]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["Eigenvectors"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; Eigenvectors[electromagneticTensor]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeablity_, index1_, index2_]["ReducedEigenvectors"] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; FullSimplify[Eigenvectors[electromagneticTensor]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["CovariantElectromagneticTensor"] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    electromagneticPotential, vacuumPermeability, True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["ContravariantElectromagneticTensor"] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    electromagneticPotential, vacuumPermeability, False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], newCoordinates_List] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], electromagneticPotential /. Thread[coordinates -> newCoordinates], 
    vacuumPermeability /. Thread[coordinates -> newCoordinates], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], newIndex1_, newIndex2_] := 
  ElectromagneticTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    electromagneticPotential, vacuumPermeability, newIndex1, newIndex2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[newIndex1] && BooleanQ[newIndex2]
ElectromagneticTensor[ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], electromagneticPotential_List, vacuumPermeability_, index1_, index2_], newCoordinates_List, 
   newIndex1_, newIndex2_] := ElectromagneticTensor[ResourceFunction["MetricTensor"][
     matrixRepresentation /. Thread[coordinates -> newCoordinates], newCoordinates, metricIndex1, metricIndex2], 
    electromagneticPotential /. Thread[coordinates -> newCoordinates], 
    vacuumPermeability /. Thread[coordinates -> newCoordinates], newIndex1, newIndex2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[electromagneticPotential] == 
     Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[newIndex1] && BooleanQ[newIndex2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_]["Properties"] := 
  {"MatrixRepresentation", "ReducedMatrixRepresentation", "SymbolicMatrixRepresentation", "MetricTensor", "Coordinates", 
    "CoordinateOneForms", "Indices", "CovariantQ", "ContravariantQ", "MixedQ", "Symbol", "ElectricField", 
    "ReducedElectricField", "SymbolicElectricField", "MagneticField", "ReducedMagneticField", "SymbolicMagneticField", 
    "ChargeDensity", "ReducedChargeDensity", "SymbolicChargeDensity", "CurrentDensity", "ReducedCurrentDensity", 
    "SymbolicCurrentDensity", "SpacetimeCurrentDensity", "ReducedSpacetimeCurrentDensity", 
    "SymbolicSpacetimeCurrentDensity", "ElectromagneticStressEnergyTensor", "PoyntingVector", "ReducedPoyntingVector", 
    "SymbolicPoyntingVector", "MaxwellStressTensor", "ReducedMaxwellStressTensor", "SymbolicMaxwellStressTensor", 
    "LagrangianDensity", "ReducedLagrangianDensity", "SymbolicLagrangianDensity", "LorentzForceDensity", 
    "ReducedLorentzForceDensity", "SymbolicLorentzForceDensity", "ElectromagneticDisplacementDensity", 
    "ReducedElectromagneticDisplacementDensity", "SymbolicElectromagneticDisplacementDensity", "CovariantDerivatives", 
    "ReducedCovariantDerivatives", "SymbolicCovariantDerivatives", "InhomogeneousMaxwellEquations", 
    "ReducedInhomogeneousMaxwellEquations", "SymbolicInhomogeneousMaxwellEquations", "ChargeConservationEquation", 
    "ReducedChargeConservationEquation", "SymbolicChargeConservationEquation", "HomogeneousMaxwellEquations", 
    "SymbolicHomogeneousMaxwellEquations", "Signature", "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", 
    "RiemannianConditions", "PseudoRiemannianConditions", "LorentzianConditions", "ElectromagneticSingularities", 
    "Determinant", "ReducedDeterminant", "SymbolicDeterminant", "Eigenvalues", "ReducedEigenvalues", "Eigenvectors", 
    "ReducedEigenvectors", "CovariantElectromagneticTensor", "ContravariantElectromagneticTensor", "Properties"} /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
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
       Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
              D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
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
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_][row_Integer, column_Integer] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True, electromagneticTensor[[row,column]], 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
           Tuples[Range[Length[electromagneticPotential]], 2]]][[row,column]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                   electromagneticTensor[[First[index],#1]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
            Tuples[Range[Length[electromagneticPotential]], 2]]][[row,column]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    electromagneticTensor[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
             Tuples[Range[Length[electromagneticPotential]], 2]]][[row,column]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_][row_Integer, All] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True, electromagneticTensor[[row,All]], 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
           Tuples[Range[Length[electromagneticPotential]], 2]]][[row,All]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                   electromagneticTensor[[First[index],#1]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
            Tuples[Range[Length[electromagneticPotential]], 2]]][[row,All]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    electromagneticTensor[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
             Tuples[Range[Length[electromagneticPotential]], 2]]][[row,All]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
ElectromagneticTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    electromagneticPotential_List, vacuumPermeability_, index1_, index2_][All, column_Integer] := 
  Module[{newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
     electromagneticTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newElectromagneticPotential = electromagneticPotential /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     covariantElectromagneticPotential = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*
                newElectromagneticPotential[[#1]] & ) /@ Range[Length[newElectromagneticPotential]]]] & ) /@ 
         Range[Length[newElectromagneticPotential]]]]; electromagneticTensor = 
      Normal[SparseArray[(#1 -> D[covariantElectromagneticPotential[[Last[#1]]], newCoordinates[[First[#1]]]] - 
             D[covariantElectromagneticPotential[[First[#1]]], newCoordinates[[Last[#1]]]] & ) /@ 
          Tuples[Range[Length[newElectromagneticPotential]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True, electromagneticTensor[[All,column]], 
      If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                  Inverse[matrixRepresentation][[Last[#1],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
           Tuples[Range[Length[electromagneticPotential]], 2]]][[All,column]], If[index1 === True && index2 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*
                   electromagneticTensor[[First[index],#1]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
            Tuples[Range[Length[electromagneticPotential]], 2]]][[All,column]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                    electromagneticTensor[[#1,Last[index]]] & ) /@ Range[Length[electromagneticPotential]]]] & ) /@ 
             Tuples[Range[Length[electromagneticPotential]], 2]]][[All,column]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticPotential] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
