(* ::Package:: *)

StressEnergyTensor[] := {"Symmetric", "SymmetricField", "Asymmetric", "AsymmetricField", "PerfectFluid", 
   "PerfectFluidField", "Dust", "DustField", "Radiation", "RadiationField", "ElectromagneticField", "MassiveScalarField"}
StressEnergyTensor[dimensionCount_Integer] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
   Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[dimensionCount], 2]]], False, False]
StressEnergyTensor[dimensionCount_Integer, (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[matrixRepresentation] == dimensionCount
StressEnergyTensor[dimensionCount_Integer, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
    Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[dimensionCount], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[dimensionCount_Integer, (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[matrixRepresentation] == dimensionCount
StressEnergyTensor["Symmetric"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["Symmetric", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[4], 2]]], 
    False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[matrixRepresentation] == 4
StressEnergyTensor["Symmetric", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[4], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["Symmetric", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[matrixRepresentation] == 4
StressEnergyTensor[{"Symmetric", dimensionCount_Integer}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
   Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[dimensionCount], 2]]], False, False]
StressEnergyTensor[{"Symmetric", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[matrixRepresentation] == dimensionCount
StressEnergyTensor[{"Symmetric", dimensionCount_Integer}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
    Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[dimensionCount], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Symmetric", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", Sort[#1]] & ) /@ Tuples[Range[dimensionCount], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[matrixRepresentation] == dimensionCount
StressEnergyTensor["SymmetricField"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], 
   Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ 
          Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["SymmetricField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ coordinates] & ) /@ 
       Tuples[Range[4], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[matrixRepresentation] == 4
StressEnergyTensor["SymmetricField", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ 
           Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Tuples[Range[4], 2]]], index1, 
    index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["SymmetricField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ coordinates] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[matrixRepresentation] == 4
StressEnergyTensor[{"SymmetricField", dimensionCount_Integer}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
   Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ 
          Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]]] & ) /@ 
      Tuples[Range[dimensionCount], 2]]], False, False]
StressEnergyTensor[{"SymmetricField", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ coordinates] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[matrixRepresentation] == dimensionCount
StressEnergyTensor[{"SymmetricField", dimensionCount_Integer}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ 
           Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]]] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"SymmetricField", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", Sort[index]] @@ coordinates] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[matrixRepresentation] == dimensionCount
StressEnergyTensor["Asymmetric"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["Asymmetric", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ Tuples[Range[4], 2]]], False, 
    False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[matrixRepresentation] == 4
StressEnergyTensor["Asymmetric", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ Tuples[Range[4], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["Asymmetric", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[matrixRepresentation] == 4
StressEnergyTensor[{"Asymmetric", dimensionCount_Integer}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
   Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ Tuples[Range[dimensionCount], 2]]], False, False]
StressEnergyTensor[{"Asymmetric", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[matrixRepresentation] == dimensionCount
StressEnergyTensor[{"Asymmetric", dimensionCount_Integer}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
    Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ Tuples[Range[dimensionCount], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Asymmetric", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> Superscript["\[FormalCapitalT]", #1] & ) /@ Tuples[Range[dimensionCount], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[matrixRepresentation] == dimensionCount
StressEnergyTensor["AsymmetricField"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], 
   Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ 
          Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["AsymmetricField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ coordinates] & ) /@ 
       Tuples[Range[4], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[matrixRepresentation] == 4
StressEnergyTensor["AsymmetricField", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ 
           Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Tuples[Range[4], 2]]], index1, 
    index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["AsymmetricField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ coordinates] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[matrixRepresentation] == 4
StressEnergyTensor[{"AsymmetricField", dimensionCount_Integer}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
   Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ 
          Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]]] & ) /@ 
      Tuples[Range[dimensionCount], 2]]], False, False]
StressEnergyTensor[{"AsymmetricField", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ coordinates] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[matrixRepresentation] == dimensionCount
StressEnergyTensor[{"AsymmetricField", dimensionCount_Integer}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, dimensionCount - 1]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ 
           Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]]] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"AsymmetricField", dimensionCount_Integer}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> Superscript["\[FormalCapitalT]", index] @@ coordinates] & ) /@ 
       Tuples[Range[dimensionCount], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[matrixRepresentation] == dimensionCount
StressEnergyTensor["PerfectFluid"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], 
   Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
           ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["PerfectFluid", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
            ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["PerfectFluid", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
            ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["PerfectFluid", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex1], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
            ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
         "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
         "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
         pressure*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor["PerfectFluidField"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], 
   Normal[SparseArray[
     (Module[{index = #1}, index -> ("\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]] + 
            "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]])*
           Superscript["\[FormalU]", ToString[First[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*
           Superscript["\[FormalU]", ToString[Last[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
              Range[3]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*
           Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[index],Last[index]]]] & ) /@ 
      Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["PerfectFluidField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> ("\[FormalRho]" @@ coordinates + "\[FormalCapitalP]" @@ coordinates)*
            Superscript["\[FormalU]", ToString[First[index]]] @@ coordinates*Superscript["\[FormalU]", ToString[Last[index]]] @@ 
             coordinates + "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["PerfectFluidField", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> ("\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[3]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]])*
            Superscript["\[FormalU]", ToString[First[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                3]]*Superscript["\[FormalU]", ToString[Last[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
               Range[3]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*
            Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["PerfectFluidField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> ("\[FormalRho]" @@ coordinates + "\[FormalCapitalP]" @@ coordinates)*
            Superscript["\[FormalU]", ToString[First[index]]] @@ coordinates*Superscript["\[FormalU]", ToString[Last[index]]] @@ 
             coordinates + "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[
     (Module[{index = #1}, index -> ("\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
             (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]] + 
          "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
           Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[index],
            Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> ("\[FormalRho]" @@ coordinates + "\[FormalCapitalP]" @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> ("\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[Length[fourVelocity] - 1]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
              (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
             Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]] + 
           "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
            Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[index],
             Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> ("\[FormalRho]" @@ coordinates + "\[FormalCapitalP]" @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[
     (Module[{index = #1}, index -> (density @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
             (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]] + 
          "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
           Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[index],
            Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (density @@ coordinates + "\[FormalCapitalP]" @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> (density @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[Length[fourVelocity] - 1]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
              (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
             Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]] + 
           "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
            Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[index],
             Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (density @@ coordinates + "\[FormalCapitalP]" @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_, pressure_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[
     (Module[{index = #1}, index -> (density @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]] + pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
             (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]] + 
          pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
           Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[index],
            Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_, pressure_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (density @@ coordinates + pressure @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           pressure @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_, pressure_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> (density @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[Length[fourVelocity] - 1]] + pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
              (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
             Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]] + 
           pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
            Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[index],
             Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluidField", fourVelocity_List, density_, pressure_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (density @@ coordinates + pressure @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           pressure @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor["Dust"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], Normal[SparseArray[(#1 -> "\[FormalRho]"*Superscript["\[FormalU]", ToString[First[#1]]]*
         Superscript["\[FormalU]", ToString[Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["Dust", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> "\[FormalRho]"*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", ToString[Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["Dust", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> "\[FormalRho]"*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", ToString[Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["Dust", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> "\[FormalRho]"*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", ToString[Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Dust", fourVelocity_List}] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(#1 -> "\[FormalRho]"*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
      Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"Dust", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> "\[FormalRho]"*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"Dust", fourVelocity_List}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(#1 -> "\[FormalRho]"*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
       Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Dust", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> "\[FormalRho]"*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"Dust", fourVelocity_List, density_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(#1 -> density*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
      Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"Dust", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> density*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"Dust", fourVelocity_List, density_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(#1 -> density*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
       Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Dust", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> density*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor["DustField"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], 
   Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
             Range[3]]*Superscript["\[FormalU]", ToString[First[index]]] @@ Join[{"\[FormalT]"}, 
            (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*Superscript["\[FormalU]", ToString[Last[index]]] @@ 
           Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["DustField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ coordinates*Superscript["\[FormalU]", ToString[First[index]]] @@ 
            coordinates*Superscript["\[FormalU]", ToString[Last[index]]] @@ coordinates] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["DustField", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
              Range[3]]*Superscript["\[FormalU]", ToString[First[index]]] @@ Join[{"\[FormalT]"}, 
             (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*Superscript["\[FormalU]", ToString[Last[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Tuples[Range[4], 2]]], index1, 
    index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["DustField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ coordinates*Superscript["\[FormalU]", ToString[First[index]]] @@ 
            coordinates*Superscript["\[FormalU]", ToString[Last[index]]] @@ coordinates] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"DustField", fourVelocity_List}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
             Range[Length[fourVelocity] - 1]]*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
            (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
           Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]] & ) /@ 
      Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"DustField", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ coordinates*fourVelocity[[First[index]]] @@ coordinates*
           fourVelocity[[Last[index]]] @@ coordinates] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], False, 
    False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"DustField", fourVelocity_List}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
              Range[Length[fourVelocity] - 1]]*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
             (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]] & ) /@ 
       Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"DustField", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> "\[FormalRho]" @@ coordinates*fourVelocity[[First[index]]] @@ coordinates*
           fourVelocity[[Last[index]]] @@ coordinates] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], index1, 
    index2] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"DustField", fourVelocity_List, density_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(Module[{index = #1}, index -> density @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
             Range[Length[fourVelocity] - 1]]*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
            (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
           Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]] & ) /@ 
      Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"DustField", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> density @@ coordinates*fourVelocity[[First[index]]] @@ coordinates*
           fourVelocity[[Last[index]]] @@ coordinates] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], False, 
    False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"DustField", fourVelocity_List, density_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> density @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
              Range[Length[fourVelocity] - 1]]*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
             (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]] & ) /@ 
       Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"DustField", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> density @@ coordinates*fourVelocity[[First[index]]] @@ coordinates*
           fourVelocity[[Last[index]]] @@ coordinates] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], index1, 
    index2] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor["Radiation"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], 
   Normal[SparseArray[(#1 -> (4*"\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", ToString[Last[#1]]] + 
         "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
      Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["Radiation", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (Length[matrixRepresentation]*"\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*
           Superscript["\[FormalU]", ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["Radiation", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> (4*"\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", ToString[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["Radiation", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (Length[matrixRepresentation]*"\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*
           Superscript["\[FormalU]", ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Radiation", fourVelocity_List}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(#1 -> (Length[fourVelocity]*"\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
         "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"Radiation", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (Length[matrixRepresentation]*"\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"Radiation", fourVelocity_List}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(#1 -> (Length[fourVelocity]*"\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Radiation", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (Length[matrixRepresentation]*"\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
        2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"Radiation", fourVelocity_List, pressure_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[(#1 -> (Length[fourVelocity]*pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
         pressure*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"Radiation", fourVelocity_List, pressure_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (Length[matrixRepresentation]*pressure)*fourVelocity[[First[#1]]]*
           fourVelocity[[Last[#1]]] + pressure*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"Radiation", fourVelocity_List, pressure_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[(#1 -> (Length[fourVelocity]*pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"Radiation", fourVelocity_List, pressure_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (Length[matrixRepresentation]*pressure)*fourVelocity[[First[#1]]]*
           fourVelocity[[Last[#1]]] + pressure*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor["RadiationField"] := StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
    True, True], 
   Normal[SparseArray[
     (Module[{index = #1}, index -> (4*"\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]])*
           Superscript["\[FormalU]", ToString[First[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*
           Superscript["\[FormalU]", ToString[Last[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
              Range[3]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*
           Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[index],Last[index]]]] & ) /@ 
      Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["RadiationField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (Length[matrixRepresentation]*"\[FormalCapitalP]" @@ coordinates)*
            Superscript["\[FormalU]", ToString[First[index]]] @@ coordinates*Superscript["\[FormalU]", ToString[Last[index]]] @@ 
             coordinates + "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["RadiationField", index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(Module[{index = #1}, index -> (4*"\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
                Range[3]])*Superscript["\[FormalU]", ToString[First[index]]] @@ Join[{"\[FormalT]"}, 
              (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*Superscript["\[FormalU]", ToString[Last[index]]] @@ 
             Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]] + 
           "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]*
            Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["RadiationField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (Length[matrixRepresentation]*"\[FormalCapitalP]" @@ coordinates)*
            Superscript["\[FormalU]", ToString[First[index]]] @@ coordinates*Superscript["\[FormalU]", ToString[Last[index]]] @@ 
             coordinates + "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"RadiationField", fourVelocity_List}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[
     (Module[{index = #1}, index -> (Length[fourVelocity]*"\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
               Range[Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ Join[{"\[FormalT]"}, 
             (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*fourVelocity[[Last[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]] + 
          "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
           Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]]][[First[index],
            Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"RadiationField", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (Length[matrixRepresentation]*"\[FormalCapitalP]" @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"RadiationField", fourVelocity_List}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[
      (Module[{index = #1}, index -> (Length[fourVelocity]*"\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", 
                  ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ 
             Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
            fourVelocity[[Last[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]] + "\[FormalCapitalP]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]]*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 
                  1]]]][[First[index],Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"RadiationField", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (Length[matrixRepresentation]*"\[FormalCapitalP]" @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           "\[FormalCapitalP]" @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"RadiationField", fourVelocity_List, pressure_}] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][
    DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]], True, True], 
   Normal[SparseArray[
     (Module[{index = #1}, index -> (Length[fourVelocity]*pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", 
                 ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ 
            Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
           fourVelocity[[Last[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
              Range[Length[fourVelocity] - 1]] + pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
              Range[Length[fourVelocity] - 1]]*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 
                 1]]]][[First[index],Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], False, False]
StressEnergyTensor[{"RadiationField", fourVelocity_List, pressure_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (Length[matrixRepresentation]*pressure @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           pressure @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor[{"RadiationField", fourVelocity_List, pressure_}, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[fourVelocity] - 1]]], Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[fourVelocity] - 1]], True, True], 
    Normal[SparseArray[
      (Module[{index = #1}, index -> (Length[fourVelocity]*pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", 
                  ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]])*fourVelocity[[First[index]]] @@ 
             Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]*
            fourVelocity[[Last[index]]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]] + pressure @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[
                Length[fourVelocity] - 1]]*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[fourVelocity] - 
                  1]]]][[First[index],Last[index]]]] & ) /@ Tuples[Range[Length[fourVelocity]], 2]]], index1, index2] /; 
   BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"RadiationField", fourVelocity_List, pressure_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(Module[{index = #1}, index -> (Length[matrixRepresentation]*pressure @@ coordinates)*
            fourVelocity[[First[index]]] @@ coordinates*fourVelocity[[Last[index]]] @@ coordinates + 
           pressure @@ coordinates*Inverse[matrixRepresentation][[First[index],Last[index]]]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation]], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == Length[matrixRepresentation]
StressEnergyTensor["ElectromagneticField"] := Module[{matrixRepresentation, electromagneticPotential, coordinates, 
    newMatrixRepresentation, newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, 
    electromagneticTensor, contravariantElectromagneticTensor, mixedElectromagneticTensor}, 
   matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; 
    electromagneticPotential = Join[{"\[FormalCapitalPhi]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]}, 
      (Module[{index = #1}, Superscript["\[FormalCapitalA]", ToString[index]] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
            Range[3]]] & ) /@ Range[Length[matrixRepresentation] - 1]]; 
    coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
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
        Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[
     ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
     (1/Subscript["\[FormalMu]", "0"])*Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                 mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
             (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]], False, False]]
StressEnergyTensor["ElectromagneticField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := Module[{electromagneticPotential, newMatrixRepresentation, newElectromagneticPotential, 
     newCoordinates, covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor}, electromagneticPotential = Join[{"\[FormalCapitalPhi]" @@ coordinates}, 
       (Superscript["\[FormalCapitalA]", ToString[#1]] @@ coordinates & ) /@ Range[Length[matrixRepresentation] - 1]]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, metricIndex1, metricIndex2], (1/Subscript["\[FormalMu]", "0"])*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], False, False]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["ElectromagneticField", index1_, index2_] := 
  Module[{matrixRepresentation, electromagneticPotential, coordinates, newMatrixRepresentation, 
     newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, electromagneticTensor, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor}, 
    matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; electromagneticPotential = 
      Join[{"\[FormalCapitalPhi]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]}, 
       (Module[{index = #1}, Superscript["\[FormalCapitalA]", ToString[index]] @@ Join[{"\[FormalT]"}, 
            (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Range[Length[matrixRepresentation] - 1]]; 
     coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
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
         Tuples[Range[Length[electromagneticTensor]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, True, True], (1/Subscript["\[FormalMu]", "0"])*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], index1, index2]] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["ElectromagneticField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := 
  Module[{electromagneticPotential, newMatrixRepresentation, newElectromagneticPotential, newCoordinates, 
     covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor}, electromagneticPotential = Join[{"\[FormalCapitalPhi]" @@ coordinates}, 
       (Superscript["\[FormalCapitalA]", ToString[#1]] @@ coordinates & ) /@ Range[Length[matrixRepresentation] - 1]]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, metricIndex1, metricIndex2], (1/Subscript["\[FormalMu]", "0"])*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], index1, index2]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List}] := 
  Module[{matrixRepresentation, electromagneticPotential, coordinates, newMatrixRepresentation, 
    newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, electromagneticTensor, 
    contravariantElectromagneticTensor, mixedElectromagneticTensor}, 
   matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[electromagneticFourPotential] - 1]]]; 
    electromagneticPotential = (Module[{index = #1}, electromagneticFourPotential[[index]] @@ 
         Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[electromagneticFourPotential] - 1]]] & ) /@ 
      Range[Length[electromagneticFourPotential]]; 
    coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[electromagneticFourPotential] - 1]]; newMatrixRepresentation = 
     matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
        Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[
     ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
     (1/Subscript["\[FormalMu]", "0"])*Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                 mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
             (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]], False, False]]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  Module[{electromagneticPotential, newMatrixRepresentation, newElectromagneticPotential, newCoordinates, 
     covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor}, electromagneticPotential = 
      (Module[{index = #1}, electromagneticFourPotential[[index]] @@ coordinates] & ) /@ 
       Range[Length[electromagneticFourPotential]]; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newElectromagneticPotential = 
      electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, metricIndex1, metricIndex2], (1/Subscript["\[FormalMu]", "0"])*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], False, False]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticFourPotential] == Length[matrixRepresentation]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List}, index1_, index2_] := 
  Module[{matrixRepresentation, electromagneticPotential, coordinates, newMatrixRepresentation, 
     newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, electromagneticTensor, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor}, 
    matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[electromagneticFourPotential] - 1]]]; 
     electromagneticPotential = (Module[{index = #1}, electromagneticFourPotential[[index]] @@ 
          Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[electromagneticFourPotential] - 1]]] & ) /@ 
       Range[Length[electromagneticFourPotential]]; coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
        Range[Length[electromagneticFourPotential] - 1]]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, True, True], (1/Subscript["\[FormalMu]", "0"])*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], index1, index2]] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  Module[{electromagneticPotential, newMatrixRepresentation, newElectromagneticPotential, newCoordinates, 
     covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor}, electromagneticPotential = 
      (Module[{index = #1}, electromagneticFourPotential[[index]] @@ coordinates] & ) /@ 
       Range[Length[electromagneticFourPotential]]; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newElectromagneticPotential = 
      electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, metricIndex1, metricIndex2], (1/Subscript["\[FormalMu]", "0"])*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticTensor]], 2]]] & ) /@ Tuples[Range[Length[electromagneticPotential]], 
           2]]], index1, index2]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[electromagneticFourPotential] == Length[matrixRepresentation]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List, vacuumPermeability_}] := 
  Module[{matrixRepresentation, electromagneticPotential, coordinates, newMatrixRepresentation, 
    newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, electromagneticTensor, 
    contravariantElectromagneticTensor, mixedElectromagneticTensor}, 
   matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[electromagneticFourPotential] - 1]]]; 
    electromagneticPotential = (Module[{index = #1}, electromagneticFourPotential[[index]] @@ 
         Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[electromagneticFourPotential] - 1]]] & ) /@ 
      Range[Length[electromagneticFourPotential]]; 
    coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
       Range[Length[electromagneticFourPotential] - 1]]; newMatrixRepresentation = 
     matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
        Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[
     ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
     (1/vacuumPermeability)*Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                 mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
             (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                   Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
         Tuples[Range[Length[electromagneticPotential]], 2]]], False, False]]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List, vacuumPermeability_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  Module[{electromagneticPotential, newMatrixRepresentation, newElectromagneticPotential, newCoordinates, 
     covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor}, electromagneticPotential = 
      (Module[{index = #1}, electromagneticFourPotential[[index]] @@ coordinates] & ) /@ 
       Range[Length[electromagneticFourPotential]]; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newElectromagneticPotential = 
      electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, metricIndex1, metricIndex2], (1/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], False, False]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[electromagneticFourPotential] == Length[matrixRepresentation]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List, vacuumPermeability_}, index1_, index2_] := 
  Module[{matrixRepresentation, electromagneticPotential, coordinates, newMatrixRepresentation, 
     newElectromagneticPotential, newCoordinates, covariantElectromagneticPotential, electromagneticTensor, 
     contravariantElectromagneticTensor, mixedElectromagneticTensor}, 
    matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, Length[electromagneticFourPotential] - 1]]]; 
     electromagneticPotential = (Module[{index = #1}, electromagneticFourPotential[[index]] @@ 
          Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[electromagneticFourPotential] - 1]]] & ) /@ 
       Range[Length[electromagneticFourPotential]]; coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
        Range[Length[electromagneticFourPotential] - 1]]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, True, True], (1/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], index1, index2]] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"ElectromagneticField", electromagneticFourPotential_List, vacuumPermeability_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  Module[{electromagneticPotential, newMatrixRepresentation, newElectromagneticPotential, newCoordinates, 
     covariantElectromagneticPotential, electromagneticTensor, contravariantElectromagneticTensor, 
     mixedElectromagneticTensor}, electromagneticPotential = 
      (Module[{index = #1}, electromagneticFourPotential[[index]] @@ coordinates] & ) /@ 
       Range[Length[electromagneticFourPotential]]; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newElectromagneticPotential = 
      electromagneticPotential /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[electromagneticPotential]], 2]]]; StressEnergyTensor[ResourceFunction["MetricTensor"][
       matrixRepresentation, coordinates, metricIndex1, metricIndex2], (1/vacuumPermeability)*
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(contravariantElectromagneticTensor[[First[index],#1]]*
                  mixedElectromagneticTensor[[Last[index],#1]] & ) /@ Range[Length[electromagneticPotential]]] - 
              (1/4)*Total[(Inverse[matrixRepresentation][[First[index],Last[index]]]*electromagneticTensor[[First[#1],
                    Last[#1]]]*contravariantElectromagneticTensor[[First[#1],Last[#1]]] & ) /@ 
                 Tuples[Range[Length[electromagneticPotential]], 2]]] & ) /@ 
          Tuples[Range[Length[electromagneticPotential]], 2]]], index1, index2]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[electromagneticFourPotential] == Length[matrixRepresentation]
StressEnergyTensor["MassiveScalarField"] := Module[{matrixRepresentation, scalarField, conjugateScalarField, coordinates, 
    newMatrixRepresentation, newScalarField, newConjugateScalarField, newCoordinates}, 
   matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; 
    scalarField = "\[FormalPsi]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    conjugateScalarField = OverBar["\[FormalPsi]"] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
     Normal[SparseArray[(Module[{index = #1}, index -> (1/"\[FormalM]")*Total[((Inverse[newMatrixRepresentation][[First[index],
                     First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                   Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                     Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                    Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                    First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                 Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],Last[
                index]]]*"\[FormalM]"*newConjugateScalarField*newScalarField] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ], False, False]]
StressEnergyTensor["MassiveScalarField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := Module[{scalarField, conjugateScalarField, newMatrixRepresentation, newScalarField, 
     newConjugateScalarField, newCoordinates}, scalarField = "\[FormalPsi]" @@ coordinates; 
     conjugateScalarField = OverBar["\[FormalPsi]"] @@ coordinates; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/"\[FormalM]")*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                     First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*"\[FormalM]"*newConjugateScalarField*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], False, False]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["MassiveScalarField", index1_, index2_] := 
  Module[{matrixRepresentation, scalarField, conjugateScalarField, coordinates, newMatrixRepresentation, newScalarField, 
     newConjugateScalarField, newCoordinates}, matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; 
     scalarField = "\[FormalPsi]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     conjugateScalarField = OverBar["\[FormalPsi]"] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/"\[FormalM]")*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                     First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*"\[FormalM]"*newConjugateScalarField*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], index1, index2]] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["MassiveScalarField", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := Module[{scalarField, conjugateScalarField, newMatrixRepresentation, 
     newScalarField, newConjugateScalarField, newCoordinates}, scalarField = "\[FormalPsi]" @@ coordinates; 
     conjugateScalarField = OverBar["\[FormalPsi]"] @@ coordinates; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/"\[FormalM]")*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                     First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*"\[FormalM]"*newConjugateScalarField*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], index1, index2]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"MassiveScalarField", mass_}] := Module[{matrixRepresentation, scalarField, conjugateScalarField, 
    coordinates, newMatrixRepresentation, newScalarField, newConjugateScalarField, newCoordinates}, 
   matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; 
    scalarField = "\[FormalPsi]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    conjugateScalarField = OverBar["\[FormalPsi]"] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
     Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                     First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                   Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                     Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                    Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                    First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                 Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],Last[
                index]]]*mass*newConjugateScalarField*newScalarField] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ], False, False]]
StressEnergyTensor[{"MassiveScalarField", mass_}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := Module[{scalarField, conjugateScalarField, newMatrixRepresentation, newScalarField, 
     newConjugateScalarField, newCoordinates}, scalarField = "\[FormalPsi]" @@ coordinates; 
     conjugateScalarField = OverBar["\[FormalPsi]"] @@ coordinates; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                     First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*mass*newConjugateScalarField*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], False, False]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor[{"MassiveScalarField", mass_}, index1_, index2_] := 
  Module[{matrixRepresentation, scalarField, conjugateScalarField, coordinates, newMatrixRepresentation, newScalarField, 
     newConjugateScalarField, newCoordinates}, matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; 
     scalarField = "\[FormalPsi]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     conjugateScalarField = OverBar["\[FormalPsi]"] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                     First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*mass*newConjugateScalarField*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], index1, index2]] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"MassiveScalarField", mass_}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  Module[{scalarField, conjugateScalarField, newMatrixRepresentation, newScalarField, newConjugateScalarField, 
     newCoordinates}, scalarField = "\[FormalPsi]" @@ coordinates; conjugateScalarField = OverBar["\[FormalPsi]"] @@ coordinates; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newConjugateScalarField = conjugateScalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*D[newConjugateScalarField, newCoordinates[[
                     First[#1]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*mass*newConjugateScalarField*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], index1, index2]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"MassiveScalarField", mass_, scalarFunction_}] := 
  Module[{matrixRepresentation, scalarField, coordinates, newMatrixRepresentation, newScalarField, newCoordinates}, 
   matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; 
    scalarField = scalarFunction @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
    StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
     Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                     First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                   Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                     Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                    Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*Conjugate[D[newScalarField, newCoordinates[[
                     First[#1]]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                 Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],Last[
                index]]]*mass*Conjugate[newScalarField]*newScalarField] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ], False, False]]
StressEnergyTensor[{"MassiveScalarField", mass_, scalarFunction_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  Module[{scalarField, newMatrixRepresentation, newScalarField, newCoordinates}, 
    scalarField = scalarFunction @@ coordinates; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*Conjugate[D[newScalarField, newCoordinates[[
                      First[#1]]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*mass*Conjugate[newScalarField]*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], False, False]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor[{"MassiveScalarField", mass_, scalarFunction_}, index1_, index2_] := 
  Module[{matrixRepresentation, scalarField, coordinates, newMatrixRepresentation, newScalarField, newCoordinates}, 
    matrixRepresentation = DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]; 
     scalarField = scalarFunction @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     coordinates = Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, True, True], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*Conjugate[D[newScalarField, newCoordinates[[
                      First[#1]]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*mass*Conjugate[newScalarField]*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], index1, index2]] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"MassiveScalarField", mass_, scalarFunction_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  Module[{scalarField, newMatrixRepresentation, newScalarField, newCoordinates}, 
    scalarField = scalarFunction @@ coordinates; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newScalarField = scalarField /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     StressEnergyTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/mass)*Total[((Inverse[newMatrixRepresentation][[First[index],
                      First[#1]]]*Inverse[newMatrixRepresentation][[Last[#1],Last[index]]] + 
                    Inverse[newMatrixRepresentation][[First[index],Last[#1]]]*Inverse[newMatrixRepresentation][[First[#1],
                      Last[index]]] - Inverse[newMatrixRepresentation][[First[index],Last[index]]]*
                     Inverse[newMatrixRepresentation][[First[#1],Last[#1]]])*Conjugate[D[newScalarField, newCoordinates[[
                      First[#1]]]]]*D[newScalarField, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                  Range[Length[newMatrixRepresentation]], 2]] - Inverse[newMatrixRepresentation][[First[index],
                Last[index]]]*mass*Conjugate[newScalarField]*newScalarField] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ], index1, index2]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[matrixRepresentation_List] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[matrixRepresentation] - 1]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[matrixRepresentation] - 1]], True, True], 
    matrixRepresentation, False, False] /; Length[Dimensions[matrixRepresentation]] == 2
StressEnergyTensor[matrixRepresentation_List, (metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[ResourceFunction["MetricTensor"][metricMatrixRepresentation, 
     coordinates, metricIndex1, metricIndex2], matrixRepresentation, False, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation]
StressEnergyTensor[matrixRepresentation_List, index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][DiagonalMatrix[
      Join[{-1}, ConstantArray[1, Length[matrixRepresentation] - 1]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[matrixRepresentation] - 1]], True, True], 
    matrixRepresentation, index1, index2] /; Length[Dimensions[matrixRepresentation]] == 2 && BooleanQ[index1] && 
    BooleanQ[index2]
StressEnergyTensor[matrixRepresentation_List, (metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, 
     metricIndex2], matrixRepresentation, index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["MatrixRepresentation"] := 
  If[index1 === True && index2 === True, 
    Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
              metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
       Tuples[Range[Length[metricMatrixRepresentation]], 2]]], If[index1 === False && index2 === False, 
     matrixRepresentation, If[index1 === True && index2 === False, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],#1]]*
                matrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]], If[index1 === False && index2 === True, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                 matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]], Indeterminate]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedMatrixRepresentation"] := 
  If[index1 === True && index2 === True, FullSimplify[
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
               metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
             Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
        Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], If[index1 === False && index2 === False, 
     FullSimplify[matrixRepresentation], If[index1 === True && index2 === False, 
      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],#1]]*
                 matrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], If[index1 === False && index2 === True, 
       FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                  matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], Indeterminate]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Trace"] := 
  Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
     Tuples[Range[Length[metricMatrixRepresentation]], 2]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedTrace"] := 
  FullSimplify[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
      Tuples[Range[Length[metricMatrixRepresentation]], 2]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["MetricTensor"] := 
  ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Coordinates"] := 
  coordinates /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Indices"] := 
  {index1, index2} /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 
     2 && Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && 
    BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["CovariantQ"] := If[index1 === True && index2 === True, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ContravariantQ"] := 
  If[index1 === False && index2 === False, True, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["MixedQ"] := 
  If[(index1 === True && index2 === False) || (index1 === False && index2 === True), True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Symbol"] := 
  If[index1 === True && index2 === True, Subscript["\[FormalCapitalT]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
     Superscript["\[FormalCapitalT]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, Subsuperscript["\[FormalCapitalT]", "\[FormalMu]", "\[FormalNu]"], 
      If[index1 === False && index2 === True, Subsuperscript["\[FormalCapitalT]", "\[FormalNu]", "\[FormalMu]"], Indeterminate]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["EnergyDensity"] := First[First[matrixRepresentation]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedEnergyDensity"] := 
  FullSimplify[First[First[matrixRepresentation]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["MomentumDensity"] := 
  ((1/2)*(matrixRepresentation[[1,#1 + 1]] + matrixRepresentation[[#1 + 1,1]]) & ) /@ 
    Range[Length[matrixRepresentation] - 1] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedMomentumDensity"] := 
  FullSimplify[((1/2)*(matrixRepresentation[[1,#1 + 1]] + matrixRepresentation[[#1 + 1,1]]) & ) /@ 
     Range[Length[matrixRepresentation] - 1]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["SpacetimeMomentumDensity"] := 
  ((1/2)*(matrixRepresentation[[1,#1]] + matrixRepresentation[[#1,1]]) & ) /@ Range[Length[matrixRepresentation]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedSpacetimeMomentumDensity"] := 
  FullSimplify[((1/2)*(matrixRepresentation[[1,#1]] + matrixRepresentation[[#1,1]]) & ) /@ 
     Range[Length[matrixRepresentation]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Pressure"] := 
  Total[((1/(Length[matrixRepresentation] - 1))*matrixRepresentation[[#1 + 1,#1 + 1]] & ) /@ 
     Range[Length[matrixRepresentation] - 1]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedPressure"] := 
  FullSimplify[Total[((1/(Length[matrixRepresentation] - 1))*matrixRepresentation[[#1 + 1,#1 + 1]] & ) /@ 
      Range[Length[matrixRepresentation] - 1]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["StressTensor"] := 
  Normal[SparseArray[(#1 -> matrixRepresentation[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
      Tuples[Range[Length[matrixRepresentation] - 1], 2]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedStressTensor"] := 
  FullSimplify[Normal[SparseArray[(#1 -> matrixRepresentation[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation] - 1], 2]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ShearStressTensor"] := 
  Normal[SparseArray[(#1 -> matrixRepresentation[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation] - 1], 2]]] - 
    Total[((1/(Length[matrixRepresentation] - 1))*matrixRepresentation[[#1 + 1,#1 + 1]] & ) /@ 
       Range[Length[matrixRepresentation] - 1]]*IdentityMatrix[Length[matrixRepresentation] - 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedShearStressTensor"] := 
  FullSimplify[Normal[SparseArray[(#1 -> matrixRepresentation[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation] - 1], 2]]] - 
     Total[((1/(Length[matrixRepresentation] - 1))*matrixRepresentation[[#1 + 1,#1 + 1]] & ) /@ 
        Range[Length[matrixRepresentation] - 1]]*IdentityMatrix[Length[matrixRepresentation] - 1]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["CovariantDerivatives"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     stressEnergyTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; If[index1 === True && index2 === True, 
      stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[
                   First[index],First[#1]]]*newMetricMatrixRepresentation[[Last[#1],Last[index]]]*
                  newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[
                   newMetricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
            2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                  index[[1]]]], StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalT]", StringJoin[
                 ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                  StandardForm]]], StandardForm]] -> D[stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[
                index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*stressEnergyTensor[[#1,
                   index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       stressEnergyTensor = newMatrixRepresentation; Association[
         (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalT]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> D[stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + Total[
                (christoffelSymbols[[index[[2]],index[[1]],#1]]*stressEnergyTensor[[#1,index[[3]]]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                   stressEnergyTensor[[index[[2]],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[
                     First[index],#1]]*newMatrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                    newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalT]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               D[stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMetricMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    stressEnergyTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[#1,
                      Last[index]]]*newMatrixRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalT]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                D[stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*stressEnergyTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMetricMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     stressEnergyTensor[[index[[2]],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedCovariantDerivatives"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     stressEnergyTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; If[index1 === True && index2 === True, 
      stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[
                   First[index],First[#1]]]*newMetricMatrixRepresentation[[Last[#1],Last[index]]]*
                  newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[
                   newMetricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
            2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                  index[[1]]]], StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalT]", StringJoin[
                 ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                  StandardForm]]], StandardForm]] -> FullSimplify[D[stressEnergyTensor[[index[[2]],index[[3]]]], 
                newCoordinates[[index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*stressEnergyTensor[[
                    #1,index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - Total[
                (christoffelSymbols[[#1,index[[1]],index[[3]]]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       stressEnergyTensor = newMatrixRepresentation; Association[
         (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalT]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> FullSimplify[D[stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[
                  index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*stressEnergyTensor[[#1,
                     index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMetricMatrixRepresentation]]]]] & ) /@ 
           Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[
                     First[index],#1]]*newMatrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                    newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalT]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> FullSimplify[
                D[stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                   Range[Length[newMetricMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                     stressEnergyTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]]] & ) /@ 
            Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[#1,
                      Last[index]]]*newMatrixRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalT]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                FullSimplify[D[stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                  Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*stressEnergyTensor[[#1,index[[3]]]] & ) /@ 
                    Range[Length[newMetricMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                      stressEnergyTensor[[index[[2]],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]]] & ) /@ 
             Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["SymbolicCovariantDerivatives"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, christoffelSymbols, 
     stressEnergyTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newMatrixRepresentation = 
      matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; If[index1 === True && index2 === True, 
      stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[
                   First[index],First[#1]]]*newMetricMatrixRepresentation[[Last[#1],Last[index]]]*
                  newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[
                   newMetricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
            2]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                  index[[1]]]], StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalT]", StringJoin[
                 ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                  StandardForm]]], StandardForm]] -> Inactive[D][stressEnergyTensor[[index[[2]],index[[3]]]], 
               newCoordinates[[index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*stressEnergyTensor[[
                   #1,index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False, 
       stressEnergyTensor = newMatrixRepresentation; Association[
         (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalT]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]]], 
                StandardForm]] -> Inactive[D][stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[
                 index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*stressEnergyTensor[[#1,
                    index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[3]],index[[1]],#1]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                 Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False, 
        stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[
                     First[index],#1]]*newMatrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                    newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalT]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
               Inactive[D][stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*stressEnergyTensor[[index[[2]],#1]] & ) /@ 
                  Range[Length[newMetricMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                    stressEnergyTensor[[#1,index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True, 
         stressEnergyTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[#1,
                      Last[index]]]*newMatrixRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalT]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                Inactive[D][stressEnergyTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] + 
                 Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*stressEnergyTensor[[#1,index[[3]]]] & ) /@ 
                   Range[Length[newMetricMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                     stressEnergyTensor[[index[[2]],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMetricMatrixRepresentation]], 3] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ContinuityEquations"] := 
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
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedContinuityEquations"] := 
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
          3]]]; FullSimplify[(Module[{index = #1}, D[newMatrixRepresentation[[First[index],Last[index]]], 
             newCoordinates[[Last[index]]]] + Total[(christoffelSymbols[[First[index],Last[index],#1]]*
                newMatrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
            Total[(christoffelSymbols[[Last[index],Last[index],#1]]*newMatrixRepresentation[[First[index],#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] == 0] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["SymbolicContinuityEquations"] := 
  Module[{newMetricMatrixRepresentation, newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Dimensions"] := Length[metricMatrixRepresentation] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["SymmetricQ"] := SymmetricMatrixQ[matrixRepresentation] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["DiagonalQ"] := DiagonalMatrixQ[matrixRepresentation] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Signature"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 
     2 && Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && 
    BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["RiemannianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[metricMatrixRepresentation] || Length[negativeEigenvalues] == 
         Length[metricMatrixRepresentation], True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["PseudoRiemannianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[metricMatrixRepresentation] || Length[negativeEigenvalues] == 
         Length[metricMatrixRepresentation], False, True], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["LorentzianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["RiemannianConditions"] := 
  Module[{eigenvalues, riemannianConditions}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; If[riemannianConditions === True, {}, 
      If[riemannianConditions === False, Indeterminate, If[Length[Select[riemannianConditions, #1 === False & ]] > 0, 
        Indeterminate, DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["PseudoRiemannianConditions"] := 
  Module[{eigenvalues, pseudoRiemannianConditions}, eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
     pseudoRiemannianConditions = FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["LorentzianConditions"] := 
  Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinate, lorentzianConditions}, 
    eigensystem = Eigensystem[metricMatrixRepresentation]; eigenvalues = First[eigensystem]; 
     eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinate, eigenvalues[[#1]] < 0, eigenvalues[[#1]] > 0] & ) /@ 
          Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, If[lorentzianConditions === False, 
         Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, Indeterminate, 
          DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["StressEnergySingularities"] := 
  (Quiet[DeleteDuplicates[Catenate[
       (If[Head[Solve[#1, coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]]] === Solve, {{#1}}, 
          Solve[#1, coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]]] & ) /@ 
        Flatten[{FunctionSingularities[Catenate[FullSimplify[matrixRepresentation] /. (#1 -> ToExpression[#1] & ) /@ 
               Select[coordinates, StringQ]], coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, 
               StringQ]] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]) /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["TraceSingularities"] := 
  (Quiet[DeleteDuplicates[Catenate[
       (If[Head[Solve[#1, coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]]] === Solve, {{#1}}, 
          Solve[#1, coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]]] & ) /@ 
        Flatten[{FunctionSingularities[FullSimplify[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*
                  matrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
                 2]]] /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ], 
            coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]] /. Or -> List}]]]] /. 
    (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]) /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Determinant"] := Det[matrixRepresentation] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedDeterminant"] := FullSimplify[Det[matrixRepresentation]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Eigenvalues"] := Eigenvalues[matrixRepresentation] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedEigenvalues"] := 
  FullSimplify[Eigenvalues[matrixRepresentation]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Eigenvectors"] := Eigenvectors[matrixRepresentation] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedEigenvectors"] := 
  FullSimplify[Eigenvectors[matrixRepresentation]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["CovariantStressEnergyTensor"] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, 
     metricIndex2], matrixRepresentation, True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ContravariantStressEnergyTensor"] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, 
     metricIndex2], matrixRepresentation, False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["NullEnergyCondition"] := 
  Module[{covariantStressEnergyTensor}, covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
           Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] == 0, 
      Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
           Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 0]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedNullEnergyCondition"] := 
  Module[{covariantStressEnergyTensor}, covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     FullSimplify[Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
            Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] == 0, 
       Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
            Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 
        0]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["WeakEnergyCondition"] := 
  Module[{covariantStressEnergyTensor}, covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
           Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] < 0, 
      Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
           Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 0]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedWeakEnergyCondition"] := 
  Module[{covariantStressEnergyTensor}, covariantStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     FullSimplify[Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
            Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] < 0, 
       Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
            Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 
        0]]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["DominantEnergyCondition"] := 
  Module[{covariantStressEnergyTensor, mixedStressEnergyTensor}, 
    covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; mixedStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
           Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] < 0, 
      Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
            Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 0 && 
       Total[(Module[{index = #1}, metricMatrixRepresentation[[First[index],Last[index]]]*
             (Total[(mixedStressEnergyTensor[[First[index],#1]]*Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]]*Total[(mixedStressEnergyTensor[[Last[index],#1]]*
                  Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ Range[Length[metricMatrixRepresentation]]])] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]] <= 0]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedDominantEnergyCondition"] := 
  Module[{covariantStressEnergyTensor, mixedStressEnergyTensor}, 
    covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; mixedStressEnergyTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
     FullSimplify[Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
            Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] < 0, 
       Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
             Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 0 && 
        Total[(Module[{index = #1}, metricMatrixRepresentation[[First[index],Last[index]]]*
              (Total[(mixedStressEnergyTensor[[First[index],#1]]*Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ 
                 Range[Length[metricMatrixRepresentation]]]*Total[(mixedStressEnergyTensor[[Last[index],#1]]*
                   Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ Range[Length[metricMatrixRepresentation]]])] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]] <= 0]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["StrongEnergyCondition"] := 
  Module[{covariantStressEnergyTensor, stressEnergyTrace}, 
    covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; stressEnergyTrace = 
      Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
           Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] < 0, 
      Total[((covariantStressEnergyTensor[[First[#1],Last[#1]]] - (1/2)*stressEnergyTrace*matrixRepresentation[[First[#1],
              Last[#1]]])*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 0]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedStrongEnergyCondition"] := 
  Module[{covariantStressEnergyTensor, stressEnergyTrace}, 
    covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; stressEnergyTrace = 
      Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[metricMatrixRepresentation]], 2]]; 
     FullSimplify[Implies[Total[(metricMatrixRepresentation[[First[#1],Last[#1]]]*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*
            Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]] < 0, 
       Total[((covariantStressEnergyTensor[[First[#1],Last[#1]]] - (1/2)*stressEnergyTrace*matrixRepresentation[[First[
                #1],Last[#1]]])*Superscript["\[FormalCapitalX]", ToString[First[#1]]]*Superscript["\[FormalCapitalX]", ToString[Last[#1]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]] >= 0]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], matrixRepresentation_List, index1_, index2_], newCoordinates_List] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][metricMatrixRepresentation /. 
      Thread[coordinates -> newCoordinates], newCoordinates, metricIndex1, metricIndex2], 
    matrixRepresentation /. Thread[coordinates -> newCoordinates], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && Length[newCoordinates] == 
     Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], matrixRepresentation_List, index1_, index2_], newIndex1_, newIndex2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, 
     metricIndex2], matrixRepresentation, newIndex1, newIndex2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[newIndex1] && 
    BooleanQ[newIndex2]
StressEnergyTensor[StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], matrixRepresentation_List, index1_, index2_], newCoordinates_List, newIndex1_, newIndex2_] := 
  StressEnergyTensor[ResourceFunction["MetricTensor"][metricMatrixRepresentation /. 
      Thread[coordinates -> newCoordinates], newCoordinates, metricIndex1, metricIndex2], 
    matrixRepresentation /. Thread[coordinates -> newCoordinates], newIndex1, newIndex2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && Length[newCoordinates] == 
     Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[newIndex1] && BooleanQ[newIndex2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Properties"] := 
  {"MatrixRepresentation", "ReducedMatrixRepresentation", "Trace", "ReducedTrace", "MetricTensor", "Coordinates", 
    "CoordinateOneForms", "Indices", "CovariantQ", "ContravariantQ", "MixedQ", "Symbol", "EnergyDensity", 
    "ReducedEnergyDensity", "MomentumDensity", "ReducedMomentumDensity", "SpacetimeMomentumDensity", 
    "ReducedSpacetimeMomentumDensity", "Pressure", "ReducedPressure", "StressTensor", "ReducedStressTensor", 
    "ShearStressTensor", "ReducedShearStressTensor", "CovariantDerivatives", "ReducedCovariantDerivatives", 
    "SymbolicCovariantDerivatives", "ContinuityEquations", "ReducedContinuityEquations", "SymbolicContinuityEquations", 
    "Dimensions", "SymmetricQ", "DiagonalQ", "Signature", "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", 
    "RiemannianConditions", "PseudoRiemannianConditions", "LorentzianConditions", "StressEnergySingularities", 
    "TraceSingularities", "Determinant", "ReducedDeterminant", "Eigenvalues", "ReducedEigenvalues", "Eigenvectors", 
    "ReducedEigenvectors", "CovariantStressEnergyTensor", "ContravariantStressEnergyTensor", "NullEnergyCondition", 
    "ReducedNullEnergyCondition", "WeakEnergyCondition", "ReducedWeakEnergyCondition", "DominantEnergyCondition", 
    "ReducedDominantEnergyCondition", "StrongEnergyCondition", "ReducedStrongEnergyCondition", "Properties"} /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor /: MakeBoxes[stressEnergyTensor:StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, 
       coordinates_List, metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], format_] := 
   Module[{matrixForm, type, symbol, dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, signature, icon}, 
     If[index1 === True && index2 === True, 
       matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],
                    First[#1]]]*metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],
                    Last[#1]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; type = "Covariant"; 
        symbol = Subscript["\[FormalCapitalT]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, matrixForm = matrixRepresentation; 
         type = "Contravariant"; symbol = Superscript["\[FormalCapitalT]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
         matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],
                      #1]]*matrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                     metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
          type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalT]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, 
          matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,
                       Last[index]]]*matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[
                      metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]]; 
           type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalT]", "\[FormalNu]", "\[FormalMu]"], 
          matrixForm = ConstantArray[Indeterminate, {Length[matrixRepresentation], Length[matrixRepresentation]}]; 
           type = Indeterminate; symbol = Indeterminate]]]]; dimensions = Length[metricMatrixRepresentation]; 
      eigenvalues = Eigenvalues[metricMatrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[metricMatrixRepresentation] || Length[negativeEigenvalues] == 
          Length[metricMatrixRepresentation], signature = "Riemannian", 
        If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, signature = "Lorentzian", 
         signature = "Pseudo-Riemannian"]], signature = Indeterminate]; 
      icon = MatrixPlot[matrixForm, ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/
             AbsoluteCurrentValue[Magnification])}], Frame -> False, FrameTicks -> None]; 
      BoxForm`ArrangeSummaryBox["StressEnergyTensor", stressEnergyTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
     Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
     BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_][row_Integer, column_Integer] := 
  If[index1 === True && index2 === True, 
    Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
               metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
             Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
        Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[row,column]], If[index1 === False && index2 === False, 
     matrixRepresentation[[row,column]], If[index1 === True && index2 === False, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],#1]]*
                 matrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[row,column]], If[index1 === False && index2 === True, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                  matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[row,column]], Indeterminate]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_][row_Integer, All] := 
  If[index1 === True && index2 === True, 
    Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
               metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
             Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
        Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[row,All]], If[index1 === False && index2 === False, 
     matrixRepresentation[[row,All]], If[index1 === True && index2 === False, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],#1]]*
                 matrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[row,All]], If[index1 === False && index2 === True, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                  matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[row,All]], Indeterminate]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_][All, column_Integer] := 
  If[index1 === True && index2 === True, 
    Transpose[
     {Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],First[#1]]]*
                 metricMatrixRepresentation[[Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
               Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[All,column]]}], 
    If[index1 === False && index2 === False, Transpose[{matrixRepresentation[[All,column]]}], 
     If[index1 === True && index2 === False, 
      Transpose[{Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[First[index],#1]]*
                   matrixRepresentation[[#1,Last[index]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[All,column]]}], 
      If[index1 === False && index2 === True, Transpose[
        {Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,Last[index]]]*
                    matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[metricMatrixRepresentation]], 2]]][[All,column]]}], Indeterminate]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
