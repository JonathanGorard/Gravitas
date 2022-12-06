(* ::Package:: *)

StressEnergyTensor["PerfectFluid"] := StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
   Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
           ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["PerfectFluid", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_]] := StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
            ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], 
    False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2]
StressEnergyTensor["PerfectFluid", index1_, index2_] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
            ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor["PerfectFluid", (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
    metricIndex2_], index1_, index2_] := StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, 
     metricIndex1], Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*
           Superscript["\[FormalU]", ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], False, False] /; Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_]] := StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, 
     metricIndex2], Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}, index1_, index2_] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], False, False] /; Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}, index1_, index2_] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_}, (metricTensor_)[matrixRepresentation_List, 
    coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], False, False] /; Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_]] := 
  StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}, index1_, index2_] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], index1, index2] /; BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List, density_, pressure_}, 
   (metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_] := 
  StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          pressure*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[fourVelocity] == 4
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
    matrixRepresentation_List, index1_, index2_]["MetricTensor"] := 
  MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2] /; 
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
    matrixRepresentation_List, index1_, index2_]["Energy"] := First[First[matrixRepresentation]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedEnergy"] := FullSimplify[First[First[matrixRepresentation]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Momentum"] := 
  ((1/2)*(matrixRepresentation[[1,#1 + 1]] + matrixRepresentation[[#1 + 1,1]]) & ) /@ 
    Range[Length[matrixRepresentation] - 1] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ReducedMomentum"] := 
  FullSimplify[((1/2)*(matrixRepresentation[[1,#1 + 1]] + matrixRepresentation[[#1 + 1,1]]) & ) /@ 
     Range[Length[matrixRepresentation] - 1]] /; SymbolName[metricTensor] === "MetricTensor" && 
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
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex1] && 
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
  StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    matrixRepresentation, True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ContravariantStressEnergyTensor"] := 
  StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    matrixRepresentation, False, False] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, 
     metricIndex2_], matrixRepresentation_List, index1_, index2_], newIndex1_, newIndex2_] := 
  StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    matrixRepresentation, newIndex1, newIndex2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[newIndex1] && 
    BooleanQ[newIndex2]
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
