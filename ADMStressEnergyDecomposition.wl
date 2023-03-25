(* ::Package:: *)

ADMStressEnergyDecomposition["PerfectFluid"] := Module[{lapseFunction, shiftVector, matrixRepresentation, shiftCovector, 
    spacetimeMetricTensor}, lapseFunction = "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]; 
    shiftVector = (Module[{index = #1}, Superscript["\[FormalBeta]", index] @@ Join[{"\[FormalT]"}, 
          (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]]] & ) /@ Range[3]; 
    matrixRepresentation = DiagonalMatrix[ConstantArray[1, 3]]; 
    shiftCovector = Normal[SparseArray[
       (Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ 
             Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]]; 
    spacetimeMetricTensor = Normal[SparseArray[
       Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[matrixRepresentation]]] - 
           lapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
            Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]; 
    ADMStressEnergyDecomposition[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
       matrixRepresentation, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3], True, True], "\[FormalT]", lapseFunction, 
      shiftVector], Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*
            Superscript["\[FormalU]", ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[4], 2]]]]]
ADMStressEnergyDecomposition["PerfectFluid", (admDecomposition_)[(metricTensor_)[matrixRepresentation_List, 
     coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List]] := 
  Module[{shiftCovector, spacetimeMetricTensor}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; ADMStressEnergyDecomposition[
      ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, 
        index2], timeCoordinate, lapseFunction, shiftVector], 
      Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
              ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMStressEnergyDecomposition[{"PerfectFluid", fourVelocity_List}] := 
  Module[{lapseFunction, shiftVector, matrixRepresentation, shiftCovector, spacetimeMetricTensor}, 
   lapseFunction = "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]; 
    shiftVector = (Module[{index = #1}, Superscript["\[FormalBeta]", index] @@ Join[{"\[FormalT]"}, 
          (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]] & ) /@ 
      Range[Length[fourVelocity] - 1]; matrixRepresentation = DiagonalMatrix[ConstantArray[1, Length[fourVelocity] - 1]]; 
    shiftCovector = Normal[SparseArray[
       (Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ 
             Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]]; 
    spacetimeMetricTensor = Normal[SparseArray[
       Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[matrixRepresentation]]] - 
           lapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
            Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]; 
    ADMStressEnergyDecomposition[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
       matrixRepresentation, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1], True, True], 
      "\[FormalT]", lapseFunction, shiftVector], 
     Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
           "\[FormalCapitalP]"*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 
         2]]]]]
ADMStressEnergyDecomposition[{"PerfectFluid", fourVelocity_List}, 
   (admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]] := Module[{shiftCovector, spacetimeMetricTensor}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; ADMStressEnergyDecomposition[
      ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, 
        index2], timeCoordinate, lapseFunction, shiftVector], 
      Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
            "\[FormalCapitalP]"*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation] && Length[fourVelocity] == Length[matrixRepresentation] + 1
ADMStressEnergyDecomposition[{"PerfectFluid", fourVelocity_List, density_}] := 
  Module[{lapseFunction, shiftVector, matrixRepresentation, shiftCovector, spacetimeMetricTensor}, 
   lapseFunction = "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]; 
    shiftVector = (Module[{index = #1}, Superscript["\[FormalBeta]", index] @@ Join[{"\[FormalT]"}, 
          (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]] & ) /@ 
      Range[Length[fourVelocity] - 1]; matrixRepresentation = DiagonalMatrix[ConstantArray[1, Length[fourVelocity] - 1]]; 
    shiftCovector = Normal[SparseArray[
       (Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ 
             Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]]; 
    spacetimeMetricTensor = Normal[SparseArray[
       Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[matrixRepresentation]]] - 
           lapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
            Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]; 
    ADMStressEnergyDecomposition[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
       matrixRepresentation, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1], True, True], 
      "\[FormalT]", lapseFunction, shiftVector], 
     Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
           "\[FormalCapitalP]"*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 
         2]]]]]
ADMStressEnergyDecomposition[{"PerfectFluid", fourVelocity_List, density_}, 
   (admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]] := Module[{shiftCovector, spacetimeMetricTensor}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; ADMStressEnergyDecomposition[
      ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, 
        index2], timeCoordinate, lapseFunction, shiftVector], 
      Normal[SparseArray[(#1 -> (density + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
            "\[FormalCapitalP]"*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation] && Length[fourVelocity] == Length[matrixRepresentation] + 1
ADMStressEnergyDecomposition[{"PerfectFluid", fourVelocity_List, density_, pressure_}] := 
  Module[{lapseFunction, shiftVector, matrixRepresentation, shiftCovector, spacetimeMetricTensor}, 
   lapseFunction = "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]; 
    shiftVector = (Module[{index = #1}, Superscript["\[FormalBeta]", index] @@ Join[{"\[FormalT]"}, 
          (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1]]] & ) /@ 
      Range[Length[fourVelocity] - 1]; matrixRepresentation = DiagonalMatrix[ConstantArray[1, Length[fourVelocity] - 1]]; 
    shiftCovector = Normal[SparseArray[
       (Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ 
             Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]]]]; 
    spacetimeMetricTensor = Normal[SparseArray[
       Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[matrixRepresentation]]] - 
           lapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
            Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]; 
    ADMStressEnergyDecomposition[ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
       matrixRepresentation, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[Length[fourVelocity] - 1], True, True], 
      "\[FormalT]", lapseFunction, shiftVector], 
     Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
           pressure*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]]]]
ADMStressEnergyDecomposition[{"PerfectFluid", fourVelocity_List, density_, pressure_}, 
   (admDecomposition_)[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]] := Module[{shiftCovector, spacetimeMetricTensor}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; ADMStressEnergyDecomposition[
      ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, 
        index2], timeCoordinate, lapseFunction, shiftVector], 
      Normal[SparseArray[(#1 -> (density + pressure)*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
            pressure*Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation] && Length[fourVelocity] == Length[matrixRepresentation] + 1
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "ADMDecomposition"] := ResourceFunction["ADMDecomposition"][ResourceFunction["MetricTensor"][
     metricMatrixRepresentation, coordinates, index1, index2], timeCoordinate, lapseFunction, shiftVector] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "SpatialMetricTensor"] := ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, index1, index2] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "SpacetimeMetricTensor"] := 
  Module[{shiftCovector}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
         Range[Length[metricMatrixRepresentation]]]]; ResourceFunction["MetricTensor"][
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[metricMatrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(metricMatrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ 
               Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(metricMatrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ 
               Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> metricMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], Join[{timeCoordinate}, coordinates], True, True]] /; 
   SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "StressEnergyTensor"] := 
  Module[{shiftCovector}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
         Range[Length[metricMatrixRepresentation]]]]; ResourceFunction["StressEnergyTensor"][matrixRepresentation, 
      ResourceFunction["MetricTensor"][Normal[SparseArray[
         Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[metricMatrixRepresentation]]] - 
             lapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(metricMatrixRepresentation[[index,#1]]*
                  shiftVector[[#1]] & )[Range[Length[metricMatrixRepresentation]]]]] & ) /@ 
           Range[Length[metricMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
              Total[(metricMatrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
          ({First[#1] + 1, Last[#1] + 1} -> metricMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]]]], Join[{timeCoordinate}, coordinates], True, True], 
      False, False]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[metricMatrixRepresentation] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "EnergyDensity"] := Module[{newMatrixRepresentation, newMetricMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spacetimeMetricTensor, covariantStressEnergyTensor, normalVector}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMetricMatrixRepresentation = 
      metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spacetimeMetricTensor[[First[index],First[#1]]]*spacetimeMetricTensor[[
                 Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[
                Length[spacetimeMetricTensor]], 2]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*normalVector[[First[#1]]]*normalVector[[Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[metricMatrixRepresentation] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "MomentumDensity"] := Module[{newMatrixRepresentation, newMetricMatrixRepresentation, newCoordinates, 
     newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, spacetimeMetricTensor, 
     covariantStressEnergyTensor, normalVector, projectionOperator, momentumCovector}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMetricMatrixRepresentation = 
      metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spacetimeMetricTensor[[First[index],First[#1]]]*spacetimeMetricTensor[[
                 Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[
                Length[spacetimeMetricTensor]], 2]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     momentumCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*normalVector[[
                  First[#1]]]*projectionOperator[[index + 1,Last[#1]]] & ) /@ Tuples[
                Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[index,#1]]*
                momentumCovector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[metricMatrixRepresentation] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "StressTensor"] := Module[{newMatrixRepresentation, newMetricMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newLapseFunction, newShiftVector, shiftCovector, spacetimeMetricTensor, covariantStressEnergyTensor, normalVector, 
     projectionOperator}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMetricMatrixRepresentation = 
      metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index,#1]]*
                newShiftVector[[#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMetricMatrixRepresentation[[index,#1]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMetricMatrixRepresentation[[#1,index]]*newShiftVector[[
                  #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]], ({First[#1] + 1, Last[#1] + 1} -> newMetricMatrixRepresentation[[
             First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]]; 
     covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spacetimeMetricTensor[[First[index],First[#1]]]*spacetimeMetricTensor[[
                 Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[
                Length[spacetimeMetricTensor]], 2]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*
                projectionOperator[[First[index] + 1,First[#1]]]*projectionOperator[[Last[index] + 1,Last[#1]]] & ) /@ 
              Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[metricMatrixRepresentation] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "EnergyConservationEquation"] := Module[{newMatrixRepresentation, newMetricMatrixRepresentation, newCoordinates, 
     newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, 
     extrinsicCurvatureTensor, extrinsicCurvatureTrace, spacetimeMetricTensor, covariantStressEnergyTensor, normalVector, 
     energyDensity, projectionOperator, momentumCovector, momentumVector, stressTensor, contravariantStressTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMetricMatrixRepresentation = 
      metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
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
     covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spacetimeMetricTensor[[First[index],First[#1]]]*spacetimeMetricTensor[[
                 Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[
                Length[spacetimeMetricTensor]], 2]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     energyDensity = Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*normalVector[[First[#1]]]*
          normalVector[[Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     momentumCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*normalVector[[
                  First[#1]]]*projectionOperator[[index + 1,Last[#1]]] & ) /@ Tuples[
                Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]]; 
     momentumVector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[index,#1]]*momentumCovector[[
                 #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]]; stressTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*
                projectionOperator[[First[index] + 1,First[#1]]]*projectionOperator[[Last[index] + 1,Last[#1]]] & ) /@ 
              Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; contravariantStressTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],
                 First[#1]]]*Inverse[newMetricMatrixRepresentation][[Last[#1],Last[index]]]*stressTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     D[energyDensity, newTimeCoordinate] - Total[(newShiftVector[[#1]]*D[energyDensity, newCoordinates[[#1]]] & ) /@ 
          Range[Length[newMetricMatrixRepresentation]]] + newLapseFunction*
         (Total[(D[momentumVector[[#1]], newCoordinates[[#1]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
          Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*momentumVector[[Last[#1]]] & ) /@ 
            Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] - extrinsicCurvatureTrace*energyDensity - 
          Total[(extrinsicCurvatureTensor[[First[#1],Last[#1]]]*contravariantStressTensor[[First[#1],Last[#1]]] & ) /@ 
            Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]) + 
        2*Total[(momentumVector[[#1]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ 
           Range[Length[newMetricMatrixRepresentation]]] == 0 /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[metricMatrixRepresentation] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition[(admDecomposition_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], matrixRepresentation_List][
   "MomentumConservationEquations"] := Module[{newMatrixRepresentation, newMetricMatrixRepresentation, newCoordinates, 
     newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, spatialChristoffelSymbols, 
     extrinsicCurvatureTensor, extrinsicCurvatureTrace, spacetimeMetricTensor, covariantStressEnergyTensor, normalVector, 
     energyDensity, projectionOperator, momentumCovector, stressTensor, mixedStressTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newMetricMatrixRepresentation = 
      metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
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
     covariantStressEnergyTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spacetimeMetricTensor[[First[index],First[#1]]]*spacetimeMetricTensor[[
                 Last[#1],Last[index]]]*matrixRepresentation[[First[#1],Last[#1]]] & ) /@ Tuples[Range[
                Length[spacetimeMetricTensor]], 2]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     energyDensity = Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*normalVector[[First[#1]]]*
          normalVector[[Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     momentumCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*normalVector[[
                  First[#1]]]*projectionOperator[[index + 1,Last[#1]]] & ) /@ Tuples[
                Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]]; 
     stressTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(covariantStressEnergyTensor[[First[#1],Last[#1]]]*projectionOperator[[
                 First[index] + 1,First[#1]]]*projectionOperator[[Last[index] + 1,Last[#1]]] & ) /@ 
              Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; mixedStressTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMetricMatrixRepresentation][[First[index],#1]]*
                stressTensor[[#1,Last[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> D[momentumCovector[[index]], newTimeCoordinate] - 
              Total[(newShiftVector[[#1]]*D[momentumCovector[[index]], newCoordinates[[#1]]] & ) /@ 
                Range[Length[newMetricMatrixRepresentation]]] - Total[(momentumCovector[[#1]]*D[newShiftVector[[#1]], 
                   newCoordinates[[index]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
              newLapseFunction*(Total[(D[mixedStressTensor[[#1,index]], newCoordinates[[#1]]] & ) /@ 
                  Range[Length[newMetricMatrixRepresentation]]] + Total[(spatialChristoffelSymbols[[First[#1],First[#1],
                     Last[#1]]]*mixedStressTensor[[Last[#1],index]] & ) /@ Tuples[Range[Length[
                     newMetricMatrixRepresentation]], 2]] - Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*
                    mixedStressTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
                   2]]) + Total[(stressTensor[[index,First[#1]]]*Inverse[newMetricMatrixRepresentation][[First[#1],
                   Last[#1]]]*D[newLapseFunction, newCoordinates[[Last[#1]]]] & ) /@ Tuples[
                 Range[Length[newMetricMatrixRepresentation]], 2]] - newLapseFunction*extrinsicCurvatureTrace*
               momentumCovector[[index]] + energyDensity*D[newLapseFunction, newCoordinates[[index]]] == 0] & ) /@ 
         Range[Length[newMetricMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[admDecomposition] === "ADMDecomposition" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[metricMatrixRepresentation] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] + 1 == Length[matrixRepresentation]
ADMStressEnergyDecomposition /: 
  MakeBoxes[admStressEnergyDecomposition:ADMStressEnergyDecomposition[(admDecomposition_)[
       (metricTensor_)[metricMatrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
       lapseFunction_, shiftVector_List], matrixRepresentation_List], format_] := 
   Module[{shiftCovector, spacetimeMetricTensor, type, symbol, dimensions, eigenvalues, positiveEigenvalues, 
      negativeEigenvalues, signature, icon}, 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index,#1]]*
                 shiftVector[[#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
          Range[Length[metricMatrixRepresentation]]]]; spacetimeMetricTensor = 
       Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[
                Length[metricMatrixRepresentation]]] - lapseFunction^2}, 
          (Module[{index = #1}, {1, index + 1} -> Total[(metricMatrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
          (Module[{index = #1}, {index + 1, 1} -> Total[(metricMatrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
          ({First[#1] + 1, Last[#1] + 1} -> metricMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
           Tuples[Range[Length[metricMatrixRepresentation]], 2]]]]; dimensions = Length[spacetimeMetricTensor]; 
      eigenvalues = Eigenvalues[spacetimeMetricTensor]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
       If[Length[positiveEigenvalues] == Length[spacetimeMetricTensor] || Length[negativeEigenvalues] == 
          Length[spacetimeMetricTensor], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[matrixRepresentation, 
        ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], 
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["ADMStressEnergyDecomposition", 
       admStressEnergyDecomposition, icon, {{BoxForm`SummaryItem[{"Dimensions: ", dimensions}], 
         BoxForm`SummaryItem[{"Signature: ", signature}]}, {BoxForm`SummaryItem[{"Time Coordinate: ", timeCoordinate}], 
         BoxForm`SummaryItem[{"Spatial Coordinates: ", coordinates}]}}, {{}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[admDecomposition] === "ADMDecomposition" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
     BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[metricMatrixRepresentation] && 
     Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] + 1 == Length[matrixRepresentation]
