(* ::Package:: *)

AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_]] := 
  AngularMomentumDensityTensor[ResourceFunction["StressEnergyTensor"][ResourceFunction["MetricTensor"][
      metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], matrixRepresentation, index1, index2], 
    (Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ Range[Length[coordinates]], False, False, False] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], positionVector_List] := 
  AngularMomentumDensityTensor[ResourceFunction["StressEnergyTensor"][ResourceFunction["MetricTensor"][
      metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], matrixRepresentation, index1, index2], 
    positionVector, False, False, False] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[positionVector]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], index1_, 
   index2_, index3_] := AngularMomentumDensityTensor[ResourceFunction["StressEnergyTensor"][
     ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2], 
     matrixRepresentation, stressEnergyIndex1, stressEnergyIndex2], (Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ 
     Range[Length[coordinates]], index1, index2, index3] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && BooleanQ[index1] && BooleanQ[index2] && 
    BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["TensorRepresentation"] := 
  Module[{angularMomentumDensityTensor}, angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              matrixRepresentation[[index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
              matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
          3]]]; If[index1 === True && index2 === True && index3 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*
                metricMatrixRepresentation[[index[[2]],#1[[2]]]]*metricMatrixRepresentation[[#1[[3]],index[[3]]]]*
                angularMomentumDensityTensor[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[
                Length[metricMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
      If[index1 === False && index2 === False && index3 === False, angularMomentumDensityTensor, 
       If[index1 === True && index2 === False && index3 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1]]*
                  angularMomentumDensityTensor[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                  metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
        If[index1 === False && index2 === True && index3 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[2]],#1]]*
                   angularMomentumDensityTensor[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[
                   metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
         If[index1 === False && index2 === False && index3 === True, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,index[[3]]]]*
                    angularMomentumDensityTensor[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[
                    metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
          If[index1 === True && index2 === True && index3 === False, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*
                     metricMatrixRepresentation[[index[[2]],#1[[2]]]]*angularMomentumDensityTensor[[#1[[1]],#1[[2]],
                      index[[3]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 3]]], If[index1 === True && index2 === False && 
             index3 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[
                       index[[1]],#1[[1]]]]*metricMatrixRepresentation[[#1[[2]],index[[3]]]]*
                      angularMomentumDensityTensor[[#1[[1]],index[[2]],#1[[2]]]] & ) /@ Tuples[Range[Length[
                       metricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
            If[index1 === False && index2 === True && index3 === True, Normal[SparseArray[(Module[{index = #1}, 
                  index -> Total[(metricMatrixRepresentation[[index[[2]],#1[[1]]]]*metricMatrixRepresentation[[#1[[2]],
                        index[[3]]]]*angularMomentumDensityTensor[[index[[1]],#1[[1]],#1[[2]]]] & ) /@ 
                     Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ Tuples[
                 Range[Length[metricMatrixRepresentation]], 3]]], Indeterminate]]]]]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["ReducedTensorRepresentation"] := 
  Module[{angularMomentumDensityTensor}, angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              matrixRepresentation[[index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
              matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
          3]]]; If[index1 === True && index2 === True && index3 === True, 
      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],
                  #1[[1]]]]*metricMatrixRepresentation[[index[[2]],#1[[2]]]]*metricMatrixRepresentation[[#1[[3]],
                  index[[3]]]]*angularMomentumDensityTensor[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                Range[Length[metricMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
           3]]]], If[index1 === False && index2 === False && index3 === False, 
       FullSimplify[angularMomentumDensityTensor], If[index1 == True && index2 === False && index3 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1]]*
                   angularMomentumDensityTensor[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                   metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]], 
        If[index1 === False && index2 === True && index3 === False, FullSimplify[
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[2]],#1]]*
                    angularMomentumDensityTensor[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[
                    metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]], 
         If[index1 === False && index2 === False && index3 === True, FullSimplify[
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[#1,index[[3]]]]*
                     angularMomentumDensityTensor[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[
                     metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]], 
          If[index1 == True && index2 === True && index3 === False, FullSimplify[
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*
                      metricMatrixRepresentation[[index[[2]],#1[[2]]]]*angularMomentumDensityTensor[[#1[[1]],#1[[2]],
                       index[[3]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[metricMatrixRepresentation]], 3]]]], If[index1 === True && index2 === False && 
             index3 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[
                    (metricMatrixRepresentation[[index[[1]],#1[[1]]]]*metricMatrixRepresentation[[#1[[2]],index[[3]]]]*
                       angularMomentumDensityTensor[[#1[[1]],index[[2]],#1[[2]]]] & ) /@ Tuples[Range[Length[
                        metricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
                 3]]]], If[index1 === False && index2 === True && index3 === True, FullSimplify[
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[2]],#1[[1]]]]*
                        metricMatrixRepresentation[[#1[[2]],index[[3]]]]*angularMomentumDensityTensor[[index[[1]],#1[[1]],
                         #1[[2]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
                 Tuples[Range[Length[metricMatrixRepresentation]], 3]]]], Indeterminate]]]]]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["MetricTensor"] := 
  ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, metricIndex1, metricIndex2] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["StressEnergyTensor"] := 
  ResourceFunction["StressEnergyTensor"][ResourceFunction["MetricTensor"][metricMatrixRepresentation, coordinates, 
     metricIndex1, metricIndex2], matrixRepresentation, stressEnergyIndex1, stressEnergyIndex2] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["Coordinates"] := 
  coordinates /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["Indices"] := {index1, index2, index3} /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["CovariantQ"] := 
  If[index1 === True && index2 === True && index3 === True, True, False] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["ContravariantQ"] := 
  If[index1 === False && index2 === False && index3 === False, True, False] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["MixedQ"] := 
  If[ !((index1 === True && index2 === True && index3 === True) || (index1 === False && index2 === False && 
       index3 === False)), True, False] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["Symbol"] := 
  If[index1 === True && index2 === True && index3 === True, Subscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]\[FormalNu]"], 
    If[index1 === False && index2 === False && index3 === False, Superscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]\[FormalNu]"], 
     If[index1 === True && index2 === False && index3 === False, Subsuperscript["\[FormalCapitalM]", "\[FormalRho]", "\[FormalMu]\[FormalNu]"], 
      If[index1 === False && index2 === True && index3 === False, Subsuperscript["\[FormalCapitalM]", "\[FormalMu]", "\[FormalRho]\[FormalNu]"], 
       If[index1 === False && index2 === False && index3 === True, Subsuperscript["\[FormalCapitalM]", "\[FormalNu]", "\[FormalRho]\[FormalMu]"], 
        If[index1 === True && index2 === True && index3 === False, Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]", "\[FormalNu]"], 
         If[index1 === True && index2 === False && index3 === True, Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalNu]", "\[FormalMu]"], 
          If[index1 === False && index2 === True && index3 === True, Subsuperscript["\[FormalCapitalM]", "\[FormalMu]\[FormalNu]", "\[FormalRho]"], 
           Indeterminate]]]]]]]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["VanishingAngularMomentumDensityQ"] := 
  Module[{angularMomentumDensityTensor, fieldEquations}, 
    angularMomentumDensityTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              matrixRepresentation[[index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
              matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
          3]]]; fieldEquations = FullSimplify[Thread[Catenate[Catenate[angularMomentumDensityTensor]] == 
         Catenate[Catenate[ConstantArray[0, {Length[metricMatrixRepresentation], Length[metricMatrixRepresentation], 
             Length[metricMatrixRepresentation]}]]]]]; If[fieldEquations === True, True, 
      If[fieldEquations === False, False, If[Length[Select[fieldEquations, #1 === True & ]] == 
         Length[metricMatrixRepresentation]*Length[metricMatrixRepresentation]*Length[metricMatrixRepresentation], True, 
        False]]]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["VanishingAngularMomentumDensityConditions"] := 
  Module[{angularMomentumDensityTensor, fieldEquations}, 
    angularMomentumDensityTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              matrixRepresentation[[index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
              matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
          3]]]; fieldEquations = FullSimplify[Thread[Catenate[Catenate[angularMomentumDensityTensor]] == 
         Catenate[Catenate[ConstantArray[0, {Length[metricMatrixRepresentation], Length[metricMatrixRepresentation], 
             Length[metricMatrixRepresentation]}]]]]]; If[fieldEquations === True, {}, 
      If[fieldEquations === False, Indeterminate, If[Length[Select[fieldEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[fieldEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["IndexContractions"] := 
  Module[{angularMomentumDensityTensor, newAngularMomentumDensityTensor}, 
    angularMomentumDensityTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              matrixRepresentation[[index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
              matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
          3]]]; If[(index1 === True && index2 === True && index3 === True) || (index1 === False && index2 === False && 
        index3 === False), Association[], If[index1 === True && index2 === False && index3 === False, 
       newAngularMomentumDensityTensor = Normal[SparseArray[
           (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1]]*
                   angularMomentumDensityTensor[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                   metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; 
        Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> 
          (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ Range[
                Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
         Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalMu]\[FormalSigma]"] -> (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,index,
                 #1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
           Range[Length[metricMatrixRepresentation]]], If[index1 === False && index2 === True && index3 === False, 
        newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(metricMatrixRepresentation[[index[[2]],#1]]*angularMomentumDensityTensor[[index[[1]],#1,
                     index[[3]]]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; 
         Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> 
           (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
          Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[index,
                  #1,#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
            Range[Length[metricMatrixRepresentation]]], If[index1 === False && index2 === False && index3 === True, 
         newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                 Total[(metricMatrixRepresentation[[#1,index[[3]]]]*angularMomentumDensityTensor[[index[[1]],index[[2]],
                      #1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[metricMatrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalSigma]\[FormalMu]"] -> 
            (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,index,#1]] & ) /@ 
                 Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
           Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[index,
                   #1,#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
             Range[Length[metricMatrixRepresentation]]], If[index1 === True && index2 === True && index3 === False, 
          newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                  Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*metricMatrixRepresentation[[index[[2]],
                       #1[[2]]]]*angularMomentumDensityTensor[[#1[[1]],#1[[2]],index[[3]]]] & ) /@ 
                    Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[metricMatrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]\[FormalMu]", "\[FormalSigma]"] -> 
             (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,index,#1]] & ) /@ 
                  Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
            Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[index,
                    #1,#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[
                metricMatrixRepresentation]]], If[index1 === True && index2 === False && index3 === True, 
           newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                   Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*metricMatrixRepresentation[[#1[[2]],
                        index[[3]]]]*angularMomentumDensityTensor[[#1[[1]],index[[2]],#1[[2]]]] & ) /@ 
                     Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ Tuples[
                 Range[Length[metricMatrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> 
              (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ 
                   Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
             Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[
                     index,#1,#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[
                Length[metricMatrixRepresentation]]], If[index1 === False && index2 === True && index3 === True, 
            newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                     (metricMatrixRepresentation[[index[[2]],#1[[1]]]]*metricMatrixRepresentation[[#1[[2]],index[[3]]]]*
                        angularMomentumDensityTensor[[index[[1]],#1[[1]],#1[[2]]]] & ) /@ Tuples[Range[Length[
                         metricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
                  3]]]; Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> (Module[{index = #1}, 
                  Total[(newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ Range[Length[
                      metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]], 
              Subsuperscript["\[FormalCapitalM]", "\[FormalMu]\[FormalSigma]", "\[FormalSigma]"] -> (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,
                      index,#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]], Indeterminate]]]]]]]] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
    BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["ReducedIndexContractions"] := 
  Module[{angularMomentumDensityTensor, newAngularMomentumDensityTensor}, 
    angularMomentumDensityTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              matrixRepresentation[[index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
              matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
          3]]]; If[(index1 === True && index2 === True && index3 === True) || (index1 === False && index2 === False && 
        index3 === False), Association[], If[index1 === True && index2 === False && index3 === False, 
       newAngularMomentumDensityTensor = Normal[SparseArray[
           (Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1]]*
                   angularMomentumDensityTensor[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                   metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; 
        Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> FullSimplify[
           (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]], 
         Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalMu]\[FormalSigma]"] -> FullSimplify[
           (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,index,#1]] & ) /@ 
                Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]]], 
       If[index1 === False && index2 === True && index3 === False, newAngularMomentumDensityTensor = 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[2]],#1]]*
                    angularMomentumDensityTensor[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[
                    metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; 
         Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalSigma]\[FormalNu]"] -> FullSimplify[
            (Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ 
                 Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]], 
          Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, Total[
                (newAngularMomentumDensityTensor[[index,#1,#1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ 
             Range[Length[metricMatrixRepresentation]]]], If[index1 === False && index2 === False && index3 === True, 
         newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                 Total[(metricMatrixRepresentation[[#1,index[[3]]]]*angularMomentumDensityTensor[[index[[1]],index[[2]],
                      #1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[metricMatrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalSigma]\[FormalMu]"] -> 
            FullSimplify[(Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,index,#1]] & ) /@ 
                  Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]], 
           Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]", "\[FormalRho]\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, 
                Total[(newAngularMomentumDensityTensor[[index,#1,#1]] & ) /@ Range[Length[
                    metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]]], 
         If[index1 === True && index2 === True && index3 === False, newAngularMomentumDensityTensor = 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*
                      metricMatrixRepresentation[[index[[2]],#1[[2]]]]*angularMomentumDensityTensor[[#1[[1]],#1[[2]],
                       index[[3]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[metricMatrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]\[FormalMu]", "\[FormalSigma]"] -> 
             FullSimplify[(Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,index,#1]] & ) /@ 
                   Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]], 
            Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, 
                 Total[(newAngularMomentumDensityTensor[[index,#1,#1]] & ) /@ Range[Length[
                     metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]]], 
          If[index1 === True && index2 === False && index3 === True, newAngularMomentumDensityTensor = 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*
                       metricMatrixRepresentation[[#1[[2]],index[[3]]]]*angularMomentumDensityTensor[[#1[[1]],index[[2]],
                        #1[[2]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; Association[
             Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]\[FormalNu]", "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, Total[
                   (newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ Range[Length[
                      metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]], 
             Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalSigma]", "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, Total[
                   (newAngularMomentumDensityTensor[[index,#1,#1]] & ) /@ Range[Length[
                      metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]]], 
           If[index1 === False && index2 === True && index3 === True, newAngularMomentumDensityTensor = 
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[2]],#1[[1]]]]*
                        metricMatrixRepresentation[[#1[[2]],index[[3]]]]*angularMomentumDensityTensor[[index[[1]],#1[[1]],
                         #1[[2]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
                 Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; Association[Subsuperscript["\[FormalCapitalM]", "\[FormalSigma]\[FormalNu]", 
                "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, Total[(newAngularMomentumDensityTensor[[#1,#1,index]] & ) /@ 
                     Range[Length[metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]], 
              Subsuperscript["\[FormalCapitalM]", "\[FormalMu]\[FormalSigma]", "\[FormalSigma]"] -> FullSimplify[(Module[{index = #1}, Total[
                    (newAngularMomentumDensityTensor[[#1,index,#1]] & ) /@ Range[Length[
                       metricMatrixRepresentation]]]] & ) /@ Range[Length[metricMatrixRepresentation]]]], 
            Indeterminate]]]]]]]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["CovariantDerivatives"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, newMatrixRepresentation, newPositionVector, christoffelSymbols, 
     angularMomentumDensityTensor, newAngularMomentumDensityTensor}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; newPositionVector = 
      positionVector /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (newCoordinates[[index[[1]]]] - newPositionVector[[index[[1]]]])*
              newMatrixRepresentation[[index[[2]],index[[3]]]] - (newCoordinates[[index[[2]]]] - newPositionVector[[
                index[[2]]]])*newMatrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; 
     If[index1 === True && index2 === True && index3 === True, 
      newAngularMomentumDensityTensor = Normal[SparseArray[
          (Module[{index = #1}, index -> Total[(newMetricMatrixRepresentation[[index[[1]],#1[[1]]]]*
                  newMetricMatrixRepresentation[[index[[2]],#1[[2]]]]*newMetricMatrixRepresentation[[#1[[3]],index[[3]]]]*
                  angularMomentumDensityTensor[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[
                   newMetricMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
            3]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                  index[[1]]]], StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalM]", StringJoin[
                 ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                  StandardForm], ToString[newCoordinates[[index[[4]]]], StandardForm]]], StandardForm]] -> 
             D[newAngularMomentumDensityTensor[[index[[2]],index[[3]],index[[4]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newAngularMomentumDensityTensor[[#1,index[[3]],
                   index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*newAngularMomentumDensityTensor[[index[[2]],#1,
                   index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*newAngularMomentumDensityTensor[[index[[2]],
                   index[[3]],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMetricMatrixRepresentation]], 4] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False && index3 === False, 
       newAngularMomentumDensityTensor = angularMomentumDensityTensor; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalM]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm], 
                  ToString[newCoordinates[[index[[4]]]], StandardForm]]], StandardForm]] -> 
              D[newAngularMomentumDensityTensor[[index[[2]],index[[3]],index[[4]]]], newCoordinates[[index[[1]]]]] + 
               Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newAngularMomentumDensityTensor[[#1,index[[3]],
                    index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[3]],index[[1]],#1]]*newAngularMomentumDensityTensor[[index[[2]],#1,
                    index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[4]],index[[1]],#1]]*newAngularMomentumDensityTensor[[index[[2]],index[[3]],
                    #1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMetricMatrixRepresentation]], 4] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False && index3 === False, 
        newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(newMetricMatrixRepresentation[[index[[1]],#1]]*angularMomentumDensityTensor[[#1,index[[2]],
                     index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; 
         Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalM]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                    newCoordinates[[index[[4]]]], StandardForm]]], StandardForm]] -> D[newAngularMomentumDensityTensor[[
                  index[[2]],index[[3]],index[[4]]]], newCoordinates[[index[[1]]]]] + 
                Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newAngularMomentumDensityTensor[[index[[2]],#1,
                     index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
                Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newAngularMomentumDensityTensor[[index[[2]],
                     index[[3]],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
                Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newAngularMomentumDensityTensor[[#1,index[[3]],
                     index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[newMetricMatrixRepresentation]], 4] /. (ToExpression[#1] -> #1 & ) /@ 
            Select[coordinates, StringQ]], If[index1 === False && index2 === True && index3 === False, 
         newAngularMomentumDensityTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                 Total[(newMetricMatrixRepresentation[[index[[2]],#1]]*angularMomentumDensityTensor[[index[[1]],#1,
                      index[[3]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
              Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalM]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                     newCoordinates[[index[[4]]]], StandardForm]]], StandardForm]] -> D[newAngularMomentumDensityTensor[[
                   index[[2]],index[[3]],index[[4]]]], newCoordinates[[index[[1]]]]] + Total[
                  (christoffelSymbols[[index[[2]],index[[1]],#1]]*newAngularMomentumDensityTensor[[#1,index[[3]],
                      index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
                 Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newAngularMomentumDensityTensor[[index[[2]],
                      index[[3]],#1]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] - 
                 Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*newAngularMomentumDensityTensor[[index[[2]],#1,
                      index[[4]]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[newMetricMatrixRepresentation]], 4] /. (ToExpression[#1] -> #1 & ) /@ 
             Select[coordinates, StringQ]]]]]]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["ContinuityEquations"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, newMatrixRepresentation, newPositionVector, christoffelSymbols, 
     angularMomentumDensityTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newPositionVector = positionVector /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (newCoordinates[[index[[1]]]] - newPositionVector[[index[[1]]]])*
              newMatrixRepresentation[[index[[2]],index[[3]]]] - (newCoordinates[[index[[2]]]] - newPositionVector[[
                index[[2]]]])*newMatrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; 
     (Module[{index = #1}, Total[(D[angularMomentumDensityTensor[[First[index],Last[index],#1]], newCoordinates[[
                #1]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
           Total[(christoffelSymbols[[First[index],First[#1],Last[#1]]]*angularMomentumDensityTensor[[Last[#1],
                Last[index],First[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] + 
           Total[(christoffelSymbols[[Last[index],First[#1],Last[#1]]]*angularMomentumDensityTensor[[First[index],
                Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] + 
           Total[(christoffelSymbols[[First[#1],First[#1],Last[#1]]]*angularMomentumDensityTensor[[First[index],
                Last[index],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] == 0] & ) /@ 
       Tuples[Range[Length[newMetricMatrixRepresentation]], 2] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["ReducedContinuityEquations"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, newMatrixRepresentation, newPositionVector, christoffelSymbols, 
     angularMomentumDensityTensor}, newMetricMatrixRepresentation = metricMatrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newPositionVector = positionVector /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],#1]]*
                (D[newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 D[newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 D[newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (newCoordinates[[index[[1]]]] - newPositionVector[[index[[1]]]])*
              newMatrixRepresentation[[index[[2]],index[[3]]]] - (newCoordinates[[index[[2]]]] - newPositionVector[[
                index[[2]]]])*newMatrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ 
         Tuples[Range[Length[newMetricMatrixRepresentation]], 3]]]; 
     FullSimplify[(Module[{index = #1}, Total[(D[angularMomentumDensityTensor[[First[index],Last[index],#1]], 
                newCoordinates[[#1]]] & ) /@ Range[Length[newMetricMatrixRepresentation]]] + 
            Total[(christoffelSymbols[[First[index],First[#1],Last[#1]]]*angularMomentumDensityTensor[[Last[#1],
                 Last[index],First[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] + 
            Total[(christoffelSymbols[[Last[index],First[#1],Last[#1]]]*angularMomentumDensityTensor[[First[index],
                 Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] + 
            Total[(christoffelSymbols[[First[#1],First[#1],Last[#1]]]*angularMomentumDensityTensor[[First[index],
                 Last[index],Last[#1]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 2]] == 0] & ) /@ 
        Tuples[Range[Length[newMetricMatrixRepresentation]], 2] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]]] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[stressEnergyIndex1] && BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor /: 
  MakeBoxes[angularMomentumDensityTensor:AngularMomentumDensityTensor[(stressEnergyTensor_)[
       (metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
       matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], positionVector_List, index1_, index2_, 
      index3_], format_] := Module[{tensorRepresentation, newTensorRepresentation, type, symbol, dimensions, eigenvalues, 
      positiveEigenvalues, negativeEigenvalues, signature, icon}, 
     tensorRepresentation = Normal[SparseArray[
         (Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*matrixRepresentation[[
                index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
               matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
           3]]]; If[index1 === True && index2 === True && index3 === True, 
       newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (metricMatrixRepresentation[[index[[1]],#1[[1]]]]*metricMatrixRepresentation[[index[[2]],#1[[2]]]]*
                   metricMatrixRepresentation[[#1[[3]],index[[3]]]]*tensorRepresentation[[#1[[1]],#1[[2]],
                    #1[[3]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; type = "Covariant"; 
        symbol = Subscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && index3 === False, 
        newTensorRepresentation = tensorRepresentation; type = "Contravariant"; symbol = Superscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]\[FormalNu]"], 
        If[index1 === True && index2 === False && index3 === False, 
         newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (metricMatrixRepresentation[[index[[1]],#1]]*tensorRepresentation[[#1,index[[2]],index[[3]]]] & ) /@ 
                   Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
               3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalRho]", "\[FormalMu]\[FormalNu]"], 
         If[index1 === False && index2 === True && index3 === False, newTensorRepresentation = 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[2]],#1]]*
                      tensorRepresentation[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[
                      metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; 
           type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalMu]", "\[FormalRho]\[FormalNu]"], If[index1 === False && index2 === False && 
            index3 === True, newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> 
                   Total[(metricMatrixRepresentation[[#1,index[[3]]]]*tensorRepresentation[[index[[1]],index[[2]],
                        #1]] & ) /@ Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[
                 Range[Length[metricMatrixRepresentation]], 3]]]; type = "Mixed"; 
            symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalNu]", "\[FormalRho]\[FormalMu]"], If[index1 === True && index2 === True && index3 === False, 
            newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                     (metricMatrixRepresentation[[index[[1]],#1[[1]]]]*metricMatrixRepresentation[[index[[2]],#1[[2]]]]*
                        tensorRepresentation[[#1[[1]],#1[[2]],index[[3]]]] & ) /@ Tuples[Range[Length[
                         metricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
                  3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]", "\[FormalNu]"], 
            If[index1 === True && index2 === False && index3 === True, newTensorRepresentation = Normal[
                SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[1]],#1[[1]]]]*
                         metricMatrixRepresentation[[#1[[2]],index[[3]]]]*tensorRepresentation[[#1[[1]],index[[2]],
                          #1[[2]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
                  Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", 
                "\[FormalRho]\[FormalNu]", "\[FormalMu]"], If[index1 === False && index2 === True && index3 === True, newTensorRepresentation = 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(metricMatrixRepresentation[[index[[2]],#1[[1]]]]*
                          metricMatrixRepresentation[[#1[[2]],index[[3]]]]*tensorRepresentation[[index[[1]],#1[[1]],
                           #1[[2]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
                   Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", 
                 "\[FormalMu]\[FormalNu]", "\[FormalRho]"], newTensorRepresentation = ConstantArray[Indeterminate, 
                 {Length[metricMatrixRepresentation], Length[metricMatrixRepresentation], Length[
                   metricMatrixRepresentation]}]; type = Indeterminate; symbol = Indeterminate]]]]]]]]; 
      dimensions = Length[metricMatrixRepresentation]; eigenvalues = Eigenvalues[metricMatrixRepresentation]; 
      positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[metricMatrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[metricMatrixRepresentation] || Length[negativeEigenvalues] == 
          Length[metricMatrixRepresentation], signature = "Riemannian", 
        If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, signature = "Lorentzian", 
         signature = "Pseudo-Riemannian"]], signature = Indeterminate]; 
      icon = MatrixPlot[Total[newTensorRepresentation], ImageSize -> 
         Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], Frame -> False, 
        FrameTicks -> None]; BoxForm`ArrangeSummaryBox["AngularMomentumDensityTensor", angularMomentumDensityTensor, 
       icon, {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
     BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
     BooleanQ[stressEnergyIndex2] && Length[coordinates] == Length[positionVector] && BooleanQ[index1] && 
     BooleanQ[index2] && BooleanQ[index3]
