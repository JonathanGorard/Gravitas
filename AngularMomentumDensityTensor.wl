(* ::Package:: *)

AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_]] := 
  AngularMomentumDensityTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, 
      metricIndex2], matrixRepresentation, index1, index2], (Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ 
     Range[Length[coordinates]], False, False, False] /; SymbolName[stressEnergyTensor] === "StressEnergyTensor" && 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, index1_, index2_], positionVector_List] := 
  AngularMomentumDensityTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, coordinates, metricIndex1, 
      metricIndex2], matrixRepresentation, index1, index2], positionVector, False, False, False] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[positionVector]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
     metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], index1_, 
   index2_, index3_] := AngularMomentumDensityTensor[StressEnergyTensor[MetricTensor[metricMatrixRepresentation, 
      coordinates, metricIndex1, metricIndex2], matrixRepresentation, stressEnergyIndex1, stressEnergyIndex2], 
    (Superscript["\[FormalCapitalX]", ToString[#1]] & ) /@ Range[Length[coordinates]], index1, index2, index3] /; 
   SymbolName[stressEnergyTensor] === "StressEnergyTensor" && SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[stressEnergyIndex1] && 
    BooleanQ[stressEnergyIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3]
AngularMomentumDensityTensor[(stressEnergyTensor_)[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, 
      metricIndex1_, metricIndex2_], matrixRepresentation_List, stressEnergyIndex1_, stressEnergyIndex2_], 
    positionVector_List, index1_, index2_, index3_]["TensorRepresentation"] := 
  Module[{angularMomentumDensityTensor}, angularMomentumDensityTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (coordinates[[index[[1]]]] - positionVector[[index[[1]]]])*
              matrixRepresentation[[index[[2]],index[[3]]]] - (coordinates[[index[[2]]]] - positionVector[[index[[2]]]])*
              matrixRepresentation[[index[[1]],index[[3]]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
          3]]]; If[index1 === True && index2 === True && index3 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                matrixRepresentation[[index[[2]],#1[[2]]]]*matrixRepresentation[[#1[[3]],index[[3]]]]*
                angularMomentumDensityTensor[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[
                Length[metricMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
      If[index1 === False && index2 === False && index3 === False, angularMomentumDensityTensor, 
       If[index1 === True && index2 === False && index3 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*
                  angularMomentumDensityTensor[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                  metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
        If[index1 === False && index2 === True && index3 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[2]],#1]]*
                   angularMomentumDensityTensor[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[
                   metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
         If[index1 === False && index2 === False && index3 === True, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[#1,index[[3]]]]*
                    angularMomentumDensityTensor[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[
                    metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]], 
          If[index1 === True && index2 === True && index3 === False, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                     matrixRepresentation[[index[[2]],#1[[2]]]]*angularMomentumDensityTensor[[#1[[1]],#1[[2]],
                      index[[3]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[metricMatrixRepresentation]], 3]]], If[index1 === True && index2 === False && 
             index3 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                       #1[[1]]]]*matrixRepresentation[[#1[[2]],index[[3]]]]*angularMomentumDensityTensor[[#1[[1]],
                       index[[2]],#1[[2]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[metricMatrixRepresentation]], 3]]], If[index1 === False && index2 === True && 
              index3 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[2]],
                        #1[[1]]]]*matrixRepresentation[[#1[[2]],index[[3]]]]*angularMomentumDensityTensor[[index[[1]],
                        #1[[1]],#1[[2]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[metricMatrixRepresentation]], 3]]], Indeterminate]]]]]]]]] /; 
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
      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                 matrixRepresentation[[index[[2]],#1[[2]]]]*matrixRepresentation[[#1[[3]],index[[3]]]]*
                 angularMomentumDensityTensor[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                Range[Length[metricMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
           3]]]], If[index1 === False && index2 === False && index3 === False, 
       FullSimplify[angularMomentumDensityTensor], If[index1 == True && index2 === False && index3 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*
                   angularMomentumDensityTensor[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                   metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]], 
        If[index1 === False && index2 === True && index3 === False, FullSimplify[
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[2]],#1]]*
                    angularMomentumDensityTensor[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[
                    metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]], 
         If[index1 === False && index2 === False && index3 === True, FullSimplify[
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[#1,index[[3]]]]*
                     angularMomentumDensityTensor[[index[[1]],index[[2]],#1]] & ) /@ Range[Length[
                     metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]], 
          If[index1 == True && index2 === True && index3 === False, FullSimplify[
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                      matrixRepresentation[[index[[2]],#1[[2]]]]*angularMomentumDensityTensor[[#1[[1]],#1[[2]],
                       index[[3]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[metricMatrixRepresentation]], 3]]]], If[index1 === True && index2 === False && 
             index3 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[
                    (matrixRepresentation[[index[[1]],#1[[1]]]]*matrixRepresentation[[#1[[2]],index[[3]]]]*
                       angularMomentumDensityTensor[[#1[[1]],index[[2]],#1[[2]]]] & ) /@ Tuples[Range[Length[
                        metricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
                 3]]]], If[index1 === False && index2 === True && index3 === True, FullSimplify[
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[2]],#1[[1]]]]*
                        matrixRepresentation[[#1[[2]],index[[3]]]]*angularMomentumDensityTensor[[index[[1]],#1[[1]],
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
                (matrixRepresentation[[index[[1]],#1[[1]]]]*matrixRepresentation[[index[[2]],#1[[2]]]]*
                   matrixRepresentation[[#1[[3]],index[[3]]]]*tensorRepresentation[[#1[[1]],#1[[2]],#1[[3]]]] & ) /@ 
                 Tuples[Range[Length[metricMatrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; type = "Covariant"; 
        symbol = Subscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && index3 === False, 
        newTensorRepresentation = tensorRepresentation; type = "Contravariant"; symbol = Superscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]\[FormalNu]"], 
        If[index1 === True && index2 === False && index3 === False, 
         newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (matrixRepresentation[[index[[1]],#1]]*tensorRepresentation[[#1,index[[2]],index[[3]]]] & ) /@ 
                   Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
               3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalRho]", "\[FormalMu]\[FormalNu]"], 
         If[index1 === False && index2 === True && index3 === False, newTensorRepresentation = 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[2]],#1]]*
                      tensorRepresentation[[index[[1]],#1,index[[3]]]] & ) /@ Range[Length[
                      metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; 
           type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalMu]", "\[FormalRho]\[FormalNu]"], If[index1 === False && index2 === False && 
            index3 === True, newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> 
                   Total[(matrixRepresentation[[#1,index[[3]]]]*tensorRepresentation[[index[[1]],index[[2]],#1]] & ) /@ 
                     Range[Length[metricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
                 3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalNu]", "\[FormalRho]\[FormalMu]"], 
           If[index1 === True && index2 === True && index3 === False, newTensorRepresentation = 
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                        matrixRepresentation[[index[[2]],#1[[2]]]]*tensorRepresentation[[#1[[1]],#1[[2]],index[[
                          3]]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 2]]] & ) /@ 
                 Tuples[Range[Length[metricMatrixRepresentation]], 3]]]; type = "Mixed"; 
             symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalMu]", "\[FormalNu]"], If[index1 === True && index2 === False && index3 === True, 
             newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                      (matrixRepresentation[[index[[1]],#1[[1]]]]*matrixRepresentation[[#1[[2]],index[[3]]]]*
                         tensorRepresentation[[#1[[1]],index[[2]],#1[[2]]]] & ) /@ Tuples[Range[Length[
                          metricMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[metricMatrixRepresentation]], 
                   3]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalM]", "\[FormalRho]\[FormalNu]", "\[FormalMu]"], 
             If[index1 === False && index2 === True && index3 === True, newTensorRepresentation = 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[2]],#1[[1]]]]*
                          matrixRepresentation[[#1[[2]],index[[3]]]]*tensorRepresentation[[index[[1]],#1[[1]],
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
