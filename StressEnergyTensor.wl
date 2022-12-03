(* ::Package:: *)

StressEnergyTensor["PerfectFluid"] := StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
    Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
   Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
           ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False]
StressEnergyTensor["PerfectFluid", (metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*Superscript["\[FormalU]", ToString[First[#1]]]*Superscript["\[FormalU]", 
            ToString[Last[#1]]] + "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], 
    False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}] := 
  StressEnergyTensor[MetricTensor[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]], 
     Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], True, True], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[DiagonalMatrix[Join[{-1}, ConstantArray[1, 3]]]][[First[#1],Last[#1]]] & ) /@ 
       Tuples[Range[4], 2]]], False, False] /; Length[fourVelocity] == 4
StressEnergyTensor[{"PerfectFluid", fourVelocity_List}, (metricTensor_)[matrixRepresentation_List, coordinates_List, 
    index1_, index2_]] := StressEnergyTensor[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    Normal[SparseArray[(#1 -> ("\[FormalRho]" + "\[FormalCapitalP]")*fourVelocity[[First[#1]]]*fourVelocity[[Last[#1]]] + 
          "\[FormalCapitalP]"*Inverse[matrixRepresentation][[First[#1],Last[#1]]] & ) /@ Tuples[Range[4], 2]]], False, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[fourVelocity] == 4
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
    Length[Dimensions[matrixRepresentation]] == 2 && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Symbol"] := 
  If[index1 === True && index2 === True, Subscript["\[FormalCapitalT]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
     Superscript["\[FormalCapitalT]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, Subsuperscript["\[FormalCapitalT]", "\[FormalMu]", "\[FormalNu]"], 
      If[index1 === False && index2 === True, Subsuperscript["\[FormalCapitalT]", "\[FormalNu]", "\[FormalMu]"], Indeterminate]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Energy"] := First[First[matrixRepresentation]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Momentum"] := 
  ((1/2)*(matrixRepresentation[[1,#1 + 1]] + matrixRepresentation[[#1 + 1,1]]) & ) /@ 
    Range[Length[matrixRepresentation] - 1] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["Pressure"] := 
  Total[((1/(Length[matrixRepresentation] - 1))*matrixRepresentation[[#1 + 1,#1 + 1]] & ) /@ 
     Range[Length[matrixRepresentation] - 1]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["StressTensor"] := 
  Normal[SparseArray[(#1 -> matrixRepresentation[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
      Tuples[Range[Length[matrixRepresentation] - 1], 2]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
    BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["ShearStressTensor"] := 
  Normal[SparseArray[(#1 -> matrixRepresentation[[First[#1] + 1,Last[#1] + 1]] & ) /@ 
       Tuples[Range[Length[matrixRepresentation] - 1], 2]]] - 
    Total[((1/(Length[matrixRepresentation] - 1))*matrixRepresentation[[#1 + 1,#1 + 1]] & ) /@ 
       Range[Length[matrixRepresentation] - 1]]*IdentityMatrix[Length[matrixRepresentation] - 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[metricMatrixRepresentation]] == 2 && 
    Length[coordinates] == Length[metricMatrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    Length[Dimensions[matrixRepresentation]] == 2 && BooleanQ[index1] && BooleanQ[index2]
StressEnergyTensor[(metricTensor_)[metricMatrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    matrixRepresentation_List, index1_, index2_]["SymbolicContinuityEquations"] := 
  Module[{newMetricMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMetricMatrixRepresentation = metricMatrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMetricMatrixRepresentation][[index[[1]],
                 #1]]*(Inactive[D][newMetricMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMetricMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMetricMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMetricMatrixRepresentation]], 
          3]]]; (Module[{index = #1}, Inactive[D][matrixRepresentation[[First[index],Last[index]]], Last[index]] + 
          Total[(christoffelSymbols[[First[index],#1,Last[index]]]*matrixRepresentation[[#1,Last[index]]] & ) /@ 
            Range[Length[matrixRepresentation]]] + Total[(christoffelSymbols[[Last[index],#1,Last[index]]]*
              matrixRepresentation[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]] == 0] & ) /@ 
      Tuples[Range[Length[matrixRepresentation]], 2]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[metricMatrixRepresentation]] == 2 && Length[coordinates] == Length[metricMatrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && Length[Dimensions[matrixRepresentation]] == 2 && 
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
     Length[Dimensions[matrixRepresentation]] == 2 && BooleanQ[index1] && BooleanQ[index2]
