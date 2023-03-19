(* ::Package:: *)

ADMDecomposition[] := {"Minkowski", "Schwarzschild", "Kerr", "ReissnerNordstrom", "KerrNewman", "BrillLindquist", "FLRW"}
ADMDecomposition["Minkowski"] := ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[ConstantArray[1, 3]], 
    (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3], True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[3]], 
   (Module[{index = #1}, Superscript["\[FormalBeta]", index] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
         Range[3]]] & ) /@ Range[3]]
ADMDecomposition["Minkowski", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[ConstantArray[1, 3]], coordinates, True, True], 
    timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition["Minkowski", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[ConstantArray[1, 3]], coordinates, True, True], 
    timeCoordinate, lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"Minkowski", dimensionCount_Integer}] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[ConstantArray[1, dimensionCount - 1]], 
    (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1], True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ Range[dimensionCount - 1]], 
   (Module[{index = #1}, Superscript["\[Beta]", index] @@ Join[{"\[FormalT]"}, (Superscript["\[FormalX]", ToString[#1]] & ) /@ 
         Range[dimensionCount - 1]]] & ) /@ Range[dimensionCount - 1]]
ADMDecomposition[{"Minkowski", dimensionCount_Integer}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[ConstantArray[1, dimensionCount - 1]], coordinates, 
     True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[dimensionCount - 1]] /; 
   Length[coordinates] == dimensionCount - 1
ADMDecomposition[{"Minkowski", dimensionCount_Integer}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := ADMDecomposition[ResourceFunction["MetricTensor"][
     DiagonalMatrix[ConstantArray[1, dimensionCount - 1]], coordinates, True, True], timeCoordinate, lapseFunction, 
    shiftVector] /; Length[coordinates] == dimensionCount - 1 && Length[shiftVector] == dimensionCount - 1
ADMDecomposition["Schwarzschild"] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/"\[FormalR]"), "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["Schwarzschild", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]]), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["Schwarzschild", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]]), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"Schwarzschild", mass_}] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{1/(1 - (2*mass)/"\[FormalR]"), "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"Schwarzschild", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]]), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"Schwarzschild", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]]), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["Kerr"] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*"\[FormalCapitalM]"*"\[FormalR]" + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2), 
      "\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2, ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + (2*"\[FormalR]"*("\[FormalCapitalJ]"^2/"\[FormalCapitalM]")*Sin["\[FormalTheta]"]^2)/
         ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["Kerr", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, 
       (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/"\[FormalCapitalM]")*Sin[coordinates[[2]]]^2)/
          (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, 
     True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition["Kerr", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, 
       (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/"\[FormalCapitalM]")*Sin[coordinates[[2]]]^2)/
          (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, 
     True], timeCoordinate, lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"Kerr", mass_}] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*mass*"\[FormalR]" + ("\[FormalCapitalJ]"/mass)^2), 
      "\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2, ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2 + (2*"\[FormalR]"*("\[FormalCapitalJ]"^2/mass)*Sin["\[FormalTheta]"]^2)/
         ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"Kerr", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/mass)^2), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, 
       (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2 + (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/mass)*Sin[coordinates[[2]]]^2)/
          (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, 
     True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"Kerr", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/mass)^2), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, 
       (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2 + (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/mass)*Sin[coordinates[[2]]]^2)/
          (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, 
     True], timeCoordinate, lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"Kerr", mass_, angularMomentum_}] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
     {("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*mass*"\[FormalR]" + (angularMomentum/mass)^2), 
      "\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2, ("\[FormalR]"^2 + (angularMomentum/mass)^2 + 
        (2*"\[FormalR]"*(angularMomentum^2/mass)*Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2))*
       Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"Kerr", mass_, angularMomentum_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 
         2*mass*coordinates[[1]] + (angularMomentum/mass)^2), coordinates[[1]]^2 + (angularMomentum/mass)^2*
         Cos[coordinates[[2]]]^2, (coordinates[[1]]^2 + (angularMomentum/mass)^2 + 
         (2*coordinates[[1]]*(angularMomentum^2/mass)*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
           (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], 
    timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"Kerr", mass_, angularMomentum_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := ADMDecomposition[ResourceFunction["MetricTensor"][
     DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (coordinates[[1]]^2 + (angularMomentum/mass)^2 + (2*coordinates[[1]]*(angularMomentum^2/mass)*
           Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*
        Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, shiftVector] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["ReissnerNordstrom"] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/"\[FormalR]" + "\[FormalCapitalQ]"^2/(4*Pi*"\[FormalR]"^2)), "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], 
    {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["ReissnerNordstrom", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["ReissnerNordstrom", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, 
    shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_}] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{1/(1 - (2*mass)/"\[FormalR]" + "\[FormalCapitalQ]"^2/(4*Pi*"\[FormalR]"^2)), "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], 
    {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"ReissnerNordstrom", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {1/(1 - (2*mass)/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {1/(1 - (2*mass)/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, 
    shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_, charge_}] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{1/(1 - (2*mass)/"\[FormalR]" + charge^2/(4*Pi*"\[FormalR]"^2)), 
      "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"ReissnerNordstrom", mass_, charge_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {1/(1 - (2*mass)/coordinates[[1]] + charge^2/(4*Pi*coordinates[[1]]^2)), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_, charge_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := ADMDecomposition[ResourceFunction["MetricTensor"][
     DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]] + charge^2/(4*Pi*coordinates[[1]]^2)), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, 
    shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["KerrNewman"] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*"\[FormalCapitalM]"*"\[FormalR]" + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + 
        "\[FormalCapitalQ]"^2/(4*Pi)), "\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2)^2 - ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*("\[FormalR]"^2 - 2*"\[FormalCapitalM]"*"\[FormalR]" + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi))*
          Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], 
   "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["KerrNewman", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi)), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2)^2 - ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["KerrNewman", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi)), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2)^2 - ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"KerrNewman", mass_}] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*mass*"\[FormalR]" + ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
      "\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2)^2 - ("\[FormalCapitalJ]"/mass)^2*("\[FormalR]"^2 - 2*mass*"\[FormalR]" + ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*
          Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], 
   "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"KerrNewman", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2)^2 - ("\[FormalCapitalJ]"/mass)^2*(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"KerrNewman", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
         ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2)^2 - ("\[FormalCapitalJ]"/mass)^2*(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_}] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
     {("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*mass*"\[FormalR]" + (angularMomentum/mass)^2 + 
        "\[FormalCapitalQ]"^2/(4*Pi)), "\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*("\[FormalR]"^2 - 2*mass*"\[FormalR]" + 
           (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2))*
       Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 
         2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, 
     True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := ADMDecomposition[ResourceFunction["MetricTensor"][
     DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, 
     True], timeCoordinate, lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_, charge_}] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
     {("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*mass*"\[FormalR]" + (angularMomentum/mass)^2 + 
        charge^2/(4*Pi)), "\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*("\[FormalR]"^2 - 2*mass*"\[FormalR]" + 
           (angularMomentum/mass)^2 + charge^2/(4*Pi))*Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2))*
       Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_, charge_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/(coordinates[[1]]^2 - 
         2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, 
     True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_, charge_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := ADMDecomposition[ResourceFunction["MetricTensor"][
     DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, 
     True, True], timeCoordinate, lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["BrillLindquist"] := Module[{brillLindquistPotential}, 
   brillLindquistPotential = 1 + (1/2)*("\[FormalCapitalM]"/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] - Subscript["\[FormalZ]", "0"])^2] + 
        "\[FormalCapitalM]"/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] + Subscript["\[FormalZ]", "0"])^2]); 
    ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
        "\[FormalR]"^2*brillLindquistPotential^4, ("\[FormalR]"^2*Sin["\[FormalTheta]"]^2)*brillLindquistPotential^4}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
      True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
     (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]]
ADMDecomposition["BrillLindquist", timeCoordinate_, coordinates_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*("\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + "\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
         coordinates[[1]]^2*brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*
          brillLindquistPotential^4}], coordinates, True, True], timeCoordinate, 
      "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
       Range[3]]] /; Length[coordinates] == 3
ADMDecomposition["BrillLindquist", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*("\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + "\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
         coordinates[[1]]^2*brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*
          brillLindquistPotential^4}], coordinates, True, True], timeCoordinate, lapseFunction, shiftVector]] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"BrillLindquist", mass_}] := Module[{brillLindquistPotential}, 
   brillLindquistPotential = 1 + (1/2)*(mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] - Subscript["\[FormalZ]", "0"])^2] + 
        mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] + Subscript["\[FormalZ]", "0"])^2]); 
    ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
        "\[FormalR]"^2*brillLindquistPotential^4, ("\[FormalR]"^2*Sin["\[FormalTheta]"]^2)*brillLindquistPotential^4}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
      True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
     (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]]
ADMDecomposition[{"BrillLindquist", mass_}, timeCoordinate_, coordinates_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
         coordinates[[1]]^2*brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*
          brillLindquistPotential^4}], coordinates, True, True], timeCoordinate, 
      "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
       Range[3]]] /; Length[coordinates] == 3
ADMDecomposition[{"BrillLindquist", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
         coordinates[[1]]^2*brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*
          brillLindquistPotential^4}], coordinates, True, True], timeCoordinate, lapseFunction, shiftVector]] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"BrillLindquist", mass_, position_}] := Module[{brillLindquistPotential}, 
   brillLindquistPotential = 1 + (1/2)*(mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] - position)^2] + 
        mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] + position)^2]); 
    ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
        "\[FormalR]"^2*brillLindquistPotential^4, ("\[FormalR]"^2*Sin["\[FormalTheta]"]^2)*brillLindquistPotential^4}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
      True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
     (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]]
ADMDecomposition[{"BrillLindquist", mass_, position_}, timeCoordinate_, coordinates_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              position)^2] + mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + position)^2]); 
     ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
         coordinates[[1]]^2*brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*
          brillLindquistPotential^4}], coordinates, True, True], timeCoordinate, 
      "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
       Range[3]]] /; Length[coordinates] == 3
ADMDecomposition[{"BrillLindquist", mass_, position_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := Module[{brillLindquistPotential}, 
    brillLindquistPotential = 1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] - position)^2] + 
         mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] + position)^2]); 
     ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{brillLindquistPotential^4, 
         coordinates[[1]]^2*brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*
          brillLindquistPotential^4}], coordinates, True, True], timeCoordinate, lapseFunction, shiftVector]] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["FLRW"] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{"\[FormalA]"["\[FormalT]"]^2/(1 - "\[FormalK]"*"\[FormalR]"^2), "\[FormalA]"["\[FormalT]"]^2*"\[FormalR]"^2, "\[FormalA]"["\[FormalT]"]^2*"\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], 
    {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["FLRW", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {"\[FormalA]"[timeCoordinate]^2/(1 - "\[FormalK]"*coordinates[[1]]^2), "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2, 
       "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalA]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["FLRW", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {"\[FormalA]"[timeCoordinate]^2/(1 - "\[FormalK]"*coordinates[[1]]^2), "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2, 
       "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"FLRW", curvature_}] := ADMDecomposition[ResourceFunction["MetricTensor"][
    DiagonalMatrix[{"\[FormalA]"["\[FormalT]"]^2/(1 - curvature*"\[FormalR]"^2), "\[FormalA]"["\[FormalT]"]^2*"\[FormalR]"^2, 
      "\[FormalA]"["\[FormalT]"]^2*"\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"FLRW", curvature_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {"\[FormalA]"[timeCoordinate]^2/(1 - curvature*coordinates[[1]]^2), "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2, 
       "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[Alpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"FLRW", curvature_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {"\[FormalA]"[timeCoordinate]^2/(1 - curvature*coordinates[[1]]^2), "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2, 
       "\[FormalA]"[timeCoordinate]^2*coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"FLRW", curvature_, scaleFactor_}] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[{scaleFactor["\[FormalT]"]^2/(1 - curvature*"\[FormalR]"^2), 
      scaleFactor["\[FormalT]"]^2*"\[FormalR]"^2, scaleFactor["\[FormalT]"]^2*"\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], 
   "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"FLRW", curvature_, scaleFactor_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][DiagonalMatrix[
      {scaleFactor[timeCoordinate]^2/(1 - curvature*coordinates[[1]]^2), scaleFactor[timeCoordinate]^2*
        coordinates[[1]]^2, scaleFactor[timeCoordinate]^2*coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, 
     True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"FLRW", curvature_, scaleFactor_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := ADMDecomposition[ResourceFunction["MetricTensor"][
     DiagonalMatrix[{scaleFactor[timeCoordinate]^2/(1 - curvature*coordinates[[1]]^2), 
       scaleFactor[timeCoordinate]^2*coordinates[[1]]^2, scaleFactor[timeCoordinate]^2*coordinates[[1]]^2*
        Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, shiftVector] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], "\[FormalT]", 
    "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{"\[FormalT]"}, coordinates] & ) /@ 
     Range[Length[matrixRepresentation]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[Length[matrixRepresentation]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], lapseFunction_, 
   shiftVector_List] := ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, 
     index2], "\[FormalT]", lapseFunction, shiftVector] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SpatialMetricTensor"] := 
  ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SpacetimeMetricTensor"] := 
  Module[{shiftCovector}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; ResourceFunction["MetricTensor"][
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]], Join[{timeCoordinate}, coordinates], True, True]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["NormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,
                  #1]]*D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedNormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     FullSimplify[
      Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,
                   #1]]*D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ 
                Range[Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicNormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,
                  #1]]*Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["TimeVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, normalVector}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*normalVector[[index]] + 
             Join[{0}, newShiftVector][[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicTimeVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, normalVector}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*normalVector[[index]] + 
             Join[{0}, newShiftVector][[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["TimeCoordinate"] := 
  timeCoordinate /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SpatialCoordinates"] := 
  coordinates /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ Join[{timeCoordinate}, coordinates] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["LapseFunction"] := 
  lapseFunction /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ShiftVector"] := shiftVector /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["GaussEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, spatialRiemannTensor, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, normalVector, projectionOperator, 
     leftHandSide, rightHandSide}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[
                1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     leftHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, projectionOperator[[nestedIndex[[1]],
                  index[[1]] + 1]]*projectionOperator[[index[[2]] + 1,nestedIndex[[2]]]]*projectionOperator[[
                  index[[3]] + 1,nestedIndex[[3]]]]*projectionOperator[[index[[4]] + 1,nestedIndex[[4]]]]*
                 spacetimeRiemannTensor[[nestedIndex[[1]],nestedIndex[[2]],nestedIndex[[3]],nestedIndex[[4]]]]] & ) /@ 
              Tuples[Range[Length[spacetimeMetricTensor]], 4]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          4]]]; rightHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> spatialRiemannTensor[[index[[1]],index[[2]],index[[3]],index[[4]]]] + 
             mixedExtrinsicCurvatureTensor[[index[[1]],index[[3]]]]*extrinsicCurvatureTensor[[index[[2]],index[[4]]]] - 
             mixedExtrinsicCurvatureTensor[[index[[1]],index[[4]]]]*extrinsicCurvatureTensor[[index[[2]],index[[
                3]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     Thread[Catenate[Catenate[Catenate[leftHandSide]]] == Catenate[Catenate[Catenate[rightHandSide]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicGaussEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, spatialRiemannTensor, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, normalVector, projectionOperator, 
     leftHandSide, rightHandSide}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][spatialChristoffelSymbols[[index[[1]],
               index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                  index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
             Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[index[[4]]]]] + Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spacetimeChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     leftHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, projectionOperator[[nestedIndex[[1]],
                  index[[1]] + 1]]*projectionOperator[[index[[2]] + 1,nestedIndex[[2]]]]*projectionOperator[[
                  index[[3]] + 1,nestedIndex[[3]]]]*projectionOperator[[index[[4]] + 1,nestedIndex[[4]]]]*
                 spacetimeRiemannTensor[[nestedIndex[[1]],nestedIndex[[2]],nestedIndex[[3]],nestedIndex[[4]]]]] & ) /@ 
              Tuples[Range[Length[spacetimeMetricTensor]], 4]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          4]]]; rightHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> spatialRiemannTensor[[index[[1]],index[[2]],index[[3]],index[[4]]]] + 
             mixedExtrinsicCurvatureTensor[[index[[1]],index[[3]]]]*extrinsicCurvatureTensor[[index[[2]],index[[4]]]] - 
             mixedExtrinsicCurvatureTensor[[index[[1]],index[[4]]]]*extrinsicCurvatureTensor[[index[[2]],index[[
                3]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     Thread[Catenate[Catenate[Catenate[leftHandSide]]] == Catenate[Catenate[Catenate[rightHandSide]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["CodazziMainardiEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, normalVector, 
     projectionOperator, leftHandSide, rightHandSide}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[
                1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     leftHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[index,#1]], newCoordinates[[#1]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]] + Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*
                 mixedExtrinsicCurvatureTensor[[index,Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                2]] - Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[
                  Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
             D[extrinsicCurvatureTrace, newCoordinates[[index]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     rightHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((-spacetimeRicciTensor[[First[#1],Last[#1]]])*normalVector[[Last[#1]]]*
                projectionOperator[[index + 1,First[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Range[Length[newMatrixRepresentation]]]]; Thread[leftHandSide == rightHandSide] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicCodazziMainardiEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, normalVector, 
     projectionOperator, leftHandSide, rightHandSide}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
             Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[index[[4]]]]] + Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spacetimeChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     normalVector = Normal[SparseArray[
        (Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]]; 
     projectionOperator = Normal[SparseArray[(Module[{index = #1}, index -> KroneckerDelta[First[index], Last[index]] + 
             Total[(normalVector[[Last[index]]]*spacetimeMetricTensor[[First[index],#1]]*normalVector[[#1]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     leftHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inactive[D][mixedExtrinsicCurvatureTensor[[index,#1]], newCoordinates[[
                  #1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[(spatialChristoffelSymbols[[First[#1],
                  First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,Last[#1]]] & ) /@ Tuples[
                Range[Length[newMatrixRepresentation]], 2]] - Total[(spatialChristoffelSymbols[[Last[#1],First[#1],
                  index]]*mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[
                Range[Length[newMatrixRepresentation]], 2]] - Inactive[D][extrinsicCurvatureTrace, 
              newCoordinates[[index]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     rightHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((-spacetimeRicciTensor[[First[#1],Last[#1]]])*normalVector[[Last[#1]]]*
                projectionOperator[[index + 1,First[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Range[Length[newMatrixRepresentation]]]]; Thread[leftHandSide == rightHandSide] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["GeodesicSlicingCondition"] := 
  lapseFunction == 1 /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedGeodesicSlicingCondition"] := 
  FullSimplify[lapseFunction] == 1 /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["MaximalSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     lapseFunctionLaplacian}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; lapseFunctionLaplacian = 
      Total[(Module[{index = #1}, (1/Sqrt[Det[newMatrixRepresentation]])*D[Sqrt[Det[newMatrixRepresentation]]*
             Inverse[newMatrixRepresentation][[First[index],Last[index]]]*D[newLapseFunction, newCoordinates[[Last[
                index]]]], newCoordinates[[First[index]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     lapseFunctionLaplacian == newLapseFunction*Total[(contravariantExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
             extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
        D[extrinsicCurvatureTrace, newTimeCoordinate] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedMaximalSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     lapseFunctionLaplacian}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; lapseFunctionLaplacian = 
      Total[(Module[{index = #1}, (1/Sqrt[Det[newMatrixRepresentation]])*D[Sqrt[Det[newMatrixRepresentation]]*
             Inverse[newMatrixRepresentation][[First[index],Last[index]]]*D[newLapseFunction, newCoordinates[[Last[
                index]]]], newCoordinates[[First[index]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     FullSimplify[lapseFunctionLaplacian == 
        newLapseFunction*Total[(contravariantExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[
               First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
         D[extrinsicCurvatureTrace, newTimeCoordinate] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicMaximalSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     lapseFunctionLaplacian}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; lapseFunctionLaplacian = 
      Total[(Module[{index = #1}, (1/Sqrt[Det[newMatrixRepresentation]])*Inactive[D][Sqrt[Det[newMatrixRepresentation]]*
             Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*Inactive[D][newLapseFunction, 
              newCoordinates[[Last[index]]]], newCoordinates[[First[index]]]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     lapseFunctionLaplacian == newLapseFunction*Total[(contravariantExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
             extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
        Inactive[D][extrinsicCurvatureTrace, newTimeCoordinate] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["HarmonicSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; 
     Total[(Module[{index = #1}, Inverse[spacetimeMetricTensor][[First[index],Last[index]]]*
            (D[D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[First[index]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[Last[index]]]] - Total[(spacetimeChristoffelSymbols[[#1,Last[index],First[index]]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]])] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]] == 0 /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedHarmonicSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; 
     FullSimplify[Total[(Module[{index = #1}, Inverse[spacetimeMetricTensor][[First[index],Last[index]]]*
             (D[D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[First[index]]]], 
               Join[{newTimeCoordinate}, newCoordinates][[Last[index]]]] - Total[(spacetimeChristoffelSymbols[[#1,
                   Last[index],First[index]]]*D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[
                    #1]]] & ) /@ Range[Length[spacetimeMetricTensor]]])] & ) /@ 
          Tuples[Range[Length[spacetimeMetricTensor]], 2]] == 0 /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicHarmonicSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; 
     Total[(Module[{index = #1}, Inverse[spacetimeMetricTensor][[First[index],Last[index]]]*
            (Inactive[D][Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[First[index]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[Last[index]]]] - 
             Total[(spacetimeChristoffelSymbols[[#1,Last[index],First[index]]]*Inactive[D][newTimeCoordinate, 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[Length[spacetimeMetricTensor]]])] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]] == 0 /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["1+LogSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, extrinsicCurvatureTrace}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; extrinsicCurvatureTrace = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*
          extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     D[newLapseFunction, newTimeCoordinate] == 
       Total[(newShiftVector[[#1]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ 
          Range[Length[newMatrixRepresentation]]] - 2*newLapseFunction*extrinsicCurvatureTrace /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["Reduced1+LogSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, extrinsicCurvatureTrace}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordnate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; extrinsicCurvatureTrace = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*
          extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     FullSimplify[D[newLapseFunction, newTimeCoordinate] == 
        Total[(newShiftVector[[#1]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ 
           Range[Length[newMatrixRepresentation]]] - 2*newLapseFunction*extrinsicCurvatureTrace /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["Symbolic1+LogSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, extrinsicCurvatureTrace}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; Inactive[D][newLapseFunction, newTimeCoordinate] == 
       Total[(newShiftVector[[#1]]*Inactive[D][newLapseFunction, newCoordinates[[#1]]] & ) /@ 
          Range[Length[newMatrixRepresentation]]] - 2*newLapseFunction*extrinsicCurvatureTrace /. 
      (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["NormalCoordinateConditions"] := 
  Thread[shiftVector == ConstantArray[0, Length[matrixRepresentation]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedNormalCoordinateConditions"] := 
  FullSimplify[Thread[shiftVector == ConstantArray[0, Length[matrixRepresentation]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["HarmonicCoordinateConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; 
     Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                  Inverse[spacetimeMetricTensor][[First[nestedIndex],Last[nestedIndex]]]*(D[D[newCoordinates[[index]], 
                      Join[{newTimeCoordinate}, newCoordinates][[First[nestedIndex]]]], Join[{newTimeCoordinate}, 
                       newCoordinates][[Last[nestedIndex]]]] - Total[(spacetimeChristoffelSymbols[[#1,Last[nestedIndex],
                         First[nestedIndex]]]*D[newCoordinates[[index]], Join[{newTimeCoordinate}, newCoordinates][[
                          #1]]] & ) /@ Range[Length[spacetimeMetricTensor]]])] & ) /@ Tuples[
                 Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]]]] == 
        ConstantArray[0, Length[newMatrixRepresentation]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedHarmonicCoordinateConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; 
     FullSimplify[
      Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                   Inverse[spacetimeMetricTensor][[First[nestedIndex],Last[nestedIndex]]]*(D[D[newCoordinates[[index]], 
                       Join[{newTimeCoordinate}, newCoordinates][[First[nestedIndex]]]], Join[{newTimeCoordinate}, 
                        newCoordinates][[Last[nestedIndex]]]] - Total[(spacetimeChristoffelSymbols[[#1,Last[nestedIndex],
                          First[nestedIndex]]]*D[newCoordinates[[index]], Join[{newTimeCoordinate}, newCoordinates][[
                           #1]]] & ) /@ Range[Length[spacetimeMetricTensor]]])] & ) /@ Tuples[
                  Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]]]] == 
         ConstantArray[0, Length[newMatrixRepresentation]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicHarmonicCoordinateConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; 
     Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                  Inverse[spacetimeMetricTensor][[First[nestedIndex],Last[nestedIndex]]]*(Inactive[D][Inactive[D][
                      newCoordinates[[index]], Join[{newTimeCoordinate}, newCoordinates][[First[nestedIndex]]]], 
                     Join[{newTimeCoordinate}, newCoordinates][[Last[nestedIndex]]]] - Total[
                     (spacetimeChristoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*Inactive[D][newCoordinates[[
                          index]], Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[Length[
                        spacetimeMetricTensor]]])] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
           Range[Length[newMatrixRepresentation]]]] == ConstantArray[0, Length[newMatrixRepresentation]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["MinimalDistortionConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor, shiftVectorDerivative}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; shiftVectorDerivative = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[newShiftVector[[Last[index]]], newCoordinates[[First[
                index]]]] + Total[(spatialChristoffelSymbols[[Last[index],First[index],#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                   Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*(D[shiftVectorDerivative[[
                       First[nestedIndex],index]], newCoordinates[[Last[nestedIndex]]]] + Total[
                      (spatialChristoffelSymbols[[index,Last[nestedIndex],#1]]*shiftVectorDerivative[[First[nestedIndex],
                          #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,
                          Last[nestedIndex],First[nestedIndex]]]*shiftVectorDerivative[[#1,index]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                  2]] + Total[(Module[{nestedIndex = #1}, Inverse[newMatrixRepresentation][[index,Last[nestedIndex]]]*
                    (D[shiftVectorDerivative[[First[nestedIndex],First[nestedIndex]]], newCoordinates[[
                       Last[nestedIndex]]]] + Total[(spatialChristoffelSymbols[[First[nestedIndex],Last[nestedIndex],#1]]*
                         shiftVectorDerivative[[First[nestedIndex],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                     Total[(spatialChristoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*shiftVectorDerivative[[
                          #1,First[nestedIndex]]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                 Tuples[Range[Length[newMatrixRepresentation]], 2]] - 2*(Total[(D[newLapseFunction*
                      contravariantExtrinsicCurvatureTensor[[#1,index]], newCoordinates[[#1]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] + Total[(spatialChristoffelSymbols[[First[#1],First[#1],
                      Last[#1]]]*(newLapseFunction*contravariantExtrinsicCurvatureTensor[[Last[#1],index]]) & ) /@ 
                   Tuples[Range[Length[newMatrixRepresentation]], 2]] + Total[(spatialChristoffelSymbols[[index,First[#1],
                      Last[#1]]]*(newLapseFunction*contravariantExtrinsicCurvatureTensor[[First[#1],Last[#1]]]) & ) /@ 
                   Tuples[Range[Length[newMatrixRepresentation]], 2]])] & ) /@ 
           Range[Length[newMatrixRepresentation]]]] == ConstantArray[0, Length[newMatrixRepresentation]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedMinimalDistortionConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor, shiftVectorDerivative}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; shiftVectorDerivative = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[newShiftVector[[Last[index]]], newCoordinates[[First[
                index]]]] + Total[(spatialChristoffelSymbols[[Last[index],First[index],#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     FullSimplify[
      Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                    Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*(D[shiftVectorDerivative[[
                        First[nestedIndex],index]], newCoordinates[[Last[nestedIndex]]]] + Total[
                       (spatialChristoffelSymbols[[index,Last[nestedIndex],#1]]*shiftVectorDerivative[[First[nestedIndex],
                           #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,
                           Last[nestedIndex],First[nestedIndex]]]*shiftVectorDerivative[[#1,index]] & ) /@ 
                        Range[Length[newMatrixRepresentation]]])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                   2]] + Total[(Module[{nestedIndex = #1}, Inverse[newMatrixRepresentation][[index,Last[nestedIndex]]]*
                     (D[shiftVectorDerivative[[First[nestedIndex],First[nestedIndex]]], newCoordinates[[
                        Last[nestedIndex]]]] + Total[(spatialChristoffelSymbols[[First[nestedIndex],Last[nestedIndex],
                           #1]]*shiftVectorDerivative[[First[nestedIndex],#1]] & ) /@ Range[Length[
                          newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[#1,Last[nestedIndex],
                           First[nestedIndex]]]*shiftVectorDerivative[[#1,First[nestedIndex]]] & ) /@ Range[
                         Length[newMatrixRepresentation]]])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
                2*(Total[(D[newLapseFunction*contravariantExtrinsicCurvatureTensor[[#1,index]], newCoordinates[[
                       #1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[(spatialChristoffelSymbols[[First[#1],
                       First[#1],Last[#1]]]*(newLapseFunction*contravariantExtrinsicCurvatureTensor[[Last[#1],
                        index]]) & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] + 
                  Total[(spatialChristoffelSymbols[[index,First[#1],Last[#1]]]*(newLapseFunction*
                       contravariantExtrinsicCurvatureTensor[[First[#1],Last[#1]]]) & ) /@ Tuples[Range[
                      Length[newMatrixRepresentation]], 2]])] & ) /@ Range[Length[newMatrixRepresentation]]]] == 
         ConstantArray[0, Length[newMatrixRepresentation]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicMinimalDistortionConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor, shiftVectorDerivative}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; shiftVectorDerivative = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][newShiftVector[[Last[index]]], 
              newCoordinates[[First[index]]]] + Total[(spatialChristoffelSymbols[[Last[index],First[index],#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                   Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*(Inactive[D][
                      shiftVectorDerivative[[First[nestedIndex],index]], newCoordinates[[Last[nestedIndex]]]] + 
                     Total[(spatialChristoffelSymbols[[index,Last[nestedIndex],#1]]*shiftVectorDerivative[[
                          First[nestedIndex],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                     Total[(spatialChristoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*shiftVectorDerivative[[
                          #1,index]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                 Tuples[Range[Length[newMatrixRepresentation]], 2]] + Total[(Module[{nestedIndex = #1}, 
                   Inverse[newMatrixRepresentation][[index,Last[nestedIndex]]]*(Inactive[D][shiftVectorDerivative[[
                       First[nestedIndex],First[nestedIndex]]], newCoordinates[[Last[nestedIndex]]]] + 
                     Total[(spatialChristoffelSymbols[[First[nestedIndex],Last[nestedIndex],#1]]*shiftVectorDerivative[[
                          First[nestedIndex],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                     Total[(spatialChristoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*shiftVectorDerivative[[
                          #1,First[nestedIndex]]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                 Tuples[Range[Length[newMatrixRepresentation]], 2]] - 2*(Total[(Inactive[D][newLapseFunction*
                      contravariantExtrinsicCurvatureTensor[[#1,index]], newCoordinates[[#1]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]] + Total[(spatialChristoffelSymbols[[First[#1],First[#1],
                      Last[#1]]]*(newLapseFunction*contravariantExtrinsicCurvatureTensor[[Last[#1],index]]) & ) /@ 
                   Tuples[Range[Length[newMatrixRepresentation]], 2]] + Total[(spatialChristoffelSymbols[[index,First[#1],
                      Last[#1]]]*(newLapseFunction*contravariantExtrinsicCurvatureTensor[[First[#1],Last[#1]]]) & ) /@ 
                   Tuples[Range[Length[newMatrixRepresentation]], 2]])] & ) /@ 
           Range[Length[newMatrixRepresentation]]]] == ConstantArray[0, Length[newMatrixRepresentation]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["PseudoMinimalDistortionConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                   Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*D[D[newShiftVector[[index]], 
                      newCoordinates[[First[nestedIndex]]]], newCoordinates[[Last[nestedIndex]]]]] & ) /@ 
                 Tuples[Range[Length[newMatrixRepresentation]], 2]] + Total[(Module[{nestedIndex = #1}, 
                   Inverse[newMatrixRepresentation][[index,Last[nestedIndex]]]*D[D[newShiftVector[[First[nestedIndex]]], 
                      newCoordinates[[First[nestedIndex]]]], newCoordinates[[Last[nestedIndex]]]]] & ) /@ 
                 Tuples[Range[Length[newMatrixRepresentation]], 2]] - 2*Total[(D[newLapseFunction*
                     contravariantExtrinsicCurvatureTensor[[#1,index]], newCoordinates[[#1]]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] == 
        ConstantArray[0, Length[newMatrixRepresentation]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedPseudoMinimalDistortionConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     FullSimplify[
      Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                    Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*D[D[newShiftVector[[index]], 
                       newCoordinates[[First[nestedIndex]]]], newCoordinates[[Last[nestedIndex]]]]] & ) /@ 
                  Tuples[Range[Length[newMatrixRepresentation]], 2]] + Total[(Module[{nestedIndex = #1}, 
                    Inverse[newMatrixRepresentation][[index,Last[nestedIndex]]]*D[D[newShiftVector[[First[nestedIndex]]], 
                       newCoordinates[[First[nestedIndex]]]], newCoordinates[[Last[nestedIndex]]]]] & ) /@ 
                  Tuples[Range[Length[newMatrixRepresentation]], 2]] - 2*Total[(D[newLapseFunction*
                      contravariantExtrinsicCurvatureTensor[[#1,index]], newCoordinates[[#1]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] == 
         ConstantArray[0, Length[newMatrixRepresentation]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicPseudoMinimalDistortionConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, contravariantExtrinsicCurvatureTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; contravariantExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],First[#1]]]*
                Inverse[newMatrixRepresentation][[Last[#1],Last[index]]]*extrinsicCurvatureTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Module[{nestedIndex = #1}, 
                   Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*Inactive[D][Inactive[D][
                      newShiftVector[[index]], newCoordinates[[First[nestedIndex]]]], newCoordinates[[
                      Last[nestedIndex]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] + Total[
                (Module[{nestedIndex = #1}, Inverse[newMatrixRepresentation][[index,Last[nestedIndex]]]*Inactive[D][
                     Inactive[D][newShiftVector[[First[nestedIndex]]], newCoordinates[[First[nestedIndex]]]], 
                     newCoordinates[[Last[nestedIndex]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 2*
                Total[(Inactive[D][newLapseFunction*contravariantExtrinsicCurvatureTensor[[#1,index]], newCoordinates[[
                     #1]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Range[Length[newMatrixRepresentation]]]] == ConstantArray[0, Length[newMatrixRepresentation]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["Dimensions"] := Length[matrixRepresentation] + 1 /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["Signature"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[matrixRepresentation]]] - lapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         (Module[{index = #1}, {index + 1, #1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["RiemannianQ"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      If[Length[positiveEigenvalues] == Length[spacetimeMetricTensor] || Length[negativeEigenvalues] == 
         Length[spacetimeMetricTensor], True, False], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["PseudoRiemannianQ"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      If[Length[positiveEigenvalues] == Length[spacetimeMetricTensor] || Length[negativeEigenvalues] == 
         Length[spacetimeMetricTensor], False, True], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["LorentzianQ"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["RiemannianConditions"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, riemannianConditions}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; If[riemannianConditions === True, {}, 
      If[riemannianConditions === False, Indeterminate, If[Length[Select[riemannianConditions, #1 === False & ]] > 0, 
        Indeterminate, DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["PseudoRiemannianConditions"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigenvalues, pseudoRiemannianConditions}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigenvalues = Eigenvalues[spacetimeMetricTensor]; 
     pseudoRiemannianConditions = FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["LorentzianConditions"] := 
  Module[{shiftCovector, spacetimeMetricTensor, eigensystem, eigenvalues, eigenvectors, newTimeCoordinate, 
     lorentzianConditions}, 
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
          Tuples[Range[Length[matrixRepresentation]], 2]]]]; eigensystem = Eigensystem[spacetimeMetricTensor]; 
     eigenvalues = First[eigensystem]; eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[spacetimeMetricTensor] - 1]]]] > 0, 
      newTimeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[spacetimeMetricTensor] - 
              1]]]]]; lorentzianConditions = FullSimplify[(If[#1 == newTimeCoordinate, eigenvalues[[#1]] < 0, 
            eigenvalues[[#1]] > 0] & ) /@ Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, 
        If[lorentzianConditions === False, Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, 
          Indeterminate, DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["EvolutionEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, 
     spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, leftHandSide, rightHandSide}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedSpatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                spatialRicciTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[
                1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> D[mixedExtrinsicCurvatureTensor[[First[index],
              Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (D[D[newLapseFunction, newCoordinates[[
                      Last[index]]]], newCoordinates[[nestedIndex]]] - Total[(spatialChristoffelSymbols[[#1,nestedIndex,
                        Last[index]]]*D[newLapseFunction, newCoordinates[[#1]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]])*Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(D[mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(D[newShiftVector[[nestedIndex]], newCoordinates[[Last[index]]]] + 
                   Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[nestedIndex,Last[index]]]*
                  (D[newShiftVector[[First[index]]], newCoordinates[[nestedIndex]]] + Total[
                    (spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[#1]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]])] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             newLapseFunction*Total[(spacetimeRicciTensor[[#1 + 1,Last[index] + 1]]*Inverse[newMatrixRepresentation][[#1,
                   First[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicEvolutionEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, mixedSpatialRicciTensor, spacetimeMetricTensor, 
     spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, leftHandSide, rightHandSide}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][spatialChristoffelSymbols[[index[[1]],
               index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                  index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     mixedSpatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*spatialRicciTensor[[#1,
                 Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
             Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[index[[4]]]]] + Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spacetimeChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     leftHandSide = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][mixedExtrinsicCurvatureTensor[[
              First[index],Last[index]]], newTimeCoordinate]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     rightHandSide = Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*mixedSpatialRicciTensor[[First[
                index],Last[index]]] - Total[(Module[{nestedIndex = #1}, (Inactive[D][Inactive[D][newLapseFunction, 
                     newCoordinates[[Last[index]]]], newCoordinates[[nestedIndex]]] - Total[
                    (spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*Inactive[D][newLapseFunction, 
                        newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]])*
                  Inverse[newMatrixRepresentation][[nestedIndex,First[index]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] + newLapseFunction*extrinsicCurvatureTrace*
              mixedExtrinsicCurvatureTensor[[First[index],Last[index]]] + Total[(Module[{nestedIndex = #1}, 
                 newShiftVector[[nestedIndex]]*(Inactive[D][mixedExtrinsicCurvatureTensor[[First[index],Last[index]]], 
                    newCoordinates[[nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*
                       mixedExtrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                   Total[(spatialChristoffelSymbols[[#1,nestedIndex,Last[index]]]*mixedExtrinsicCurvatureTensor[[
                        First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] + Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   First[index],nestedIndex]]*(Inactive[D][newShiftVector[[nestedIndex]], newCoordinates[[
                     Last[index]]]] + Total[(spatialChristoffelSymbols[[nestedIndex,Last[index],#1]]*newShiftVector[[
                        #1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(Module[{nestedIndex = #1}, mixedExtrinsicCurvatureTensor[[
                   nestedIndex,Last[index]]]*(Inactive[D][newShiftVector[[First[index]]], newCoordinates[[
                     nestedIndex]]] + Total[(spatialChristoffelSymbols[[First[index],nestedIndex,#1]]*newShiftVector[[
                        #1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ Range[
                Length[newMatrixRepresentation]]] - newLapseFunction*Total[(spacetimeRicciTensor[[#1 + 1,
                   Last[index] + 1]]*Inverse[newMatrixRepresentation][[#1,First[index]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["HamiltonianConstraint"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeChristoffelSymbols, 
     spacetimeRiemannTensor, spacetimeRicciTensor, spacetimeRicciScalar, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spatialRicciScalar = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[
                1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     (spatialRicciScalar + extrinsicCurvatureTrace^2 - Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
            mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]) - 2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicHamiltonianConstraint"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeChristoffelSymbols, 
     spacetimeRiemannTensor, spacetimeRicciTensor, spacetimeRicciScalar, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][spatialChristoffelSymbols[[index[[1]],
               index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                  index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     spatialRicciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
             Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[index[[4]]]]] + Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spacetimeChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     (spatialRicciScalar + extrinsicCurvatureTrace^2 - Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
            mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]) - 2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["MomentumConstraints"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, 
     spacetimeRicciScalar, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[
                1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[index,#1]], 
                 newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
             Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                  Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
             Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                  First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
              newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ 
         Range[Length[newMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicMomentumConstraints"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, 
     spacetimeRicciScalar, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
             Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[index[[4]]]]] + Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spacetimeChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inactive[D][mixedExtrinsicCurvatureTensor[[index,#1]], 
                 newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
             Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                  Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
             Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                  First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
             Inactive[D][extrinsicCurvatureTrace, newCoordinates[[index]]] - newLapseFunction*
              mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ Range[Length[newMatrixRepresentation]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["HamiltonianConstraintEquation"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeChristoffelSymbols, 
     spacetimeRiemannTensor, spacetimeRicciTensor, spacetimeRicciScalar, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              newCoordinates[[index[[3]]]]] - D[spatialChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spatialChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spatialRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; spatialRicciScalar = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[
                1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     (spatialRicciScalar + extrinsicCurvatureTrace^2 - Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
             mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
            2]]) - 2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] == 0 /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicHamiltonianConstraintEquation"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spatialRicciTensor, spatialRicciScalar, spacetimeMetricTensor, spacetimeChristoffelSymbols, 
     spacetimeRiemannTensor, spacetimeRicciTensor, spacetimeRicciScalar, spacetimeEinsteinTensor, 
     contravariantSpacetimeEinsteinTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][spatialChristoffelSymbols[[index[[1]],
               index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                  index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     spatialRicciTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(spatialRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     spatialRicciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*spatialRicciTensor[[First[#1],
           Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
             Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[index[[4]]]]] + Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spacetimeChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; contravariantSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[First[index],First[#1]]]*
                Inverse[spacetimeMetricTensor][[Last[#1],Last[index]]]*spacetimeEinsteinTensor[[First[#1],
                 Last[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     (spatialRicciScalar + extrinsicCurvatureTrace^2 - Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*
             mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
            2]]) - 2*newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] == 0 /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["MomentumConstraintEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, 
     spacetimeRicciScalar, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; extrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> (1/(2*newLapseFunction))*(D[shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + D[shiftCovector[[Last[index]]], 
               newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],Last[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - D[newMatrixRepresentation[[
                First[index],Last[index]]], newTimeCoordinate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
          2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (D[spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[2]]]]] + 
                 D[spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
                 D[spacetimeMetricTensor[[index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[
                1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Thread[(Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[index,#1]], 
                   newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                    Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - Total[
                (spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                    First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
                newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ 
           Range[Length[newMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]) == ConstantArray[0, Length[matrixRepresentation]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicMomentumConstraintEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, spacetimeRicciTensor, 
     spacetimeRicciScalar, spacetimeEinsteinTensor, mixedSpacetimeEinsteinTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         String]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     shiftCovector = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     spatialChristoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     extrinsicCurvatureTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> (1/(2*newLapseFunction))*(Inactive[D][shiftCovector[[First[index]]], 
               newCoordinates[[Last[index]]]] - Total[(spatialChristoffelSymbols[[#1,Last[index],First[index]]]*
                  shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] + Inactive[D][shiftCovector[[
                Last[index]]], newCoordinates[[First[index]]]] - Total[(spatialChristoffelSymbols[[#1,First[index],
                   Last[index]]]*shiftCovector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Inactive[D][newMatrixRepresentation[[First[index],Last[index]]], newTimeCoordinate])] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; mixedExtrinsicCurvatureTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[#1,Last[index]]]*
                extrinsicCurvatureTensor[[First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]*shiftCovector[[#1]] & ) /@ 
              Range[Length[newMatrixRepresentation]]] - newLapseFunction^2}, 
         (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         (Module[{index = #1}, {index + 1, 1} -> Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; spacetimeChristoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[spacetimeMetricTensor][[index[[1]],#1]]*
                (Inactive[D][spacetimeMetricTensor[[#1,index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[
                   index[[2]]]]] + Inactive[D][spacetimeMetricTensor[[index[[2]],#1]], Join[{newTimeCoordinate}, 
                    newCoordinates][[index[[3]]]]] - Inactive[D][spacetimeMetricTensor[[index[[2]],index[[3]]]], 
                  Join[{newTimeCoordinate}, newCoordinates][[#1]]]) & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - 
             Inactive[D][spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[3]]]], Join[{newTimeCoordinate}, 
                newCoordinates][[index[[4]]]]] + Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 spacetimeChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[spacetimeMetricTensor]]] - 
             Total[(spacetimeChristoffelSymbols[[index[[1]],#1,index[[4]]]]*spacetimeChristoffelSymbols[[#1,index[[2]],
                  index[[3]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 4]]]; spacetimeRicciTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(spacetimeRiemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; spacetimeRicciScalar = 
      Total[(Inverse[spacetimeMetricTensor][[First[#1],Last[#1]]]*spacetimeRicciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[spacetimeMetricTensor]], 2]]; spacetimeEinsteinTensor = 
      spacetimeRicciTensor - (1/2)*spacetimeRicciScalar*spacetimeMetricTensor; mixedSpacetimeEinsteinTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[spacetimeMetricTensor][[#1,Last[index]]]*
                spacetimeEinsteinTensor[[First[index],#1]] & ) /@ Range[Length[spacetimeMetricTensor]]]] & ) /@ 
         Tuples[Range[Length[spacetimeMetricTensor]], 2]]]; 
     Thread[(Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inactive[D][mixedExtrinsicCurvatureTensor[[index,
                    #1]], newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                    Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - Total[
                (spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                    First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - Inactive[D][
                extrinsicCurvatureTrace, newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[
                 index + 1,1]]] & ) /@ Range[Length[newMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[Join[coordinates, {timeCoordinate}], StringQ]) == ConstantArray[0, Length[matrixRepresentation]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    timeCoordinate_, lapseFunction_, shiftVector_List], newTimeCoordinate_] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation /. timeCoordinate -> newTimeCoordinate, 
     coordinates, index1, index2], newTimeCoordinate, lapseFunction, shiftVector] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    timeCoordinate_, lapseFunction_, shiftVector_List], newLapseFunction_, newShiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], timeCoordinate, 
    newLapseFunction, newShiftVector] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation] && 
    Length[newShiftVector] == Length[matrixRepresentation]
ADMDecomposition[ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    timeCoordinate_, lapseFunction_, shiftVector_List], newTimeCoordinate_, newLapseFunction_, newShiftVector_List] := 
  ADMDecomposition[ResourceFunction["MetricTensor"][matrixRepresentation /. timeCoordinate -> newTimeCoordinate, 
     coordinates, index1, index2], newTimeCoordinate, newLapseFunction, newShiftVector] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation] && Length[newShiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["Properties"] := {"SpatialMetricTensor", "SpacetimeMetricTensor", "NormalVector", 
    "ReducedNormalVector", "SymbolicNormalVector", "TimeVector", "SymbolicTimeVector", "TimeCoordinate", 
    "SpatialCoordinates", "CoordinateOneForms", "LapseFunction", "ShiftVector", "GaussEquations", 
    "SymbolicGaussEquations", "CodazziMainardiEquations", "SymbolicCodazziMainardiEquations", "GeodesicSlicingCondition", 
    "ReducedGeodesicSlicingCondition", "MaximalSlicingCondition", "ReducedMaximalSlicingCondition", 
    "SymbolicMaximalSlicingCondition", "HarmonicSlicingCondition", "ReducedHarmonicSlicingCondition", 
    "SymbolicHarmonicSlicingCondition", "1+LogSlicingCondition", "Reduced1+LogSlicingCondition", 
    "Symbolic1+LogSlicingCondition", "NormalCoordinateConditions", "ReducedNormalCoordinateConditions", 
    "HarmonicCoordinateConditions", "ReducedHarmonicCoordinateConditions", "SymbolicHarmonicCoordinateConditions", 
    "MinimalDistortionConditions", "ReducedMinimalDistortionConditions", "SymbolicMinimalDistortionConditions", 
    "PseudoMinimalDistortionConditions", "ReducedPseudoMinimalDistortionConditions", 
    "SymbolicPseudoMinimalDistortionConditions", "Dimensions", "Signature", "RiemannianQ", "PseudoRiemannianQ", 
    "LorentzianQ", "RiemannianConditions", "PseudoRiemannianConditions", "LorentzianConditions", "EvolutionEquations", 
    "SymbolicEvolutionEquations", "HamiltonianConstraint", "SymbolicHamiltonianConstraint", "MomentumConstraints", 
    "SymbolicMomentumConstraints", "HamiltonianConstraintEquation", "SymbolicHamiltonianConstraintEquation", 
    "MomentumConstraintEquations", "SymbolicMomentumConstraintEquations", "Properties"} /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition /: MakeBoxes[admDecomposition:ADMDecomposition[(metricTensor_)[matrixRepresentation_List, 
       coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], format_] := 
   Module[{shiftCovector, spacetimeMetricTensor, type, symbol, dimensions, eigenvalues, positiveEigenvalues, 
      negativeEigenvalues, signature, icon}, 
     shiftCovector = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index,#1]]*
                 shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Range[Length[matrixRepresentation]]]]; spacetimeMetricTensor = 
       Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]*shiftCovector[[#1]] & ) /@ Range[
                Length[matrixRepresentation]]] - lapseFunction^2}, 
          (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ 
                Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
          (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ 
                Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
          ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]]]; dimensions = Length[spacetimeMetricTensor]; 
      eigenvalues = Eigenvalues[spacetimeMetricTensor]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
       If[Length[positiveEigenvalues] == Length[spacetimeMetricTensor] || Length[negativeEigenvalues] == 
          Length[spacetimeMetricTensor], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[spacetimeMetricTensor, 
        ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], 
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["ADMDecomposition", admDecomposition, icon, 
       {{BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}, 
        {BoxForm`SummaryItem[{"Time Coordinate: ", timeCoordinate}], BoxForm`SummaryItem[
          {"Spatial Coordinates: ", coordinates}]}}, {{}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
     Length[shiftVector] == Length[matrixRepresentation]
