(* ::Package:: *)

ADMDecomposition["Schwarzschild"] := ADMDecomposition[
   MetricTensor[DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/"\[FormalR]"), "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, 
    True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ 
    Range[3]]
ADMDecomposition["Schwarzschild", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]]), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["Schwarzschild", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]]), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, 
    shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"Schwarzschild", mass_}] := ADMDecomposition[
   MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/"\[FormalR]"), "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, 
    True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ 
    Range[3]]
ADMDecomposition[{"Schwarzschild", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]]), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"Schwarzschild", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]]), coordinates[[1]]^2, 
       coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, 
    shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["Kerr"] := ADMDecomposition[
   MetricTensor[DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*"\[FormalCapitalM]"*"\[FormalR]" + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2), 
      "\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2, ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + (2*"\[FormalR]"*("\[FormalCapitalJ]"^2/"\[FormalCapitalM]")*Sin["\[FormalTheta]"]^2)/
         ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["Kerr", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2), coordinates[[1]]^2 + 
        ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + 
         (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/"\[FormalCapitalM]")*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
           ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["Kerr", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2), coordinates[[1]]^2 + 
        ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + 
         (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/"\[FormalCapitalM]")*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
           ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"Kerr", mass_}] := ADMDecomposition[
   MetricTensor[DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*mass*"\[FormalR]" + ("\[FormalCapitalJ]"/mass)^2), 
      "\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2, ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2 + (2*"\[FormalR]"*("\[FormalCapitalJ]"^2/mass)*Sin["\[FormalTheta]"]^2)/
         ("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"Kerr", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + ("\[FormalCapitalJ]"/mass)^2), coordinates[[1]]^2 + 
        ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2 + 
         (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/mass)*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
           ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"Kerr", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + ("\[FormalCapitalJ]"/mass)^2), coordinates[[1]]^2 + 
        ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2 + 
         (2*coordinates[[1]]*("\[FormalCapitalJ]"^2/mass)*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
           ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"Kerr", mass_, angularMomentum_}] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2)/
       ("\[FormalR]"^2 - 2*mass*"\[FormalR]" + (angularMomentum/mass)^2), "\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2, 
      ("\[FormalR]"^2 + (angularMomentum/mass)^2 + (2*"\[FormalR]"*(angularMomentum^2/mass)*Sin["\[FormalTheta]"]^2)/
         ("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
   "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"Kerr", mass_, angularMomentum_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (coordinates[[1]]^2 + (angularMomentum/mass)^2 + (2*coordinates[[1]]*(angularMomentum^2/mass)*
           Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*
        Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"Kerr", mass_, angularMomentum_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (coordinates[[1]]^2 + (angularMomentum/mass)^2 + (2*coordinates[[1]]*(angularMomentum^2/mass)*
           Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*
        Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, lapseFunction, shiftVector] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["ReissnerNordstrom"] := ADMDecomposition[
   MetricTensor[DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/"\[FormalR]" + "\[FormalCapitalQ]"^2/(4*Pi*"\[FormalR]"^2)), "\[FormalR]"^2, "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], 
    {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["ReissnerNordstrom", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["ReissnerNordstrom", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*"\[FormalCapitalM]")/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_}] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/"\[FormalR]" + "\[FormalCapitalQ]"^2/(4*Pi*"\[FormalR]"^2)), "\[FormalR]"^2, 
      "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"ReissnerNordstrom", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]] + "\[FormalCapitalQ]"^2/(4*Pi*coordinates[[1]]^2)), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_, charge_}] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/"\[FormalR]" + charge^2/(4*Pi*"\[FormalR]"^2)), "\[FormalR]"^2, 
      "\[FormalR]"^2*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"ReissnerNordstrom", mass_, charge_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]] + charge^2/(4*Pi*coordinates[[1]]^2)), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"ReissnerNordstrom", mass_, charge_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{1/(1 - (2*mass)/coordinates[[1]] + charge^2/(4*Pi*coordinates[[1]]^2)), 
       coordinates[[1]]^2, coordinates[[1]]^2*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["KerrNewman"] := ADMDecomposition[
   MetricTensor[DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*"\[FormalCapitalM]"*"\[FormalR]" + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + 
        "\[FormalCapitalQ]"^2/(4*Pi)), "\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2)^2 - ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*("\[FormalR]"^2 - 2*"\[FormalCapitalM]"*"\[FormalR]" + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi))*
          Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], 
   "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition["KerrNewman", timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2)^2 - ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition["KerrNewman", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2)^2 - ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*(coordinates[[1]]^2 - 2*"\[FormalCapitalM]"*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/"\[FormalCapitalM]")^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"KerrNewman", mass_}] := ADMDecomposition[
   MetricTensor[DiagonalMatrix[{("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2)/("\[FormalR]"^2 - 2*mass*"\[FormalR]" + ("\[FormalCapitalJ]"/mass)^2 + 
        "\[FormalCapitalQ]"^2/(4*Pi)), "\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2)^2 - ("\[FormalCapitalJ]"/mass)^2*("\[FormalR]"^2 - 2*mass*"\[FormalR]" + ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*
          Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + ("\[FormalCapitalJ]"/mass)^2*Cos["\[FormalTheta]"]^2))*Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], 
   "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"KerrNewman", mass_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2)^2 - ("\[FormalCapitalJ]"/mass)^2*(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"KerrNewman", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2)^2 - ("\[FormalCapitalJ]"/mass)^2*(coordinates[[1]]^2 - 2*mass*coordinates[[1]] + 
            ("\[FormalCapitalJ]"/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/(coordinates[[1]]^2 + 
          ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, True], timeCoordinate, 
    lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_}] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2)/
       ("\[FormalR]"^2 - 2*mass*"\[FormalR]" + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
      "\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*("\[FormalR]"^2 - 2*mass*"\[FormalR]" + 
           (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2))*
       Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, 
     True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + "\[FormalCapitalQ]"^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + ("\[FormalCapitalJ]"/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, True, 
     True], timeCoordinate, lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_, charge_}] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2)/
       ("\[FormalR]"^2 - 2*mass*"\[FormalR]" + (angularMomentum/mass)^2 + charge^2/(4*Pi)), 
      "\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2, 
      ((("\[FormalR]"^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*("\[FormalR]"^2 - 2*mass*"\[FormalR]" + 
           (angularMomentum/mass)^2 + charge^2/(4*Pi))*Sin["\[FormalTheta]"]^2)/("\[FormalR]"^2 + (angularMomentum/mass)^2*Cos["\[FormalTheta]"]^2))*
       Sin["\[FormalTheta]"]^2}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, 
   (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_, charge_}, timeCoordinate_, coordinates_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, 
     True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
    (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]] /; Length[coordinates] == 3
ADMDecomposition[{"KerrNewman", mass_, angularMomentum_, charge_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := 
  ADMDecomposition[MetricTensor[DiagonalMatrix[{(coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2)/
        (coordinates[[1]]^2 - 2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi)), 
       coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2, 
       (((coordinates[[1]]^2 + (angularMomentum/mass)^2)^2 - (angularMomentum/mass)^2*(coordinates[[1]]^2 - 
            2*mass*coordinates[[1]] + (angularMomentum/mass)^2 + charge^2/(4*Pi))*Sin[coordinates[[2]]]^2)/
         (coordinates[[1]]^2 + (angularMomentum/mass)^2*Cos[coordinates[[2]]]^2))*Sin[coordinates[[2]]]^2}], coordinates, 
     True, True], timeCoordinate, lapseFunction, shiftVector] /; Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition["BrillLindquist"] := Module[{brillLindquistPotential}, 
   brillLindquistPotential = 1 + (1/2)*("\[FormalCapitalM]"/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] - Subscript["\[FormalZ]", "0"])^2] + 
        "\[FormalCapitalM]"/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] + Subscript["\[FormalZ]", "0"])^2]); 
    ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, "\[FormalR]"^2*brillLindquistPotential^4, 
        ("\[FormalR]"^2*Sin["\[FormalTheta]"]^2)*brillLindquistPotential^4}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
     "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]]
ADMDecomposition["BrillLindquist", timeCoordinate_, coordinates_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*("\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + "\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, coordinates[[1]]^2*
          brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*brillLindquistPotential^4}], 
       coordinates, True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
      (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]]] /; Length[coordinates] == 3
ADMDecomposition["BrillLindquist", timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*("\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + "\[FormalCapitalM]"/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, coordinates[[1]]^2*
          brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*brillLindquistPotential^4}], 
       coordinates, True, True], timeCoordinate, lapseFunction, shiftVector]] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"BrillLindquist", mass_}] := Module[{brillLindquistPotential}, 
   brillLindquistPotential = 1 + (1/2)*(mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] - Subscript["\[FormalZ]", "0"])^2] + 
        mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] + Subscript["\[FormalZ]", "0"])^2]); 
    ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, "\[FormalR]"^2*brillLindquistPotential^4, 
        ("\[FormalR]"^2*Sin["\[FormalTheta]"]^2)*brillLindquistPotential^4}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
     "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]]
ADMDecomposition[{"BrillLindquist", mass_}, timeCoordinate_, coordinates_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, coordinates[[1]]^2*
          brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*brillLindquistPotential^4}], 
       coordinates, True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
      (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]]] /; Length[coordinates] == 3
ADMDecomposition[{"BrillLindquist", mass_}, timeCoordinate_, coordinates_List, lapseFunction_, shiftVector_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              Subscript["\[FormalZ]", "0"])^2] + mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + Subscript["\[FormalZ]", "0"])^2]); 
     ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, coordinates[[1]]^2*
          brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*brillLindquistPotential^4}], 
       coordinates, True, True], timeCoordinate, lapseFunction, shiftVector]] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[{"BrillLindquist", mass_, position_}] := Module[{brillLindquistPotential}, 
   brillLindquistPotential = 1 + (1/2)*(mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] - position)^2] + 
        mass/Sqrt["\[FormalR]"^2*Sin["\[FormalTheta]"]^2 + ("\[FormalR]"*Cos["\[FormalTheta]"] + position)^2]); 
    ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, "\[FormalR]"^2*brillLindquistPotential^4, 
        ("\[FormalR]"^2*Sin["\[FormalTheta]"]^2)*brillLindquistPotential^4}], {"\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, True, True], "\[FormalT]", 
     "\[FormalAlpha]" @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"}, (Superscript["\[FormalBeta]", #1] @@ {"\[FormalT]", "\[FormalR]", "\[FormalTheta]", "\[FormalPhi]"} & ) /@ Range[3]]]
ADMDecomposition[{"BrillLindquist", mass_, position_}, timeCoordinate_, coordinates_List] := 
  Module[{brillLindquistPotential}, brillLindquistPotential = 
      1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] - 
              position)^2] + mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] + position)^2]); 
     ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, coordinates[[1]]^2*
          brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*brillLindquistPotential^4}], 
       coordinates, True, True], timeCoordinate, "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], 
      (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ Range[3]]] /; Length[coordinates] == 3
ADMDecomposition[{"BrillLindquist", mass_, position_}, timeCoordinate_, coordinates_List, lapseFunction_, 
   shiftVector_List] := Module[{brillLindquistPotential}, 
    brillLindquistPotential = 1 + (1/2)*(mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + 
            (coordinates[[1]]*Cos[coordinates[[2]]] - position)^2] + 
         mass/Sqrt[coordinates[[1]]^2*Sin[coordinates[[2]]]^2 + (coordinates[[1]]*Cos[coordinates[[2]]] + position)^2]); 
     ADMDecomposition[MetricTensor[DiagonalMatrix[{brillLindquistPotential^4, coordinates[[1]]^2*
          brillLindquistPotential^4, (coordinates[[1]]^2*Sin[coordinates[[2]]]^2)*brillLindquistPotential^4}], 
       coordinates, True, True], timeCoordinate, lapseFunction, shiftVector]] /; 
   Length[coordinates] == 3 && Length[shiftVector] == 3
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  ADMDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], "\[FormalT]", 
    "\[FormalAlpha]" @@ Join[{"\[FormalT]"}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{"\[FormalT]"}, coordinates] & ) /@ 
     Range[Length[matrixRepresentation]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_] := 
  ADMDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], timeCoordinate, 
    "\[FormalAlpha]" @@ Join[{timeCoordinate}, coordinates], (Superscript["\[FormalBeta]", #1] @@ Join[{timeCoordinate}, coordinates] & ) /@ 
     Range[Length[matrixRepresentation]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SpatialMetricTensor"] := 
  MetricTensor[matrixRepresentation, coordinates, index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SpacetimeMetricTensor"] := 
  MetricTensor[Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]^2 & ) /@ Range[Length[matrixRepresentation]]] - 
          lapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> 
           Total[(matrixRepresentation[[index,#1]]*shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
        Range[Length[matrixRepresentation]], 
       (Module[{index = #1}, {index + 1, 1} -> Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ 
             Range[Length[matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
       ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]]], Join[{timeCoordinate}, coordinates], True, True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["NormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedNormalVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
            newLapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(newMatrixRepresentation[[index,#1]]*
                 newShiftVector[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Range[Length[newMatrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
             Total[(newMatrixRepresentation[[#1,index]]*newShiftVector[[#1]] & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Range[Length[newMatrixRepresentation]], 
         ({First[#1] + 1, Last[#1] + 1} -> newMatrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]]; 
     Normal[SparseArray[(Module[{index = #1}, index -> -Total[(newLapseFunction*Inverse[spacetimeMetricTensor][[index,#1]]*
                 Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ Range[
                Length[spacetimeMetricTensor]]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["TimeVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor, normalVector}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
    lapseFunction_, shiftVector_List]["ReducedTimeVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor, normalVector}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> newLapseFunction*normalVector[[index]] + 
              Join[{0}, newShiftVector][[index]]] & ) /@ Range[Length[spacetimeMetricTensor]]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicTimeVector"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor, normalVector}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
    lapseFunction_, shiftVector_List]["GaussEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, normalVector, 
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
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
    lapseFunction_, shiftVector_List]["ReducedGaussEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, normalVector, 
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
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; spacetimeMetricTensor = 
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
     FullSimplify[Thread[Catenate[Catenate[Catenate[leftHandSide]]] == Catenate[Catenate[Catenate[rightHandSide]]]] /. 
       (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicGaussEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, shiftCovector, 
     spatialChristoffelSymbols, extrinsicCurvatureTensor, mixedExtrinsicCurvatureTensor, extrinsicCurvatureTrace, 
     spatialRiemannTensor, spacetimeMetricTensor, spacetimeChristoffelSymbols, spacetimeRiemannTensor, normalVector, 
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
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[First[index],#1]]*
                extrinsicCurvatureTensor[[#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; extrinsicCurvatureTrace = 
      Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*extrinsicCurvatureTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; spatialRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[2]],
               index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][spatialChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(spatialChristoffelSymbols[[index[[1]],#1,
                  index[[3]]]]*spatialChristoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(spatialChristoffelSymbols[[index[[1]],#1,index[[4]]]]*
                 spatialChristoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
                2]] - Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                  First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
              newCoordinates[[index]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     rightHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((-spacetimeRicciTensor[[First[#1],Last[#1]]])*normalVector[[Last[#1]]]*
                projectionOperator[[index + 1,First[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Thread[leftHandSide == rightHandSide] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedCodazziMainardiEquations"] := 
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
                2]] - Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                  First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
              newCoordinates[[index]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]; 
     rightHandSide = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((-spacetimeRicciTensor[[First[#1],Last[#1]]])*normalVector[[Last[#1]]]*
                projectionOperator[[index + 1,First[#1]]] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; FullSimplify[Thread[leftHandSide == rightHandSide] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
                Range[Length[newMatrixRepresentation]], 2]] - Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*
                 mixedExtrinsicCurvatureTensor[[Last[#1],First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                2]] - Inactive[D][extrinsicCurvatureTrace, newCoordinates[[index]]]] & ) /@ 
         Range[Length[newMatrixRepresentation]]]]; rightHandSide = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((-spacetimeRicciTensor[[First[#1],Last[#1]]])*
                normalVector[[Last[#1]]]*projectionOperator[[index + 1,First[#1]]] & ) /@ Tuples[Range[
                Length[spacetimeMetricTensor]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     Thread[leftHandSide == rightHandSide] /. (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], 
        StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
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
      Total[(Module[{index = #1}, Sqrt[Det[newMatrixRepresentation]]*D[Sqrt[Det[newMatrixRepresentation]]*
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
      Total[(Module[{index = #1}, Sqrt[Det[newMatrixRepresentation]]*D[Sqrt[Det[newMatrixRepresentation]]*
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
      Total[(Module[{index = #1}, Sqrt[Det[newMatrixRepresentation]]*Inactive[D][Sqrt[Det[newMatrixRepresentation]]*
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
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
             (D[D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[First[index]]]], Join[{newTimeCoordinate}, 
                 newCoordinates][[Last[index]]]] - Total[(spacetimeChristoffelSymbols[[#1,Last[index],First[index]]]*
                  D[newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[#1]]] & ) /@ 
                Range[Length[spacetimeMetricTensor]]])] & ) /@ Tuples[Range[Length[spacetimeMetricTensor]], 2]] == 0 /. 
       (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["SymbolicHarmonicSlicingCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newLapseFunction, newShiftVector, 
     spacetimeMetricTensor, spacetimeChristoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newCoordinates = 
      coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], 
         StringQ]; newLapseFunction = lapseFunction /. (#1 -> ToExpression[#1] & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]; newShiftVector = 
      shiftVector /. (#1 -> ToExpression[#1] & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]; 
     spacetimeMetricTensor = Normal[SparseArray[
        Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
              Join[{newTimeCoordinate}, newCoordinates][[Last[index]]]] - Total[(spacetimeChristoffelSymbols[[#1,
                  Last[index],First[index]]]*Inactive[D][newTimeCoordinate, Join[{newTimeCoordinate}, newCoordinates][[
                   #1]]] & ) /@ Range[Length[spacetimeMetricTensor]]])] & ) /@ 
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
                First[index],Last[index]]], newTimeCooridnate])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; Thread[Catenate[leftHandSide] == Catenate[rightHandSide]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[Join[coordinates, {timeCoordinate}], StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[shiftVector] == Length[matrixRepresentation]
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
     (1/2)*(spatialRicciScalar + extrinsicCurvatureTrace^2 - 
         Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[Last[#1],
              First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]) - 
       newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. (ToExpression[#1] -> #1 & ) /@ 
       Select[Join[coordinates, {timeCoordinate}], StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], timeCoordinate_, 
    lapseFunction_, shiftVector_List]["ReducedHamiltonianConstraint"] := 
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
     FullSimplify[(1/2)*(spatialRicciScalar + extrinsicCurvatureTrace^2 - 
          Total[(mixedExtrinsicCurvatureTensor[[First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[Last[#1],First[
                #1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]) - 
        newLapseFunction^2*contravariantSpacetimeEinsteinTensor[[1,1]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
    lapseFunction_, shiftVector_List]["ReducedMomentumConstraints"] := 
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
      Normal[SparseArray[Join[{{1, 1} -> Total[(newShiftVector[[#1]]^2 & ) /@ Range[Length[newMatrixRepresentation]]] - 
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
         Tuples[Range[Length[spacetimeMetricTensor]], 3]]]; spacetimeRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> D[spacetimeChristoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
              Join[{newTimeCoordinate}, newCoordinates][[index[[3]]]]] - D[spacetimeChristoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], Join[{newTimeCoordinate}, newCoordinates][[index[[4]]]]] + 
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
     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(D[mixedExtrinsicCurvatureTensor[[index,#1]], 
                  newCoordinates[[#1]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
              Total[(spatialChristoffelSymbols[[First[#1],First[#1],Last[#1]]]*mixedExtrinsicCurvatureTensor[[index,
                   Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
              Total[(spatialChristoffelSymbols[[Last[#1],First[#1],index]]*mixedExtrinsicCurvatureTensor[[Last[#1],
                   First[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - D[extrinsicCurvatureTrace, 
               newCoordinates[[index]]] - newLapseFunction*mixedSpacetimeEinsteinTensor[[index + 1,1]]] & ) /@ 
          Range[Length[newMatrixRepresentation]]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[Join[coordinates, {timeCoordinate}], StringQ]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
ADMDecomposition /: MakeBoxes[admDecomposition:ADMDecomposition[(metricTensor_)[matrixRepresentation_List, 
       coordinates_List, index1_, index2_], timeCoordinate_, lapseFunction_, shiftVector_List], format_] := 
   Module[{spacetimeMetricTensor, type, symbol, dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, 
      signature, icon}, spacetimeMetricTensor = 
       Normal[SparseArray[Join[{{1, 1} -> Total[(shiftVector[[#1]]^2 & ) /@ Range[Length[matrixRepresentation]]] - 
             lapseFunction^2}, (Module[{index = #1}, {1, index + 1} -> Total[(matrixRepresentation[[index,#1]]*
                  shiftVector[[#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
           Range[Length[matrixRepresentation]], (Module[{index = #1}, {index + 1, 1} -> 
              Total[(matrixRepresentation[[#1,index]]*shiftVector[[#1]] & ) /@ Range[Length[
                  matrixRepresentation]]]] & ) /@ Range[Length[matrixRepresentation]], 
          ({First[#1] + 1, Last[#1] + 1} -> matrixRepresentation[[First[#1],Last[#1]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]]]; dimensions = Length[spacetimeMetricTensor]; 
      eigenvalues = Eigenvalues[spacetimeMetricTensor]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[spacetimeMetricTensor], 
       If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
          Length[matrixRepresentation], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[spacetimeMetricTensor, 
        ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], 
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["ADMDecomposition", admDecomposition, icon, 
       {{BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}, 
        {BoxForm`SummaryItem[{"Time Coordinate: ", timeCoordinate}], BoxForm`SummaryItem[{"Spatial Coordinates: ", 
           coordinates}]}}, {{}}, format, "Interpretable" -> Automatic]] /; SymbolName[metricTensor] === "MetricTensor" && 
     Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
     BooleanQ[index1] && BooleanQ[index2] && Length[shiftVector] == Length[matrixRepresentation]
