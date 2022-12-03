(* ::Package:: *)

SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["GeodesicEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[initialVector] == Length[matrixRepresentation] && 
    Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["ReducedGeodesicEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     FullSimplify[Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
             Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                   (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                   parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ Tuples[
                Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[initialVector] == Length[matrixRepresentation] && 
    Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["SymbolicGeodesicEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[initialVector] == Length[matrixRepresentation] && 
    Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["CoordinateTimeGeodesicEquations"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]]*
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     Select[Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][First[newCoordinates]] + 
              Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                    (#1 -> #1[First[newCoordinates]] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[
                      First[nestedIndex]]]][First[newCoordinates]]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][
                    First[newCoordinates]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
              Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[1,First[nestedIndex],Last[nestedIndex]]] /. 
                    (#1 -> #1[First[newCoordinates]] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[
                      First[nestedIndex]]]][First[newCoordinates]]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][
                    First[newCoordinates]]*Derivative[1][newCoordinates[[index]]][First[newCoordinates]]] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 
          0] /. {First[newCoordinates][First[newCoordinates]] -> First[newCoordinates], 
         Derivative[1][First[newCoordinates]][First[newCoordinates]] -> 1, 
         Derivative[2][First[newCoordinates]][First[newCoordinates]] -> 0}, #1 =!= True & ] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[initialVector] == Length[matrixRepresentation] && 
    Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["Solution"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, equations}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[parameter] & ) /@ newCoordinates, 
       {parameter, initialParameter, finalParameter}] /. ((ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) -> #1 & ) /@ 
       Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[initialVector] == Length[matrixRepresentation] && 
    Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["CoordinateTimeSolution"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, equations}, 
    newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi], \[FormalT] -> t}) & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi], \[FormalT] -> t}) & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]]*
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     equations = Select[Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][First[newCoordinates]] + 
              Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                    (#1 -> #1[First[newCoordinates]] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[
                      First[nestedIndex]]]][First[newCoordinates]]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][
                    First[newCoordinates]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]] - 
              Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[1,First[nestedIndex],Last[nestedIndex]]] /. 
                    (#1 -> #1[First[newCoordinates]] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[
                      First[nestedIndex]]]][First[newCoordinates]]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][
                    First[newCoordinates]]*Derivative[1][newCoordinates[[index]]][First[newCoordinates]]] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 
          0] /. {First[newCoordinates][First[newCoordinates]] -> First[newCoordinates], 
         Derivative[1][First[newCoordinates]][First[newCoordinates]] -> 1, 
         Derivative[2][First[newCoordinates]][First[newCoordinates]] -> 0}, #1 =!= True & ]; 
     NDSolve[Join[equations, Select[(newCoordinates[[#1]][-initialParameter] == initialVector[[#1]] & ) /@ 
           Range[Length[newMatrixRepresentation]] /. {First[newCoordinates][-initialParameter] -> 0}, 
         #1 =!= True && #1 =!= False & ], Select[(Derivative[1][newCoordinates[[#1]]][-initialParameter] == 
             initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]] /. 
          {Derivative[1][First[newCoordinates]][-initialParameter] -> 0}, #1 =!= True && #1 =!= False & ]], 
       Select[(#1[First[newCoordinates]] & ) /@ newCoordinates /. {First[newCoordinates][First[newCoordinates]] -> True}, 
        #1 =!= True & ], {First[newCoordinates], -initialParameter, -finalParameter}] /. 
      ((ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[initialVector] == Length[matrixRepresentation] && Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["PolarPlot"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, equations, solutions}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[parameter] & ) /@ newCoordinates, 
       {parameter, initialParameter, finalParameter}]; ParametricPlot[
      {Last[First[solutions][[2]]]*Cos[Last[First[solutions][[3]]]], Last[First[solutions][[2]]]*
        Sin[Last[First[solutions][[3]]]]}, {parameter, initialParameter, finalParameter}]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[initialVector] == Length[matrixRepresentation] && Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}][
   {"PolarPlot", radialCoordinate_, thetaCoordinate_}] := 
  Module[{newMatrixRepresentation, newCoordinates, newRadialCoordinate, newThetaCoordinate, christoffelSymbols, 
     equations, solutions}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newRadialCoordinate = radialCoordinate /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newThetaCoordinate = thetaCoordinate /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[parameter] & ) /@ newCoordinates, 
       {parameter, initialParameter, finalParameter}]; ParametricPlot[
      {newRadialCoordinate[parameter]*Cos[newThetaCoordinate[parameter]], newRadialCoordinate[parameter]*
         Sin[newThetaCoordinate[parameter]]} /. solutions, {parameter, initialParameter, finalParameter}]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[initialVector] == Length[matrixRepresentation] && Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["PolarPlot3D"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, equations, solutions}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[parameter] & ) /@ newCoordinates, 
       {parameter, initialParameter, finalParameter}]; ParametricPlot3D[
      {Last[First[solutions][[2]]]*Sin[Last[First[solutions][[3]]]]*Cos[Last[First[solutions][[4]]]], 
       Last[First[solutions][[2]]]*Sin[Last[First[solutions][[3]]]]*Sin[Last[First[solutions][[4]]]], 
       Last[First[solutions][[2]]]*Cos[Last[First[solutions][[3]]]]}, {parameter, initialParameter, finalParameter}]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[initialVector] == Length[matrixRepresentation] && Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["CartesianPlot"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, equations, solutions}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[parameter] & ) /@ newCoordinates, 
       {parameter, initialParameter, finalParameter}]; ParametricPlot[{Last[First[solutions][[2]]], 
       Last[First[solutions][[3]]]}, {parameter, initialParameter, finalParameter}]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[initialVector] == Length[matrixRepresentation] && Length[initialVelocity] == Length[matrixRepresentation]
SpacetimeGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], initialVector_List, 
    initialVelocity_List, {parameter_, initialParameter_, finalParameter_}]["CartesianPlot3D"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, equations, solutions}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][parameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[parameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  parameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][parameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[parameter] & ) /@ newCoordinates, 
       {parameter, initialParameter, finalParameter}]; ParametricPlot3D[{Last[First[solutions][[2]]], 
       Last[First[solutions][[3]]], Last[First[solutions][[4]]]}, {parameter, initialParameter, finalParameter}]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[initialVector] == Length[matrixRepresentation] && Length[initialVelocity] == Length[matrixRepresentation]
