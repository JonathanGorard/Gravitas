(* ::Package:: *)

DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    ConstantArray[0, Length[matrixRepresentation]], Join[{-1}, ConstantArray[0, Length[matrixRepresentation] - 1]], 
    {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, 100, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   vertexCount_Integer] := DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    ConstantArray[0, Length[matrixRepresentation]], Join[{-1}, ConstantArray[0, Length[matrixRepresentation] - 1]], 
    {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   vertexCount_Integer, discretizationScale_] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    ConstantArray[0, Length[matrixRepresentation]], Join[{-1}, ConstantArray[0, Length[matrixRepresentation] - 1]], 
    {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, discretizationScale] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List] := DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    initialVector, Join[{-1}, ConstantArray[0, Length[matrixRepresentation] - 1]], {\[FormalLambda], 0, 1}, 
    {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, 100, 1] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[initialVector]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, vertexCount_Integer] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    Join[{-1}, ConstantArray[0, Length[matrixRepresentation] - 1]], {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, 
    {coordinates[[3]], -2, 2}, vertexCount, 1] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[initialVector]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, vertexCount_Integer, discretizationScale_] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    Join[{-1}, ConstantArray[0, Length[matrixRepresentation] - 1]], {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, 
    {coordinates[[3]], -2, 2}, vertexCount, discretizationScale] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[initialVector]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    initialVelocity, {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, 100, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List, vertexCount_Integer] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    initialVelocity, {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List, vertexCount_Integer, discretizationScale_] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    initialVelocity, {\[FormalLambda], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, 
    discretizationScale] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    initialVelocity, {parameter, initialParameter, finalParameter}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, 
    100, 1] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, vertexCount_Integer] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    initialVelocity, {parameter, initialParameter, finalParameter}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, 
    vertexCount, 1] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, vertexCount_Integer, 
   discretizationScale_] := DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    initialVector, initialVelocity, {parameter, initialParameter, finalParameter}, {coordinates[[2]], -2, 2}, 
    {coordinates[[3]], -2, 2}, vertexCount, discretizationScale] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[initialVector] && 
    Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, 
   {coordinate1_, initialCoordinate1_, finalCoordinate1_}, {coordinate2_, initialCoordinate2_, finalCoordinate2_}] := 
  DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], initialVector, 
    initialVelocity, {parameter, initialParameter, finalParameter}, {coordinate1, initialCoordinate1, finalCoordinate1}, 
    {coordinate2, initialCoordinate2, finalCoordinate2}, 100, 1] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[initialVector] && 
    Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, 
   {coordinate1_, initialCoordinate1_, finalCoordinate1_}, {coordinate2_, initialCoordinate2_, finalCoordinate2_}, 
   vertexCount_Integer] := DiscreteHypersurfaceGeodesic[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    initialVector, initialVelocity, {parameter, initialParameter, finalParameter}, 
    {coordinate1, initialCoordinate1, finalCoordinate1}, {coordinate2, initialCoordinate2, finalCoordinate2}, 
    vertexCount, 1] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, 
    {coordinate1_, initialCoordinate1_, finalCoordinate1_}, {coordinate2_, initialCoordinate2_, finalCoordinate2_}, 
    vertexCount_Integer, discretizationScale_]["CoordinatizedPolarGraphColored"] := 
  Module[{newMatrixRepresentation, newCoordinates, newParameter, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, equations, 
     solutions, geodesicPoints, region, points, curvatureRange, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newParameter = parameter /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newParameter, newCoordinate1, newCoordinate2}] -> 0]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     covariantRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,index[[2]],
                 index[[3]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                 index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[
                Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     kretschmannScalar = FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*
            contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]] /. flatteningRules]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][newParameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[newParameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  newParameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][newParameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[newParameter] & ) /@ newCoordinates, 
       {newParameter, initialParameter, finalParameter}]; geodesicPoints = 
      Take[Select[Catenate[(Module[{parameterValue = #1}, {newCoordinate1[newParameter]*Cos[newCoordinate2[
                   newParameter]] + RandomReal[{-discretizationScale/8, discretizationScale/8}], 
               newCoordinate1[newParameter]*Sin[newCoordinate2[newParameter]] + RandomReal[{-discretizationScale/8, 
                  discretizationScale/8}], -(kretschmannScalar /. (#1 -> #1[newParameter] & ) /@ newCoordinates)} /. 
              solutions /. newParameter -> parameterValue] & ) /@ Range[initialParameter, finalParameter, 
           discretizationScale]], #1[[1]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && 
          #1[[1]] < Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && 
          #1[[2]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && #1[[2]] < Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]] & ], UpTo[vertexCount]]; 
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
             newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newParameter -> finalParameter) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
       {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}}]; 
     points = RandomPoint[region, vertexCount - Length[geodesicPoints]]; 
     curvatureRange = Max[Last /@ points] - Min[Last /@ points]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[Union[points, geodesicPoints], 2], 
        Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]; 
     SimpleGraph[HighlightGraph[Graph[Union[points, geodesicPoints], edgeList], 
       Subgraph[Graph[Union[points, geodesicPoints], edgeList], geodesicPoints]], 
      VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRange)] & ) /@ points, 
      EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRange)] & ) /@ edgeList, 
      VertexCoordinates -> (#1 -> #1 & ) /@ Union[points, geodesicPoints]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, 
    {coordinate1_, initialCoordinate1_, finalCoordinate1_}, {coordinate2_, initialCoordinate2_, finalCoordinate2_}, 
    vertexCount_Integer, discretizationScale_]["PolarGraphColored"] := 
  Module[{newMatrixRepresentation, newCoordinates, newParameter, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, equations, 
     solutions, geodesicPoints, region, points, curvatureRange, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, String]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newParameter = parameter /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newParameter, newCoordinate1, newCoordinate2}] -> 0]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     covariantRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,index[[2]],
                 index[[3]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                 index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[
                Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     kretschmannScalar = FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*
            contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]] /. flatteningRules]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][newParameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[newParameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  newParameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][newParameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[newParameter] & ) /@ newCoordinates, 
       {newParameter, initialParameter, finalParameter}]; geodesicPoints = 
      Take[Select[Catenate[(Module[{parameterValue = #1}, {newCoordinate1[newParameter]*Cos[newCoordinate2[
                   newParameter]] + RandomReal[{-discretizationScale/8, discretizationScale/8}], 
               newCoordinate1[newParameter]*Sin[newCoordinate2[newParameter]] + RandomReal[{-discretizationScale/8, 
                  discretizationScale/8}], -(kretschmannScalar /. (#1 -> #1[newParameter] & ) /@ newCoordinates)} /. 
              solutions /. newParameter -> parameterValue] & ) /@ Range[initialParameter, finalParameter, 
           discretizationScale]], #1[[1]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && 
          #1[[1]] < Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && 
          #1[[2]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && #1[[2]] < Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]] & ], UpTo[vertexCount]]; 
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
             newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newParameter -> finalParameter) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
       {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}}]; 
     points = RandomPoint[region, vertexCount - Length[geodesicPoints]]; 
     curvatureRange = Max[Last /@ points] - Min[Last /@ points]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[Union[points, geodesicPoints], 2], 
        Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]; 
     SimpleGraph[HighlightGraph[Graph[Union[points, geodesicPoints], edgeList], 
       Subgraph[Graph[Union[points, geodesicPoints], edgeList], geodesicPoints]], 
      VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRange)] & ) /@ points, 
      EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRange)] & ) /@ edgeList]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
    Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, 
    {coordinate1_, initialCoordinate1_, finalCoordinate1_}, {coordinate2_, initialCoordinate2_, finalCoordinate2_}, 
    vertexCount_Integer, discretizationScale_]["CoordinatizedPolarGraphEvolutionColored"] := 
  Module[{newMatrixRepresentation, newCoordinates, newParameter, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, equations, 
     solutions, geodesicPoints, regions, points, curvatureRanges, edgeLists}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newParameter = parameter /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newParameter, newCoordinate1, newCoordinate2}] -> 0]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     covariantRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,index[[2]],
                 index[[3]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                 index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[
                Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     kretschmannScalar = FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*
            contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]] /. flatteningRules]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][newParameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[newParameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  newParameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][newParameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[newParameter] & ) /@ newCoordinates, 
       {newParameter, initialParameter, finalParameter}]; geodesicPoints = 
      (Module[{finalParameterValue = #1}, Take[Select[Catenate[(Module[{parameterValue = #1}, 
               {newCoordinate1[newParameter]*Cos[newCoordinate2[newParameter]] + RandomReal[{-discretizationScale/8, 
                     discretizationScale/8}], newCoordinate1[newParameter]*Sin[newCoordinate2[newParameter]] + 
                   RandomReal[{-discretizationScale/8, discretizationScale/8}], -(kretschmannScalar /. 
                    (#1 -> #1[newParameter] & ) /@ newCoordinates)} /. solutions /. newParameter -> 
                 parameterValue] & ) /@ Range[initialParameter, finalParameterValue, discretizationScale]], 
           #1[[1]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && #1[[1]] < Max[Abs[initialCoordinate1], Abs[
                finalCoordinate1]] && #1[[2]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && 
             #1[[2]] < Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] & ], UpTo[vertexCount]]] & ) /@ 
       Range[initialParameter, finalParameter, (finalParameter - initialParameter)/10]; 
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newParameter -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialParameter, finalParameter, (finalParameter - initialParameter)/
         10]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ points; 
     edgeLists = (Module[{index = #1}, Apply[UndirectedEdge, Select[Tuples[Union[points[[index]], 
             geodesicPoints[[index]]], 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
             First[#1] =!= Last[#1] & ], {1}]] & ) /@ Range[Length[points]]; 
     (Module[{index = #1}, SimpleGraph[HighlightGraph[Graph[Union[points[[index]], geodesicPoints[[index]]], 
           edgeLists[[index]]], Subgraph[Graph[Union[points[[index]], geodesicPoints[[index]]], edgeLists[[index]]], 
           geodesicPoints[[index]]]], VertexStyle -> (#1 -> ColorData["LightTemperatureMap", 
              -(Last[#1]/curvatureRanges[[index]])] & ) /@ points[[index]], 
         EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[index]])] & ) /@ 
           edgeLists[[index]], VertexCoordinates -> (#1 -> #1 & ) /@ Union[points[[index]], 
            geodesicPoints[[index]]]]] & ) /@ Range[Length[points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[initialVector] && 
    Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    initialVector_List, initialVelocity_List, {parameter_, initialParameter_, finalParameter_}, 
    {coordinate1_, initialCoordinate1_, finalCoordinate1_}, {coordinate2_, initialCoordinate2_, finalCoordinate2_}, 
    vertexCount_Integer, discretizationScale_]["PolarGraphEvolutionColored"] := 
  Module[{newMatrixRepresentation, newCoordinates, newParameter, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, equations, 
     solutions, geodesicPoints, regions, points, curvatureRanges, edgeLists}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newParameter = parameter /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. 
       (#1 -> (ToExpression[#1] /. {\[FormalTheta] -> \[Theta], \[FormalPhi] -> \[Phi]}) & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newParameter, newCoordinate1, newCoordinate2}] -> 0]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[
                4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
              newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                  #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     covariantRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,index[[2]],
                 index[[3]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                 index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[
                Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     kretschmannScalar = FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*
            contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]] /. flatteningRules]; 
     equations = Thread[(Module[{index = #1}, Derivative[2][newCoordinates[[index]]][newParameter] + 
            Total[(Module[{nestedIndex = #1}, (christoffelSymbols[[index,First[nestedIndex],Last[nestedIndex]]] /. 
                  (#1 -> #1[newParameter] & ) /@ newCoordinates)*Derivative[1][newCoordinates[[First[nestedIndex]]]][
                  newParameter]*Derivative[1][newCoordinates[[Last[nestedIndex]]]][newParameter]] & ) /@ 
              Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Range[Length[newMatrixRepresentation]] == 0]; 
     solutions = NDSolve[Join[equations, (newCoordinates[[#1]][initialParameter] == initialVector[[#1]] & ) /@ 
         Range[Length[newMatrixRepresentation]], (Derivative[1][newCoordinates[[#1]]][initialParameter] == 
           initialVelocity[[#1]] & ) /@ Range[Length[newMatrixRepresentation]]], (#1[newParameter] & ) /@ newCoordinates, 
       {newParameter, initialParameter, finalParameter}]; geodesicPoints = 
      (Module[{finalParameterValue = #1}, Take[Select[Catenate[(Module[{parameterValue = #1}, 
               {newCoordinate1[newParameter]*Cos[newCoordinate2[newParameter]] + RandomReal[{-discretizationScale/8, 
                     discretizationScale/8}], newCoordinate1[newParameter]*Sin[newCoordinate2[newParameter]] + 
                   RandomReal[{-discretizationScale/8, discretizationScale/8}], -(kretschmannScalar /. 
                    (#1 -> #1[newParameter] & ) /@ newCoordinates)} /. solutions /. newParameter -> 
                 parameterValue] & ) /@ Range[initialParameter, finalParameterValue, discretizationScale]], 
           #1[[1]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && #1[[1]] < Max[Abs[initialCoordinate1], Abs[
                finalCoordinate1]] && #1[[2]] > -Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] && 
             #1[[2]] < Max[Abs[initialCoordinate1], Abs[finalCoordinate1]] & ], UpTo[vertexCount]]] & ) /@ 
       Range[initialParameter, finalParameter, (finalParameter - initialParameter)/10]; 
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newParameter -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialParameter, finalParameter, (finalParameter - initialParameter)/
         10]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ points; 
     edgeLists = (Module[{index = #1}, Apply[UndirectedEdge, Select[Tuples[Union[points[[index]], 
             geodesicPoints[[index]]], 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
             First[#1] =!= Last[#1] & ], {1}]] & ) /@ Range[Length[points]]; 
     (Module[{index = #1}, SimpleGraph[HighlightGraph[Graph[Union[points[[index]], geodesicPoints[[index]]], 
           edgeLists[[index]]], Subgraph[Graph[Union[points[[index]], geodesicPoints[[index]]], edgeLists[[index]]], 
           geodesicPoints[[index]]]], VertexStyle -> (#1 -> ColorData["LightTemperatureMap", 
              -(Last[#1]/curvatureRanges[[index]])] & ) /@ points[[index]], 
         EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[index]])] & ) /@ 
           edgeLists[[index]]]] & ) /@ Range[Length[points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2] && Length[coordinates] == Length[initialVector] && 
    Length[coordinates] == Length[initialVelocity]
DiscreteHypersurfaceGeodesic /: 
  MakeBoxes[discreteHypersurfaceGeodesic:DiscreteHypersurfaceGeodesic[(metricTensor_)[matrixRepresentation_List, 
       coordinates_List, index1_, index2_], initialVector_List, initialVelocity_List, 
      {parameter_, initialParameter_, finalParameter_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
      {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_], format_] := 
   Module[{dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, signature, icon}, 
     dimensions = Length[matrixRepresentation]; eigenvalues = Eigenvalues[matrixRepresentation]; 
      positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
          Length[matrixRepresentation], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[matrixRepresentation, 
        ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], 
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["DiscreteHypersurfaceGeodesic", 
       discreteHypersurfaceGeodesic, icon, {{BoxForm`SummaryItem[{"Parameter: ", parameter}], 
         BoxForm`SummaryItem[{"Parameter Interval: ", {initialParameter, finalParameter}}]}, 
        {BoxForm`SummaryItem[{"Vertices: ", vertexCount}], BoxForm`SummaryItem[{"Discretization Scale: ", 
           discretizationScale}]}}, {{BoxForm`SummaryItem[{"Spatial Coordinates: ", {coordinate1, coordinate2}}], 
         BoxForm`SummaryItem[{"Spatial Region: ", {{initialCoordinate1, finalCoordinate1}, 
            {initialCoordinate2, finalCoordinate2}}}]}, {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], 
         BoxForm`SummaryItem[{"Signature: ", signature}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2] && 
     Length[coordinates] == Length[initialVector] && Length[coordinates] == Length[initialVelocity]
