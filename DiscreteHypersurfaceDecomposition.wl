(* ::Package:: *)

DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    {coordinates[[1]], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, 100, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   vertexCount_Integer] := DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, 
     index2], {coordinates[[1]], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   vertexCount_Integer, discretizationScale_] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    {coordinates[[1]], 0, 1}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, discretizationScale] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   {timeCoordinate_, initialTime_, finalTime_}] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    {timeCoordinate, initialTime, finalTime}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, 100, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   {timeCoordinate_, initialTime_, finalTime_}, vertexCount_Integer] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    {timeCoordinate, initialTime, finalTime}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   {timeCoordinate_, initialTime_, finalTime_}, vertexCount_Integer, discretizationScale_] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    {timeCoordinate, initialTime, finalTime}, {coordinates[[2]], -2, 2}, {coordinates[[3]], -2, 2}, vertexCount, 
    discretizationScale] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
   {coordinate2_, initialCoordinate2_, finalCoordinate2_}] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    {timeCoordinate, initialTime, finalTime}, {coordinate1, initialCoordinate1, finalCoordinate1}, 
    {coordinate2, initialCoordinate2, finalCoordinate2}, 100, 1] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
   {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
   {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation, coordinates, index1, index2], 
    {timeCoordinate, initialTime, finalTime}, {coordinate1, initialCoordinate1, finalCoordinate1}, 
    {coordinate2, initialCoordinate2, finalCoordinate2}, vertexCount, 1] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedPolarGraphColored"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newCoordinate1, newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, region, points, curvatureRange, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
             newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
       {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}}]; 
     points = RandomPoint[region, vertexCount]; curvatureRange = Max[Last /@ points] - Min[Last /@ points]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
          First[#1] =!= Last[#1] & ], {1}]; SimpleGraph[Graph[points, edgeList, 
       VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRange)] & ) /@ points, 
       EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRange)] & ) /@ edgeList, 
       VertexCoordinates -> (#1 -> #1 & ) /@ points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "PolarGraphColored"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, region, points, curvatureRange, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
             newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
       {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}}]; 
     points = RandomPoint[region, vertexCount]; curvatureRange = Max[Last /@ points] - Min[Last /@ points]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
          First[#1] =!= Last[#1] & ], {1}]; SimpleGraph[Graph[points, edgeList, 
       VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRange)] & ) /@ points, 
       EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRange)] & ) /@ edgeList]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedPolarGraph"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, region, points, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
             newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
       {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}}]; 
     points = RandomPoint[region, vertexCount]; edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], 
        Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]; 
     SimpleGraph[Graph[points, edgeList, VertexCoordinates -> (#1 -> #1 & ) /@ points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["PolarGraph"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, region, 
     points, edgeList}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; flatteningRules = 
      Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
             newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
       {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
        {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}}]; 
     points = RandomPoint[region, vertexCount]; edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], 
        Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]; 
     SimpleGraph[Graph[points, edgeList]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedPolarGraphEvolutionColored"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, curvatureRanges, edgeLists}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/10]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ 
       points; edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]], VertexCoordinates -> (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ 
      Range[Length[points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"CoordinatizedPolarGraphEvolutionColored", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, curvatureRanges, edgeLists}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/stepCount]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ 
       points; edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]], VertexCoordinates -> (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ 
      Range[Length[points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "PolarGraphEvolutionColored"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, regions, points, curvatureRanges, edgeLists}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/10]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ 
       points; edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"PolarGraphEvolutionColored", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, curvatureRanges, edgeLists}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/stepCount]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ 
       points; edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedPolarGraphEvolution"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newCoordinate1, newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, regions, points, edgeLists}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/10]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], VertexCoordinates -> 
           (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"CoordinatizedPolarGraphEvolution", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, edgeLists}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; flatteningRules = 
      Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/stepCount]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], VertexCoordinates -> 
           (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "PolarGraphEvolution"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, regions, points, edgeLists}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/10]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"PolarGraphEvolution", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, edgeLists}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; flatteningRules = 
      Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> Sqrt[\[FormalX]^2 + \[FormalY]^2], 
               newCoordinate2 -> ArcTan[\[FormalY]/\[FormalX]]} /. newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], 
         {{-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1]]}, 
          {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1]], Max[Abs[initialCoordinate1], 
            Abs[finalCoordinate1]]}}] & ) /@ Range[initialTime, finalTime, (finalTime - initialTime)/stepCount]; 
     points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedCartesianGraphColored"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, region, 
     points, curvatureRange, edgeList}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
           newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
        {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
           Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
          Abs[initialCoordinate2], Abs[finalCoordinate2]]}}]; points = RandomPoint[region, vertexCount]; 
     curvatureRange = Max[Last /@ points] - Min[Last /@ points]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
          First[#1] =!= Last[#1] & ], {1}]; SimpleGraph[Graph[points, edgeList, 
       VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRange)] & ) /@ points, 
       EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRange)] & ) /@ edgeList, 
       VertexCoordinates -> (#1 -> #1 & ) /@ points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CartesianGraphColored"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, region, points, curvatureRange, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
           newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
        {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
           Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
          Abs[initialCoordinate2], Abs[finalCoordinate2]]}}]; points = RandomPoint[region, vertexCount]; 
     curvatureRange = Max[Last /@ points] - Min[Last /@ points]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
          First[#1] =!= Last[#1] & ], {1}]; SimpleGraph[Graph[points, edgeList, 
       VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRange)] & ) /@ points, 
       EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRange)] & ) /@ edgeList]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedCartesianGraph"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, region, points, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
           newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
        {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
           Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
          Abs[initialCoordinate2], Abs[finalCoordinate2]]}}]; points = RandomPoint[region, vertexCount]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
          First[#1] =!= Last[#1] & ], {1}]; SimpleGraph[Graph[points, edgeList, 
       VertexCoordinates -> (#1 -> #1 & ) /@ points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CartesianGraph"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, region, points, edgeList}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     region = DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
           newTimeCoordinate -> finalTime) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
        {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
           Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
          Abs[initialCoordinate2], Abs[finalCoordinate2]]}}]; points = RandomPoint[region, vertexCount]; 
     edgeList = Apply[UndirectedEdge, Select[Tuples[points, 2], Norm[First[#1] - Last[#1]] < discretizationScale && 
          First[#1] =!= Last[#1] & ], {1}]; SimpleGraph[Graph[points, edgeList]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedCartesianGraphEvolutionColored"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, curvatureRanges, edgeLists}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/10]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ points; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]], VertexCoordinates -> (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ 
      Range[Length[points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"CoordinatizedCartesianGraphEvolutionColored", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, curvatureRanges, edgeLists}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/stepCount]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ points; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]], VertexCoordinates -> (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ 
      Range[Length[points]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CartesianGraphEvolutionColored"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, 
     newCoordinate1, newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, regions, points, curvatureRanges, edgeLists}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/10]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ points; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"CartesianGraphEvolutionColored", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, curvatureRanges, edgeLists}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/stepCount]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     curvatureRanges = (Max[Last /@ #1] - Min[Last /@ #1] & ) /@ points; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], 
          VertexStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[#1]/curvatureRanges[[index]])] & ) /@ 
            points[[index]], EdgeStyle -> (#1 -> ColorData["LightTemperatureMap", -(Last[Last[#1]]/curvatureRanges[[
                  index]])] & ) /@ edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinatizedCartesianGraphEvolution"] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, edgeLists}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; flatteningRules = 
      Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/10]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], VertexCoordinates -> 
           (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"CoordinatizedCartesianGraphEvolution", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, edgeLists}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; flatteningRules = 
      Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/stepCount]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]], VertexCoordinates -> 
           (#1 -> #1 & ) /@ points[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CartesianGraphEvolution"] := Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, 
     newCoordinate2, flatteningRules, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar, regions, points, edgeLists}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     flatteningRules = Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/10]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   {"CartesianGraphEvolution", stepCount_Integer}] := 
  Module[{newMatrixRepresentation, newCoordinates, newTimeCoordinate, newCoordinate1, newCoordinate2, flatteningRules, 
     christoffelSymbols, riemannTensor, covariantRiemannTensor, contravariantRiemannTensor, kretschmannScalar, regions, 
     points, edgeLists}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newTimeCoordinate = timeCoordinate /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate1 = coordinate1 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinate2 = coordinate2 /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; flatteningRules = 
      Thread[Complement[newCoordinates, {newTimeCoordinate, newCoordinate1, newCoordinate2}] -> 0]; 
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
     regions = (DiscretizeRegion[ImplicitRegion[-(kretschmannScalar /. {newCoordinate1 -> \[FormalX], newCoordinate2 -> \[FormalY]} /. 
             newTimeCoordinate -> #1) == \[FormalL], {\[FormalX], \[FormalY], \[FormalL]}], {{initialCoordinate1, finalCoordinate1}, 
          {initialCoordinate2, finalCoordinate2}, {-Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
             Abs[initialCoordinate2], Abs[finalCoordinate2]], Max[Abs[initialCoordinate1], Abs[finalCoordinate1], 
            Abs[initialCoordinate2], Abs[finalCoordinate2]]}}] & ) /@ Range[initialTime, finalTime, 
        (finalTime - initialTime)/stepCount]; points = (RandomPoint[#1, vertexCount] & ) /@ regions; 
     edgeLists = (Module[{point = #1}, Apply[UndirectedEdge, Select[Tuples[point, 2], 
           Norm[First[#1] - Last[#1]] < discretizationScale && First[#1] =!= Last[#1] & ], {1}]] & ) /@ points; 
     (Module[{index = #1}, SimpleGraph[Graph[points[[index]], edgeLists[[index]]]]] & ) /@ Range[Length[points]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["MetricTensor"] := 
  MetricTensor[matrixRepresentation, coordinates, index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["Coordinates"] := 
  coordinates /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "TimeCoordinate"] := timeCoordinate /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["TimeInterval"] := 
  {initialTime, finalTime} /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "SpatialCoordinates"] := {coordinate1, coordinate2} /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "SpatialRegion"] := {{initialCoordinate1, finalCoordinate1}, {initialCoordinate2, finalCoordinate2}} /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["VertexCount"] := 
  vertexCount /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "DiscretizationScale"] := discretizationScale /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCooerdinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["Dimensions"] := 
  Length[matrixRepresentation] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["Signature"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["RiemannianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], True, False], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "PseudoRiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], False, True], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["LorentzianQ"] := 
  Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "RiemannianConditions"] := Module[{eigenvalues, riemannianConditions}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; 
     If[riemannianConditions === True, {}, If[riemannianConditions === False, Indeterminate, 
       If[Length[Select[riemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "PseudoRiemannianConditions"] := Module[{eigenvalues, pseudoRiemannianConditions}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; pseudoRiemannianConditions = 
      FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_][
   "LorentzianConditions"] := Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinateSymbol, 
     lorentzianConditions}, eigensystem = Eigensystem[matrixRepresentation]; eigenvalues = First[eigensystem]; 
     eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinateSymbol = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinateSymbol, eigenvalues[[#1]] < 0, 
            eigenvalues[[#1]] > 0] & ) /@ Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, 
        If[lorentzianConditions === False, Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, 
          Indeterminate, DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
    {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
    {coordinate2_, initialCoordinate2_, finalCoordinate2_}, vertexCount_Integer, discretizationScale_]["Properties"] := 
  {"CoordinatizedPolarGraphColored", "PolarGraphColored", "CoordinatizedPolarGraph", "PolarGraph", 
    "CoordinatizedPolarGraphEvolutionColored", "PolarGraphEvolutionColored", "CoordinatizedPolarGraphEvolution", 
    "PolarGraphEvolution", "CoordinatizedCartesianGraphColored", "CartesianGraphColored", "CoordinatizedCartesianGraph", 
    "CartesianGraph", "CoordinatizedCartesianGraphEvolutionColored", "CartesianGraphEvolutionColored", 
    "CoordinatizedCartesianGraphEvolution", "CartesianGraphEvolution", "MetricTensor", "Coordinates", 
    "CoordinateOneForms", "TimeCoordinate", "TimeInterval", "SpatialCoordinates", "SpatialRegion", "VertexCount", 
    "DiscretizationScale", "Dimensions", "Signature", "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", 
    "RiemannianConditions", "PseudoRiemannianConditions", "LorentzianConditions", "Properties"} /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition[DiscreteHypersurfaceDecomposition[(metricTensor_)[matrixRepresentation_List, 
     coordinates_List, index1_, index2_], {timeCoordinate_, initialTime_, finalTime_}, 
    {coordinate1_, initialCoordinate1_, finalCoordinate1_}, {coordinate2_, initialCoordinate2_, finalCoordinate2_}, 
    vertexCount_Integer, discretizationScale_], newCoordinates_List] := 
  DiscreteHypersurfaceDecomposition[MetricTensor[matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates], {timeCoordinate /. Thread[coordinates -> newCoordinates], initialTime, finalTime}, 
    {coordinate1 /. Thread[coordinates -> newCoordinates], initialCoordinate1, finalCoordinate1}, 
    {coordinate2 /. Thread[coordinates -> newCoordinates], initialCoordinate2, finalCoordinate2}, vertexCount, 
    discretizationScale] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
DiscreteHypersurfaceDecomposition /: 
  MakeBoxes[discreteHypersurfaceDecomposition:DiscreteHypersurfaceDecomposition[
      (metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], 
      {timeCoordinate_, initialTime_, finalTime_}, {coordinate1_, initialCoordinate1_, finalCoordinate1_}, 
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
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["DiscreteHypersurfaceDecomposition", 
       discreteHypersurfaceDecomposition, icon, {{BoxForm`SummaryItem[{"Time Coordinate: ", timeCoordinate}], 
         BoxForm`SummaryItem[{"Time Interval: ", {initialTime, finalTime}}]}, 
        {BoxForm`SummaryItem[{"Vertices: ", vertexCount}], BoxForm`SummaryItem[{"Discretization Scale: ", 
           discretizationScale}]}}, {{BoxForm`SummaryItem[{"Spatial Coordinates: ", {coordinate1, coordinate2}}], 
         BoxForm`SummaryItem[{"Spatial Region: ", {{initialCoordinate1, finalCoordinate1}, 
            {initialCoordinate2, finalCoordinate2}}}]}, {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], 
         BoxForm`SummaryItem[{"Signature: ", signature}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
