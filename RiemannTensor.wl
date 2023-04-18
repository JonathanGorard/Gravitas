(* ::Package:: *)

RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], False, True, True, 
    True] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], newCoordinates_List] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, index1, index2], False, True, True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   newCoordinates_List, index1_, index2_, index3_, index4_] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2, index3, index4] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["TensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     If[index1 === True && index2 === True && index3 === True && index4 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === False && index3 === False && 
        index4 === False, Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                 riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
       If[index1 === True && index2 === False && index3 === False && index4 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                  Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],
                   #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && 
          index3 === False && index4 === False, Normal[SparseArray[
           (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*
                   Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],index[[2]],#1[[1]],
                    #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === False && 
           index3 === True && index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                     index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],#1[[2]]]] & ) /@ 
                  Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
              4]]], If[index1 === False && index2 === False && index3 === False && index4 === True, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                      index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
             index3 === False && index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> 
                  Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*
                      Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],
                       #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && index3 === True && 
              index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                        #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                        #1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 4]]], If[index1 === True && index2 === False && index3 === False && index4 === True, 
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                        Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                         index[[3]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
              If[index1 === False && index2 === True && index3 === True && index4 === False, Normal[
                SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[4]]]]*
                         riemannTensor[[index[[1]],index[[2]],index[[3]],#1]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], If[
                index1 === False && index2 === True && index3 === False && index4 === True, 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*
                          riemannTensor[[index[[1]],index[[2]],#1,index[[4]]]] & ) /@ Range[Length[
                          matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
                If[index1 === False && index2 === False && index3 === True && index4 === True, 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                           riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                           matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
                 If[index1 === True && index2 === True && index3 === True && index4 === False, Normal[SparseArray[
                    (Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                            Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],
                             index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
                    index3 === False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                         Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                              index[[3]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[
                            Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                       4]]], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                    Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                              Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],
                               index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && 
                      index3 === True && index4 === True, riemannTensor, Indeterminate]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ReducedTensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     If[index1 === True && index2 === True && index3 === True && index4 === True, 
      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*
                 riemannTensor[[#1,index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === False && index2 === False && 
        index3 === False && index4 === False, FullSimplify[
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                  riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
       If[index1 === True && index2 === False && index3 === False && index4 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                   Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[3]]]]*Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],
                    #1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === False && index2 === True && 
          index3 === False && index4 === False, FullSimplify[
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*
                    Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],index[[2]],#1[[1]],
                     #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === False && index2 === False && 
           index3 === True && index4 === False, FullSimplify[Normal[SparseArray[
             (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],
                      #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === False && index2 === False && 
            index3 === False && index4 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                 index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[
                       #1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                4]]]], If[index1 === True && index2 === True && index3 === False && index4 === False, 
            FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                        #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[
                        #1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 4]]]], If[index1 === True && index2 === False && index3 === True && index4 === False, 
             FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                         #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                         #1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ 
                      Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                  4]]]], If[index1 === True && index2 === False && index3 === False && index4 === True, 
              FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                          #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                          #1[[3]],index[[3]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[
                     matrixRepresentation]], 4]]]], If[index1 === False && index2 === True && index3 === True && 
                index4 === False, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[
                       (Inverse[matrixRepresentation][[#1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],index[[3]],
                           #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                      matrixRepresentation]], 4]]]], If[index1 === False && index2 === True && index3 === False && 
                 index4 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[
                        (Inverse[matrixRepresentation][[#1,index[[3]]]]*riemannTensor[[index[[1]],index[[3]],#1,
                            index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 4]]]], If[index1 === False && index2 === False && 
                  index3 === True && index4 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                       index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*riemannTensor[[index[[1]],#1,
                             index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === True && index2 === True && 
                   index3 === True && index4 === False, FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                        index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                              index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ Tuples[
                            Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                       4]]]], If[index1 === True && index2 === True && index3 === False && index4 === True, 
                   FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                               #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[#1[[1]],
                               index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                             2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === True && 
                     index2 === False && index3 === True && index4 === True, FullSimplify[Normal[SparseArray[
                       (Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                               Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],
                                index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                        Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === False && index2 === True && 
                      index3 === True && index4 === True, FullSimplify[riemannTensor], Indeterminate]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["SymbolicTensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[index1 === True && index2 === True && index3 === True && index4 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === False && index3 === False && 
        index4 === False, Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                 riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
       If[index1 === True && index2 === False && index3 === False && index4 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                  Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],
                   #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && 
          index3 === False && index4 === False, Normal[SparseArray[
           (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*
                   Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],index[[2]],#1[[1]],
                    #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === False && 
           index3 === True && index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                     index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],#1[[2]]]] & ) /@ 
                  Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
              4]]], If[index1 === False && index2 === False && index3 === False && index4 === True, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                      index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
             index3 === False && index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> 
                  Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*
                      Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],
                       #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && index3 === True && 
              index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                        #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                        #1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 4]]], If[index1 === True && index2 === False && index3 === False && index4 === True, 
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                        Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                         index[[3]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
              If[index1 === False && index2 === True && index3 === True && index4 === False, Normal[
                SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[4]]]]*
                         riemannTensor[[index[[1]],index[[2]],index[[3]],#1]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], If[
                index1 === False && index2 === True && index3 === False && index4 === True, 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*
                          riemannTensor[[index[[1]],index[[2]],#1,index[[4]]]] & ) /@ Range[Length[
                          matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
                If[index1 === False && index2 === False && index3 === True && index4 === True, 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                           riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                           matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
                 If[index1 === True && index2 === True && index3 === True && index4 === False, Normal[SparseArray[
                    (Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                            Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],
                             index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
                    index3 === False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                         Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                              index[[3]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[
                            Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                       4]]], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                    Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                              Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],
                               index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && 
                      index3 === True && index4 === True, riemannTensor, Indeterminate]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["KretschmannScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],
          #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ReducedKretschmannScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantRiemannTensor[[#1[[1]],
           #1[[2]],#1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["SymbolicKretschmannScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; covariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],
          #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ChernPontryaginScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     mixedRiemannTensor, leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; mixedRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                 riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],
           #1[[5]],#1[[6]]]]*mixedRiemannTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 6]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ReducedChernPontryaginScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     mixedRiemannTensor, leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; mixedRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                 riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[
            #1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*mixedRiemannTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 6]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["SymbolicChernPontryaginScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     mixedRiemannTensor, leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                  Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                  Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
      riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                 index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                 index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                    index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[
                   newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; mixedRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                 riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],
           #1[[5]],#1[[6]]]]*mixedRiemannTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 6]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["EulerScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*covariantRiemannTensor[[#1[[5]],#1[[6]],#1[[7]],
           #1[[8]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*contravariantLeviCivitaTensor[[
           #1[[3]],#1[[4]],#1[[7]],#1[[8]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 8]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ReducedEulerScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*covariantRiemannTensor[[#1[[5]],
            #1[[6]],#1[[7]],#1[[8]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*
           contravariantLeviCivitaTensor[[#1[[3]],#1[[4]],#1[[7]],#1[[8]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 8]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["SymbolicEulerScalar"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                  Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                  Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ Range[
                Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
      riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                 index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                 index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                    index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[
                   newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*covariantRiemannTensor[[#1[[5]],#1[[6]],#1[[7]],
           #1[[8]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*contravariantLeviCivitaTensor[[
           #1[[3]],#1[[4]],#1[[7]],#1[[8]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 8]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["MetricTensor"] := ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
    metricIndex1, metricIndex2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["Coordinates"] := coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["Indices"] := {index1, index2, index3, index4} /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["CovariantQ"] := 
  If[index1 === True && index2 === True && index3 === True && index4 === True, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ContravariantQ"] := 
  If[index1 === False && index2 === False && index3 === False && index4 === False, True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["MixedQ"] := 
  If[ !((index1 === True && index2 === True && index3 === True && index4 === True) || 
      (index1 === False && index2 === False && index3 === False && index4 === False)), True, False] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["Symbol"] := If[index1 === True && index2 === True && index3 === True && index4 === True, 
    Subscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && index3 === False && index4 === False, 
     Superscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && index3 === False && index4 === False, 
      Subsuperscript["\[FormalCapitalR]", "\[FormalRho]", "\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === True && index3 === False && 
        index4 === False, Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && 
         index3 === True && index4 === False, Subsuperscript["\[FormalCapitalR]", "\[FormalMu]", "\[FormalRho]\[FormalSigma]\[FormalNu]"], 
        If[index1 === False && index2 === False && index3 === False && index4 === True, 
         Subsuperscript["\[FormalCapitalR]", "\[FormalNu]", "\[FormalRho]\[FormalSigma]\[FormalMu]"], If[index1 === True && index2 === True && index3 === False && 
           index4 === False, Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && 
            index3 === True && index4 === False, Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalMu]", "\[FormalSigma]\[FormalNu]"], 
           If[index1 === True && index2 === False && index3 === False && index4 === True, Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalNu]", 
             "\[FormalSigma]\[FormalMu]"], If[index1 === False && index2 === True && index3 === True && index4 === False, 
             Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalMu]", "\[FormalRho]\[FormalNu]"], If[index1 === False && index2 === True && index3 === False && 
               index4 === True, Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalNu]", "\[FormalRho]\[FormalMu]"], If[index1 === False && index2 === False && 
                index3 === True && index4 === True, Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalNu]", "\[FormalRho]\[FormalSigma]"], If[index1 === True && 
                 index2 === True && index3 === True && index4 === False, Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalMu]", "\[FormalNu]"], 
                If[index1 === True && index2 === True && index3 === False && index4 === True, Subsuperscript["\[FormalCapitalR]", 
                  "\[FormalRho]\[FormalSigma]\[FormalNu]", "\[FormalMu]"], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                  Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalMu]\[FormalNu]", "\[FormalSigma]"], If[index1 === False && index2 === True && index3 === True && 
                    index4 === True, Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalMu]\[FormalNu]", "\[FormalRho]"], Indeterminate]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["RiemannFlatQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, fieldEquations}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     fieldEquations = FullSimplify[Thread[Catenate[Catenate[Catenate[riemannTensor]]] == 
         Catenate[Catenate[Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation], 
              Length[matrixRepresentation], Length[matrixRepresentation]}]]]]]]; 
     If[fieldEquations === True, True, If[fieldEquations === False, False, 
       If[Length[Select[fieldEquations, #1 === True & ]] == Length[matrixRepresentation]*Length[matrixRepresentation]*
          Length[matrixRepresentation]*Length[matrixRepresentation], True, False]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["VanishingKretschmannScalarQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantRiemannTensor[[#1[[1]],
             #1[[2]],#1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]] == 0] === True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["VanishingChernPontryaginScalarQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     mixedRiemannTensor, leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; mixedRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                 riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[
              #1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*mixedRiemannTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 6]] == 0] === True, Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["VanishingEulerScalarQ"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     leviCivitaTensor, contravariantLeviCivitaTensor}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*covariantRiemannTensor[[#1[[5]],
              #1[[6]],#1[[7]],#1[[8]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*
             contravariantLeviCivitaTensor[[#1[[3]],#1[[4]],#1[[7]],#1[[8]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 8]] == 0] === True, Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["RiemannFlatConditions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, fieldEquations}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     fieldEquations = FullSimplify[Thread[Catenate[Catenate[Catenate[riemannTensor]]] == 
         Catenate[Catenate[Catenate[ConstantArray[0, {Length[matrixRepresentation], Length[matrixRepresentation], 
              Length[matrixRepresentation], Length[matrixRepresentation]}]]]]]]; 
     If[fieldEquations === True, {}, If[fieldEquations === False, Indeterminate, 
       If[Length[Select[fieldEquations, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[fieldEquations, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["VanishingKretschmannScalarCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, fieldEquation}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]]; contravariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     fieldEquation = FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*
            contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]] == 0]; If[fieldEquation === False, Indeterminate, 
      fieldEquation]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["VanishingChernPontryaginScalarCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     mixedRiemannTensor, leviCivitaTensor, contravariantLeviCivitaTensor, fieldEquation}, 
    If[Length[matrixRepresentation] == 4, newMatrixRepresentation = matrixRepresentation /. 
        (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; mixedRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                 riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; fieldEquation = 
       FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[
              #1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*mixedRiemannTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 6]] == 0]; If[fieldEquation === False, Indeterminate, 
       fieldEquation], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["VanishingEulerScalarCondition"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     leviCivitaTensor, contravariantLeviCivitaTensor, fieldEquation}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; covariantRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; fieldEquation = 
       FullSimplify[Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*covariantRiemannTensor[[#1[[5]],
              #1[[6]],#1[[7]],#1[[8]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*
             contravariantLeviCivitaTensor[[#1[[3]],#1[[4]],#1[[7]],#1[[8]]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 8]] == 0]; If[fieldEquation === False, Indeterminate, 
       fieldEquation], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["IndexContractions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, newRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     If[(index1 === True && index2 === True && index3 === True && index4 === True) || 
       (index1 === False && index2 === False && index3 === False && index4 === False), Association[], 
      If[index1 === True && index2 === False && index3 === False && index4 === False, 
       newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                    #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[3]]]]*Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],
                    #1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalMu]\[FormalNu]"] -> 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                    Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalSigma]\[FormalLambda]\[FormalNu]"] -> 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                    Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalSigma]\[FormalMu]\[FormalLambda]"] -> 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                    #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
              2]]]], If[index1 === False && index2 === True && index3 === False && index4 === False, 
        newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                     #1[[1]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],
                     index[[2]],#1[[1]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalMu]\[FormalNu]"] -> 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                     Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalNu]"] -> 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                     Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalMu]\[FormalLambda]"] -> 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                     #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               2]]]], If[index1 === False && index2 === False && index3 === True && index4 === False, 
         newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                      index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],
                      #1[[1]],index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalSigma]\[FormalNu]"] -> 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                      Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalNu]"] -> 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                      Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalSigma]\[FormalLambda]"] -> 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                      #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]]], If[index1 === False && index2 === False && 
           index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                 index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[
                       #1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalSigma]\[FormalMu]"] -> 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                       #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                   matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalMu]"] -> 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                       #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                   matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalSigma]\[FormalLambda]"] -> 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                       #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                   matrixRepresentation]], 2]]]], If[index1 === True && index2 === True && index3 === False && 
            index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                   Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*
                       Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],
                        #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]", 
               "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                        Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                  Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]", "\[FormalMu]\[FormalLambda]"] -> 
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                        #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                    matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,Last[index]]] & ) /@ 
                      Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
             Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalMu]\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                    Total[(newRiemannTensor[[First[index],#1,Last[index],#1]] & ) /@ Range[Length[
                        matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
           If[index1 === True && index2 === False && index3 === True && index4 === False, 
            newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                         #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                         #1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ 
                      Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                  4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[
                 (Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],Last[index]]] & ) /@ 
                       Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalSigma]\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(newRiemannTensor[[First[index],#1,#1,Last[index]]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalSigma]\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(newRiemannTensor[[First[index],Last[index],#1,#1]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
            If[index1 === True && index2 === False && index3 === False && index4 === True, 
             newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                          #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                          #1[[3]],index[[3]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[
                     matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalMu]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                          Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalSigma]\[FormalLambda]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                          Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalMu]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                          #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                      matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalSigma]\[FormalLambda]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                          #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                      matrixRepresentation]], 2]]]], If[index1 === False && index2 === True && index3 === True && 
               index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                      Total[(Inverse[matrixRepresentation][[#1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],
                           index[[3]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalLambda]\[FormalNu]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                           Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalRho]\[FormalLambda]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                           #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                       matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                           Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                           #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                       matrixRepresentation]], 2]]]], If[index1 === False && index2 === True && index3 === False && 
                index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                       Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*riemannTensor[[index[[1]],index[[2]],#1,
                            index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalMu]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                            Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalRho]\[FormalLambda]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                            Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalLambda]\[FormalMu]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                            #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                        matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                            #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                        matrixRepresentation]], 2]]]], If[index1 === False && index2 === False && index3 === True && 
                 index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[matrixRepresentation][[index[[2]],#1]]*riemannTensor[[index[[1]],#1,index[[3]],
                             index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalSigma]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                             Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalRho]\[FormalLambda]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                             Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalLambda]", "\[FormalLambda]\[FormalSigma]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                             #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                         matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                             #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                         matrixRepresentation]], 2]]]], If[index1 === True && index2 === True && index3 === True && 
                  index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                         Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                              index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ Tuples[
                            Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                       4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]\[FormalMu]", "\[FormalLambda]"] -> Normal[SparseArray[
                      (Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ 
                            Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                        2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalMu]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, 
                         index -> Total[(newRiemannTensor[[First[index],#1,Last[index],#1]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
                   Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalLambda]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                          Total[(newRiemannTensor[[First[index],Last[index],#1,#1]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                 If[index1 === True && index2 === True && index3 === False && index4 === True, newRiemannTensor = 
                    Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                              Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[#1[[1]],index[[2]],
                               #1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]\[FormalNu]", 
                      "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,
                               First[index],#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                        Tuples[Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> 
                     Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                               Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                          Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalLambda]", "\[FormalLambda]"] -> 
                     Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],
                               #1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                           matrixRepresentation]], 2]]]], If[index1 === True && index2 === False && index3 === True && 
                    index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                            (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],
                                #1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ Tuples[
                              Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                           matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]\[FormalNu]", "\[FormalLambda]"] -> 
                      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                           Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> 
                      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                           Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalMu]\[FormalLambda]", "\[FormalLambda]"] -> 
                      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,
                                Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                          Range[Length[matrixRepresentation]], 2]]]], If[index1 === False && index2 === True && 
                     index3 === True && index4 === True, newRiemannTensor = riemannTensor; Association[
                      Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]\[FormalNu]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                             Total[(newRiemannTensor[[#1,#1,First[index],Last[index]]] & ) /@ Range[Length[
                                 matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
                      Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                             Total[(newRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[Length[
                                 matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
                      Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalMu]\[FormalLambda]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                             Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ Range[Length[
                                 matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                    Indeterminate]]]]]]]]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ReducedIndexContractions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, newRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     If[(index1 === True && index2 === True && index3 === True && index4 === True) || 
       (index1 === False && index2 === False && index3 === False && index4 === False), Association[], 
      If[index1 === True && index2 === False && index3 == False && index4 === False, 
       newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                    #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[3]]]]*Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],
                    #1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalMu]\[FormalNu]"] -> 
          FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                     Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalSigma]\[FormalLambda]\[FormalNu]"] -> 
          FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                     Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalSigma]\[FormalMu]\[FormalLambda]"] -> 
          FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],
                     Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 2]]]]], If[index1 === False && index2 === True && index3 === False && 
         index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[2]],
                     index[[4]]]]*riemannTensor[[index[[1]],index[[2]],#1[[1]],#1[[2]]]] & ) /@ 
                  Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
              4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalMu]\[FormalNu]"] -> FullSimplify[
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                      Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalNu]"] -> 
           FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                      Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalMu]\[FormalLambda]"] -> 
           FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,
                      Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]]]], If[index1 === False && index2 === False && 
          index3 === True && index4 === False, newRiemannTensor = 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],
                      #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalSigma]\[FormalNu]"] -> 
            FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                       Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalNu]"] -> 
            FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                       Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalSigma]\[FormalLambda]"] -> 
            FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],
                       Last[index],#1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 2]]]]], If[index1 === False && index2 === False && 
           index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                 index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[
                       #1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalSigma]\[FormalMu]"] -> FullSimplify[Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ 
                      Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
            Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalMu]"] -> FullSimplify[Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],#1]] & ) /@ 
                      Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
            Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalSigma]\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,#1]] & ) /@ 
                      Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]], 
          If[index1 === True && index2 === True && index3 === False && index4 === False, 
           newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                        #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[
                        #1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]", "\[FormalLambda]\[FormalNu]"] -> FullSimplify[Normal[
                SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                         Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                   Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]", "\[FormalMu]\[FormalLambda]"] -> 
              FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],
                         Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                   Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> 
              FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                         Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                   Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalMu]\[FormalLambda]"] -> 
              FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,
                         Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                   Range[Length[matrixRepresentation]], 2]]]]], If[index1 === True && index2 === False && 
             index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                        Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                         index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
             Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalLambda]\[FormalNu]"] -> FullSimplify[Normal[SparseArray[
                  (Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],Last[index]]] & ) /@ 
                        Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalSigma]\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[
                  (Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ 
                        Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> FullSimplify[Normal[SparseArray[
                  (Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,Last[index]]] & ) /@ 
                        Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalSigma]\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[
                  (Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,#1]] & ) /@ 
                        Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]], 
            If[index1 === True && index2 === False && index3 === False && index4 === True, 
             newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                          #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                          #1[[3]],index[[3]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[
                     matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalMu]"] -> 
                FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,
                           First[index],Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalSigma]\[FormalLambda]"] -> 
                FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],
                           #1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalMu]"] -> 
                FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,
                           Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalSigma]\[FormalLambda]"] -> 
                FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],
                           Last[index],#1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 2]]]]], If[index1 === False && index2 === True && 
               index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                     index -> Total[(Inverse[matrixRepresentation][[#1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],
                           index[[3]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalLambda]\[FormalNu]"] -> 
                 FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,
                            First[index],Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalRho]\[FormalLambda]"] -> 
                 FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,
                            Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> 
                 FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],
                            #1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                 FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],
                            Last[index],#1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]]]], If[index1 === False && index2 === True && 
                index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                      index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*riemannTensor[[index[[1]],index[[2]],
                            #1,index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalMu]"] -> 
                  FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,
                             First[index],Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                      Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalRho]\[FormalLambda]"] -> 
                  FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,
                             #1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalLambda]\[FormalMu]"] -> 
                  FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],
                             Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                  FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],
                             Last[index],#1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]]]], If[index1 === False && index2 === False && 
                 index3 === True && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                       index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*riemannTensor[[index[[1]],#1,
                             index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", 
                    "\[FormalLambda]\[FormalSigma]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[
                           (newRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                  Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalRho]\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                         index -> Total[(newRiemannTensor[[First[index],#1,#1,Last[index]]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                  Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalLambda]", "\[FormalLambda]\[FormalSigma]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                         index -> Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                  Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                         index -> Total[(newRiemannTensor[[First[index],#1,Last[index],#1]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]], 
                If[index1 === True && index2 === True && index3 === True && index4 === False, 
                 newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[
                              index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[
                              #1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                            2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[
                   Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]\[FormalMu]", "\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                          index -> Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ Range[Length[
                               matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                   Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalMu]", "\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                          index -> Total[(newRiemannTensor[[First[index],#1,Last[index],#1]] & ) /@ Range[Length[
                               matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                   Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalLambda]", "\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                          index -> Total[(newRiemannTensor[[First[index],Last[index],#1,#1]] & ) /@ Range[Length[
                               matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]], 
                 If[index1 === True && index2 === True && index3 === False && index4 === True, newRiemannTensor = 
                    Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                              Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[#1[[1]],index[[2]],
                               #1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]\[FormalNu]", 
                      "\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[
                                #1,First[index],#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                         Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> 
                     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],
                                #1,#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                          Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalLambda]", "\[FormalLambda]"] -> 
                     FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],
                                Last[index],#1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                          Range[Length[matrixRepresentation]], 2]]]]], If[index1 === True && index2 === False && 
                    index3 === True && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                          index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[
                                index[[2]],#1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ 
                             Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                           matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]\[FormalNu]", "\[FormalLambda]"] -> 
                      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,
                                 First[index],Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                          Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> 
                      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[
                                  index],#1,#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                          Tuples[Range[Length[matrixRepresentation]], 2]]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalMu]\[FormalLambda]", "\[FormalLambda]"] -> 
                      FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[
                                  index],#1,Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                          Tuples[Range[Length[matrixRepresentation]], 2]]]]], If[index1 === False && index2 === True && 
                     index3 === True && index4 === True, newRiemannTensor = riemannTensor; Association[
                      Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]\[FormalNu]", "\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                             index -> Total[(newRiemannTensor[[#1,#1,First[index],Last[index]]] & ) /@ Range[Length[
                                  matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                      Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                             index -> Total[(newRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[Length[
                                  matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                      Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalMu]\[FormalLambda]", "\[FormalLambda]"] -> FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                             index -> Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ Range[Length[
                                  matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]]], 
                    Indeterminate]]]]]]]]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["SymbolicIndexContractions"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, newRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],
                index[[2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,
                   index[[3]]]]*christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*
                  christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. (ToExpression[#1] -> #1 & ) /@ 
        Select[coordinates, StringQ]; If[(index1 == True && index2 === True && index3 === True && index4 === True) || 
       (index1 === False && index2 === False && index3 === False && index4 === False), Association[], 
      If[index1 === True && index2 === False && index3 === False && index4 === False, 
       newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                    #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[3]]]]*Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],
                    #1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalMu]\[FormalNu]"] -> 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                    Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalSigma]\[FormalLambda]\[FormalNu]"] -> 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                    Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalSigma]\[FormalMu]\[FormalLambda]"] -> 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                    #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
              2]]]], If[index1 === False && index2 === True && index3 === False && index4 === False, 
        newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                     #1[[1]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],
                     index[[2]],#1[[1]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalMu]\[FormalNu]"] -> 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                     Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalNu]"] -> 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                     Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalMu]\[FormalLambda]"] -> 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                     #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               2]]]], If[index1 === False && index2 === False && index3 === True && index4 === False, 
         newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                      index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],
                      #1[[1]],index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalSigma]\[FormalNu]"] -> 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                      Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalNu]"] -> 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                      Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[Sigma]\[FormalLambda]"] -> 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                      #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]]], If[index1 === False && index2 === False && 
           index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                 index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[
                       #1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalLambda]\[FormalSigma]\[FormalMu]"] -> 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                       #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                   matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalLambda]\[FormalMu]"] -> 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                       #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                   matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]", "\[FormalRho]\[FormalSigma]\[FormalLambda]"] -> 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                       #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                   matrixRepresentation]], 2]]]], If[index1 === True && index2 === True && index3 === False && 
            index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                   Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[2]]]]*
                       Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],
                        #1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]", 
               "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                        Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                  Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]", "\[FormalMu]\[FormalLambda]"] -> 
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                        #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                    matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,Last[index]]] & ) /@ 
                      Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
             Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalMu]\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                    Total[(newRiemannTensor[[First[index],#1,Last[index],#1]] & ) /@ Range[Length[
                        matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
           If[index1 === True && index2 === False && index3 === True && index4 === False, 
            newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                         #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*matrixRepresentation[[#1[[3]],
                         index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
             Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[
                 (Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],Last[index]]] & ) /@ 
                       Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalSigma]\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(newRiemannTensor[[First[index],#1,#1,Last[index]]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
              Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalSigma]\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(newRiemannTensor[[First[index],Last[index],#1,#1]] & ) /@ Range[Length[
                         matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
            If[index1 === True && index2 === False && index3 === False && index4 === True, 
             newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],
                          #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                          #1[[3]],index[[3]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[
                     matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalMu]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                          Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalSigma]\[FormalLambda]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                          Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[FormalLambda]\[FormalMu]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                          #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                      matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]", "\[Sigma]\[FormalLambda]"] -> 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                          #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                      matrixRepresentation]], 2]]]], If[index1 === False && index2 === True && index3 === True && 
               index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                      Total[(Inverse[matrixRepresentation][[#1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],
                           index[[3]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalLambda]\[FormalNu]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                           Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]", "\[FormalRho]\[FormalLambda]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                           #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                       matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalLambda]\[FormalNu]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                           Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                           #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                       matrixRepresentation]], 2]]]], If[index1 === False && index2 === True && index3 === False && 
                index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                       Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*riemannTensor[[index[[1]],index[[2]],#1,
                            index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                     Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalMu]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                            Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalRho]\[FormalLambda]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                            Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalLambda]\[FormalMu]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                            #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                        matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],#1,
                            #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                        matrixRepresentation]], 2]]]], If[index1 === False && index2 === False && index3 === True && 
                 index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[matrixRepresentation][[index[[2]],#1]]*riemannTensor[[index[[1]],#1,index[[3]],
                             index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalLambda]\[FormalSigma]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],#1,
                             Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalNu]", "\[FormalRho]\[FormalLambda]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                             Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalLambda]", "\[FormalLambda]\[FormalSigma]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],
                             #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                         matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalLambda]", "\[FormalRho]\[FormalLambda]"] -> 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,Last[index],
                             #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                         matrixRepresentation]], 2]]]], If[index1 === True && index2 === True && index3 === True && 
                  index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                         Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                              index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ Tuples[
                            Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                       4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]\[FormalMu]", "\[FormalLambda]"] -> Normal[SparseArray[
                      (Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ 
                            Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                        2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalMu]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, 
                         index -> Total[(newRiemannTensor[[First[index],#1,Last[index],#1]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
                   Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalLambda]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                          Total[(newRiemannTensor[[First[index],Last[index],#1,#1]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                 If[index1 === True && index2 === True && index3 === False && index4 === True, newRiemannTensor = 
                    Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                              Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[#1[[1]],index[[2]],
                               #1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalSigma]\[FormalNu]", 
                      "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,
                               First[index],#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                        Tuples[Range[Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> 
                     Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                               Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                          Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalLambda]", "\[FormalLambda]"] -> 
                     Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],Last[index],
                               #1,#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                           matrixRepresentation]], 2]]]], If[index1 === True && index2 === False && index3 === True && 
                    index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                            (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],
                                #1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ Tuples[
                              Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                           matrixRepresentation]], 4]]]; Association[Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]\[FormalNu]", "\[FormalLambda]"] -> 
                      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[#1,#1,First[index],
                                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                           Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> 
                      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,#1,
                                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                           Length[matrixRepresentation]], 2]]], Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalMu]\[FormalLambda]", "\[FormalLambda]"] -> 
                      Normal[SparseArray[(Module[{index = #1}, index -> Total[(newRiemannTensor[[First[index],#1,
                                Last[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                          Range[Length[matrixRepresentation]], 2]]]], If[index1 === False && index2 === True && 
                     index3 === True && index4 === True, newRiemannTensor = riemannTensor; Association[
                      Subsuperscript["\[FormalCapitalR]", "\[FormalLambda]\[FormalMu]\[FormalNu]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                             Total[(newRiemannTensor[[#1,#1,First[index],Last[index]]] & ) /@ Range[Length[
                                 matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
                      Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalLambda]\[FormalNu]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                             Total[(newRiemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ Range[Length[
                                 matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
                      Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalMu]\[FormalLambda]", "\[FormalLambda]"] -> Normal[SparseArray[(Module[{index = #1}, index -> 
                             Total[(newRiemannTensor[[#1,First[index],Last[index],#1]] & ) /@ Range[Length[
                                 matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]], 
                    Indeterminate]]]]]]]]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["CovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, newRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
     If[index1 === True && index2 === True && index3 === True && index4 === True, 
      newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],
                   #1]]*riemannTensor[[#1,index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[
                  newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
       Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                 StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                   index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm], 
                 ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                  StandardForm]]], StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
               newCoordinates[[index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,
                   index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                   index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                   index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                   #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False && index3 === False && index4 === False, 
       newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    index[[2]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*
                   Inverse[newMatrixRepresentation][[#1[[3]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                    #1[[3]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm], 
                  ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                   StandardForm]]], StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[
                    #1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                    index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                    index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False && index3 === False && index4 === False, 
        newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],
                     #1[[1]]]]*Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[
                     #1[[3]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],
                     #1[[2]],#1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                    newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                 StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                  index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,
                     index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                     index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                 (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                     index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
           (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], 
        If[index1 === False && index2 === True && index3 === False && index4 === False, 
         newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      #1[[1]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[
                      index[[1]],index[[2]],#1[[1]],#1[[2]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                    2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                     newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                  StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                   index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],
                      index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                 Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                      index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                  (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                      #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                      index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
            (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && index2 === False && 
           index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                 index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[newMatrixRepresentation][[
                       #1[[2]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],#1[[2]]]] & ) /@ 
                    Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[newMatrixRepresentation]], 4]]]; Association[
            (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                     StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[4]]]], 
                     StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                      newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                      StandardForm]]], StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                    index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                      newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                      newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*
                      newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                      newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
             (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && index2 === False && 
            index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                  index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[
                         newMatrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                        index[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
             (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                      StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[
                       index[[5]]]], StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                      ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                       StandardForm]]], StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                     index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                       newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                       newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*
                       newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                       newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                       newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
              (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === True && 
             index3 === False && index4 === False, newRiemannTensor = Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*
                        Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                         index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                       Range[Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                  4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                        index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[
                       ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                        StandardForm]], StringJoin[ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[
                        newCoordinates[[index[[5]]]], StandardForm]]], StandardForm]] -> D[newRiemannTensor[[index[[2]],
                      index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + Total[
                     (christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                         index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                     (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                         #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                         index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                      Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                        newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                        newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
               (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False && 
              index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[
                 (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*
                         Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                          index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                        Range[Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[
                     newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, StringJoin[ToString[
                      Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], 
                     ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                        ToString[newCoordinates[[index[[4]]]], StandardForm]], StringJoin[ToString[newCoordinates[[
                          index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                      StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                      newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                         newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                         newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*
                         newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                         newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                         newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                         newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                         newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                         newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
                (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False && 
               index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                     index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[
                           index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],index[[3]]]]*riemannTensor[[
                           #1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                         3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
                (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                         StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[
                          newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                          StandardForm]], StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                          newCoordinates[[index[[4]]]], StandardForm]]], StandardForm]] -> D[newRiemannTensor[[index[[2]],
                        index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + Total[
                       (christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                           index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                       (christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                           index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                       (christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],
                           index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                       (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                           #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                   Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                   StringQ]], If[index1 === False && index2 === True && index3 === True && index4 === False, 
               newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                        (Inverse[newMatrixRepresentation][[#1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],
                            index[[3]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
                    Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
                 (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                          StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[
                           newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                           StandardForm]], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                           newCoordinates[[index[[5]]]], StandardForm]]], StandardForm]] -> D[newRiemannTensor[[
                         index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + 
                       Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                        (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],
                            index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                        (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                        (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                    StringQ]], If[index1 === False && index2 === True && index3 === False && index4 === True, 
                newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[newMatrixRepresentation][[#1,index[[3]]]]*riemannTensor[[index[[1]],index[[2]],#1,
                             index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, 
                      StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
                         StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[3]]]], 
                            StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]], StringJoin[ToString[
                            newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                            StandardForm]]], StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                          index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],
                             #1]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                            newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*
                            newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                            newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                            newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                            newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                            newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                            newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
                   (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && 
                  index2 === False && index3 === True && index4 === True, newRiemannTensor = Normal[SparseArray[
                     (Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1]]*
                             riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                             newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
                  Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                             index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[
                            ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                             StandardForm]], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                            ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
                        D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[
                            1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[#1,
                              index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                         Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                              index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                          (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                              index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                          (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                              index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
                     Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
                     Select[coordinates, StringQ]], If[index1 === True && index2 === True && index3 === True && 
                   index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                          Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],
                               index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ Tuples[
                             Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                          newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, StringJoin[ToString[
                           Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], 
                          ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                             ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                              StandardForm]], ToString[newCoordinates[[index[[5]]]], StandardForm]], StandardForm]] -> 
                         D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[
                             1]]]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],
                               index[[3]],index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                          Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                               index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                           (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                               index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                           (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                               index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                       StringQ]], If[index1 === True && index2 === True && index3 === False && index4 === True, 
                   newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[
                                index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*
                               riemannTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[
                                newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                         4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[
                              newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", 
                             StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[
                                index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]], 
                             ToString[newCoordinates[[index[[4]]]], StandardForm]], StandardForm]] -> 
                          D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[
                              1]]]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],
                                index[[3]],#1,index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                           Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                                index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                            (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                                index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                            (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                                index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                        Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                        StringQ]], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                    newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[
                                 index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*
                                riemannTensor[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[
                                 newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                          4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[
                               newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript[
                              "\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                                newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                                StandardForm]], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
                           D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                              index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[
                                 index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                            Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                                 index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                             (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                                 index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                             (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                                 index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                         Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                         StringQ]], If[index1 === False && index2 === True && index3 == True && index4 === True, 
                     newRiemannTensor = riemannTensor; Association[(Module[{index = #1}, StringJoin[ToString[Subscript[
                               "\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[
                              Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], 
                                ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[
                                   5]]]], StandardForm]], ToString[newCoordinates[[index[[2]]]], StandardForm]], 
                              StandardForm]] -> D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                              newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                                 newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                                 newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                                 newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                                 newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                                 newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                                 newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                                 newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
                        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ReducedCovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, newRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
     If[index1 === True && index2 === True && index3 === True && index4 === True, 
      newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],
                   #1]]*riemannTensor[[#1,index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[
                  newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
       Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                 StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                   index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm], 
                 ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                  StandardForm]]], StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                 index[[5]]]], newCoordinates[[index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                   newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                   newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[
                    index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                    index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False && index3 === False && index4 === False, 
       newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    index[[2]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*
                   Inverse[newMatrixRepresentation][[#1[[3]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                    #1[[3]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm], 
                  ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                   StandardForm]]], StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                  index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                    newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                    newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[
                     index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                     index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                 (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False && index3 === False && index4 === False, 
        newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],
                     #1[[1]]]]*Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[
                     #1[[3]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],
                     #1[[2]],#1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                    newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                 StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                  newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[
                      index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                 Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                      index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                  (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                      #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                      index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
           (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], 
        If[index1 === False && index2 === True && index3 === False && index4 === False, 
         newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      #1[[1]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[
                      index[[1]],index[[2]],#1[[1]],#1[[2]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                    2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                     newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                  StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                   newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                      newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*
                      newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*
                      newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                      newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
            (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && index2 === False && 
           index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                 index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[newMatrixRepresentation][[
                       #1[[2]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],#1[[2]]]] & ) /@ 
                    Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[newMatrixRepresentation]], 4]]]; Association[
            (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                     StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[4]]]], 
                     StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                      newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                      StandardForm]]], StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],
                     index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],
                        index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                     Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                       newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*
                       newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                       newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
             (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && index2 === False && 
            index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                  index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[
                         newMatrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                        index[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
             (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                      StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[
                       index[[5]]]], StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                      ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                       StandardForm]]], StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],
                      index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],
                         index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                      Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                        newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                        newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*
                        newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                        newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                        newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                        newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
              (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === True && 
             index3 === False && index4 === False, newRiemannTensor = Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*
                        Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                         index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                       Range[Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                  4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                        index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[
                       ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                        StandardForm]], StringJoin[ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[
                        newCoordinates[[index[[5]]]], StandardForm]]], StandardForm]] -> FullSimplify[
                    D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + 
                     Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                          index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                      (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                          #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                          index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                         newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                         newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
               (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False && 
              index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[
                 (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*
                         Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                          index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                        Range[Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[
                     newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, StringJoin[ToString[
                      Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], 
                     ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                        ToString[newCoordinates[[index[[4]]]], StandardForm]], StringJoin[ToString[newCoordinates[[
                          index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                      StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                       newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                          newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                          newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*
                          newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                          newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                          newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                          newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                          newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                          newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
                (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False && 
               index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                     index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[
                           index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],index[[3]]]]*riemannTensor[[
                           #1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                         3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
                (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                         StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[
                          newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                          StandardForm]], StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                          newCoordinates[[index[[4]]]], StandardForm]]], StandardForm]] -> FullSimplify[
                      D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + 
                       Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                        (christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                        (christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                        (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                            index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
                  Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                   StringQ]], If[index1 === False && index2 === True && index3 === True && index4 === False, 
               newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                        (Inverse[newMatrixRepresentation][[#1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],
                            index[[3]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 4]]]; Association[
                 (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                          StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[
                           newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                           StandardForm]], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                           newCoordinates[[index[[5]]]], StandardForm]]], StandardForm]] -> FullSimplify[
                       D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + 
                        Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],
                             index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                         (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],
                             index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                         (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                             index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                         (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                             index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
                   Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
                   Select[coordinates, StringQ]], If[index1 === False && index2 === True && index3 === False && 
                 index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[newMatrixRepresentation][[#1,index[[3]]]]*riemannTensor[[index[[1]],index[[2]],#1,
                             index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, 
                      StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
                         StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[3]]]], 
                            StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]], StringJoin[ToString[
                            newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                            StandardForm]]], StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],
                           index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],
                              index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                           Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],
                              #1]]*newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                             newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                             newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                             newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                             newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                             newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
                   (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && 
                  index2 === False && index3 === True && index4 === True, newRiemannTensor = Normal[SparseArray[
                     (Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1]]*
                             riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                             newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
                  Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                             index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[
                            ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                             StandardForm]], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                            ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> FullSimplify[
                         D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[
                             1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[#1,
                               index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                          Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,
                               index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                           (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                               index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                           (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                               index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
                     Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
                     Select[coordinates, StringQ]], If[index1 === True && index2 === True && index3 === True && 
                   index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                          Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],
                               index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ Tuples[
                             Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                          newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, StringJoin[ToString[
                           Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], 
                          ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                             ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                              StandardForm]], ToString[newCoordinates[[index[[5]]]], StandardForm]], StandardForm]] -> 
                         FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                             index[[1]]]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[
                                index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                           Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                                index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                            (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                                index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                            (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                                index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ Tuples[
                       Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                       StringQ]], If[index1 === True && index2 === True && index3 === False && index4 === True, 
                   newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[
                                index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*
                               riemannTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[
                                newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                         4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[
                              newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", 
                             StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[
                                index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]], 
                             ToString[newCoordinates[[index[[4]]]], StandardForm]], StandardForm]] -> FullSimplify[
                           D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                              index[[1]]]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[
                                 index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                            Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                                 index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                             (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                                 index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                             (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                                 index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]]] & ) /@ 
                       Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
                       Select[coordinates, StringQ]], If[index1 === True && index2 === False && index3 === True && 
                     index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                             (newMatrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[index[[2]],
                                 #1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ Tuples[
                               Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                            newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, StringJoin[ToString[
                             Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], 
                            ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], 
                                StandardForm], ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[
                                newCoordinates[[index[[5]]]], StandardForm]], ToString[newCoordinates[[index[[3]]]], 
                               StandardForm]], StandardForm]] -> FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],
                               index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[
                                  index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,index[[4]],index[[
                                   5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,
                                  index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                               Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                                  index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ 
                               Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                                  index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ 
                               Range[Length[newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[
                           newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], 
                    If[index1 === False && index2 === True && index3 === True && index4 === True, newRiemannTensor = 
                       riemannTensor; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[
                                newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript[
                               "\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                                 newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                                 StandardForm]], ToString[newCoordinates[[index[[2]]]], StandardForm]], StandardForm]] -> 
                            FullSimplify[D[newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                               newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                                  newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                                  newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                                  newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                                  newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                                  newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                                  newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                                  newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                                  newMatrixRepresentation]]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                          5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]]]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["SymbolicCovariantDerivatives"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, newRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     If[index1 === True && index2 === True && index3 === True && index4 === True, 
      newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],
                   #1]]*riemannTensor[[#1,index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[
                  newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
       Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                 StandardForm]], StandardForm], ToString[Subscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                   index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm], 
                 ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                  StandardForm]]], StandardForm]] -> Inactive[D][riemannTensor[[index[[2]],index[[3]],index[[4]],
                index[[5]]]], newCoordinates[[index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                  newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                  newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[
                   index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                   index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                   #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
          Select[coordinates, StringQ]], If[index1 === False && index2 === False && index3 === False && index4 === False, 
       newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                    index[[2]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*
                   Inverse[newMatrixRepresentation][[#1[[3]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                    #1[[3]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]] & ) /@ 
            Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
        Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                  StandardForm]], StandardForm], ToString[Superscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[
                    index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], StandardForm], 
                  ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                   StandardForm]]], StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                 index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                   newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                   newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[
                    index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                    index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                    #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
           Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
           Select[coordinates, StringQ]], If[index1 === True && index2 === False && index3 === False && index4 === False, 
        newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],
                     #1[[1]]]]*Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[
                     #1[[3]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],
                     #1[[2]],#1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] & ) /@ 
             Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
          (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                   StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[2]]]], 
                   StandardForm], StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                    newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                 StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                 newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[
                     index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                     index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                 (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                     #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                     index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                  Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
           (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], 
        If[index1 === False && index2 === True && index3 === False && index4 === False, 
         newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[
                      #1[[1]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[
                      index[[1]],index[[2]],#1[[1]],#1[[2]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                    2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
          Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                    StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[3]]]], 
                    StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                     newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                  StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                  newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[
                      #1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                 Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                      index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                  (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                      #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                      index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ 
                   Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
            (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && index2 === False && 
           index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                 index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[newMatrixRepresentation][[
                       #1[[2]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],#1[[2]]]] & ) /@ 
                    Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[newMatrixRepresentation]], 4]]]; Association[
            (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                     StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[index[[4]]]], 
                     StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                      newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                      StandardForm]]], StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                    index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                      newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                      newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*
                      newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                      newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                      newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                      newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
             (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && index2 === False && 
            index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                  index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[
                         newMatrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                        index[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
             (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                      StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", ToString[newCoordinates[[
                       index[[5]]]], StandardForm], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                      ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                       StandardForm]]], StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                     index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*
                       newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                       newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*
                       newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                       newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                       newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                       newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
              (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === True && 
             index3 === False && index4 === False, newRiemannTensor = Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*
                        Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                         index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                       Range[Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                  4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                        index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[
                       ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[3]]]], 
                        StandardForm]], StringJoin[ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[
                        newCoordinates[[index[[5]]]], StandardForm]]], StandardForm]] -> Inactive[D][newRiemannTensor[[
                      index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + 
                    Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                         index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                     (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                         #1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                         index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                      Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                        newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                        newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
               (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False && 
              index3 === True && index4 === False, newRiemannTensor = Normal[SparseArray[
                 (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*
                         Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                          index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                        Range[Length[newMatrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[
                     newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, StringJoin[ToString[
                      Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], 
                     ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                        ToString[newCoordinates[[index[[4]]]], StandardForm]], StringJoin[ToString[newCoordinates[[
                          index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]]], 
                      StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
                      newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*
                         newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                         newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*
                         newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                         newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                         newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                         newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                         newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                         newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
                (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === True && index2 === False && 
               index3 === False && index4 === True, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, 
                     index -> Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[
                           index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],index[[3]]]]*riemannTensor[[
                           #1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                         3]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
                (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                         StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[
                          newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                          StandardForm]], StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[
                          newCoordinates[[index[[4]]]], StandardForm]]], StandardForm]] -> Inactive[D][newRiemannTensor[[
                        index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + 
                      Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                           index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                       (christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                           index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                       (christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],index[[4]],
                           index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                       (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                           #1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                   Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                   StringQ]], If[index1 === False && index2 === True && index3 === True && index4 === False, 
               newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                        (Inverse[newMatrixRepresentation][[#1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],
                            index[[3]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
                    Tuples[Range[Length[newMatrixRepresentation]], 4]]]; Association[
                 (Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], 
                          StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[
                           newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                           StandardForm]], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                           newCoordinates[[index[[5]]]], StandardForm]]], StandardForm]] -> Inactive[D][newRiemannTensor[[
                         index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + 
                       Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + Total[
                        (christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[index[[2]],index[[3]],
                            index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                        (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                        (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                            index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                    Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                    StringQ]], If[index1 === False && index2 === True && index3 === False && index4 === True, 
                newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[newMatrixRepresentation][[#1,index[[3]]]]*riemannTensor[[index[[1]],index[[2]],#1,
                             index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, 
                      StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], 
                         StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[3]]]], 
                            StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]], StringJoin[ToString[
                            newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                            StandardForm]]], StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],
                          index[[4]],index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],
                             index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                          Range[Length[newMatrixRepresentation]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*
                            newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                            newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                            newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                            newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                            newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                            newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
                   (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]], If[index1 === False && 
                  index2 === False && index3 === True && index4 === True, newRiemannTensor = Normal[SparseArray[
                     (Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1]]*
                             riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                             newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
                  Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[newCoordinates[[
                             index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[
                            ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                             StandardForm]], StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                            ToString[newCoordinates[[index[[3]]]], StandardForm]]], StandardForm]] -> 
                        Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                           index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],index[[1]],#1]]*newRiemannTensor[[#1,
                              index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] + 
                         Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                              index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                          (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                              index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                          (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                              index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
                     Tuples[Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ 
                     Select[coordinates, StringQ]], If[index1 === True && index2 === True && index3 === True && 
                   index4 === False, newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> 
                          Total[(newMatrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],
                               index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ Tuples[
                             Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                          newMatrixRepresentation]], 4]]]; Association[(Module[{index = #1}, StringJoin[ToString[
                           Subscript["\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], 
                          ToString[Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], 
                             ToString[newCoordinates[[index[[3]]]], StandardForm], ToString[newCoordinates[[index[[4]]]], 
                              StandardForm]], ToString[newCoordinates[[index[[5]]]], StandardForm]], StandardForm]] -> 
                         Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                            index[[1]]]]] + Total[(christoffelSymbols[[index[[5]],index[[1]],#1]]*newRiemannTensor[[
                               index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                          Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                               index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                           (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                               index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                           (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                               index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                       StringQ]], If[index1 === True && index2 === True && index3 === False && index4 === True, 
                   newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[
                                index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[#1[[2]],index[[3]]]]*
                               riemannTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[
                                newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                         4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[
                              newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript["\[FormalCapitalR]", 
                             StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[newCoordinates[[
                                index[[3]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], StandardForm]], 
                             ToString[newCoordinates[[index[[4]]]], StandardForm]], StandardForm]] -> Inactive[D][
                            newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[index[[
                              1]]]]] + Total[(christoffelSymbols[[index[[4]],index[[1]],#1]]*newRiemannTensor[[index[[2]],
                                index[[3]],#1,index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                           Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                                index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                            (christoffelSymbols[[#1,index[[1]],index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],
                                index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                            (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                                index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                        Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                        StringQ]], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                    newRiemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(newMatrixRepresentation[[
                                 index[[1]],#1[[1]]]]*Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*
                                riemannTensor[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[
                                 newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                          4]]]; Association[(Module[{index = #1}, StringJoin[ToString[Subscript["\[Del]", ToString[
                               newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[Subsuperscript[
                              "\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[2]]]], StandardForm], ToString[
                                newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[5]]]], 
                                StandardForm]], ToString[newCoordinates[[index[[3]]]], StandardForm]], StandardForm]] -> 
                           Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], newCoordinates[[
                              index[[1]]]]] + Total[(christoffelSymbols[[index[[3]],index[[1]],#1]]*newRiemannTensor[[
                                 index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                            Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*newRiemannTensor[[#1,index[[3]],
                                 index[[4]],index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                             (christoffelSymbols[[#1,index[[1]],index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,
                                 index[[5]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                             (christoffelSymbols[[#1,index[[1]],index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],
                                 index[[4]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[
                         Range[Length[newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                         StringQ]], If[index1 === False && index2 === True && index3 === True && index4 === True, 
                     newRiemannTensor = riemannTensor; Association[(Module[{index = #1}, StringJoin[ToString[Subscript[
                               "\[Del]", ToString[newCoordinates[[index[[1]]]], StandardForm]], StandardForm], ToString[
                              Subsuperscript["\[FormalCapitalR]", StringJoin[ToString[newCoordinates[[index[[3]]]], StandardForm], 
                                ToString[newCoordinates[[index[[4]]]], StandardForm], ToString[newCoordinates[[index[[
                                   5]]]], StandardForm]], ToString[newCoordinates[[index[[2]]]], StandardForm]], 
                              StandardForm]] -> Inactive[D][newRiemannTensor[[index[[2]],index[[3]],index[[4]],
                               index[[5]]]], newCoordinates[[index[[1]]]]] + Total[(christoffelSymbols[[index[[2]],
                                  index[[1]],#1]]*newRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ 
                               Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                                  index[[3]]]]*newRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ 
                               Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                                  index[[4]]]]*newRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ 
                               Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],
                                  index[[5]]]]*newRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ 
                               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[
                            newMatrixRepresentation]], 5] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, 
                          StringQ]]]]]]]]]]]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["BianchiIdentities"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     (Module[{index = #1}, (D[covariantRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
             newCoordinates[[index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                covariantRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                covariantRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                newMatrixRepresentation]]]) + (D[covariantRiemannTensor[[index[[2]],index[[3]],index[[5]],index[[1]]]], 
             newCoordinates[[index[[4]]]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[2]]]]*
                covariantRiemannTensor[[#1,index[[3]],index[[5]],index[[1]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[3]]]]*
                covariantRiemannTensor[[index[[2]],#1,index[[5]],index[[1]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[5]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],#1,index[[1]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[1]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],index[[5]],#1]] & ) /@ Range[Length[
                newMatrixRepresentation]]]) + (D[covariantRiemannTensor[[index[[2]],index[[3]],index[[1]],index[[4]]]], 
             newCoordinates[[index[[5]]]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[2]]]]*
                covariantRiemannTensor[[#1,index[[3]],index[[1]],index[[4]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[3]]]]*
                covariantRiemannTensor[[index[[2]],#1,index[[1]],index[[4]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[1]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],#1,index[[4]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[4]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],index[[1]],#1]] & ) /@ Range[Length[
                newMatrixRepresentation]]]) == 0] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["SymbolicBianchiIdentities"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (Inactive[D][newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + 
                 Inactive[D][newMatrixRepresentation[[index[[2]],#1]], newCoordinates[[index[[3]]]]] - 
                 Inactive[D][newMatrixRepresentation[[index[[2]],index[[3]]]], newCoordinates[[#1]]]) & ) /@ 
              Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[4]]]], newCoordinates[[index[[3]]]]] - Inactive[D][christoffelSymbols[[index[[1]],index[[
                2]],index[[3]]]], newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*
                 christoffelSymbols[[#1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
             Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
               Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     covariantRiemannTensor = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,index[[2]],
                 index[[3]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
     (Module[{index = #1}, (Inactive[D][covariantRiemannTensor[[index[[2]],index[[3]],index[[4]],index[[5]]]], 
             newCoordinates[[index[[1]]]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*
                covariantRiemannTensor[[#1,index[[3]],index[[4]],index[[5]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                covariantRiemannTensor[[index[[2]],#1,index[[4]],index[[5]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[4]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],#1,index[[5]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[5]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],index[[4]],#1]] & ) /@ Range[Length[
                newMatrixRepresentation]]]) + (Inactive[D][covariantRiemannTensor[[index[[2]],index[[3]],index[[5]],
              index[[1]]]], newCoordinates[[index[[4]]]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[2]]]]*
                covariantRiemannTensor[[#1,index[[3]],index[[5]],index[[1]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[3]]]]*
                covariantRiemannTensor[[index[[2]],#1,index[[5]],index[[1]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[5]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],#1,index[[1]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[4]],index[[1]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],index[[5]],#1]] & ) /@ Range[Length[
                newMatrixRepresentation]]]) + (Inactive[D][covariantRiemannTensor[[index[[2]],index[[3]],index[[1]],
              index[[4]]]], newCoordinates[[index[[5]]]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[2]]]]*
                covariantRiemannTensor[[#1,index[[3]],index[[1]],index[[4]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[3]]]]*
                covariantRiemannTensor[[index[[2]],#1,index[[1]],index[[4]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[1]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],#1,index[[4]]]] & ) /@ Range[Length[
                newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[5]],index[[4]]]]*
                covariantRiemannTensor[[index[[2]],index[[3]],index[[1]],#1]] & ) /@ Range[Length[
                newMatrixRepresentation]]]) == 0] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 5] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["Dimensions"] := Length[matrixRepresentation] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["Signature"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      Join[ConstantArray[-1, Length[negativeEigenvalues]], ConstantArray[1, Length[positiveEigenvalues]]], 
      Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["RiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], True, False], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["PseudoRiemannianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
         Length[matrixRepresentation], False, True], Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["LorentzianQ"] := Module[{eigenvalues, positiveEigenvalues, negativeEigenvalues}, 
    eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
     negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
     If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
      If[Length[positiveEigenvalues] == 1 || Length[negativeEigenvalues] == 1, True, False], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["RiemannianConditions"] := 
  Module[{eigenvalues, riemannianConditions}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     riemannianConditions = FullSimplify[(#1 > 0 & ) /@ eigenvalues]; If[riemannianConditions === True, {}, 
      If[riemannianConditions === False, Indeterminate, If[Length[Select[riemannianConditions, #1 === False & ]] > 0, 
        Indeterminate, DeleteDuplicates[Select[riemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["PseudoRiemannianConditions"] := 
  Module[{eigenvalues, pseudoRiemannianConditions}, eigenvalues = Eigenvalues[matrixRepresentation]; 
     pseudoRiemannianConditions = FullSimplify[(#1 != 0 & ) /@ eigenvalues]; If[pseudoRiemannianConditions === True, {}, 
      If[pseudoRiemannianConditions === False, Indeterminate, 
       If[Length[Select[pseudoRiemannianConditions, #1 === False & ]] > 0, Indeterminate, 
        DeleteDuplicates[Reverse /@ Sort /@ Select[pseudoRiemannianConditions, #1 =!= True & ]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["LorentzianConditions"] := 
  Module[{eigensystem, eigenvalues, eigenvectors, timeCoordinate, lorentzianConditions}, 
    eigensystem = Eigensystem[matrixRepresentation]; eigenvalues = First[eigensystem]; eigenvectors = Last[eigensystem]; 
     If[Length[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]] > 0, 
      timeCoordinate = First[First[Position[eigenvectors, Join[{1}, ConstantArray[0, Length[coordinates] - 1]]]]]; 
       lorentzianConditions = FullSimplify[(If[#1 == timeCoordinate, eigenvalues[[#1]] < 0, eigenvalues[[#1]] > 0] & ) /@ 
          Range[Length[eigenvalues]]]; If[lorentzianConditions === True, {}, If[lorentzianConditions === False, 
         Indeterminate, If[Length[Select[lorentzianConditions, #1 === False & ]] > 0, Indeterminate, 
          DeleteDuplicates[Select[lorentzianConditions, #1 =!= True & ]]]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["CurvatureSingularities"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
     Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
            Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[Catenate[Catenate[
                Catenate[FullSimplify[riemannTensor]]]], newCoordinates] /. Or -> List}]]]] /. 
      (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["KretschmannSingularities"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     contravariantRiemannTensor, kretschmannScalar}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
     kretschmannScalar = Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*
          contravariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 4]]; 
     Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
            Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[FullSimplify[kretschmannScalar], 
              newCoordinates] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ChernPontryaginSingularities"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     mixedRiemannTensor, leviCivitaTensor, contravariantLeviCivitaTensor, chernPontryaginScalar}, 
    If[Length[matrixRepresentation] == 4, newMatrixRepresentation = matrixRepresentation /. 
        (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
               newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], newCoordinates[[
                index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
      covariantRiemannTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,index[[2]],
                  index[[3]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]]; mixedRiemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1]]*
                 riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                 newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
      leviCivitaTensor = Normal[LeviCivitaTensor[4]]; contravariantLeviCivitaTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                  index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],
                  #1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]]; chernPontryaginScalar = 
       Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],
            #1[[5]],#1[[6]]]]*mixedRiemannTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 6]]; 
      Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
             Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[FullSimplify[chernPontryaginScalar], 
               newCoordinates] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ], 
     Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["EulerSingularities"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     leviCivitaTensor, contravariantLeviCivitaTensor, eulerScalar}, If[Length[matrixRepresentation] == 4, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; riemannTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
               newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], newCoordinates[[
                index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[#1,index[[2]],
                   index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
      covariantRiemannTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(newMatrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,index[[2]],
                  index[[3]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[newMatrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[newMatrixRepresentation][[#1[[3]],
                  index[[3]]]]*Inverse[newMatrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],
                  #1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
      eulerScalar = Total[(covariantRiemannTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*covariantRiemannTensor[[#1[[5]],
            #1[[6]],#1[[7]],#1[[8]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],#1[[6]]]]*
           contravariantLeviCivitaTensor[[#1[[3]],#1[[4]],#1[[7]],#1[[8]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 8]]; 
      Quiet[DeleteDuplicates[Catenate[(If[Head[Solve[#1, newCoordinates]] === Solve, {{#1}}, 
             Solve[#1, newCoordinates]] & ) /@ Flatten[{FunctionSingularities[FullSimplify[eulerScalar], 
               newCoordinates] /. Or -> List}]]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ], 
     Indeterminate]] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["CovariantRiemannTensor"] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], True, 
    True, True, True] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["ContravariantRiemannTensor"] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], False, 
    False, False, False] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 
     2 && Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    index1_, index2_, index3_, index4_], newCoordinates_List] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2, index3, index4] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
RiemannTensor[RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    index1_, index2_, index3_, index4_], newIndex1_, newIndex2_, newIndex3_, newIndex4_] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, metricIndex1, metricIndex2], 
    newIndex1, newIndex2, newIndex3, newIndex4] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4] && BooleanQ[newIndex1] && BooleanQ[newIndex2] && BooleanQ[newIndex3] && BooleanQ[newIndex4]
RiemannTensor[RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
    index1_, index2_, index3_, index4_], newCoordinates_List, newIndex1_, newIndex2_, newIndex3_, newIndex4_] := 
  RiemannTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], newIndex1, newIndex2, newIndex3, newIndex4] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4] && BooleanQ[newIndex1] && BooleanQ[newIndex2] && BooleanQ[newIndex3] && BooleanQ[newIndex4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_]["Properties"] := {"TensorRepresentation", "ReducedTensorRepresentation", 
    "SymbolicTensorRepresentation", "KretschmannScalar", "ReducedKretschmannScalar", "SymbolicKretschmannScalar", 
    "ChernPontryaginScalar", "ReducedChernPontryaginScalar", "SymbolicChernPontryaginScalar", "EulerScalar", 
    "ReducedEulerScalar", "SymbolicEulerScalar", "MetricTensor", "Coordinates", "CoordinateOneForms", "Indices", 
    "CovariantQ", "ContravariantQ", "MixedQ", "Symbol", "RiemannFlatQ", "VanishingKretschmannScalarQ", 
    "VanishingChernPontryaginScalarQ", "VanishingEulerScalarQ", "RiemannFlatConditions", 
    "VanishingKretschmannScalarCondition", "VanishingChernPontryaginScalarCondition", "VanishingEulerScalarCondition", 
    "IndexContractions", "ReducedIndexContractions", "SymbolicIndexContractions", "CovariantDerivatives", 
    "ReducedCovariantDerivatives", "SymbolicCovariantDerivatives", "BianchiIdentities", "SymbolicBianchiIdentities", 
    "Dimensions", "Signature", "RiemannianQ", "PseudoRiemannianQ", "LorentzianQ", "RiemannianConditions", 
    "PseudoRiemannianConditions", "LorentzianConditions", "CurvatureSingularities", "KretschmannSingularities", 
    "ChernPontryaginSingularities", "EulerSingularities", "CovariantRiemannTensor", "ContravariantRiemannTensor", 
    "Properties"} /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor /: MakeBoxes[riemannTensor:RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, 
       metricIndex1_, metricIndex2_], index1_, index2_, index3_, index4_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, tensorRepresentation, newTensorRepresentation, 
      type, symbol, dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, signature, icon}, 
     newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
      christoffelSymbols = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                 (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                    index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                   newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],index[[4]]]], 
                newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
                newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                    #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                (christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                 Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
        (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
      If[index1 === True && index2 === True && index3 === True && index4 === True, 
       newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                (matrixRepresentation[[index[[1]],#1]]*tensorRepresentation[[#1,index[[2]],index[[3]],index[[4]]]] & ) /@ 
                 Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
        type = "Covariant"; symbol = Subscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], 
       If[index1 === False && index2 === False && index3 === False && index4 === False, 
        newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                     index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*tensorRepresentation[[index[[1]],
                     #1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Contravariant"; 
         symbol = Superscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && index3 === False && 
          index4 === False, newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, 
                index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],
                      #1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*Inverse[matrixRepresentation][[
                      #1[[4]],index[[4]]]]*tensorRepresentation[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
                   Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalRho]", "\[FormalSigma]\[FormalMu]\[FormalNu]"], 
         If[index1 === False && index2 === True && index3 === False && index4 === False, 
          newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                   (Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[2]],
                       index[[4]]]]*tensorRepresentation[[index[[1]],index[[2]],#1[[1]],#1[[2]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], 
          If[index1 === False && index2 === False && index3 === True && index4 === False, 
           newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                    (Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                        index[[4]]]]*tensorRepresentation[[index[[1]],#1[[1]],index[[3]],#1[[2]]]] & ) /@ 
                     Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalMu]", "\[FormalRho]\[FormalSigma]\[FormalNu]"], 
           If[index1 === False && index2 === False && index3 === False && index4 === True, 
            newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                     (Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                         index[[3]]]]*tensorRepresentation[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ 
                      Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                  4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalNu]", "\[FormalRho]\[FormalSigma]\[FormalMu]"], 
            If[index1 === True && index2 === True && index3 === False && index4 === False, 
             newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                      (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*
                         Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*tensorRepresentation[[#1[[1]],index[[2]],
                          #1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
                  Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", 
                "\[FormalRho]\[FormalSigma]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && index3 === True && index4 === False, 
              newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                       (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*
                          Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*tensorRepresentation[[#1[[1]],#1[[2]],
                           index[[3]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
                   Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", 
                 "\[FormalRho]\[FormalMu]", "\[FormalSigma]\[FormalNu]"], If[index1 === True && index2 === False && index3 === False && index4 === True, 
               newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                        (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*
                           Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*tensorRepresentation[[#1[[1]],#1[[2]],
                            #1[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", 
                  "\[FormalRho]\[FormalNu]", "\[FormalSigma]\[FormalMu]"], If[index1 === False && index2 === True && index3 === True && index4 === False, 
                newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[matrixRepresentation][[#1,index[[4]]]]*tensorRepresentation[[index[[1]],index[[2]],
                             index[[3]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalSigma]\[FormalMu]", 
                   "\[FormalRho]\[FormalNu]"], If[index1 === False && index2 === True && index3 === False && index4 === True, 
                 newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                          (Inverse[matrixRepresentation][[#1,index[[3]]]]*tensorRepresentation[[index[[1]],index[[2]],#1,
                              index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", 
                    "\[FormalSigma]\[FormalNu]", "\[FormalRho]\[FormalMu]"], If[index1 === False && index2 === False && index3 === True && index4 === True, 
                  newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                           (Inverse[matrixRepresentation][[index[[2]],#1]]*tensorRepresentation[[index[[1]],#1,index[[3]],
                               index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                         Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalMu]\[FormalNu]", 
                     "\[FormalRho]\[FormalSigma]"], If[index1 === True && index2 === True && index3 === True && index4 === False, 
                   newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                            (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                                index[[4]]]]*tensorRepresentation[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ 
                             Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                           matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalMu]", 
                      "\[FormalNu]"], If[index1 === True && index2 === True && index3 === False && index4 === True, 
                    newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                             (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                                 index[[3]]]]*tensorRepresentation[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ 
                              Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                            matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalSigma]\[FormalNu]", 
                       "\[FormalMu]"], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                     newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                              (matrixRepresentation[[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],
                                  #1[[2]]]]*tensorRepresentation[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ 
                               Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                             matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", "\[FormalRho]\[FormalMu]\[FormalNu]", 
                        "\[FormalSigma]"], If[index1 === False && index2 === True && index3 === True && index4 === True, 
                      newTensorRepresentation = tensorRepresentation; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalR]", 
                         "\[FormalSigma]\[FormalMu]\[FormalNu]", "\[FormalRho]"], newTensorRepresentation = ConstantArray[Indeterminate, 
                         {Length[matrixRepresentation], Length[matrixRepresentation], Length[matrixRepresentation], 
                          Length[matrixRepresentation]}]; type = Indeterminate; symbol = Indeterminate]]]]]]]]]]]]]]]]; 
      dimensions = Length[matrixRepresentation]; eigenvalues = Eigenvalues[matrixRepresentation]; 
      positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
          Length[matrixRepresentation], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[Total[Total[newTensorRepresentation]], 
        ImageSize -> Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], 
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["RiemannTensor", riemannTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, 
    index2_, index3_, index4_][depth_Integer, layer_Integer, row_Integer, column_Integer] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor}, 
    newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
     christoffelSymbols = Normal[SparseArray[
        (Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
                (D[newMatrixRepresentation[[#1,index[[3]]]], newCoordinates[[index[[2]]]]] + D[newMatrixRepresentation[[
                   index[[2]],#1]], newCoordinates[[index[[3]]]]] - D[newMatrixRepresentation[[index[[2]],index[[3]]]], 
                  newCoordinates[[#1]]]) & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     riemannTensor = Normal[SparseArray[(Module[{index = #1}, index -> D[christoffelSymbols[[index[[1]],index[[2]],
                index[[4]]]], newCoordinates[[index[[3]]]]] - D[christoffelSymbols[[index[[1]],index[[2]],index[[3]]]], 
               newCoordinates[[index[[4]]]]] + Total[(christoffelSymbols[[index[[1]],#1,index[[3]]]]*christoffelSymbols[[
                   #1,index[[2]],index[[4]]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
              Total[(christoffelSymbols[[index[[1]],#1,index[[4]]]]*christoffelSymbols[[#1,index[[2]],index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 4]]] /. 
       (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; 
     If[index1 === True && index2 === True && index3 === True && index4 === True, 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                  index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
      If[index1 === False && index2 === False && index3 === False && index4 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                  riemannTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
       If[index1 === True && index2 === False && index3 === False && index4 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                   Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[3]]]]*Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],
                    #1[[3]],#1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
        If[index1 === False && index2 === True && index3 === False && index4 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*
                    Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],index[[2]],#1[[1]],
                     #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
         If[index1 === False && index2 === False && index3 === True && index4 === False, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[index[[1]],#1[[1]],index[[3]],
                      #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
          If[index1 === False && index2 === False && index3 === False && index4 === True, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[index[[1]],#1[[1]],#1[[2]],
                       index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
           If[index1 === True && index2 === True && index3 === False && index4 === False, 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                       Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],
                        index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]][[
             depth,layer,row,column]], If[index1 === True && index2 === False && index3 === True && index4 === False, 
             Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                        Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                         index[[4]]]]*riemannTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]][[
              depth,layer,row,column]], If[index1 === True && index2 === False && index3 === False && index4 === True, 
              Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                         Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                          index[[3]]]]*riemannTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[
                        Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                   4]]][[depth,layer,row,column]], If[index1 === False && index2 === True && index3 === True && 
                index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                           #1,index[[4]]]]*riemannTensor[[index[[1]],index[[2]],index[[3]],#1]] & ) /@ Range[
                         Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,
                layer,row,column]], If[index1 === False && index2 === True && index3 === False && index4 === True, 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*
                           riemannTensor[[index[[1]],index[[2]],#1,index[[4]]]] & ) /@ Range[Length[
                           matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,
                 row,column]], If[index1 === False && index2 === False && index3 === True && index4 === True, 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*
                            riemannTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                            matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,
                  row,column]], If[index1 === True && index2 === True && index3 === True && index4 === False, 
                  Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                             Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*riemannTensor[[#1[[1]],index[[2]],
                              index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                      Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
                  If[index1 === True && index2 === True && index3 === False && index4 === True, 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                              Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*riemannTensor[[#1[[1]],index[[2]],
                               #1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
                   If[index1 === True && index2 === False && index3 === True && index4 === True, 
                    Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1[[1]]]]*
                               Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*riemannTensor[[#1[[1]],#1[[2]],
                                index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                        Tuples[Range[Length[matrixRepresentation]], 4]]][[depth,layer,row,column]], 
                    If[index1 === False && index2 === True && index3 === True && index4 === True, riemannTensor[[depth,
                      layer,row,column]], Indeterminate]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
