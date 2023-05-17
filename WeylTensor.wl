(* ::Package:: *)

WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  WeylTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], True, True, True, 
    True] /; SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], newCoordinates_List] := 
  WeylTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, index1, index2], True, True, True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   newCoordinates_List, index1_, index2_, index3_, index4_] := 
  WeylTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2, index3, index4] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && Length[newCoordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["TensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
          4]]]; If[index1 === True && index2 === True && index3 === True && index4 === True, weylTensor, 
      If[index1 === False && index2 === False && index3 === False && index4 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
               Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
       If[index1 === True && index2 === False && index3 === False && index4 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                  weylTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
        If[index1 === False && index2 === True && index3 === False && index4 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                   Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[4]]]]*weylTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                  Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
         If[index1 === False && index2 === False && index3 === True && index4 === False, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                    Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                     index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                   Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
          If[index1 === False && index2 === False && index3 === False && index4 === True, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                      index[[3]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
           If[index1 === True && index2 === True && index3 === False && index4 === False, 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],index[[2]],#1[[1]],
                       #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && index3 === True && 
              index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                        index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],
                        #1[[1]],index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && index3 === 
                False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[
                     (Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                         index[[3]]]]*weylTensor[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
              If[index1 === False && index2 === True && index3 === True && index4 === False, Normal[
                SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                         Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[#1[[1]],index[[2]],index[[3]],
                          #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                  Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && 
                 index3 === False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                      Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                           index[[3]]]]*weylTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[
                         Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                    4]]], If[index1 === False && index2 === False && index3 === True && index4 === True, 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                            #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*weylTensor[[#1[[1]],#1[[2]],
                            index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
                   index3 === True && index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> 
                        Total[(Inverse[matrixRepresentation][[#1,index[[4]]]]*weylTensor[[index[[1]],index[[2]],
                             index[[3]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
                    index3 === False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                         Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*weylTensor[[index[[1]],index[[2]],#1,
                              index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && 
                     index3 === True && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                          Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*weylTensor[[index[[1]],#1,index[[3]],
                               index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                         Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && index3 === 
                       True && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[
                            (Inverse[matrixRepresentation][[index[[1]],#1]]*weylTensor[[#1,index[[2]],index[[3]],
                                index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                          Length[matrixRepresentation]], 4]]], Indeterminate]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["ReducedTensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
         Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
          4]]]; If[index1 === True && index2 === True && index3 === True && index4 === True, FullSimplify[weylTensor], 
      If[index1 === False && index2 === False && index3 === False && index4 === False, 
       FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                   #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                   index[[3]]]]*Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],
                   #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === True && index2 === False && 
         index3 === False && index4 === False, FullSimplify[
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                   Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[4]]]]*weylTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                  Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
        If[index1 === False && index2 === True && index3 === False && index4 === False, 
         FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                     #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],
                     index[[4]]]]*weylTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                   Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
         If[index1 === False && index2 === False && index3 === True && index4 === False, 
          FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                      #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[
                      #1[[3]],index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ 
                   Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
               4]]]], If[index1 === False && index2 === False && index3 === False && index4 === True, 
           FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                       index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*
                      Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],
                       index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 4]]]], If[index1 === True && index2 === True && index3 === False && 
             index4 === False, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> 
                   Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[2]],
                        index[[4]]]]*weylTensor[[index[[1]],index[[2]],#1[[1]],#1[[2]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
            If[index1 === True && index2 === False && index3 === True && index4 === False, 
             FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                         index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],
                         #1[[1]],index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                 Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === True && index2 === False && index3 === 
                False && index4 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> 
                     Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                          index[[3]]]]*weylTensor[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ Tuples[
                        Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                   4]]]], If[index1 === False && index2 === True && index3 === True && index4 === False, FullSimplify[
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                           #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[#1[[1]],index[[2]],
                           index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                   Tuples[Range[Length[matrixRepresentation]], 4]]]], If[index1 === False && index2 === True && 
                 index3 === False && index4 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, 
                      index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[
                            #1[[2]],index[[3]]]]*weylTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ 
                         Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                       matrixRepresentation]], 4]]]], If[index1 === False && index2 === False && index3 === True && 
                  index4 === True, FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],
                             #1[[2]]]]*weylTensor[[#1[[1]],#1[[2]],index[[3]],index[[4]]]] & ) /@ Tuples[Range[
                            Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
                 If[index1 === True && index2 === True && index3 === True && index4 === False, FullSimplify[
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                              index[[4]]]]*weylTensor[[index[[1]],index[[2]],index[[3]],#1]] & ) /@ Range[Length[
                             matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
                  If[index1 === True && index2 === True && index3 === False && index4 === True, FullSimplify[
                    Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                               index[[3]]]]*weylTensor[[index[[1]],index[[2]],#1,index[[4]]]] & ) /@ Range[Length[
                              matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
                   If[index1 === True && index2 === False && index3 === True && index4 === True, FullSimplify[
                     Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],
                                #1]]*weylTensor[[index[[1]],#1,index[[3]],index[[4]]]] & ) /@ Range[Length[
                               matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
                    If[index1 === False && index2 === True && index3 === True && index4 === True, FullSimplify[
                      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                                 #1]]*weylTensor[[#1,index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[
                                matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]], 
                     Indeterminate]]]]]]]]]]]]]]]]] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["SymbolicTensorRepresentation"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor}, newMatrixRepresentation = matrixRepresentation /. 
       (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
        Select[coordinates, StringQ]; covariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
          4]]]; If[index1 === True && index2 === True && index3 === True && index4 === True, weylTensor, 
      If[index1 === False && index2 === False && index3 === False && index4 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
               Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
       If[index1 === True && index2 === False && index3 === False && index4 === False, 
        Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*
                  Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*
                  weylTensor[[index[[1]],#1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
        If[index1 === False && index2 === True && index3 === False && index4 === False, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                   Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],
                    index[[4]]]]*weylTensor[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ Tuples[
                  Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
         If[index1 === False && index2 === False && index3 === True && index4 === False, 
          Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                    Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                     index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[
                   Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
          If[index1 === False && index2 === False && index3 === False && index4 === True, 
           Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                     Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                      index[[3]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ Tuples[
                    Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
           If[index1 === True && index2 === True && index3 === False && index4 === False, 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],index[[2]],#1[[1]],
                       #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && index3 === True && 
              index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                        index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],
                        #1[[1]],index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && index3 === 
                False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[
                     (Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                         index[[3]]]]*weylTensor[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]], 
              If[index1 === False && index2 === True && index3 === True && index4 === False, Normal[
                SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                         Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[#1[[1]],index[[2]],index[[3]],
                          #1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                  Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && 
                 index3 === False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                      Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                           index[[3]]]]*weylTensor[[#1[[1]],index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[
                         Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                    4]]], If[index1 === False && index2 === False && index3 === True && index4 === True, 
                 Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                            #1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*weylTensor[[#1[[1]],#1[[2]],
                            index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
                   index3 === True && index4 === False, Normal[SparseArray[(Module[{index = #1}, index -> 
                        Total[(Inverse[matrixRepresentation][[#1,index[[4]]]]*weylTensor[[index[[1]],index[[2]],
                             index[[3]],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                      Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === True && 
                    index3 === False && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                         Total[(Inverse[matrixRepresentation][[#1,index[[3]]]]*weylTensor[[index[[1]],index[[2]],#1,
                              index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                       Range[Length[matrixRepresentation]], 4]]], If[index1 === True && index2 === False && 
                     index3 === True && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> 
                          Total[(Inverse[matrixRepresentation][[index[[2]],#1]]*weylTensor[[index[[1]],#1,index[[3]],
                               index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                         Length[matrixRepresentation]], 4]]], If[index1 === False && index2 === True && index3 === 
                       True && index4 === True, Normal[SparseArray[(Module[{index = #1}, index -> Total[
                            (Inverse[matrixRepresentation][[index[[1]],#1]]*weylTensor[[#1,index[[2]],index[[3]],
                                index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                          Length[matrixRepresentation]], 4]]], Indeterminate]]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["FirstPrincipalInvariant"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor, contravariantWeylTensor}, 
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
         Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
          4]]]; contravariantWeylTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     Total[(weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantWeylTensor[[#1[[1]],#1[[2]],#1[[3]],
          #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["ReducedFirstPrincipalInvariant"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor, contravariantWeylTensor}, 
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
         Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
          4]]]; contravariantWeylTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     FullSimplify[Total[(weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantWeylTensor[[#1[[1]],#1[[2]],#1[[3]],
           #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["SymbolicFirstPrincipalInvariant"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor, contravariantWeylTensor}, 
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
        Select[coordinates, StringQ]; covariantRiemannTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(matrixRepresentation[[index[[1]],#1]]*riemannTensor[[#1,
                 index[[2]],index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
          4]]]; contravariantWeylTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; 
     Total[(weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantWeylTensor[[#1[[1]],#1[[2]],#1[[3]],
          #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["SecondPrincipalInvariant"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor, mixedWeylTensor, leviCivitaTensor, contravariantLeviCivitaTensor}, 
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
          Tuples[Range[Length[matrixRepresentation]], 4]]]; ricciTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
               Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
      ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]; 
      weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],index[[
                3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                 matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                 matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                 (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                  matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                  matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
           4]]]; mixedWeylTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*weylTensor[[#1[[1]],#1[[2]],index[[3]],
                  index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      Total[(weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],
           #1[[6]]]]*mixedWeylTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 6]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["ReducedSecondPrincipalInvariant"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor, mixedWeylTensor, leviCivitaTensor, contravariantLeviCivitaTensor}, 
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
          Tuples[Range[Length[matrixRepresentation]], 4]]]; ricciTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
               Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
      ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]; 
      weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],index[[
                3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                 matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                 matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                 (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                  matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                  matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
           4]]]; mixedWeylTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*weylTensor[[#1[[1]],#1[[2]],index[[3]],
                  index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      FullSimplify[Total[(weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],
            #1[[5]],#1[[6]]]]*mixedWeylTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 6]]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["SymbolicSecondPrincipalInvariant"] := 
  Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
     ricciTensor, ricciScalar, weylTensor, mixedWeylTensor, leviCivitaTensor, contravariantLeviCivitaTensor}, 
    If[Length[matrixRepresentation] == 4, newMatrixRepresentation = matrixRepresentation /. 
        (#1 -> ToExpression[#1] & ) /@ Select[coordinates, StringQ]; 
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
          Tuples[Range[Length[matrixRepresentation]], 4]]]; ricciTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
               Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
      ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]; 
      weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],index[[
                3]],index[[4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                 matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                 matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                 (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                  matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                  matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
           4]]]; mixedWeylTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*weylTensor[[#1[[1]],#1[[2]],index[[3]],
                  index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; leviCivitaTensor = Normal[LeviCivitaTensor[4]]; 
      contravariantLeviCivitaTensor = Normal[SparseArray[
         (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                 Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*
                 Inverse[matrixRepresentation][[#1[[4]],index[[4]]]]*leviCivitaTensor[[#1[[1]],#1[[2]],#1[[3]],
                  #1[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ 
          Tuples[Range[Length[matrixRepresentation]], 4]]]; 
      Total[(weylTensor[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]]*contravariantLeviCivitaTensor[[#1[[1]],#1[[2]],#1[[5]],
           #1[[6]]]]*mixedWeylTensor[[#1[[3]],#1[[4]],#1[[5]],#1[[6]]]] & ) /@ 
        Tuples[Range[Length[matrixRepresentation]], 6]], Indeterminate]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["MetricTensor"] := ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, 
    metricIndex1, metricIndex2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["Coordinates"] := coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["CoordinateOneForms"] := 
  (If[Head[#1] === Subscript, Subscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
      If[Head[#1] === Superscript, Superscript[StringJoin["\[FormalD]", ToString[First[#1]]], ToString[Last[#1]]], 
       StringJoin["\[FormalD]", ToString[#1]]]] & ) /@ coordinates /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && 
    BooleanQ[index4]
WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_, 
    index3_, index4_]["Symbol"] := If[index1 === True && index2 === True && index3 === True && index4 === True, 
    Subscript["\[FormalCapitalC]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && index3 === False && index4 === False, 
     Superscript["\[FormalCapitalC]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && index3 === False && index4 === False, 
      Subsuperscript["\[FormalCapitalC]", "\[FormalRho]", "\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === True && index3 === False && 
        index4 === False, Subsuperscript["\[FormalCapitalC]", "\[FormalSigma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False && 
         index3 === True && index4 === False, Subsuperscript["\[FormalCapitalC]", "\[FormalMu]", "\[FormalRho]\[FormalSigma]\[FormalNu]"], 
        If[index1 === False && index2 === False && index3 === False && index4 === True, 
         Subsuperscript["\[FormalCapitalC]", "\[FormalNu]", "\[FormalRho]\[FormalSigma]\[FormalMu]"], If[index1 === True && index2 === True && index3 === False && 
           index4 === False, Subsuperscript["\[FormalCapitalC]", "\[FormalRho]\[FormalSigma]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False && 
            index3 === True && index4 === False, Subsuperscript["\[FormalCapitalC]", "\[FormalRho]\[FormalMu]", "\[FormalSigma]\[FormalNu]"], 
           If[index1 === True && index2 === False && index3 === False && index4 === True, Subsuperscript["\[FormalCapitalC]", "\[FormalRho]\[FormalNu]", 
             "\[FormalSigma]\[FormalMu]"], If[index1 === False && index2 === True && index3 === True && index4 === False, 
             Subsuperscript["\[FormalCapitalC]", "\[FormalSigma]\[FormalMu]", "\[FormalRho]\[FormalNu]"], If[index1 === False && index2 === True && index3 === False && 
               index4 === True, Subsuperscript["\[FormalCapitalC]", "\[FormalSigma]\[FormalNu]", "\[FormalRho]\[FormalMu]"], If[index1 === False && index2 === False && 
                index3 === True && index4 === True, Subsuperscript["\[FormalCapitalC]", "\[FormalMu]\[FormalNu]", "\[FormalRho]\[FormalSigma]"], If[index1 === True && 
                 index2 === True && index3 === True && index4 === False, Subsuperscript["\[FormalCapitalC]", "\[FormalRho]\[FormalSigma]\[FormalMu]", "\[FormalNu]"], 
                If[index1 === True && index2 === True && index3 === False && index4 === True, Subsuperscript["\[FormalCapitalC]", 
                  "\[FormalRho]\[FormalSigma]\[FormalNu]", "\[FormalMu]"], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                  Subsuperscript["\[FormalCapitalC]", "\[FormalRho]\[FormalMu]\[FormalNu]", "\[FormalSigma]"], If[index1 === False && index2 === True && index3 === True && 
                    index4 === True, Subsuperscript["\[FormalCapitalC]", "\[FormalSigma]\[FormalMu]\[FormalNu]", "\[FormalRho]"], Indeterminate]]]]]]]]]]]]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
WeylTensor /: MakeBoxes[weylTensor:WeylTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
       metricIndex2_], index1_, index2_, index3_, index4_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
      ricciTensor, ricciScalar, tensorRepresentation, newTensorRepresentation, type, symbol, dimensions, eigenvalues, 
      positiveEigenvalues, negativeEigenvalues, signature, icon}, 
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
          Tuples[Range[Length[matrixRepresentation]], 4]]]; ricciTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,Last[index]]] & ) /@ 
               Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; 
      ricciScalar = Total[(Inverse[matrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
         Tuples[Range[Length[matrixRepresentation]], 2]]; tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],index[[3]],index[[
                4]]]] + (1/(Length[matrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                 matrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                 matrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                 matrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                 matrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[matrixRepresentation] - 1)*
                 (Length[matrixRepresentation] - 2)))*(ricciScalar*(matrixRepresentation[[index[[1]],index[[3]]]]*
                  matrixRepresentation[[index[[2]],index[[4]]]] - matrixRepresentation[[index[[1]],index[[4]]]]*
                  matrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
           4]]]; If[index1 === True && index2 === True && index3 === True && index4 === True, 
       newTensorRepresentation = tensorRepresentation; type = "Covariant"; symbol = Subscript["\[FormalCapitalC]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], 
       If[index1 === False && index2 === False && index3 === False && index4 === False, 
        newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> 
                Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],
                     #1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[4]],
                     index[[4]]]]*tensorRepresentation[[#1[[1]],#1[[2]],#1[[3]],#1[[4]]]] & ) /@ 
                  Tuples[Range[Length[matrixRepresentation]], 4]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
              4]]]; type = "Contravariant"; symbol = Superscript["\[FormalCapitalC]", "\[FormalRho]\[FormalSigma]\[FormalMu]\[FormalNu]"], 
        If[index1 === True && index2 === False && index3 === False && index4 === False, 
         newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                  (Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                      index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*tensorRepresentation[[index[[1]],
                      #1[[1]],#1[[2]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
              Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; 
          symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalRho]", "\[FormalSigma]\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === True && 
           index3 === False && index4 === False, newTensorRepresentation = 
            Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                      Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[3]],
                       index[[4]]]]*tensorRepresentation[[#1[[1]],index[[2]],#1[[2]],#1[[3]]]] & ) /@ 
                    Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalSigma]", "\[FormalRho]\[FormalMu]\[FormalNu]"], 
          If[index1 === False && index2 === False && index3 === True && index4 === False, 
           newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                    (Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[index[[2]],
                        #1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],index[[4]]]]*tensorRepresentation[[#1[[1]],
                        #1[[2]],index[[3]],#1[[3]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ 
                Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; 
            symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalMu]", "\[FormalRho]\[FormalSigma]\[FormalNu]"], If[index1 === False && index2 === False && 
             index3 === False && index4 === True, newTensorRepresentation = Normal[SparseArray[
                (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                        Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*Inverse[matrixRepresentation][[#1[[3]],
                         index[[3]]]]*tensorRepresentation[[#1[[1]],#1[[2]],#1[[3]],index[[4]]]] & ) /@ 
                      Tuples[Range[Length[matrixRepresentation]], 3]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                  4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalNu]", "\[FormalRho]\[FormalSigma]\[FormalMu]"], 
            If[index1 === True && index2 === True && index3 === False && index4 === False, 
             newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                      (Inverse[matrixRepresentation][[#1[[1]],index[[3]]]]*Inverse[matrixRepresentation][[#1[[2]],
                          index[[4]]]]*tensorRepresentation[[index[[1]],index[[2]],#1[[1]],#1[[2]]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                     matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalRho]\[FormalSigma]", "\[FormalMu]\[FormalNu]"], 
             If[index1 === True && index2 === False && index3 === True && index4 === False, newTensorRepresentation = 
                Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[2]],
                           #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[4]]]]*tensorRepresentation[[index[[1]],
                           #1[[1]],index[[3]],#1[[2]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                   Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", 
                 "\[FormalRho]\[FormalMu]", "\[FormalSigma]\[FormalNu]"], If[index1 === True && index2 === False && index3 === False && index4 === True, 
               newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                        (Inverse[matrixRepresentation][[index[[2]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                            index[[3]]]]*tensorRepresentation[[index[[1]],#1[[1]],#1[[2]],index[[4]]]] & ) /@ 
                         Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                       matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalRho]\[FormalNu]", "\[FormalSigma]\[FormalMu]"], 
               If[index1 === False && index2 === True && index3 === True && index4 === False, 
                newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                         (Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],
                             index[[4]]]]*tensorRepresentation[[#1[[1]],index[[2]],index[[3]],#1[[2]]]] & ) /@ 
                          Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[
                        matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalSigma]\[FormalMu]", "\[FormalRho]\[FormalNu]"], 
                If[index1 === False && index2 === True && index3 === False && index4 === True, newTensorRepresentation = 
                   Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],
                              #1[[1]]]]*Inverse[matrixRepresentation][[#1[[2]],index[[3]]]]*tensorRepresentation[[#1[[1]],
                              index[[2]],#1[[2]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
                            2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; 
                  symbol = Subsuperscript["\[FormalCapitalC]", "\[FormalSigma]\[FormalNu]", "\[FormalRho]\[FormalMu]"], If[index1 === False && index2 === False && 
                   index3 === True && index4 === True, newTensorRepresentation = Normal[SparseArray[
                      (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[index[[1]],#1[[1]]]]*
                              Inverse[matrixRepresentation][[index[[2]],#1[[2]]]]*tensorRepresentation[[#1[[1]],#1[[2]],
                               index[[3]],index[[4]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
                       Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", 
                     "\[FormalMu]\[FormalNu]", "\[FormalRho]\[FormalSigma]"], If[index1 === True && index2 === True && index3 === True && index4 === False, 
                   newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                            (Inverse[matrixRepresentation][[#1,index[[4]]]]*tensorRepresentation[[index[[1]],index[[2]],
                                index[[3]],#1]] & )/Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[
                          Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", 
                      "\[FormalRho]\[FormalSigma]\[FormalMu]", "\[FormalNu]"], If[index1 === True && index2 === True && index3 === False && index4 === True, 
                    newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                             (Inverse[matrixRepresentation][[#1,index[[3]]]]*tensorRepresentation[[index[[1]],index[[2]],
                                 #1,index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[
                          Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript["\[FormalCapitalC]", 
                       "\[FormalRho]\[FormalSigma]\[FormalNu]", "\[FormalMu]"], If[index1 === True && index2 === False && index3 === True && index4 === True, 
                     newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> Total[
                              (Inverse[matrixRepresentation][[index[[2]],#1]]*tensorRepresentation[[index[[1]],#1,
                                  index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                          Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript[
                        "\[FormalCapitalC]", "\[FormalRho]\[FormalMu]\[FormalNu]", "\[FormalSigma]"], If[index1 === False && index2 === True && index3 === True && 
                       index4 === True, newTensorRepresentation = Normal[SparseArray[(Module[{index = #1}, index -> 
                              Total[(Inverse[matrixRepresentation][[index[[1]],#1]]*tensorRepresentation[[#1,index[[2]],
                                   index[[3]],index[[4]]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
                           Tuples[Range[Length[matrixRepresentation]], 4]]]; type = "Mixed"; symbol = Subsuperscript[
                         "\[FormalCapitalC]", "\[FormalSigma]\[FormalMu]\[FormalNu]", "\[FormalRho]"], newTensorRepresentation = ConstantArray[Indeterminate, 
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
        Frame -> False, FrameTicks -> None]; BoxForm`ArrangeSummaryBox["WeylTensor", weylTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     BooleanQ[index1] && BooleanQ[index2] && BooleanQ[index3] && BooleanQ[index4]
