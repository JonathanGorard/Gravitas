(* ::Package:: *)

RiemannTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  RiemannTensor[MetricTensor[matrixRepresentation, coordinates, index1, index2], False, True, True, True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
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
