(* ::Package:: *)

BachTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_]] := 
  BachTensor[ResourceFunction["MetricTensor"][matrixRepresentation, coordinates, index1, index2], True, True] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
BachTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, index1_, index2_], newCoordinates_List] := 
  BachTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, index1, index2], True, True] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[index1] && BooleanQ[index2]
BachTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], 
   newCoordinates_List, index1_, index2_] := 
  BachTensor[ResourceFunction["MetricTensor"][matrixRepresentation /. Thread[coordinates -> newCoordinates], 
     newCoordinates, metricIndex1, metricIndex2], index1, index2] /; SymbolName[metricTensor] === "MetricTensor" && 
    Length[Dimensions[matrixRepresentation]] == 2 && Length[coordinates] == Length[matrixRepresentation] && 
    Length[newCoordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
BachTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "MatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     covariantRiemannTensor, ricciTensor, ricciScalar, weylTensor, mixedWeylTensor, schoutenTensor, covariantDerivatives, 
     bachTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
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
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[newMatrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                newMatrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                newMatrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                newMatrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                newMatrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[newMatrixRepresentation] - 1)*
                (Length[newMatrixRepresentation] - 2)))*(ricciScalar*(newMatrixRepresentation[[index[[1]],index[[3]]]]*
                 newMatrixRepresentation[[index[[2]],index[[4]]]] - newMatrixRepresentation[[index[[1]],index[[4]]]]*
                 newMatrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; mixedWeylTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[newMatrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],#1[[1]],index[[3]],
                 #1[[2]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; schoutenTensor = (1/(Length[newMatrixRepresentation] - 2))*
       (ricciTensor - (ricciScalar/(2*(Length[newMatrixRepresentation] - 1)))*newMatrixRepresentation); 
     covariantDerivatives = Normal[SparseArray[
        (Module[{index = #1}, index -> D[schoutenTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
             Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*schoutenTensor[[#1,index[[3]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                 schoutenTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     bachTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(schoutenTensor[[First[#1],Last[#1]]]*
                  mixedWeylTensor[[First[index],First[#1],Last[index],Last[#1]]] & ) /@ Tuples[
                 Range[Length[newMatrixRepresentation]], 2]] + Total[(Module[{nestedIndex = #1}, 
                  Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*(D[covariantDerivatives[[
                      First[nestedIndex],First[index],Last[index]]], newCoordinates[[Last[nestedIndex]]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*covariantDerivatives[[#1,
                         First[index],Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],First[index]]]*covariantDerivatives[[
                         First[nestedIndex],#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],Last[index]]]*covariantDerivatives[[First[
                          nestedIndex],First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 2]] - Total[(Module[{nestedIndex = #1}, 
                  Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*(D[covariantDerivatives[[
                      First[index],Last[index],First[nestedIndex]]], newCoordinates[[Last[nestedIndex]]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],First[index]]]*covariantDerivatives[[#1,Last[index],
                         First[nestedIndex]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                     (christoffelSymbols[[#1,Last[nestedIndex],Last[index]]]*covariantDerivatives[[First[index],#1,
                         First[nestedIndex]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                     (christoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*covariantDerivatives[[First[index],
                         Last[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; If[index1 === True && index2 === True, 
      bachTensor, If[index1 === False && index2 === False, 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],First[#1]]]*
                 Inverse[matrixRepresentation][[Last[#1],Last[index]]]*bachTensor[[First[#1],Last[#1]]] & ) /@ Tuples[
                Range[Length[matrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]], 
       If[index1 === True && index2 === False, Normal[SparseArray[
          (Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,Last[index]]]*bachTensor[[First[index],
                   #1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 
            2]]], If[index1 === False && index2 === True, 
         Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],#1]]*
                   bachTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
BachTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, metricIndex2_], index1_, index2_][
   "ReducedMatrixRepresentation"] := Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, 
     covariantRiemannTensor, ricciTensor, ricciScalar, weylTensor, mixedWeylTensor, schoutenTensor, covariantDerivatives, 
     bachTensor}, newMatrixRepresentation = matrixRepresentation /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; newCoordinates = coordinates /. (#1 -> ToExpression[#1] & ) /@ 
        Select[coordinates, StringQ]; christoffelSymbols = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[((1/2)*Inverse[newMatrixRepresentation][[index[[1]],#1]]*
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
     ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
     ricciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],Last[#1]]] & ) /@ 
        Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
     weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],
              index[[3]],index[[4]]]] + (1/(Length[newMatrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                newMatrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                newMatrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                newMatrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                newMatrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[newMatrixRepresentation] - 1)*
                (Length[newMatrixRepresentation] - 2)))*(ricciScalar*(newMatrixRepresentation[[index[[1]],index[[3]]]]*
                 newMatrixRepresentation[[index[[2]],index[[4]]]] - newMatrixRepresentation[[index[[1]],index[[4]]]]*
                 newMatrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; mixedWeylTensor = 
      Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*
                Inverse[newMatrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],#1[[1]],index[[3]],
                 #1[[2]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 4]]]; schoutenTensor = (1/(Length[newMatrixRepresentation] - 2))*
       (ricciTensor - (ricciScalar/(2*(Length[newMatrixRepresentation] - 1)))*newMatrixRepresentation); 
     covariantDerivatives = Normal[SparseArray[
        (Module[{index = #1}, index -> D[schoutenTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
             Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*schoutenTensor[[#1,index[[3]]]] & ) /@ Range[
                Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                 schoutenTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
         Tuples[Range[Length[newMatrixRepresentation]], 3]]]; 
     bachTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(schoutenTensor[[First[#1],Last[#1]]]*
                  mixedWeylTensor[[First[index],First[#1],Last[index],Last[#1]]] & ) /@ Tuples[
                 Range[Length[newMatrixRepresentation]], 2]] + Total[(Module[{nestedIndex = #1}, 
                  Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*(D[covariantDerivatives[[
                      First[nestedIndex],First[index],Last[index]]], newCoordinates[[Last[nestedIndex]]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*covariantDerivatives[[#1,
                         First[index],Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],First[index]]]*covariantDerivatives[[
                         First[nestedIndex],#1,Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],Last[index]]]*covariantDerivatives[[First[
                          nestedIndex],First[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 2]] - Total[(Module[{nestedIndex = #1}, 
                  Inverse[newMatrixRepresentation][[First[nestedIndex],Last[nestedIndex]]]*(D[covariantDerivatives[[
                      First[index],Last[index],First[nestedIndex]]], newCoordinates[[Last[nestedIndex]]]] - 
                    Total[(christoffelSymbols[[#1,Last[nestedIndex],First[index]]]*covariantDerivatives[[#1,Last[index],
                         First[nestedIndex]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                     (christoffelSymbols[[#1,Last[nestedIndex],Last[index]]]*covariantDerivatives[[First[index],#1,
                         First[nestedIndex]]] & ) /@ Range[Length[newMatrixRepresentation]]] - Total[
                     (christoffelSymbols[[#1,Last[nestedIndex],First[nestedIndex]]]*covariantDerivatives[[First[index],
                         Last[index],#1]] & ) /@ Range[Length[newMatrixRepresentation]]])] & ) /@ 
                Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
           2]]] /. (ToExpression[#1] -> #1 & ) /@ Select[coordinates, StringQ]; If[index1 === True && index2 === True, 
      FullSimplify[bachTensor], If[index1 === False && index2 === False, 
       FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                   First[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*bachTensor[[First[#1],
                   Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
           Tuples[Range[Length[matrixRepresentation]], 2]]]], If[index1 === True && index2 === False, 
        FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                    Last[index]]]*bachTensor[[First[index],#1]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
            Tuples[Range[Length[matrixRepresentation]], 2]]]], If[index1 === False && index2 === True, 
         FullSimplify[Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                     First[index],#1]]*bachTensor[[#1,Last[index]]] & ) /@ Range[Length[matrixRepresentation]]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]]], Indeterminate]]]]] /; 
   SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
    Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
    BooleanQ[index1] && BooleanQ[index2]
BachTensor /: MakeBoxes[bachTensor:BachTensor[(metricTensor_)[matrixRepresentation_List, coordinates_List, metricIndex1_, 
       metricIndex2_], index1_, index2_], format_] := 
   Module[{newMatrixRepresentation, newCoordinates, christoffelSymbols, riemannTensor, covariantRiemannTensor, 
      ricciTensor, ricciScalar, weylTensor, mixedWeylTensor, schoutenTensor, covariantDerivatives, tensorRepresentation, 
      matrixForm, type, symbol, dimensions, eigenvalues, positiveEigenvalues, negativeEigenvalues, signature, icon}, 
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
          Tuples[Range[Length[newMatrixRepresentation]], 4]]]; 
      ricciTensor = Normal[SparseArray[(Module[{index = #1}, index -> Total[(riemannTensor[[#1,First[index],#1,
                 Last[index]]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 2]]]; 
      ricciScalar = Total[(Inverse[newMatrixRepresentation][[First[#1],Last[#1]]]*ricciTensor[[First[#1],
            Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]; 
      weylTensor = Normal[SparseArray[(Module[{index = #1}, index -> covariantRiemannTensor[[index[[1]],index[[2]],index[[
                3]],index[[4]]]] + (1/(Length[newMatrixRepresentation] - 2))*(ricciTensor[[index[[1]],index[[4]]]]*
                 newMatrixRepresentation[[index[[2]],index[[3]]]] - ricciTensor[[index[[1]],index[[3]]]]*
                 newMatrixRepresentation[[index[[2]],index[[4]]]] + ricciTensor[[index[[2]],index[[3]]]]*
                 newMatrixRepresentation[[index[[1]],index[[4]]]] - ricciTensor[[index[[2]],index[[4]]]]*
                 newMatrixRepresentation[[index[[1]],index[[3]]]]) + (1/((Length[newMatrixRepresentation] - 1)*
                 (Length[newMatrixRepresentation] - 2)))*(ricciScalar*(newMatrixRepresentation[[index[[1]],index[[3]]]]*
                  newMatrixRepresentation[[index[[2]],index[[4]]]] - newMatrixRepresentation[[index[[1]],index[[4]]]]*
                  newMatrixRepresentation[[index[[2]],index[[3]]]]))] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]]; mixedWeylTensor = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[newMatrixRepresentation][[index[[2]],#1[[1]]]]*
                 Inverse[newMatrixRepresentation][[#1[[2]],index[[4]]]]*weylTensor[[index[[1]],#1[[1]],index[[3]],
                  #1[[2]]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 4]]]; schoutenTensor = (1/(Length[newMatrixRepresentation] - 2))*
        (ricciTensor - (ricciScalar/(2*(Length[newMatrixRepresentation] - 1)))*newMatrixRepresentation); 
      covariantDerivatives = Normal[SparseArray[
         (Module[{index = #1}, index -> D[schoutenTensor[[index[[2]],index[[3]]]], newCoordinates[[index[[1]]]]] - 
              Total[(christoffelSymbols[[#1,index[[1]],index[[2]]]]*schoutenTensor[[#1,index[[3]]]] & ) /@ 
                Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,index[[1]],index[[3]]]]*
                  schoutenTensor[[index[[2]],#1]] & ) /@ Range[Length[newMatrixRepresentation]]]] & ) /@ 
          Tuples[Range[Length[newMatrixRepresentation]], 3]]]; tensorRepresentation = 
       Normal[SparseArray[(Module[{index = #1}, index -> Total[(schoutenTensor[[First[#1],Last[#1]]]*mixedWeylTensor[[
                    First[index],First[#1],Last[index],Last[#1]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                  2]] + Total[(Module[{nestedIndex = #1}, Inverse[newMatrixRepresentation][[First[nestedIndex],
                     Last[nestedIndex]]]*(D[covariantDerivatives[[First[nestedIndex],First[index],Last[index]]], 
                      newCoordinates[[Last[nestedIndex]]]] - Total[(christoffelSymbols[[#1,Last[nestedIndex],
                          First[nestedIndex]]]*covariantDerivatives[[#1,First[index],Last[index]]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,Last[nestedIndex],
                          First[index]]]*covariantDerivatives[[First[nestedIndex],#1,Last[index]]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,Last[nestedIndex],
                          Last[index]]]*covariantDerivatives[[First[nestedIndex],First[index],#1]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                  2]] - Total[(Module[{nestedIndex = #1}, Inverse[newMatrixRepresentation][[First[nestedIndex],
                     Last[nestedIndex]]]*(D[covariantDerivatives[[First[index],Last[index],First[nestedIndex]]], 
                      newCoordinates[[Last[nestedIndex]]]] - Total[(christoffelSymbols[[#1,Last[nestedIndex],
                          First[index]]]*covariantDerivatives[[#1,Last[index],First[nestedIndex]]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,Last[nestedIndex],
                          Last[index]]]*covariantDerivatives[[First[index],#1,First[nestedIndex]]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]] - Total[(christoffelSymbols[[#1,Last[nestedIndex],
                          First[nestedIndex]]]*covariantDerivatives[[First[index],Last[index],#1]] & ) /@ 
                       Range[Length[newMatrixRepresentation]]])] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 
                  2]]] & ) /@ Tuples[Range[Length[newMatrixRepresentation]], 2]]] /. (ToExpression[#1] -> #1 & ) /@ 
         Select[coordinates, StringQ]; If[index1 === True && index2 === True, matrixForm = tensorRepresentation; 
        type = "Covariant"; symbol = Subscript["\[FormalCapitalB]", "\[FormalMu]\[FormalNu]"], If[index1 === False && index2 === False, 
        matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[First[index],
                     Last[#1]]]*Inverse[matrixRepresentation][[Last[#1],Last[index]]]*tensorRepresentation[[First[#1],
                     Last[#1]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]] & ) /@ 
             Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Contravariant"; 
         symbol = Superscript["\[FormalCapitalB]", "\[FormalMu]\[FormalNu]"], If[index1 === True && index2 === False, 
         matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[#1,
                      Last[index]]]*tensorRepresentation[[First[index],#1]] & ) /@ Range[Length[
                     matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
          symbol = Subsuperscript["\[FormalCapitalB]", "\[FormalMu]", "\[FormalNu]"], If[index1 === False && index2 === True, 
          matrixForm = Normal[SparseArray[(Module[{index = #1}, index -> Total[(Inverse[matrixRepresentation][[
                       First[index],#1]]*tensorRepresentation[[#1,Last[index]]] & ) /@ Range[Length[
                      matrixRepresentation]]]] & ) /@ Tuples[Range[Length[matrixRepresentation]], 2]]]; type = "Mixed"; 
           symbol = Subsuperscript["\[FormalCapitalB]", "\[FormalNu]", "\[FormalMu]"], matrixForm = ConstantArray[Indeterminate, 
             {Length[matrixRepresentation], Length[matrixRepresentation]}]; type = Indeterminate; 
           symbol = Indeterminate]]]]; dimensions = Length[matrixRepresentation]; 
      eigenvalues = Eigenvalues[matrixRepresentation]; positiveEigenvalues = Select[eigenvalues, #1 > 0 & ]; 
      negativeEigenvalues = Select[eigenvalues, #1 < 0 & ]; 
      If[Length[positiveEigenvalues] + Length[negativeEigenvalues] == Length[matrixRepresentation], 
       If[Length[positiveEigenvalues] == Length[matrixRepresentation] || Length[negativeEigenvalues] == 
          Length[matrixRepresentation], signature = "Riemannian", If[Length[positiveEigenvalues] == 1 || 
          Length[negativeEigenvalues] == 1, signature = "Lorentzian", signature = "Pseudo-Riemannian"]], 
       signature = Indeterminate]; icon = MatrixPlot[matrixForm, ImageSize -> 
         Dynamic[{Automatic, 3.5*(CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}], Frame -> False, 
        FrameTicks -> None]; BoxForm`ArrangeSummaryBox["BachTensor", bachTensor, icon, 
       {{BoxForm`SummaryItem[{"Type: ", type}], BoxForm`SummaryItem[{"Symbol: ", symbol}]}, 
        {BoxForm`SummaryItem[{"Dimensions: ", dimensions}], BoxForm`SummaryItem[{"Signature: ", signature}]}}, 
       {{BoxForm`SummaryItem[{"Coordinates: ", coordinates}]}}, format, "Interpretable" -> Automatic]] /; 
    SymbolName[metricTensor] === "MetricTensor" && Length[Dimensions[matrixRepresentation]] == 2 && 
     Length[coordinates] == Length[matrixRepresentation] && BooleanQ[metricIndex1] && BooleanQ[metricIndex2] && 
     BooleanQ[index1] && BooleanQ[index2]
