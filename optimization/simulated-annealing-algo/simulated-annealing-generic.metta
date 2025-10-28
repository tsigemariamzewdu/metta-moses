
; Calculates the difference between two composite scores.
(= (cDelta $c1 $c2)
  (let*
    (
      (($pl $pr $cl $cr) ((getPenScore $cs1) (getPenScore $cs2) (getComp $cs1) (getComp $cs2)))
      ($pD (- $pl $pr))
      ($cD (- $cl $cr))
      ($wD (+ (* 0.5 $pD) (* 0.5 $cD)))
    )
     $wD
  )
)


;; Calculates instance energy using the composite score of that instance (
;;  (this is based on the C++ version) the composite score is the sum of
;;  the bScore and the penalized score.
(: getEnergy (-> Cscore Number))
(= (getEnergy $cScore)
  (let $pScore (getPenScore $cScore) (* -1 $pScore))
)

;; Acceptance probability which is based on the energy of the current and new instance, and the temperature.
(: acceptanceProb (-> Number Number Number Number))
(= (acceptanceProb $energy1 $energy2 $Temp)
  (if (< $energy2 $energy1)
    1.0
    (math-exp (/ (- $energy1 $energy2) $Temp))
  )
)

;;helper functions
(= (extractInstanceFromPOA (mkDeme $rep $instSet $id))$instSet)

(= (extractRepFromPOA (mkDeme $rep $instSet $id))$rep)

(= (extractDiscFromPOA (mkDeme (mkRep (mkKbMap (mkDscKbMp $idxMp) (mkDscMp $disc)) $tree) $instSet $id))($disc))

(= (extractCpxCoeffFromTable (mkTruthTableBScore $cpxCoeff $size $iTable)) $cpxCoeff )

(= (extractLabelFromTable (mkITable $rows $labels))($labels))





(=(simulated-annealing $POA $table $initCenter $hyperParams $scorerType )

  (if (== $scorerType mi );; if the scorer type is mi do feature selection optimization
      ( (let * (

          ($instSet $POA)
          ($iTable $table)
          ))

    (println! "Running feature selection optimization...")
    ; call unified SA loop
        (simulated-annealing-generic
          $instSet
          $iTable
          $initCenter 
          (3 3 1) 
          $hyperParams 
          $scorerParams 
          $scorerType
          (False False $centerInst 0 (* -1 (pow-math 10 308)) 0 0 1)
          estimateNeighborhoodFs
          crossoverFs 
          sampleNewInstancesFs 
          transformFs

        ;; else part for general optimization
        ))
        ((let * (
 
        ($deme $POA)
        ($TruthTableBScore $table)
        )
        )
  
        (println! "Running general optimization...")
        ; call unified SA loop
        (simulated-annealing-generic 
        $deme  
        $TruthTableBScore  
        $initCenter  
        (1 3 0) ;;$constparams ($maxDist $minXoverNeighbors $bestPossibleScore)
        $hyperParams ;;($Temp $finalTemp $coolRate $isInit $maxIter)
        () ;;$scorerParams
        () ; $scorerType empty for general optimization
        (False False $initCenter (worstCscore) (* -1 (pow-math 10 308)) 0 0 1) ;;($alreadyXover $lastChance $prevCenter $bestCScore $bestScore $currentNInstances $d $i) )) ;;$params
        estimateNeighborhood  
        transform  
        crossover
        sampleNewInstances
        )))

;;simulated annealing generic 
(: simulated-annealing-generic (-> Deme (TruthTableBScore $a) Instance $constParams $hyperParams  $scorerParams $scorerType $estimateneighborhoodFn $transformfn
                              $sampleNewInstancesfn $crossoverfn $$params (Instance Deme $state)))
( =(simulated-annealing-generic $POA
                                $table
                                $initCenter
                                ($maxDist $minXoverNeighbors $bestPossibleScore)
                                ($Temp $finalTemp $coolRate $isInit $maxIter)
                                ($miConfi $prePenalty $preMinActivation $preMaxActivation $prePositive)
                                $scorerType
                                ($alreadyXover $lastChance $prevCenter $bestRawOrCScore $bestScore $currentNInstances $d $i)
                                $estimateneighborhoodfn
                                $transformfn
                                $sampleNewInstancesfn
                                $crossoverfn

                            )
    (if (or (<= $Temp $finalTemp) (<= $maxIter 0))
          ($prevCenter  $POA ($prevCenter $bestRawOrCScore $bestScore $currentNInstances $d $i))

          ;;continue search
          (let* (
            (()(println! (Iteration : $i, Temperature : $Temp)))
            (() (println! ""))

            ($isFS (== $crossoverFn crossoverFs)) ;;flag to detect feature selection

            ($estimateVar (if $isFS 
                  (($labels (extractLabelFromTable $table)))
                  (($disc (extractDiscFromPOA $POA)))))

          ;;neighborhood estimation

          ($totalNNeighbors ($estimateneighborhoodfn $d $estimateVar))
          (() (println! (Estimated neighbors: $totalNNeighbors)))
          (() (println! ""))

          ($nNewNeighbors (min $totalNNeighbors 2))
          ($largeNbh (>= $totalNNeighbors $minXoverNeighbors))
          ($xOver (and (> $i 2) (and (not $alreadyXover) (or $largeNbh $lastChance))))


          ;;instance generation
          (($updatedPOA $newInstances) (if $isFS 
                                                (if $xOver
                                                  ($crossoverfn $currentNInstances $nNewNeighbors $prevCenter $POA)
                                                  ($sampleNewInstancesfn $totalNNeighbors $nNewNeighbors $prevCenter $table $POA $d
                                                ))
                                                ( if $xOver
                                                  ($crossoverfn $currentNInstances $nNewNeighbors $prevCenter $POA)
                                                  ($sampleNewInstancesfn $totalNNeighbors $nNewNeighbors $prevCenter $POA $d)
                                                )))
          
          (($updatedInstSet) (if $isFS 
                                    ($updatedPOA)
                                    (extractInstancesFromPOA $updatedPOA)
          ))


          (() (println! (New instances: $updatedInstSet)))
          (() (println! ""))

          ($repOrItable (if $isFS 
                              $table
                              (extractRepFromPOA $updatedPOA)
          ))
          ($iTableOrScorerParams (if $isFS 
                                      ($miConfi $prePenalty $preMinActivation $preMaxActivation $prePositive)
                                      $table
          ))
          ($cpxCoeffOrscorerType (if $isFS
                                      $scorerType
                                      (extractCpxCoeffFromTable $table)
          ))
         
          ;;score the new instance
          ((mkSInstSet $scoredInstances) $transformFn $updatedInstSet $repOrItable  $iTableOrScorerParams $cpxCoeffOrscorerType))
                                                    

          (() (println! (Scored instances: $scoredInstances)))
          (() (println! ""))
              
              
              ;;find best 
              ((mkSInst $newBestInstPair)
              (List.foldl ((curry2 returnBest) <)
                          (mkSInst (mkPair $initCenter $bestRawOrCScore))
                              $scoredInstances))

              
              ($newBestScore (if ($isFS 
                                (Pair.second $newBestInstPair )
                                (getPenScore (Pair.second $newBestInstPair)))
              ($hasImproved (> $newBestScore (+ $bestScore 0.5)))

              (() (println! (New best instance: $newBestInstPair)))
              (() (println! (Has improved: $hasImproved)))
              (() (println! ""))

              ;; energy calculation
              ($energy (if $isFS (* -1 $bestRawOrCScore )(getEnergy $bestRawOrCScore)))
              ($newEnergy (if $isFS (* -1 (Pair.second $newBestInstPair) (getEnergy (Pair.second $newBestInstPair)))
              ($acceptanceProb (acceptanceProb $energy $newEnergy $Temp))
              ($randomFloat (rand-random))
              ($actualAcceptanceProb (* $acceptanceProb 0.85))
              ($isAccepted (> $actualAcceptanceProb $randomFloat))

              (() (println! (Energy: $energy)))
              (() (println! (New energy: $newEnergy)))
              (() (println! (Acceptance probability: $acceptanceProb)))
              (() (println! (Random float: $randomFloat)))
              (() (println! (Accepted: $isAccepted)))
              (() (println! ""))

              ;;update state
              ($newDeme (if $isAccepted $updatedDeme $deme))
              ($newCenter (if $isAccepted (Pair.first $newBestInstPair) $prevCenter))
              ($newBestCScore (if $isAccepted (Pair.second $newBestInstPair) $bestRawOrCScore))
              ($newBestScore (if $isAccepted $newBestScore $bestScore))
              ($newDistance (if $hasImproved 1 (if (not $xOver) (+ 1 $d) $d)))
              ($updatedNumInstances (+ $currentNInstances $newInstances))
              ($newTemp (- $Temp (* $coolRate $Temp)))
              ($newIterCount (- $maxIter 1))
            )

            ;; recursive call
            (simulated-annealing-generic 
              $newDeme $table $newCenter
              ($maxDist $minXoverNeighbors $bestPossibleScore)
              ($newTemp $finalTemp $coolRate False $newIterCount)
              ($miConfi $prePenalty $preMinActivation $preMaxActivation $prePositive)
              $scorerType
              ((and $xOver (not $hasImproved)) $lastChance 
                $newCenter $newBestCScore $newBestScore 
                $updatedNumInstances $newDistance (+ 1 $i))
              ($estimateNeighborhoodFn $estimateVar)
              $transformFn
              $sampleNewInstancesFn
              $crossoverFn)
                


              )))
            ))))