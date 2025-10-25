
; (= (scoreInstancesGeneric 
;       $transformFn             ;; could be transform or transformFs
;       $updatedInstSet
;       $rep
;       $table
;       $cpxCoeff
;       $miConfi
;       $prePenalty
;       $preMinActivation
;       $preMaxActivation
;       $prePositive
;       $scorerType)

;   (if (== $transformFn transform)
;       ;; GENERAL OPTIMIZATION
;       (let* (
;         ((mkSInstSet $scoredInstances)
;           ($transformFn $updatedInstSet $rep $table $cpxCoeff))
;         ($updatedDeme (mkDeme $rep (mkSInstSet $scoredInstances) $id))
;       )
;         (println! "Scored instances (general):" $scoredInstances)
;         ($updatedDeme (mkSInstSet $scoredInstances)))

;       ;;  FEATURE SELECTION
;       (let* (
;         ((mkSInstSet $scoredInstances)
;           ($transformFn $updatedInstSet $table 
;                         $miConfi $prePenalty $preMinActivation 
;                         $preMaxActivation $prePositive $scorerType))
;         ($updatedInstSetScored (mkSInstSet $scoredInstances))
;       )
;         (println! "Scored feature subsets (FS):" $scoredInstances)
;         ($updatedInstSetScored (mkSInstSet $scoredInstances)))
;   )
; )




(=(simulated-annealing ($idxMp $disc $tree $instSet $id) ($cpxCoeff $size $iTable) $initCenter $hyperParams )
             (let * (
              ($deme (mkDeme (mkRep (mkKbMap (mkDscKbMp $idxMp) (mkDscMp $disc)) $tree) $instSet $id))
              ($table (mkTruthTableBScore $cpxCoeff $size $iTable))
              ($estimateVar ($disc)))

             )

             (println! "Running general optimization...")
        ; call unified SA loop
        (simulated-annealing-generic 
          $deme  ;;deme
          $table  ;;table
          $initCenter  ;;initcenter
          (1 3 0) ;;$constparams ($maxDist $minXoverNeighbors $bestPossibleScore)
          $hyperParams ;;($Temp $finalTemp $coolRate $isInit $maxIter)
          () ;;$scorerParams
          () ; $scorerType empty for general optimization
          (False False $initCenter (worstCscore) (* -1 (pow-math 10 308)) 0 0 1) ;;($alreadyXover $lastChance $prevCenter $bestCScore $bestScore $currentNInstances $d $i) )) ;;$params
          (estimateNeighborhood $estimateVar)  ;; estimateneighborhood with its one argument
          (transform  $iTable $cpxCoeff)
          crossover
          sampleNewInstances
        ))
          
(=(simulated-annealing $instSet  ($rows $labels) $initCenter $hyperParams)

      (let* (
        ($deme $instSet)
        ($table (mkITable $rows $labels))
        ($estimateVar $labels)
      ))
        
      
        (println! "Running feature selection SA...")
        ; call unified SA loop
        (simulated-annealing-generic 
          $deme ;;instset
          $table  ;;itable
          $initCenter 
          (3 3 1) 
          $hyperParams 
          $scorerParams 
          $scorerType
          (False False $centerInst 0 (* -1 (pow-math 10 308)) 0 0 1)
          (estimateNeighborhoodFs  $estimateVar)
          crossoverFs 
          sampleNewInstancesFs 
          (transformFs $scorerParams $scorerType) ))  

;;simulated annealing generic 
(: simulated-annealing-generic (-> Deme (TruthTableBScore $a) Instance $constParams $hyperParams  $scorerParams $scorerType $estimateneighborhoodFn $transformfn
                              $sampleNewInstancesfn $crossoverfn $$params (Instance Deme $state)))
( =(simulated-annealing-generic  $deme
                              $table
                              $initCenter
                              ($maxDist $minXoverNeighbors $bestPossibleScore)
                              ($Temp $finalTemp $coolRate $isInit $maxIter)
                              ($miConfi $prePenalty $preMinActivation $preMaxActivation $prePositive)
                              $scorerType
                              ($alreadyXover $lastChance $prevCenter $bestRawOrCScore $bestScore $currentNInstances $d $i)
                              ($estimateneighborhoodfn $estimateVar)
                              ($transformfn $cpxCoeffOrscorerParams $iTableOrScorerType)
                              $sampleNewInstancesfn
                              $crossoverfn

                            )
    (if (or (<= $Temp $finalTemp) (<= $maxIter 0))
          ($prevCenter  $deme ($prevCenter $bestRawOrCScore $bestScore $currentNInstances $d $i))

          ;;continue search
          (let* (
            (()(println! (Iteration : $i, Temperature : $Temp)))
            (() (println! ""))

          ;;neighborhood estimation

          ($totalNNeighbors ($estimateneighborhoodfn $d $estimateVar))
          (() (println! (Estimated neighbors: $totalNNeighbors)))
          (() (println! ""))

          ($nNewNeighbors (min $totalNNeighbors 2))
          ($largeNbh (>= $totalNNeighbors $minXoverNeighbors))
          ($xOver (and (> $i 2) (and (not $alreadyXover) (or $largeNbh $lastChance))))

          ;;instance generation

           (if (== $crossoverFn crossoverFs) ($updatedDeme (mkDeme $rep $updatedInstSet $id))($updatedDeme $updatedInstSet))
           (($updatedDeme $newInstances) 
              (if $xOver
                ($crossoverFn $currentNInstances $nNewNeighbors $prevCenter $deme)
                ($sampleNewInstancesfn $totalNNeighbors $nNewNeighbors $prevCenter $table $deme $d))) ;;i could not find definition for sample new instances


              (() (println! (New feature subsets / instancnes : $updatedDeme)))
              (() (println! ""))

              ;;score the new instance
              
              (if (== $crossoverFn crossoverFs) (
                ((mkSInstSet $scoredInstances) ($transformFn $updatedDeme  $table $cpxCoeffOrscorerParams $iTableOrScorerType))
                ($updatedDemeScored (mkSInstSet $scoredInstances))
              )((mkSInstSet $scoredInstances) ($transformFn $updatedDeme $rep  $table $cpxCoeffOrscorerParams $iTableOrScorerType))
                  ($updatedDemeScored (mkDeme $rep (mkSInstSet $scoredInstances) $id))
              )
               
              (() (println! (Scored instances: $scoredInstances)))
              (() (println! ""))
              
              
              ;;find best 
              ((mkSInst $newBestInstPair)
              (List.foldl ((curry2 returnBest) <)
                          (mkSInst (mkPair $initCenter $bestRawOrCScore))
                              $scoredInstances))

              ($newBestScore (getPenScore (Pair.second $newBestInstPair)))
              ($hasImproved (> $newBestScore (+ $bestScore 0.5)))

              (() (println! (New best instance: $newBestInstPair)))
              (() (println! (Has improved: $hasImproved)))
              (() (println! ""))

              ;; energy calculation
              ($energy (getEnergy $bestRawOrCScore))
              ($newEnergy (getEnergy (Pair.second $newBestInstPair)))
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
                


              ))
    )
        

