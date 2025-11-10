;;(: Pair (-> $a $b Type)) 
;;(: mkPair (-> $a $b (Pair $a $b)))

;;(: Pair.first (-> (Pair $a $b) $a))
(= (Pair.first ($a $b)) $a)

;;(: Pair.second (-> (Pair $a $b) $b))
(= (Pair.second ($a $b)) $b)


