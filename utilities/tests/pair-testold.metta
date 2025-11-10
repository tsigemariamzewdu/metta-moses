!(import! &self ../pair)
!(import! &self ../general-helpers)

;; Pair type tests 
!(assertEqual (let $a ( 1 2) (get-type $a)) ( Number Number))
!(assertEqual (let $a ( "Hello" 2) (get-type $a)) ( String Number))



;; Pair helper function tests
!(assertEqual (Pair.first (1 2)) 1)
!(assertEqual (Pair.second (1 2)) 2)

;; works in mettalog with no type checking
;!(assertEqual (Pair.first (Pair.second ( ( 2 3) 2))) (Pair.first 2))  -> gives ()
!(assertEqual (Pair.first (Pair.first ( ( 2 3) 2))) 2)