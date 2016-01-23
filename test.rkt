#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place your Personal Identification here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define puzz06 (read-puzzle "puzzle06.txt"))
(define puzz10 (read-puzzle "puzzle10.txt"))
(define puzz04 (read-puzzle "puzzle04.txt"))
(define puzz03 (read-puzzle "puzzle03.txt"))
(define puzz08 (read-puzzle "puzzle08.txt"))
(define puzz07 (read-puzzle "puzzle07.txt"))
(define puzz09 (read-puzzle "puzzle09.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))
(define grid-abcd '((#\A #\B #\D) (#\i #\y #\l) (#\l #\j #\m)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose g) produces a grid where the firsts of each row
;; are in a new row
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))
(check-expect (transpose grid-abcd) '((#\A #\i #\l) (#\B #\y #\j)
                                                    (#\D #\l #\m)))

(define (transpose g)
  (local[(define (firsts lst)
        (cond[(empty? lst) empty]
            
        [else (cons (first (first lst)) (firsts (rest lst)))]))
         (define (remove-firsts lst)
           (cond[(empty? lst) empty]
                
            [else
           (cons (rest (first lst)) (remove-firsts (rest lst)))]))]
         (cond[(empty? g)empty]
              [(=(length (first g))1) (list(firsts g))]
              [else
              (cons (firsts g)
              (transpose (remove-firsts g)))])))

;; Tests:
(check-expect(transpose empty) empty)


;; (find-wpos loc row)
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))



(define (find-wpos loc row)
  (local [ (define (easysimple loc row col)
             
             (cond[(empty? loc)empty]
                  [(and(char=? (first loc)#\#)
                       (>=(length (first-line loc))2))
                   (cons (make-wpos row col true (length (first-line loc)))
                         (easysimple (rest-of-lines loc) row (+ 1 (length (first-line loc))col)))]
                  [else (easysimple (rest loc) row (add1 col))]))
           
           
           (define (first-line loc)
             (cond [(empty? loc) empty]
                   [(char=? (first loc) #\.) empty]
                   [else (cons (first loc) (first-line (rest loc)))]))
           (define (rest-of-lines loc)
             (cond [(empty? loc) empty]
                   [(char=? (first loc) #\.) (rest loc)]
                   [else (rest-of-lines (rest loc))]))]
    (easysimple loc row 0)))

           
   
                          
               

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3)))
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)


;; (initial-state puzzle)
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                                
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
(define (initial-state puzzle)
(local[

(define (list-flip lst)
  (cond[(empty? lst)empty]
       [else
   (cons (flip (first lst))
         (list-flip (rest lst)))]))
(define (lstofstr->lstofchr lstofst)
  (foldr (lambda (x y)
           (cons (string->list x)y))
         empty
         lstofst))

(define (initial-help3 grid lstofwpos lstofstr)      
       (make-state  grid lstofwpos lstofstr))

                   
(define (initial-help grid row)
  (cond[(empty? grid)empty]
       [else
  (append (find-wpos (string->list (first grid))row)
        (initial-help (rest grid)(add1 row)))]))
(define (initial2trans grid row)
  (cond[(empty? grid)empty]
       [(empty? (find-wpos (first grid)row)) (initial2trans (rest grid) (add1 row))]
       [else
      (append (list-flip (find-wpos (first grid)row))
        (initial2trans (rest grid)(add1 row)))]))]
      (initial-help3 (lstofstr->lstofchr(first puzzle))
               (append (initial-help (first puzzle) 0)
               (initial2trans (transpose(lstofstr->lstofchr (first puzzle)))0))
               (second puzzle))))

(check-expect(initial-state '(("#######" "#.#.#.#" "#######" "#.#.#.#" "#######" "#.#.#.#" "#######")
 ("AVERAGE" "CASSIUS" "CUSTARD" "DESSERT"
  "IMITATE" "SECTION" "SUCCESS" "SUNBELT")))
           (make-state
 (list
  (list #\# #\# #\# #\# #\# #\# #\#)
  (list #\# #\. #\# #\. #\# #\. #\#)
  (list #\# #\# #\# #\# #\# #\# #\#)
  (list #\# #\. #\# #\. #\# #\. #\#)
  (list #\# #\# #\# #\# #\# #\# #\#)
  (list #\# #\. #\# #\. #\# #\. #\#)
  (list #\# #\# #\# #\# #\# #\# #\#))
 (list
  (make-wpos 0 0 true 7)
  (make-wpos 2 0 true 7)
  (make-wpos 4 0 true 7)
  (make-wpos 6 0 true 7)
  (make-wpos 0 0 false 7)
  (make-wpos 0 2 false 7)
  (make-wpos 0 4 false 7)
  (make-wpos 0 6 false 7))
 (list
  "AVERAGE"
  "CASSIUS"
  "CUSTARD"
  "DESSERT"
  "IMITATE"
  "SECTION"
  "SUCCESS"
  "SUNBELT")))

;; (extract-wpos g wp) produces the corresponding word in g with
;; the position wp
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
 (check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))



(define (extract-wpos g wp)
  (local[ (define (remove-firsts lst)
           (cond[(empty? lst) empty]
                
            [else
           (cons (rest (first lst)) (remove-firsts (rest lst)))]))

          (define(extract-by-len lst len)
           (cond[(zero? len)empty]
                [(empty? lst)empty]
            [else(append (list(first lst)) (extract-by-len (rest lst)(sub1 len)))]))  
         (define (extract-help g wp row col)
           (cond[(and(=(wpos-row wp)row)
                     (=(wpos-col wp)col)
                     (equal? (wpos-horiz? wp)true))
                 (extract-by-len (first g) (wpos-len wp))]
                [(and(=(wpos-row wp)row)
                     (=(wpos-col wp)col)
                     (equal? (wpos-horiz? wp)false))
                 (extract-by-len (first (transpose g)) (wpos-len wp))]
                [(=(wpos-row wp)row)(extract-help(remove-firsts g) wp row (add1 col))]
                [else (extract-help (rest g) wp (add1 row) col)]))]
    (extract-help g wp 0 0)))
                

;; Tests:

(check-expect (extract-wpos '((#\M #\N) (#\Q #\R) (#\S #\T))
                    (make-wpos 2 0 true 2)) '(#\S #\T))
(check-expect
(extract-wpos '((#\M #\N #\p #\i #\j #\l)
(#\Q #\R #\. #\# #\# #\#) (#\S #\T #\. #\x #\# #\#))
(make-wpos 1 3 true 3)) '(#\# #\# #\#))


;; (replace-wpos g wp loc)
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 1 1 true 2) '(#\J #\K))
              '((#\A #\B #\C) (#\X #\J #\K)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect(replace-wpos grid-abcd (make-wpos 2 1 true 2) '(#\K #\F))
(list (list #\A #\B #\D) (list #\i #\y #\l) (list #\l #\K #\F)))
(check-expect
(replace-wpos grid-abcd (make-wpos 2 0 true 2) '(#\K #\F))
(list (list #\A #\B #\D) (list #\i #\y #\l) (list #\K #\F #\m)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))
(check-expect(replace-wpos grid-abcd (make-wpos 0 1 false 3) '(#\J #\K #\f))
             '((#\A #\J #\D)(#\i #\K #\l)(#\l #\f #\m)))



(define (replace-wpos g wp loc)
  (local [  
          (define (remove-firsts lst)
            (cond[(empty? lst) empty]
                 [(empty? (rest lst))(list (rest (first lst)))]
                 [else
                  (cons (rest (first lst)) (remove-firsts (rest lst)))]))
          
          
          (define (replace-keep loc1 loc2 len)
            (cond[(and(empty? loc2)(not(empty? loc1))) loc1]
                 [(zero? len)empty]
                 [(empty? loc2)empty]
                 [(empty? loc1)empty]
                 [else
                  (cons (first loc2) (replace-keep (rest loc1) (rest loc2)(sub1 len)))]))
          
          
          
          (define (replace-main-help g wp row col)
            (cond[(and(=(wpos-row wp)row)
                      (=(wpos-col wp)col)
                      (equal? (wpos-horiz? wp)true))
                  (cons  (replace-keep (first g) loc (wpos-len wp)) (rest g))]
                 
                 
                 [(= (wpos-row wp)row)   (append (list(append (list (first (first g)))
                  (first (replace-main-help (append (list (rest (first g))) (remove-firsts(rest g))) wp row (add1 col)))))
                 
                           (rest g))]
                 [else (append (list (first g))
                               (replace-main-help (rest g) wp (add1 row) col))]))]
    (cond [(wpos-horiz? wp)(replace-main-help g wp 0 0)]
          [else (transpose (replace-wpos (transpose g) (flip wp) loc))]))) 

          
(check-expect (replace-wpos grid-abcd (make-wpos 1 0 false 2) '(#\J #\K))
              '((#\A #\B #\D) (#\J #\y #\l) (#\K #\j #\m)))
(check-expect(replace-wpos grid-abcd (make-wpos 1 2 false 2) '(#\V #\R))
             '((#\A #\B #\D) (#\i #\y #\V) (#\l #\j #\R)))



;; Tests:


;; (fit? word cells)
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
 (check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
 (check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)
 (check-expect (fit? (string->list "STARWARS") (string->list "########")) true)
;(replace-wpos g wp loc)
(define (fit? word cells)
  (local [
(define (fit?-help word cells)
  (cond[(not(= (length word)(length cells))) false]
       [(empty? cells) empty]
       [(char=? (first cells) #\#)
         (append (first(replace-wpos (list cells) (make-wpos 0 0 true 1) word))
               (fit?-help (rest word) (rest cells)))]
       [else (append (list(first cells)) (fit?-help (rest word) (rest cells)))]))]
    (equal? (fit?-help word cells) word)))
  

;; Tests:
 (check-expect(fit? (string->list "HElloWorld") (string->list "###.######"))false)
 (check-expect(fit? (string->list "ayyylmao") (string->list "##yy##mo"))false)
 (check-expect(fit? (string->list "ayyylmao") (string->list "##yy##l#mo"))false)
 (check-expect(fit? (string->list "ayyylmao") (string->list "###y#ma#"))true)

;; (neighbours s)
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))

;(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))
;; find-wpos: (listof Char) Nat -> (listof WPos)

(define (neighbours s)
   (local[
          (define (mostfilled? al)
           (first (sort al (lambda (x y) (> (second x)(second y))))))
          
          (define (howmuchfilled-lstwpos grid lstwpos)
           (cond[(empty? lstwpos)empty]
                [else (cons (list (first lstwpos) (howmuch-filled?(extract-wpos grid (first lstwpos))))
                            (howmuchfilled-lstwpos grid (rest lstwpos)))]))
          
           (define (howmuch-filled? word)
             (cond[(empty? word) 0]
                  [(not(char=? (first word) #\#))
                   (+ 1 (howmuch-filled? (rest word)))]
                  [else (howmuch-filled? (rest word))]))
         
         
          (define (select-same-word-lengths lstofwords)
                    (cond[(empty? lstofwords)empty]
                         [(= (wpos-len (first (mostfilled? (howmuchfilled-lstwpos (state-grid s) (state-positions s))))) (string-length (first lstofwords)))
                          (cons (first lstofwords)(select-same-word-lengths (rest lstofwords)))]
                         [else (select-same-word-lengths (rest lstofwords))]))
          
          (define (delete-wpos wpos lstofwpos)
                    (cond[(empty? lstofwpos)empty]
                         [(equal? wpos (first lstofwpos)) (rest lstofwpos)]
                         [else (cons (first lstofwpos) (delete-wpos wpos (rest lstofwpos)))]))
          (define (delete-word word lstofwords)
                 (cond[(empty? lstofwords)empty]
                      [(equal? word (first lstofwords))(rest lstofwords)]
                      [else (cons (first lstofwords)(delete-word word (rest lstofwords)))]))
          (define (state-producer s lstofwords lstofwpos)
            
              (cond[(empty? lstofwords)empty]
                   [(fit? (string->list(first lstofwords))
                          (extract-wpos (state-grid s)(first lstofwpos)))
                    (cons (make-state (replace-wpos (state-grid s) (first lstofwpos) (string->list(first lstofwords)))
                                      (delete-wpos (first lstofwpos) (state-positions s))
                                      (delete-word (first lstofwords) (state-words s)))  (state-producer s (rest lstofwords) lstofwpos))]
                   [else (state-producer s (rest lstofwords) lstofwpos)]))]
     
     (state-producer s (select-same-word-lengths (state-words s)) (mostfilled? (howmuchfilled-lstwpos (state-grid s) (state-positions s))))))
                 

; Tests:
(check-expect (neighbours (make-state '((#\h #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3)
                                            (make-wpos 0 0 true 4))
                                      '("CAT" "DOG" "hell" "CAR" "bell")))
              (list
 (make-state
  (list (list #\h #\e #\l #\l))
  (list (make-wpos 0 0 true 3))
  (list "CAT" "DOG" "CAR" "bell"))
 (make-state
  (list (list #\b #\e #\l #\l))
  (list (make-wpos 0 0 true 3))
  (list "CAT" "DOG" "hell" "CAR"))))
                    
(check-expect (neighbours (make-state (list (list #\# #\# #\# #\#))
                          (list (make-wpos 0 0 true 4))
                          (list "BOOK")))
              (list (make-state '((#\B #\O #\O #\K)) empty empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

;(define (criss-cross puzzle)
;  (local [(define result (solve (initial-state puzzle)
;                                neighbours
;                                solved?))]
;    (cond [(false? result) false]
;          [else (map list->string (state-grid result))])))
; (check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

;; (disp (criss-cross (read-puzzle "puzzle02.txt")))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

