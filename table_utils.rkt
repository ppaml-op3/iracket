#lang gamble

(require racket/string
         racket/list
         racket/vector
         rackunit
         rackunit/text-ui)


(provide missing-value?
         average-table
         cross-prod-hash
         export-tsv
         distance-table
         mean-distance-table
         mean-value-of)
         
         
;; TODO: replace this with an argument to readers identifying "missing" symbol.
;;       "NA" is a good default, since R uses it.
(define (missing-value? x) (= (string->number x) -1))



;; Take the average of a vector of numbers.
(define (vector-average v)
  (let ([lst (vector->list v)])
    (/ (apply + lst) (length lst))))

;; Test for "vector-average".
;(define test-vector (vector 2 3 7 9))
;(vector-average test-vector)



;; Take a key and a vector of hash tables, and return a
;; a number, which is the average of all the tables' entries for that key.
(define (average-entry key vector-of-tables)
  (let ([values (vector-map (lambda (tbl) (hash-ref tbl key)) vector-of-tables)])
    (vector-average values)))

;; Test for "average-entry".
;(define test-tables
;  (vector
;   (make-hash (list (cons "a" 1) (cons "b" 2) (cons "c" 3)))
;   (make-hash (list (cons "a" 10) (cons "b" 20) (cons "c" 30)))))
;
;(average-entry "a" test-tables)



;; Take a vector of hash tables, all with the same set of keys, each entry of which is
;; a number, and return a single hash table, with the same set of keys, each entry of 
;; which is the average of all the corresponding entries.
;;   This useful because gamble sampling always produces vectors of whatevers.
;; 
(define (average-table vector-of-tables)
  (let ([keys (hash-keys (vector-ref vector-of-tables 0))])
    (make-hash
     (map (lambda (key)
            (cons key
                  (vector-average
                   (vector-map (lambda (tbl) (hash-ref tbl key)) vector-of-tables)))) 
          keys))))



;; This makes a hash table for each pair of items where one is from set1 and 
;; the other is from set2.  The value of the (item1 . item2) entry is 
;; (fn item1 item2).
;;
;; cross-prod-hash list a, list b, (a,b -> c) -> hash a . b --> c
(define (cross-prod-hash set1 set2 fn)
  (make-hash
   (apply append
    (map (lambda (item1)
           (map (lambda (item2) 
                  (cons (cons item1 item2) (fn item1 item2))) set2))
         set1))))


;; Make a table in a format that R's read.table(<file name>) can read in to a 
;; dataframe.
;;
;; list String, list String, (hash String . String --> number), String -> void
(define (export-tsv row-names col-names value-table file-name)
  (with-output-to-file file-name
    (lambda ()
      (begin (printf "\t")
             (map (lambda (col-name) (printf "~s\t" col-name)) col-names)
             (printf "\n")
             (map (lambda (row-name)
                    (begin
                      (printf "~s\t" row-name)
                      (map (lambda (col-name)
                             (printf "~s\t" (hash-ref value-table (cons row-name col-name) 0)))
                           col-names)
                      (printf "\n")))
                  row-names)))))



;; Read a table in a format that R's read.table(<file name>) can read in to a 
;; dataframe.  Output a list containing a list of row names, a list of col names,
;; and a table of values.
;;
;; String -> (list String, list String, (hash String . String) --> number))
;(define (import-tsv file-name)
;  (with-input-from-file file-name
;    (lambda ()
;      (begin (let ([col-names (string-split (readline))])
;               (while (let ([line (readline)])
;                        (not (eof-object? line)
;                          
;             (map (lambda (col-name) (printf "~s\t" col-name)) col-names)
;             (printf "\n")
;             (map (lambda (row-name)
;                    (begin
;                      (printf "~s\t" row-name)
;                      (map (lambda (col-name)
;                             (printf "~s\t" (hash-ref value-table (cons row-name col-name) 0)))
;                           col-names)
;                      (printf "\n")))
;                  row-names)))))




(define (euclidean-distance table factors object1 object2)
  (sqrt (apply + (map (lambda (factor) 
                        (let ([diff (-
                                     (hash-ref table (cons object1 factor))
                                     (hash-ref table (cons object2 factor)))])
                          (* diff diff)))
                      factors))))




        

;; TODO: Make this not compute every distance twice!
(define (distance-table representation-table factors object-names)
  (cross-prod-hash object-names object-names 
                   (lambda (o1 o2) (euclidean-distance representation-table factors o1 o2))))
    

(define (mean-distance-table representation-table-vector factors object-names)
  (average-table 
   (vector-map (lambda (tbl) (distance-table tbl factors object-names)) 
               representation-table-vector)))



                

;; Use the table to get the mean value of each attribute.  Every entry in the table
;; is assumed to be of the form object . attribute --> value
(define (mean-value-of attr data-table)
  (let* ([all-keys (hash-keys data-table)]
         [these-keys (filter (lambda (k) (string=? (cdr k) attr)) all-keys)]
         [total (apply + (map (lambda (k) (hash-ref data-table k)) these-keys))]
         [n (length these-keys)])
   ; (list total n)))
    (if (> n 0)
        (/ total n)
        0)))


(define (closest n object table)
  (let ([object-dists
         (map (lambda (k) (cons k (hash-ref table k))) 
              (filter (lambda (k) (string=? (car k) object)) (hash-keys table)))])
    (take (sort object-dists (lambda (x y)
                         (< (cdr x) (cdr y)))) n)))


(define (neighbors object distance tbl) 
    (filter (lambda (k) (< (cdr k) distance)) (drop (closest 10 object tbl) 1)))


;; Evaluate with (run-tests table-utils-tests).
;;
(define table-utils-tests
  (let* ([test-tbl-vec 
         (vector
          (make-hash
           (list 
            (cons (cons "mouse" "size") 4)
            (cons (cons "mouse" "pest") 10)
            (cons (cons "mouse" "mammal") 10)
            (cons (cons "mouse" "cute") 10)
            (cons (cons "hamster" "size") 4)
            (cons (cons "hamster" "pest") 1)
            (cons (cons "hamster" "mammal") 10)
            (cons (cons "hamster" "cute") 10)
            (cons (cons "gerbil" "size") 4)
            (cons (cons "gerbil" "pest") 0)
            (cons (cons "gerbil" "mammal") 9)
            (cons (cons "gerbil" "cute") 9)
            (cons (cons "ant" "size") 0)
            (cons (cons "ant" "pest") 10)
            (cons (cons "ant" "mammal") 0)
            (cons (cons "ant" "cute") 0)
            (cons (cons "wasp" "size") 2)
            (cons (cons "wasp" "pest") 10)
            (cons (cons "wasp" "mammal") 0)
            (cons (cons "wasp" "cute") 0)))
          (make-hash
           (list 
            (cons (cons "mouse" "size") 5)
            (cons (cons "mouse" "pest") 9)
            (cons (cons "mouse" "mammal") 11)
            (cons (cons "mouse" "cute") 9)
            (cons (cons "hamster" "size") 4)
            (cons (cons "hamster" "pest") 2)
            (cons (cons "hamster" "mammal") 9)
            (cons (cons "hamster" "cute") 12)
            (cons (cons "gerbil" "size") 3)
            (cons (cons "gerbil" "pest") 1)
            (cons (cons "gerbil" "mammal") 10)
            (cons (cons "gerbil" "cute") 10)
            (cons (cons "ant" "size") 1)
            (cons (cons "ant" "pest") 9)
            (cons (cons "ant" "mammal") 1)
            (cons (cons "ant" "cute") 1)
            (cons (cons "wasp" "size") 2)
            (cons (cons "wasp" "pest") 9)
            (cons (cons "wasp" "mammal") 0)
            (cons (cons "wasp" "cute") 0))))]
        [d-tbl0 (distance-table (vector-ref test-tbl-vec 0) 
                               (list "size" "pest" "mammal" "cute")
                               (list "mouse" "hamster" "gerbil" "ant" "wasp"))]
        [mn-tbl (mean-distance-table test-tbl-vec 
                                     (list "size" "pest" "mammal" "cute")
                                     (list "mouse" "hamster" "gerbil" "ant" "wasp"))])
    (test-suite "tests of table utils"
     
     (check-equal?
      (let ([tbl
             (make-hash
              (list (cons (cons "thing1" 0) 1)
                    (cons (cons "thing1" 1) 2)
                    (cons (cons "thing1" 2) 3)
                    (cons (cons "thing2" 0) 2)
                    (cons (cons "thing2" 1) 0)
                    (cons (cons "thing2" 2) 10)))])
        (euclidean-distance tbl (range 3) "thing1" "thing2"))
      (sqrt 54))
   
     (check > (hash-ref d-tbl0 (cons "hamster" "mouse")) 
            (hash-ref d-tbl0 (cons "hamster" "gerbil"))))))
               