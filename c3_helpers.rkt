(module c3_helpers racket
  
  (require math/distributions
           math/statistics
           racket/list)
  
  (provide plot-with-c3
           scatter-c3
           line-c3
           bar-c3
           hist-c3
           multi-line-c3
	   multi-line-c3-w-legend
           multi-reg-line-plus-scatter-c3
           bar-c3-categorical
	   c3-columns-field
	   c3-columns-field-with-legend
	   c3-xs-field
	   c3-xs-field-with-legend)
  
  
  (define (plot-with-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] 
                        #:type [type "scatter"] #:xtickvalues [xticks xs]
                        #:showpoints [showpts #f] #:bind-to [bind-to "chart1"] 
                        #:legend [leg "ys1"])
    (cons 'c3-data
          (hasheq 'bindto bind-to
                  'point 
                  (hasheq 'show showpts)
                  'data
                  (hasheq
                   ;'hide "true"   ; This fails to hide the dots.
                   'xs (hasheq (string->symbol leg) "xs1")
                   'columns (list (cons "xs1" xs)
                                  (cons leg ys))
                   'type type)
                  'axis
                  (hasheq 'x (hasheq 'label xlabel 'tick (hasheq 'values xticks))
                          'y (hasheq 'label ylabel)))))
  
  (define (scatter-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] #:legend [leg "ys1"])
    (plot-with-c3 xs ys #:xlabel xlabel #:ylabel ylabel #:type "scatter" #:showpoints #t))
  
  (define (line-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] #:xtickvalues [xticks xs] 
                   #:showpoints [showpts #t] #:bind-to [bind-to "chart1"] #:legend [leg "ys1"])
    (plot-with-c3 xs ys #:xlabel xlabel #:ylabel ylabel #:type "line" 
                  #:xtickvalues xticks #:showpoints showpts #:bind-to bind-to
                  #:legend leg))
  
  (define (bar-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"])
    (plot-with-c3 xs ys #:xlabel xlabel #:ylabel ylabel #:type "bar"))
  
  
  (define (bar-c3-categorical xs ys cgs #:xlabel [xlabel "x"] #:ylabel [ylabel "y"])
    (cons 'c3-data
          (hasheq 'data
                  (hasheq
                   ;'x cgs
                   'xs (hasheq 'ys1 "xs1")
                   'columns (list (cons "xs1" cgs)
                                  (cons (symbol->string 'ys1) ys))
                   'type "bar")
                  'axis
                  (hasheq 'x (hasheq 'label xlabel 
                                     'tick (hasheq 'rotate 90)
                                     'type "category")
                          'y (hasheq 'label ylabel)))))
  
  
  
#|

  (define (plot-with-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] #:type [type "scatter"] #:tick-marks [ticks xs])
    (cons 'c3-data
          (hasheq 'data
                  (hasheq
                   'xs (hasheq 'ys1 "xs1")
                   'columns (list (cons "xs1" xs)
                                  (cons (symbol->string 'ys1) ys))
                   'type type)
                  'axis
                  (hasheq 'x (hasheq 'label xlabel
                                     'tick (hasheq 'values ticks))
                          'y (hasheq 'label ylabel)))))
  
  (define (scatter-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] #:tick-marks [ticks xs])
    (plot-with-c3 xs ys #:xlabel xlabel #:ylabel ylabel #:tick-marks ticks))
  
  (define (line-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] #:tick-marks [ticks xs])
    (plot-with-c3 xs ys #:xlabel xlabel #:ylabel ylabel #:type "line" #:tick-marks ticks))
  
  (define (bar-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] #:tick-marks [ticks xs])
    (plot-with-c3 xs ys #:xlabel xlabel #:ylabel ylabel #:type "bar" #:tick-marks ticks))
  
  



|#
  
  
  
  
  ;;; Histograms
  
  (define (count-in-range low high vals)
  (length (filter (lambda (v) 
                    (and (>= v low) (< v high))) vals)))
  
  
  (define (sbs-hist-info vals [n-bins 15])
  (let* ([low (quantile 0.001 < vals)]
         [high0 (quantile 0.999 < vals)]
         [val-range (- high0 low)]
         [high (+ high0 (* val-range 0.0001))]
         [bin-width (/ (- high low) n-bins)]
         [bin-edges (for/list ([i (range (+ 1 n-bins))])
                        (+ low (* bin-width i)))]
         [bin-centers (for/list ([i (range n-bins)])
                       (+ low (* bin-width (+ i 0.5))))]                  
         [counts (for/list ([i (range n-bins)])
                     (let ([bin-low (+ low (* bin-width i))]
                              [bin-high (+ low (* bin-width (+ i 1)))])
                          (count-in-range bin-low bin-high vals)))])
   (list bin-edges bin-centers counts)))
  
  

   
  (define (hist-c3 vals [n-bins 15] #:xlabel [xlabel "values"] #:ylabel [ylabel "counts"])
  (let([stuff (sbs-hist-info vals n-bins)])
    (let ([bin-bounds (map (lambda (x) (/ (round (* x 100)) 100)) (first stuff))]
          [xs (second stuff)]
          [ys (third stuff)])
      (cons 'c3-data
          (hasheq 'data
                   (hasheq
                     'xs (hasheq 'counts "xs1")
                    'columns (list (cons "xs1" xs)
                               (cons (symbol->string 'counts) ys))
                     'type "bar")
                  'axis
                  (hasheq 'x (hasheq 'label xlabel 
                                     'tick (hasheq 'values bin-bounds
                                                   'rotate 90))
                          'y (hasheq 'label ylabel)))))))

  
  
  ;;;;;;;;;;; Multi-line plots
  
  
  (define (c3-xs-field n) 
    (make-immutable-hasheq
     (for/list ([i n])
        (cons
         (string->symbol (string-append "ys" (number->string i)))
         (string-append "xs" (number->string i))))))


  (define (c3-xs-field-with-legend n legend) 
    (make-immutable-hasheq
     (for/list ([i n]
		[leg legend])
        (cons
         (string->symbol leg)
         (string-append "xs" (number->string i))))))
  
  (define (c3-columns-field xs ys)
    (append
     (for/list ([i (length xs)])
       (cons (string-append "xs" (number->string i)) (list-ref xs i)))
     (for/list ([i (length ys)])
       (cons (string-append "ys" (number->string i)) (list-ref ys i)))))

  
    (define (c3-columns-field-with-legend xs ys legend)
    (append
     (for/list ([i (length xs)])
       (cons (string-append "xs" (number->string i)) (list-ref xs i)))
     (for/list ([i (length ys)]
		[leg legend])
       (cons leg (list-ref ys i)))))

    
  
  (define (multi-line-c3 xs ys #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] 
                         #:showpoints [showpts #t] #:bind-to [bind-to "chart1"]
                         #:xtickvalues [xticks xs])
  (cons 'c3-data
        (hasheq
         'bindto bind-to
         'point (hasheq 'show showpts)
         'data
               (hasheq
                 'xs (c3-xs-field (length xs))
                'columns (c3-columns-field xs ys)
                 'type "line")
              'axis
              (hasheq 'x (hasheq 'label xlabel 'tick (hasheq 'values xticks))
                      'y (hasheq 'label ylabel)))))


  (define (multi-line-c3-w-legend xs ys legend
				  #:xlabel [xlabel "x"] #:ylabel [ylabel "y"] 
				  #:showpoints [showpts #t] #:bind-to [bind-to "chart1"]
				  #:xtickvalues [xticks xs])
    (cons 'c3-data
	  (hasheq
	   'bindto bind-to
	   'point (hasheq 'show showpts)
	   'data
	   (hasheq
	    'xs (c3-xs-field-with-legend (length xs) legend)
	    'columns (c3-columns-field-with-legend xs ys legend)
	    'type "line")
	   'axis
	   (hasheq 'x (hasheq 'label xlabel 'tick (hasheq 'values xticks))
		   'y (hasheq 'label ylabel)))))
  
  
  
  
(define (multi-reg-line-plus-scatter-c3 xs ys true-xs true-ys 
                                          #:xlabel [xlabel "x"] #:ylabel [ylabel "y"])
    (cons 'c3-data
          (hasheq 'point 
                  (hasheq 'show #f)
                  'data
                  (hasheq
                   'xs (hash-set (c3-xs-field (length xs))
                                 'true-ys "true-xs")
                   'columns (let ([xxxs (cons "true-xs" true-xs)]
                                  [yyys (cons "true-ys" true-ys)])
                              (cons xxxs (cons yyys (c3-columns-field xs ys))))
                   'type "line"
                   'types (hasheq 'true-ys "scatter")
                   'color "gray"
                   'colors (hasheq 'true-ys "black"))
                  'axis
                  (hasheq 'x (hasheq 'label xlabel)
                          'y (hasheq 'label ylabel))
                  'point
                  (hasheq 'show #f)
                  'legend
                  (hasheq 'show #f))))




  )

