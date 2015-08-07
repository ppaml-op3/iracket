(module lr_helpers racket
  
  (require math/distributions
           math/statistics
           racket/list
           racket/port
           racket/vector
           "c3_helpers.rkt")
  
  (provide sample-and-monitor
           show-query-means
           show-acc-rates
           show-scale-changes
	   just-the-samples)

(define (info-as-list smplr)
    (let ([str (with-output-to-string (lambda () (begin
                                                   (send smplr info)
                                                   "done")))])
      (let ([n-total-samples 
             (string->number 
              (second (regexp-match #rx"(?:Total runs: )([0-9]+)" str)))]
            [n-accepted-samples 
             (string->number 
              (second (regexp-match #rx"(?:Accepted traces: )([0-9]+)" str)))]
            [scale-increases (string->number 
                              (second (regexp-match #rx"(?:Scale increased: )([0-9]+)" str)))]
            [scale-decreases (string->number 
                              (second (regexp-match #rx"(?:Scale decreased: )([0-9]+)" str)))]
            [scale-no-changes (string->number 
                               (second (regexp-match #rx"(?:Scale maintained: )([0-9]+)" str)))])
        (list n-total-samples n-accepted-samples scale-increases 
              scale-decreases scale-no-changes))))
  
  (define (just-the-samples info-list) 
    (map first (filter (lambda (elt) (not (string? (first elt)))) info-list)))
  
   (define (sample-ns info-list) 
    (let ([collected (filter (lambda (elt) (not (string? (first elt)))) info-list)])
      (map (lambda (x) (first (second x))) collected)))
   
   
  (define (increments ns)
    (map - ns (cons 0 (take ns (- (length ns) 1)))))
  
  
  (define (plot-sequences results)
    (let ([raw (map second results)])
      (let ([cum-ns (map first raw)]
            [ns (increments (map first raw))]
            [accs (increments (map second raw))]
            [ups (map third raw)]
            [downs (map fourth raw)]
            [stays (map fifth raw)])
        (let ([scale-totals (map (lambda (x) (+ x 0.0000000001)) (map + ups downs stays))]
              [ns-plus (map (lambda (x) (+ x 0.00000000001)) ns)])
          ;(list ns accs ups downs stays scale-totals)))))
            (list cum-ns
                  (map / accs ns-plus) 
                  (map / ups scale-totals)
                  (map / downs scale-totals)
                  (map / stays scale-totals))))))
                              
  
  (define (average-of-vectors-in-list vs)
    (let ([start (make-vector (vector-length (first vs)))])
      (let ([sum-vs (foldr (lambda (v1 v2) (vector-map + v1 v2)) start vs)])
        (vector-map (lambda (x) (/ x (length vs))) sum-vs))))
  
  ; (average-of-vectors-in-list (list '#(1 2 3) '#(3 9 20) '#(5 4 1)))
  
  (define (prefix-means vs)
    (let ([fronts (map (lambda (n) (take vs n)) (range 1 (+ 1 (length vs))))])
      (map average-of-vectors-in-list fronts)))
  
  
(define (less-than-ten-from xs)
    (if (<= (length xs) 10)
        xs           
        (let ([space (floor (/ (length xs) 10))]
              [idxs (range (length xs))])
          (let ([reduced-idxs (filter (lambda (x) (= (remainder x space) 0)) idxs)])
            (map (lambda (idx) (list-ref xs idx)) reduced-idxs)))))
  
  
  (define (sample-and-monitor sampler n-samples 
                              #:burn [burn 0] #:thin [thin 1])
    (let* ([thinning-period (+ thin 1)]
           [n-discarded (floor (/ burn thinning-period))]
           [n-blerps (+ n-discarded n-samples)])
      (for/list ([i n-blerps])
        (begin
          (for ([j thin])
            (sampler))
          (let ([sample-number (* (+ i 1) thinning-period)])
            (list (if (> sample-number burn) (sampler) "burning in...")
                  (info-as-list sampler)))))))
  
  
  (define (show-query-means results var-n #:ylabel [ylabel "mean"])
    (let* ([cums (map (lambda (mn-vec) (vector-ref mn-vec var-n)) 
                                 (prefix-means (just-the-samples results)))]
           [samp-ns (sample-ns results)])
      (line-c3 samp-ns cums #:xlabel "sample #" #:ylabel ylabel #:showpoints #f 
               #:xtickvalues (less-than-ten-from samp-ns) #:legend ylabel)))
                                      
        
  (define (show-acc-rates results)
    (let ([plot-seqs (plot-sequences results)])
        (let ([cum-ns (first plot-seqs)]
              [x-ticks (less-than-ten-from (first plot-seqs))]
              [acc-rates (second plot-seqs)])
          (line-c3 cum-ns acc-rates #:xlabel "sample #" #:ylabel "acceptance rate" 
                   #:showpoints #f #:xtickvalues x-ticks #:legend "proposal acceptance rate"))))
          
          
(define (show-scale-changes results)
    (let ([plot-seqs (plot-sequences results)])
        (let ([cum-ns (first plot-seqs)]
              [x-ticks (less-than-ten-from (first plot-seqs))]
              [ups (third plot-seqs)]
              [downs (fourth plot-seqs)]
              [stays (fifth plot-seqs)])
          (multi-line-c3-w-legend (list cum-ns cum-ns cum-ns)
				  (list ups downs stays)
				  (list "increase" "decrease" "nochange")
	  ; (multi-line-c3 (list cum-ns cum-ns cum-ns)
;		     (list ups downs stays)
                         #:xtickvalues x-ticks
                         #:showpoints #f))))


  ;; Can't keep second plot from clobbering the first.
  (define (sample-and-plot smplr n-samples #:burn [burn 0] #:thin [thin 0])
    (let ([results 
            (sample-and-monitor smplr n-samples #:burn burn #:thin thin)])
      (let ([cumulative-means (prefix-means (just-the-samples results))]
            [plot-seqs (plot-sequences results)])
        (let ([cum-ns (first plot-seqs)]
              [acc-rates (second plot-seqs)]
              [ups (third plot-seqs)]
              [downs (fourth plot-seqs)]
              [stays (fifth plot-seqs)])
          (begin
            (line-c3 cum-ns acc-rates 
                     #:xlabel "sample #" #:ylabel "acceptance rate"
                     #:bind-to "chart1")
            (multi-line-c3 (list cum-ns cum-ns cum-ns) (list ups downs stays) 
                           #:bind-to "chart2"))))))

    
          
  

  
  
  )


