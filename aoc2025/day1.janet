(def sample-data
  (string/split "\n" `
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
`))

(def data1
  (->> "day1a-input.txt"
      (slurp)
      (string/trim)
      (string/split "\n")))

(def grammar
  ~{:number (/ (<- (some :d)) ,scan-number) 
    :direction (/ (<- (set "LR")) ,keyword)
    :main (* :direction :number)}) # TODO should not need * since it can only be one par

(defn rotate-right [current step]
  "Use module arithmetics with number 0-99"
  (% (+ current step) 100))

(defn rotate-left [current step]
  "Use module arithmetics with number 0-99"
  (% (+ (- current step) 100) 100))

(defn answer-1 [in-data]
  (var current-dial 50)
  (var zero-times 0)
  (loop [row :in in-data]
    (let [[direction steps] (peg/match grammar row)
	  new-dial (if (= direction :L) (rotate-left current-dial steps) (rotate-right current-dial steps))]
      (set current-dial new-dial)
      (when (= new-dial 0) (++ zero-times))))
  zero-times)

(defn answer-2 [in-data]
  (var current-dial 50)
  (var zero-times 0)
  (loop [row :in in-data]
    (let [[direction steps] (peg/match grammar row)
	  quotient (div steps 100) # Need to decide how many full turns will be made
	  remainder (% steps 100)] # Need to take the reminder and check if that would result in 0 or passing zero obs (% 22 100) => 22 so can always use remainder
      # Check if we would pass or equal zero when moving remainder
      # if we already are at 0 dont count again
      (if (and (= direction :L) (> current-dial 0))
	(when (<= (- current-dial remainder) 0) (++ zero-times))
	(when (>= (+ current-dial remainder) 100) (++ zero-times)))
      # Always adjust for number of times since (div 22 100) => 0
      (+= zero-times quotient)
      # Change current-dial after we have incremented zero-times
      (set current-dial (if (= direction :L)
			  (rotate-left current-dial remainder)
			  (rotate-right current-dial remainder)))))
  zero-times)

# Usage example
(answer-1 sample-data)
(answer-1 data1)
(answer-2 sample-data)
(answer-2 data1)


