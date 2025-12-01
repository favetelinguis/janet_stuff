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

# Usage example
#(answer-1 sample-data)
#(answer-1 data1)

