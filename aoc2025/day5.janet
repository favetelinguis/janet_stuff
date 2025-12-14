(use judge)

(def example-input ```
3-5
10-14
16-20
12-18

1
5
8
11
17
32
```)

(def grammar
  ~{:main (/ (* :ranges "\n\n" :ingredients) ,|{:ranges $0 :ingredients $1})
    :ranges (/ (* :range (any (* "\n" :range))) ,tuple)
    :range (/ (* :number "-" :number) ,|[$0 $1])
    :ingredients (/ (* :number (any (* "\n" :number))) ,tuple)
    :number (/ (<- (some :d)) ,scan-number)})

(def example-data (first (peg/match grammar example-input)))

(test example-data
  {:ingredients [1 5 8 11 17 32]
   :ranges [[3 5] [10 14] [16 20] [12 18]]})

(def test-data
  (->> "day5-input.txt"
       (slurp)
       (peg/match grammar)
       (first)))

(defn answer-1 [in-data]
  (var count 0)
  (each ingredient (in-data :ingredients)
    (each [start end] (in-data :ranges)
      (when (and
	      (>= ingredient start)
	      (<= ingredient end))
	(++ count)
	(break)))) count)

(defn merge-intervals [intervals]
  (def sorted (sort-by first (array ;intervals)))
  (def result @[(first sorted)])
  (each [start end] (slice sorted 1)
    (let [prev-range (array/peek result)
	  last-start (first prev-range)
	  last-end (last prev-range)]
      (if (>= (inc last-end) start) # also merge adjecent intervals
	(do #do merge
	  (array/pop result)
	  (array/push result [last-start (max end last-end)]))
	(array/push result [start end]))))
  result)

(test  (merge-intervals (example-data :ranges)) @[[3 5] [10 20]])

(defn answer-2 [in-data]
  (var sum 0)
  (each [start end] (merge-intervals (in-data :ranges))
    (+= sum (inc (- end start))))
  sum)

(test (answer-2 test-data) 354226555270043)

