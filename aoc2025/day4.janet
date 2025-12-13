(use judge)

(def test-input ```
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
```)

(def grammar
  ~{
    :cell (+ (* "@" (constant 1))
             (* "." (constant 0)))
    :line (group (some :cell))
    :main (some (* :line (? "\n")))
    })

(def exemple-data (peg/match grammar test-input))

(def test-data
  (->> "day4-input.txt"
       (slurp)
       (peg/match grammar)))

(defn moore-neighborhood [matrix row-idx col-idx]
  "check all 8 surrounding cells"
  (defn check [[row-adj col-adj] matrix row-idx col-idx]
    (get-in matrix [(+ row-idx row-adj) (+ col-idx col-adj)] 0))
  ((juxt
    (partial check [-1 -1])
    (partial check [-1 0])
    (partial check [-1 1])
    (partial check [0 -1])
    (partial check [0 1])
    (partial check [1 -1])
    (partial check [1 0])
    (partial check [1 1])
    ) matrix row-idx col-idx))

(defn answer-1 [in-data]
  (var count 0)
  (eachp [row-idx row] in-data
    (eachp [col-idx value] row
      (when (= value 1)
      (when (< (+ ;(moore-neighborhood in-data row-idx col-idx)) 4)
	(++ count))))) count)

(defn answer-2 [in-data]
  (var total-count 0)
  (var has-changed true)
  (while has-changed
    (set has-changed false)
    (eachp [row-idx row] in-data
      (eachp [col-idx value] row
	(when (= value 1)
	  (when (< (+ ;(moore-neighborhood in-data row-idx col-idx)) 4)
	    (set has-changed true)
	    (++ total-count)
	    (put-in in-data [row-idx col-idx] 0)))))) total-count)

(test (answer-2 test-data)
  8936)
