(use judge)
(use spork)

(def test-text ```
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
```)

(def grammar
  ~{:ws (any " ")
    :number (/ (* :ws (<- (some :d)) :ws) ,scan-number)
    :number-row (group (some :number))
    :symbol (* :ws (<- (set "+*")) :ws)
    :symbol-row (group (some :symbol))
    :main (* (some (* :number-row "\n")) :symbol-row)})

(def test-data (peg/match grammar test-text))

(test test-data
      @[@[123 328 51 64]
        @[45 64 387 23]
        @[6 98 215 314]
        @["*" "+" "*" "+"]])

(def example-data
  (->> "day6-input.txt"
       (slurp)
       (peg/match grammar)))

# i did not get ((symbol "*") ;[1 2 3]) to work for some reason
# this is the very explicit solution
(defn answer-1 [in-data]
  (def transposed (map tuple ;in-data))
  (var result 0)
  (each row transposed
    (let [op (last row)
          args (slice row 0 -2)]
      (cond
        (= op "*") (+= result (* ;args))
        (= op "+") (+= result (+ ;args)))))
  result)

(def grammar-2
  ~{:char (<- 1)
    :line (group (some (if-not "\n" :char)))
    :main (some (* :line (? "\n")))})

(def test-data-2 (peg/match grammar-2 test-text))

(defn answer-2 [in-data]
  (def ops (string/replace-all " " "" (string/join (last in-data) "")))
  (def transposed-partitions (filter |(> (length $) 1) (partition-by |(all (partial = " ") $) (map tuple ;(slice in-data 0 -2)))))
  (var result 0)
  (eachp [idx op] ops # when i iterate over string i get byte representation so 42 is * and 43 is +
    (def args @[])
    (when-let [rows (get transposed-partitions idx)]
      (each row rows
        (when-let [num (-> row (string/join "") (string/trim) (scan-number))]
          (array/push args num))))
    (cond
      (= op 42) (+= result (* ;args))
      (= op 43) (+= result (+ ;args))))
  result)

(def example-data-2
  (->> "day6-input.txt"
       (slurp)
       (peg/match grammar-2)))

(test (answer-2 example-data-2)
      13807151830618)

(test (transpose test-data-2)
      @[["1" " " " " "*"]
        ["2" "4" " " " "]
        ["3" "5" "6" " "]
        [" " " " " " " "]
        ["3" "6" "9" "+"]
        ["2" "4" "8" " "]
        ["8" " " " " " "]
        [" " " " " " " "]
        [" " "3" "2" "*"]
        ["5" "8" "1" " "]
        ["1" "7" "5" " "]
        [" " " " " " " "]
        ["6" "2" "3" "+"]
        ["4" "3" "1" " "]
        [" " " " "4" " "]])
