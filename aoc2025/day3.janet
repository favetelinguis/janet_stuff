(def test-input ```
987654321111111
811111111111119
234234234234278
818181911112111
```)

(def grammar
  ~{
    :number (<- (some :d))
    :main (some (* :number (? "\n")))
    })

(def test-data
  (->> "day3-input.txt"
       (slurp)
       (peg/match grammar)))

(def banks (peg/match grammar test-input))

(defn shift-right [[l r] n is-last]
  (if-not is-last
    (cond
      (> n l) [n 48]
      (> n r) [l n] 
      [l r])
    (cond
      (> n r) [l n] 
      [l r])))

# 48 is ascii number for 0
# 57 is ascii number for 9
# could have early termination when i get 2 9s we could stop
(defn answer-1 [in]
  (var result @[])
  (loop [bank :in in]
    (def num-bytes (length bank))
    (defn last? [i] (= num-bytes (+ i 1)))
    (var highest [48 48])
    (loop [i :range [0 num-bytes]]
      (set highest (shift-right highest (get bank i) (last? i))))
    (array/push result (scan-number (string/from-bytes ;highest))))
  (sum result))

(answer-1 test-data)

(defn zero-out! [xs n]
  "make from n to lenght of array zero (48)"
  (loop [i :range [n (length xs)]]
    (put xs i 48)))

(defn shift-right-general! [xs val count-from-end]
  "mutate xs"
  (def from (max (- 12 count-from-end) 0))
  (loop [i :range [from 12]]
    (cond (> val (get xs i)) (do (put xs i val) (zero-out! xs (+ i 1)) (break))
	  xs)))
  
(defn process-bank [bank]
  (def num-bytes (length bank))
  (var highest (array/new-filled 12 48))
  (loop [i :range [0 num-bytes]]
      (shift-right-general! highest (get bank i) (- num-bytes i)))
  highest)

(defn answer-2 [in]
  (var result @[])
  (loop [bank :in in]
    (def highest (process-bank bank))
    (array/push result (scan-number (string/from-bytes ;highest))))
  (sum result))

(answer-2 test-data)



