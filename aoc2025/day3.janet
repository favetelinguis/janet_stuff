(def test-input ```
987654321111111
811111111111119
234234234234278
818181911112111
```)

(def idx-grammar
  ~{
    :char-pos (group (* ($) (<- :d)))
    :number (group (some :char-pos))
    :main (some (* :number (? "\n")))
    })

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
      #(> n l) [n 48]
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

















(defn char-num-eq? [ch n]
  (= (string/bytes ch)
     (string/bytes (string n))))


(defn answer-1 [in]
  (var result 0)
  (loop [bank :in in]
  (pp        (sort-by |(first $) (array/slice (take -2 (sort-by |(get $ 1) bank)))))
    ))

(answer-1 banks)

# 48 is ascii number for 0
# 57 is ascii number for 9
(defn answer-1 [in]
  (var result 0)
  (loop [bank :in in]
    (var highest-byte 48)
    (var second-highest-byte 48)
    (loop [byte :in bank]
      (cond (> byte highest-byte) (set highest-byte byte)
	    (> byte second-highest-byte) (set second-highest-byte byte)
	     # early terminate when two 9s found, this will iterate one more but is nice to put in cond
	    #(= highest-byte second-highest-byte 57) (break)
	    ))
    (pp (scan-number (string/from-bytes highest-byte second-highest-byte)))
    (+= result (scan-number (string/from-bytes highest-byte second-highest-byte)))
    (set highest-byte 48)
    (set second-highest-byte 48))
  result)

(answer-1 banks)
(> 0 0)
(string/from-bytes 55 56)
(scan-number "22")

