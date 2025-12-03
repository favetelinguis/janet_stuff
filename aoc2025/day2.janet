(use judge)

(def examples "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(def grammar
  ~{:number (/ (<- (some :d)) ,scan-number)
    :range (group (* :number "-" :number))
    :main (any (* :range (? ",")))})

(def test-data
  (->> "day2-input.txt"
       (slurp)
       (peg/match grammar)))

(def ranges (peg/match grammar examples))

(defn answer-1 [in]
  (var result 0)
  (loop [[start end] :in in]
    (loop [current-num :range-to [start end]]
      (let [num-length (-> current-num string length)]
	(when (even? num-length) #uneven number can never match
	  (when (= ;(partition (div num-length 2) (string current-num)))
	    (+= result current-num)))))) result)

(defn answer-2 [in]
  (var result 0)
  (loop [[start end] :in in]
    (loop [current-num :range-to [start end]]
      (loop [i :down [(-> current-num string length (div 2))]]
	(when (= ;(partition i (string current-num)))
	  (+= result current-num)
	  (break))))) result)

(answer-2 test-data)
(answer-1 test-data)
(answer-1 ranges)
