(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e]
  	             (if (zero? e)
  	             	acc
  	             	(recur (* acc b) b (dec e))))]
  (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last-elem s]
  	             (if (empty? s)
  	             	last-elem
  	             	(recur (first s) (rest s))))]
  (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
  	             (cond
  	             	(and (empty? s1) (empty? s2)) true
  	             	(or (empty? s1) (empty? s2)) false
  	             	(== (first s1) (first s2)) (recur (rest s1) (rest s2))
  	             	:else false))]
  (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
  	     i 0]
  	(cond
  		(empty? s) nil
  		(pred (first s)) i
  		:else (recur (rest s) (inc i)))))

(defn avg [a-seq]
  (loop [acc 0
  	     i 0
  	     s a-seq]
    (if (empty? s)
    	(/ acc (if (zero? i) 1 i))
    	(recur (+ acc (first s)) (inc i) (rest s)))))

(defn parity [a-seq]
  (loop [set-1 #{}
  	     seq-1 a-seq]
  	(if (empty? seq-1)
  		set-1
  		(recur 
  			(if (contains? set-1 (first seq-1))
  			  (disj set-1 (first seq-1))
  			  (conj set-1 (first seq-1)))
  			  (rest seq-1)))))

(defn fast-fibo [n]
  (loop [next-fib 1
  	     i-fib 0
  	     i 0]
  	(if (== i n)
  		i-fib
  		(recur (+ next-fib i-fib) next-fib (inc i) ))))

(defn cut-at-repetition [a-seq]
  (loop [v []
  	     s a-seq]
  	(cond
  		(empty? s) v
  		(contains? (set v) (first s)) v
  		:else (recur (conj v (first s)) (rest s)))))


