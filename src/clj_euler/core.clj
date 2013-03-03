(ns clj-euler.core
  (:gen-class))

(defn divides
  "True if any &param divides n"
  [n & rest]
  (some #(= 0 (rem n %)) rest))

(defn one
  "Project euler problem 1"
  [n]
  (reduce + (filter #(divides % 3 5) (range n))))

(defn even [n] (divides n 2))

(defn fibs
  "Collection of fibonacci numbers up to limit."
  [upto]
  ((fn fib [fiblist]
     (let [next (+ (last fiblist) (last (butlast fiblist)))]
       (if (> next upto) fiblist
           (fib (conj fiblist next)))))
   [0 1]))

(defn two
  "Project euler problem 2"
  [n]
  (reduce + (filter even (fibs n))))

(defn factor [n]
   ((fn fac [n i]
      (cond
       (> (* i i) n) [n]
       (divides n i) (conj (fac (/ n i) i) i)
       true (fac n (+ i 1))))
    n 2))

(defn three
  "Project euler problem 3"
  [n]
  (first (factor n)))

(defn digits
  [n]
  (if (= n 0)
    []
    (conj (digits (quot n 10)) (rem n 10))))

(defn palindrome?
  [n]
  (let [digs (digits n)]
    (= (reverse digs) digs)))

(defn all-products
  [xs ys]
  (set (flatten (map (fn [n] (map #(* n %) ys)) xs))))

(defn four
  [n]
  (let [candidates (reverse (range n))]
    (apply max
           (filter palindrome? (all-products candidates candidates)))))

(defn gcd
  [& numbers]
  (reduce
   (fn gcd-worker
     [a b]
     (if (= 0 b) a (gcd-worker b (rem a b))))
   numbers))

(defn occurences
  [n seq]
  (count (filter #(= n %) seq)))

(defn most-occurences
  [n seqs]
  (apply max (map #(occurences n %) seqs)))

(defn five
  "Project euler problem 5"
  [n]
  (let [candidates (range 2 n)
        factors (map factor candidates)
        distinct (set (flatten factors))]
    (apply * (map #(int (Math/pow % (most-occurences % factors))) distinct))))

(defn sum [numbers] (apply + numbers))

(defn six
  "Project euler problem 6"
  [n]
  (let [naturals (range 1 (+ 1 n))
        sq (fn [i] (* i i))
        squares (map sq naturals)
        naturals-sum (sum naturals)]
    (- (sq naturals-sum) (sum squares))))

(defn prime? [n] (= 1 (count (factor n))))

(defn seven
  [n]
  (nth (filter #(and (> % 1) (prime? %)) (range)) n))

(def big-digit
  (clojure.string/replace "73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450" "\n" ""))

(defn group
  [size vec]
  (filter #(= size (count %))
          (map-indexed
           (fn [index _]
             (subvec vec index
                     (min (+ index size) (count vec))))
           vec)))

(defn eight
  "Project euler problem 8"
  [n]
  (let [chars (drop 1 (clojure.string/split big-digit #""))
        digits (map #(Integer/parseInt %) chars)
        groups (group n (vec digits))]
    (apply max
           (map #(reduce * %) groups))))

(def squares (drop 1 (map #(* % %) (range))))

(defn generate-triplets [n]
  (let [cands (vec (range 1 n))
        indices (range (count cands))]
    (apply concat (map (fn [index]
                         (let [a (nth cands index)
                               bs (take-while #(< % (- n a %)) (subvec cands (+ index 1)))]
                           (map #(vec [a % (- n a %)]) bs)))
                       indices))))

(defn pythagorean-triplet? [[a b c]]
  (let [sq #(* % %)]
    (= (+ (sq a) (sq b)) (sq c))))

(defn nine
  "Project euler problem 9"
  [n]
  (apply * (flatten (filter pythagorean-triplet? (generate-triplets n)))))

(defn primes-up-to
  [n]
  (let [candidates (vec (range 2 (+ n 1)))
        lim (Math/sqrt n)
        divisors (take-while #(<= % lim) candidates)]
    (let [zero-these (set
                      (flatten
                       (map-indexed #(range (+ %2 %1) (count candidates) %2 )
                                    divisors)))]
      (filter #(not= 0 %)
              (map-indexed #(if (zero-these %1) 0 %2) candidates)))))

(defn ten
  "Project euler problem 10"
  [n]
  (reduce + (primes-up-to n)))

(defn read-matrix [path]
  (let [cont (slurp path)]
    (vec (map #(vec
                (map (fn [field] (Integer/parseInt field))
                     (clojure.string/split % #" ")))
              (clojure.string/split-lines cont)))))

(defn in-matrix [[x y] dim]
  (let [legal (set (range dim))]
    (and (legal x) (legal y))))

(defn diagonals [dim]
  (let [starts (map #(vec [% 0]) (range dim))
        right (fn [[x y]] [(+ x 1) (+ y 1)])
        left (fn [[x y]] [(- x 1) (+ y 1)])
        legal (fn [pt] (in-matrix pt dim))]
    (concat (map #(take-while legal
                              (iterate right %))
                 starts)
            (map #(take-while legal
                              (iterate left %))
                 starts))))

(defn transpose [m]
  (apply mapv vector m))

(defn all-cols-rows-diagonals
  [matrix]
  (let [rows matrix
        cols (transpose matrix)
        diags (map (fn [diag] (map
                               (fn [[x y]] (nth (nth matrix y) x)) diag))
                   (diagonals (count matrix)))]
    (concat rows cols diags)))

(defn eleven
  "Project euler problem 11"
  [n f]
  (let [matrix (read-matrix f)
        product #(reduce * %)
        data (all-cols-rows-diagonals matrix)]
    (apply max
           (map product
                (apply concat (map (fn [seq] (group n (vec seq))) data))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  args)

