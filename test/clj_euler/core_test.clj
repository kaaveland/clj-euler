(ns clj-euler.core-test
  (:use clojure.test
        clj-euler.core))

(deftest one-test
  (testing "Problem one"
    (is (= 233168 (one 1000)))))

(deftest two-test
  (testing "Problem two"
    (is (= 4613732 (two 4000000)))))

(deftest three-test
  (testing "Problem three"
    (is (= 6857 (three 600851475143)))))

(deftest four-test
  (testing "Problem four"
    (is (= 906609 (four 1000)))))

(deftest five-test
  (testing "Problem five"
    (is (= 232792560 (five 21)))))

(deftest six-test
  (testing "Problem six"
    (is (= 25164150 (six 100)))))

(deftest seven-test
  (testing "Problem seven"
    (is (= 104743 (seven 10000)))))

(deftest eight-test
  (testing "Problem eight"
    (is (= 40824 (eight 5)))))

(deftest nine-test
  (testing "Problem nine"
    (is (= 31875000 (nine 1000)))))

(deftest ten-test
  (testing "Problem ten"
    (is (= 142913828922 (ten 2000000)))))

(deftest eleven-test
  (testing "Problem eleven"
    (is (= 70600674 (eleven 4
                            "test/clj_euler/eleven.txt")))))

(deftest twelve-test
  (testing "Problem twelve"
    (is (= 76576500 (twelve 500)))))