(ns parsec.core-test
  (:require [clojure.test :refer :all])
  (:use parsec.core)
  (:import [parsec.core Token]
           [parsec ParsecException TokenizeException]))

(deftest test-buildin-parser
  (testing "test always"
    (is (=
          (run (always 5) "abc")
          5))
    (is (=
          (run (always 10) ""))))

  (testing "test any"
    (is (=
          (:item (run (any) "abc edf"))
          "abc"))
    (is (=
          (:item (run (any) "12 41s"))
          "12")))

  (testing "test string"
    (is (thrown? ParsecException
          (run (string "a") "abc edf")))
    (is (=
          (:item (run (string "a") "a bc edf"))
          "a"))
    (is (thrown? ParsecException
                 (run (string "a") "12 41s"))))

  (testing "test regex"
    (is (=
          (:item (run (regex #"[a-z]+") "abc edf"))
          "abc"))
    (is (thrown? ParsecException
                 (run (regex #"[a-z]+") "12 41s"))))

  (testing "test >or"
    (is (=
          (:item (run (>or (string "abc")
                           (string "def")
                           (string "ghi"))
                      "ghi edf"))
          "ghi"))
    (is (=
          (:item (run (>or (string "abc")
                           (string "def")
                           (string "ghi"))
                      "abc edf"))
          "abc"))
    (is (thrown? ParsecException
                 (run (>or (string "abc")
                           (string "def")
                           (string "ghi"))
                      "bc edf"))))

  (testing "test >>"
    (is (=
          (:item (run (>> (string "abc")
                          (string "def"))
                      "abc def"))
          "def"))
    (is (thrown? ParsecException
                 (run (>> (string "abc")
                          (string "def")
                          (string "ghi"))
                      "bc edf"))))

  (testing "test attempt"
    (is (thrown? ParsecException
                 (run (attempt (string "abc"))
                      "bc edf")))
    (is (=
          (:item (run (>or (attempt (string "abc"))
                           (string "def"))
                      "abc edf"))
          "abc"))

    ;; attempt will consume on succ
    (is (=
          (:item (run (>> (attempt (regex #"[a-z]+"))
                          (string "def"))
                      "abc def"))
          "def"))
    (is (=
          (:item (run (>or (attempt (string "abc"))
                           (string "bc"))
                      "bc edf"))
          "bc")))

  (testing "test lookahead"
    ;; lookahead shouldn't consume anything even if it succ
    (is (=
          (:item (run (>>
                        (lookahead)
                        (string "abc"))
                      "abc edf"))
          "abc")))

  (testing "test >*"
    (is (=
          (count (run (>* (regex #"[a-z]+"))
                      "bc edf 1 2")))
        2)
    (is (=
          (count (run (>* (regex #"[a-z]+"))
                      "1 bc edf 1 2")))
        0))

  (testing "test >?"
    (is (=
          (:item (run (>? (regex #"[a-z]+"))
                      "bc edf 1 2"))
          "bc"))
    (is (=
          (:item (run (>? (regex #"[a-z]+"))
                      "2 bc edf 1 2"))
          nil)))

  (testing "test times"
    (is (=
          (count (run (times 2 (regex #"[a-z]+"))
                      "bc edf 1 2"))
          2))
    (is (thrown? ParsecException
                 (run (times -1 (regex #"[a-z]+"))
                      "bc edf")))
    (is (thrown? ParsecException
                 (run (times 2 (regex #"[a-z]+"))
                      "bc 2 edf"))))

  (testing "test eof"
    (is (thrown? ParsecException
          (run (>> (string "bc") (eof))
               "bc edf 1 2")))
    (is (nil?
          (run (>> (string "bc") (eof))
               "bc"))))

  (testing "test >+"
    (is (=
          (count (run (>+ (regex #"[a-z]+"))
                      "bc edf 1 2")))
        2)
    (is (thrown? ParsecException
          (run (>+ (regex #"[a-z]+"))
                      "1 a 1 bc edf 1 2")))
    (is (=
          (count (run (>+ (regex #"[a-z]+"))
                      "a 1 bc edf 1 2")))
        1))

  (testing "test >>-"
    (is (=
          (count (run (>>- (regex #"[a-z]+") (string "edf"))
                      "bc edf 1 2"))
          2))
    (is (=
          (count (run (>>- (regex #"[a-z]+")
                           (string "edf")
                           (eof))
                      "bc edf"))
          2))
    (is (=
          (count (run (>>- (regex #"[a-z]+")
                           (string "edf")
                           (regex #"[0-9]+"))
                      "bc edf 12"))
          3))
    (is (thrown? ParsecException
           (run (>>- (regex #"[a-z]+")
                     (string "c"))
                "a 1 bc edf 1 2")))))

(deftest test-basic-usage
  (def n (regex #"[0-9]+"))

  (defparser atomExpr []
    n)

  (defparser multiExpr []
    (let->> [a (atomExpr)]
      (>or
        (let->> [op (regex #"([*]|/)")
                 b (atomExpr)]
          (let [a (Long/valueOf (:item a))
                op (:item op)
                b (Long/valueOf (:item b))]
            (always (Token. (if (= op "*")
                              (* a b)
                              (/ a b))
                            1
                            1))))
        (always a))))

  (defparser expr []
    (let->> [a (multiExpr)]
      (>or
        (let->> [op (regex #"([+]|-)")
                 b (multiExpr)]
          (let [a (Long/valueOf (:item a))
                op (:item op)
                b (Long/valueOf (:item b))]
            (always (Token. (if (= op "+")
                              (+ a b)
                              (- a b))
                            1
                            1))))
        (always a))))


  (defparser stats []
    (let->> [item (expr)]
      (always item)))

  (defparser prog []
     (let->> [result (>+ (stats))]
       (eof)
       (always result)))

  (testing "test basic usage"
    (is (=
          (map :item (run (prog) "1 + 2 * 3\n2 * 2 + 3\n1 / 2"))
          '(7 7 1/2)))
    (is (=
          (map :item (run (prog) "1+2*3\n2+3\n1/2"))
          '(7 5 1/2)))))

(deftest test-ws-divider
  (testing "test ws-divider"
    (is (=
          (count (legacy-tokenize "a b c" ws-divider))
          3))
    (is (=
          (:item (first (legacy-tokenize "a b c" ws-divider)))
          "a"))
    (is (=
          (count (legacy-tokenize "a b \tc\n\td" ws-divider))
          4))))

(deftest test-c-tokenizer
  (testing "test c-tokenizer"
    (is (=
          (count (c-tokenizer "a b c/**/"))
          3))
    (is (=
          (:item (first (c-tokenizer "ab b c/**/")))
          "ab"))
    (is (=
          (:item (first (c-tokenizer "//abc\nefg")))
          "efg"))
    (is (let [result (c-tokenizer "'a'")]
          (and (= (:item (first result)) "'a'")
               (= (count result) 1))))
    (is (let [result (c-tokenizer "1+2")]
          (and (= (:item (first result)) "1")
               (= (count result) 3))))
    (is (let [result (c-tokenizer "/*a*/b")]
          (and (= (:item (first result)) "b")
               (= (count result) 1))))
    (is (let [result (c-tokenizer "/*a\\*/b")]
          (and (= (:item (first result)) "b")
               (= (count result) 1))))
    (is (let [result (c-tokenizer "/*a*\\/b*/c")]
          (and (= (:item (first result)) "c")
               (= (count result) 1))))
    (is (let [result (c-tokenizer "//\na b")]
          (and (= (:item (first result)) "a")
               (= (count result) 2))))
    (is (let [result (c-tokenizer "2.2 a")]
          (and (= (:item (first result)) "2.2")
               (= (count result) 2))))
;    (is (let [result (c-tokenizer "//c\\\na\nb")]
;          (and (= (:item (first result)) "b")
;               (= (count result) 1))))
;    (is (let [result (c-tokenizer "{a b c}")]
;          (and (= (:item (first result)) "{")
;               (= (count result) 5))))
;    (is (let [result (c-tokenizer "a+\\\nb")]
;          (and (= (:item (first result)) "a")
;               (= (count result) 3))))
;    (is (let [result (c-tokenizer "-2.2 a")]
;          (and (= (:item (first result)) "-2.2")
;               (= (count result) 2))))
;    (is (thrown? TokenizeException
;          (c-tokenizer "/*a b c")))
;    (is (thrown? TokenizeException
;          (c-tokenizer "'a\\''")))
;    (is (thrown? TokenizeException
;          (c-tokenizer "\"\\\""))) ;;"\"EOF
;    (is (thrown? TokenizeException
;          (c-tokenizer "/*a b c*")))
           ))
