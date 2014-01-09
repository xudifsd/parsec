(ns parsec.core
  (:import [parsec ParsecException TokenizeException]))

(def ^:dynamic *white-spaces*
  "tokenize will use this global var, you could override it by binding"
  #{\space \return \newline \tab})

(defrecord Token [item lineno columnno])
(defrecord Ok [item])
(defrecord Err [errmsg])

;; builtin parser
(defn always
  "a parser will always succeeds and consumes no input"
  [x]
  (fn [remainTokens cok cerr eok eerr]
    (eok x remainTokens)))

(defn never
  "a parser will never succeeds and consume no input"
  [errmsg]
  (fn [remainTokens cok cerr eok eerr]
    (eerr errmsg)))

(defn eof
  "matches EOF"
  []
  (fn [remainTokens cok cerr eok eerr]
    (if-not (empty? remainTokens)
      (let [item (first remainTokens)]
        (eerr (str "expecting EOF, but got '"
                   (:item item)
                   "'")))
      (eok nil remainTokens))))

(defn token
  "consume a single item from the remainTokens failed if either the
  item consume? returns nil or if the input is empty"
  [consume?]
  (fn [remainTokens cok cerr eok eerr]
    (if (empty? remainTokens)
      (eerr "unexpected EOF")
      (let [token (first remainTokens)]
        (if (consume? token)
          (cok token (rest remainTokens))
          (eerr (str "unexpected '"
                     (:item token)
                     "' at "
                     (:lineno token)
                     ":"
                     (:columnno token))))))))

(defn string
  "consume given string"
  [s]
  (token (fn [token]
           (.equals s (:item token)))))

(defn regex
  "consume it if token matches given regex"
  [re]
  (token (fn [token]
           (.matches (.matcher re (:item token))))))

(defn bind
  "bind previous parser's return and construct a new parser based on
  that value"
  [p f]
  (fn [remainTokens cok cerr eok eerr]
    (letfn [(pcok [item remainTokens]
              (let [q (f item)]
                #(q remainTokens cok cerr cok cerr)))
            (peok [item remainTokens]
              (let [q (f item)]
                #(q remainTokens cok cerr eok eerr)))]
      #(p remainTokens pcok cerr peok eerr))))

(defn >or
  "take a sequence of parser, and try them in order"
  ([p q]
   (fn [remainTokens cok cerr eok eerr]
     (letfn [(peerr [pErrMsg]
               (letfn [(qeerr [qErrMsg]
                         (eerr (str pErrMsg
                                    "\nand "
                                    qErrMsg)))]
                 #(q remainTokens cok cerr eok qeerr)))]
       #(p remainTokens cok cerr eok peerr))))

  ([p q & more]
   (if (empty? more)
     (>or p q)
     (>or p (apply >or (cons q more))))))

(defn attempt
  "try to parse one token, if failed consume no input, otherwise
  return consumed input"
  [p]
  (fn [remainTokens cok cerr eok eerr]
    #(p remainTokens cok eerr eok eerr)))

(defn any
  "accept any token but EOF"
  []
  (token (fn [_] true)))

(defn lookahead
  "return whatever token before us, and consume nothing"
  []
  (fn [remainTokens cok cerr eok eerr]
    (letfn [(pcok [item _]
              (eok item remainTokens))]
      #((any) remainTokens pcok cerr eok eerr))))

(defmacro let->>
  "expands into nested bind forms"
  [[& bindings] & body]
  (let [[bind-form p] (take 2 bindings)]
    (if (= 2 (count bindings))
      `(bind ~p (fn [~bind-form] ~@body))
      `(bind ~p (fn [~bind-form] (let->> ~(drop 2 bindings) ~@body))))))

(defn >*
  "consume 0 or more p, A RuntimeException will be thrown if this
  combinator is applied to a parser that accepts the empty string,
  as that would cause the parser to loop forever"
  [p]
  (letfn [(many-err [_ _]
            (throw (RuntimeException. (str "Combinator '*' is applied to a"
                                           " parser that accepts an empty"
                                           " string"))))
          (safe-p [remainTokens cok cerr eok eerr]
            #(p remainTokens cok cerr many-err eerr))]
    (>or
      (let->> [x safe-p
               xs (>* safe-p)]
        (always (cons x xs)))
      (always []))))

(defn >+
  "consume 1 or more p"
  [p]
  (let->> [head p
           rst (>* p)]
    (always (cons head rst))))

(defn >?
  "optionally consume p"
  [p]
  (fn [remainTokens cok cerr eok eerr]
    (letfn [(peerr [_]
              (eok nil remainTokens))]
    #(p remainTokens cok cerr eok peerr))))


(defn times
  "consume p n times"
  [n p]
  (cond (= n 0)
        (always [])

        (< n 0)
        (throw (ParsecException. "times expecting n > 0"))

        :else
        (fn [remainTokens cok cerr eok eerr]
          (letfn [(pcok [item remainTokens]
                    (let [q (times (dec n) p)]
                      (letfn [(qcok [items remainTokens]
                                (cok (cons item items) remainTokens))]
                        #(q remainTokens qcok cerr qcok eerr))))
                  (peok [item remainTokens]
                    (eok (repeat n item) remainTokens))]
            #(p remainTokens pcok cerr peok eerr)))))


(defn nxt
  "parser p and then q, returning q's value and discarding p's"
  [p q]
  (bind p (fn [_] q)))

(defmacro >>
  "expands into nested nxt forms"
  ([m] m)
  ([m n] `(nxt ~m ~n))
  ([m n & ms] `(nxt ~m (>> ~n ~@ms))))

(defn nxt-bind
  "parser p and q, returning cons of p's value and q's"
  [p q]
  (bind p (fn [p-item]
            (let->> [q-item q]
              (always (cons p-item (if (or (seq? q-item)
                                           (nil? q-item))
                                     q-item
                                     (list q-item))))))))

(defmacro >>-
  "expands into nested nxt-bind forms, return list of result"
  ([m] m)
  ([m n] `(nxt-bind ~m ~n))
  ([m n & ms] `(nxt-bind ~m (>>- ~n ~@ms))))

(defmacro defparser
  "Defines a new parser. Parsers are simply functions that accept the
  5 arguments state, cok, cerr, eok, eerr but this macro takes care
  of writing that ceremony for you and wraps the body in a >>"
  [name args & body]
  `(defn ~name ~args
     (fn [remainTokens# cok# cerr# eok# eerr#]
       (let [p# (>> ~@body)]
         #(p# remainTokens# cok# cerr# eok# eerr#)))))

;;;;;;;;;;;;;;;;;;
;; tokenize's util
(defn ws-divider
  "divide string once meet *white-spaces*, this divider is not useful
  in real programming language, because it can't skip comment, and will
  divide string literal if it contains white space, which is very common"
  [cseq-before current-char]
  (or (contains? *white-spaces* current-char) (nil? current-char)))

(defn legacy-tokenize
  "get string as args, tokenize it into list of Token, which contains
  item, lineno and columnno, accept a function with two args, divide
  token on return true, not divide on return false, if return 'discard
  discard current-str(is useful when tokenize comment)"
  [input-str divide?]

  (defn inc-lineno [is-newline lineno]
    (if is-newline
      (inc lineno)
      lineno))

  (defn inc-columnno [is-newline columnno]
    (if is-newline
      1
      (inc columnno)))

  (defn newToken [current-str lineno columnno]
    (Token. (apply str current-str)
            lineno
            (- columnno (count current-str))))

  (filter #(> (count (:item %)) 0)
          (loop [cseq (seq input-str)
                 result []
                 current-str []
                 lineno 1
                 columnno 1]
            (let [c (first cseq)
                  crest (rest cseq)
                  is-newline (= c \newline)]
              (if (empty? cseq)
                (let [value (divide? current-str nil)]
                  (if value
                    (conj result (newToken current-str lineno columnno))
                    (throw (TokenizeException. (str "divide? returns '"
                                                    value
                                                    "' in EOF")))))
                (let [value (divide? current-str c)]
                  (case value
                    ;; divide
                    true (recur crest
                                (conj result (newToken current-str lineno columnno))
                                []
                                (inc-lineno is-newline lineno)
                                (inc-columnno is-newline columnno))

                    ;; not divide
                    false (recur crest
                                 result
                                 (conj current-str c)
                                 (inc-lineno is-newline lineno)
                                 (inc-columnno is-newline columnno))

                    ;; discard current-str
                    'discard (recur crest
                                    result
                                    []
                                    (inc-lineno is-newline lineno)
                                    (inc-columnno is-newline columnno)))))))))

(defn tokenizer
  "accept bunch of regex, return lazy seq of token that matches one of regex"
  ([input-str res]
   (tokenizer input-str 1 1 res))

  ([input-str lineno columnno res]
   (if (= "" input-str)
     nil
     (let [first-matches (first
                           (drop-while #(not (first %))
                                       (->> res
                                         (map #(.matcher % input-str))
                                         (map (fn [x]
                                                (let [m (.lookingAt x)]
                                                  (list m (if m
                                                            (.end x)
                                                            nil))))))))]
       (let [matches (first first-matches)
             end (second first-matches)
             result (.substring input-str 0 end)
             rest-str (.substring input-str end)
             lineno-inc (reduce (fn [acc cur]
                                  (if (= cur \newline)
                                    (inc acc) acc))
                                0
                                result)
             columnno-inc (count (take-while #(not (= \newline %))
                                             (reverse result)))]
       (if matches
         (cons (Token. result lineno columnno)
               (lazy-seq (apply tokenizer (list rest-str
                                                (+ lineno lineno-inc)
                                                (if (= lineno-inc 0)
                                                  (+ columnno columnno-inc)
                                                  (+ 1 columnno-inc))
                                                res))))
         (throw (TokenizeException. (str "couldn't continue at "
                                         lineno
                                         ":"
                                         columnno)))))))))

(defn c-tokenizer [input-str]
  (let [ws #"[ \t\v\n\r\f]"
        block-c #"\/\*(\*(?!\/)|[^*])*\*\/"
        line-c #"//.*"]
    (filter #(not (or (.matches (.matcher ws (:item %)))
                      (.matches (.matcher block-c (:item %)))
                      (.matches (.matcher line-c (:item %)))))
            (tokenizer input-str
                       [#"[a-zA-Z_][0-9a-zA-Z_]*" ;id
                        #"(\d+\.\d+|\d+e\d+|\d+)" ;number
                        #"\"(\\.|[^\\\"])*\"" ;string
                        #"'(\\.|[^\\'])'" ;char-l
                        block-c
                        line-c
                        #"(=|\+|-|\*|/)" ; op
                        ws]))))

;; run parsers
(defn run-parser
  "Execute a parser p, given some tokens, Returns Ok or Err"
  [p tokens]
  (trampoline
    #(p tokens
        (fn cok [item _]
          (Ok. item))
        (fn cerr [err]
          (Err. err))
        (fn eok [item _]
          (Ok. item))
        (fn eerr [err]
          (Err. err)))))

(defn run
  "Run a parser p over input. The input should be string, if the parser
  produces an error, its message is wrapped in a RuntimeException and
  thrown, if the parser succeeds, its value is returned"
  ([p input-str]
   (let [token-seq (c-tokenizer input-str)
         result (run-parser p token-seq)]
     (condp instance? result
       Ok (:item result)
       Err (let [msg (:errmsg result)]
             (throw (ParsecException. ^String msg)))))))
