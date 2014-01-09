(ns parsec.core)

(def ^:dynamic *white-spaces*
  "tokenize will use this global var, you could override it by binding"
  #{\space \return \newline \tab})

(defrecord Token [item lineno columnno])
(defrecord Ok [item])
(defrecord Err [errmsg])

(defrecord ErrMsg [reason lineno columnno])

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
        (eerr (ErrMsg. (str "expecting EOF, but got '"
                            (:item item)
                            "'")
                       (:lineno item)
                       (:columnno item))))
      (eok nil remainTokens))))

(defn token
  "consume a single item from the remainTokens failed if either the
  item consume? returns nil or if the input is empty"
  [consume?]
  (fn [remainTokens cok cerr eok eerr]
    (if (empty? remainTokens)
      (eerr (ErrMsg. "unexpected EOF" 1 1))
      (let [token (first remainTokens)]
        (if (consume? token)
          (cok token (rest remainTokens))
          (eerr (ErrMsg. (str "unexpected '"
                              (:item token)
                              "'")
                         (:lineno token)
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
                (q remainTokens cok cerr cok cerr)))
            (peok [item remainTokens]
              (let [q (f item)]
                (q remainTokens cok cerr eok eerr)))]
      (p remainTokens pcok cerr peok eerr))))

(defn >or
  "take a sequence of parser, and try them in order"
  ([p q]
   (fn [remainTokens cok cerr eok eerr]
     (letfn [(peerr [pErrMsg]
               (letfn [(qeerr [qErrMsg]
                         (eerr (ErrMsg. "couldn't parse p or q"
                                        (:lineno pErrMsg)
                                        (:columnno pErrMsg))))]
                 (q remainTokens cok cerr eok qeerr)))]
       (p remainTokens cok cerr eok peerr))))

  ([p q & more]
   (if (empty? more)
     (>or p q)
     (>or p (apply >or (cons q more))))))

(defn attempt
  "try to parse one token, if failed consume no input, otherwise
  return consumed input"
  [p]
  (fn [remainTokens cok cerr eok eerr]
    (p remainTokens cok eerr eok eerr)))

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
      ((any) remainTokens pcok cerr eok eerr))))

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
            (p remainTokens cok cerr many-err eerr))]
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
    (p remainTokens cok cerr eok peerr))))


(defn times
  "consume p n times"
  [n p]
  (cond (= n 0)
        (always [])

        (< n 0)
        (throw (RuntimeException. "time expectd n > 0"))

        :else
        (fn [remainTokens cok cerr eok eerr]
          (letfn [(pcok [item remainTokens]
                    (let [q (times (dec n) p)]
                      (letfn [(qcok [items remainTokens]
                                (cok (cons item items) remainTokens))]
                        (q remainTokens qcok cerr qcok eerr))))
                  (peok [item remainTokens]
                    (eok (repeat n item) remainTokens))]
            (p remainTokens pcok cerr peok eerr)))))


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
         (p# remainTokens# cok# cerr# eok# eerr#)))))

(defn tokenize
  "get string as args, tokenize it into list of Token, which contains
  item, lineno and columnno"
  [input-str]
  (filter #(> (count (:item %)) 0)
          (loop [cseq (seq input-str)
                 result []
                 current-str []
                 lineno 1
                 columnno 1]
            (let [c (first cseq)
                  crest (rest cseq)
                  is-newline (= c \newline)]
              (cond (empty? cseq)
                    (conj result
                          (Token. (apply str current-str)
                                  lineno
                                  (- columnno (count current-str))))

                    (contains? *white-spaces* c)
                    (recur crest
                           (conj result
                                 (Token. (apply str current-str)
                                         lineno
                                         (- columnno (count current-str))))
                           []
                           (if is-newline
                             (inc lineno)
                             lineno)
                           (if is-newline
                             1
                             (inc columnno)))

                    :else
                    (recur crest
                           result
                           (conj current-str c)
                           (if is-newline
                             (inc lineno)
                             lineno)
                           (if is-newline
                             1
                             (inc columnno))))))))

;; run parsers
(defn run-parser
  "Execute a parser p, given some tokens, Returns Ok or Err"
  [p tokens]
  (p tokens
     (fn cok [item _]
       (Ok. item))
     (fn cerr [err]
       (Err. err))
     (fn eok [item _]
       (Ok. item))
     (fn eerr [err]
       (Err. err))))

(defn run
  "Run a parser p over input. The input should be string, if the parser
  produces an error, its message is wrapped in a RuntimeException and
  thrown, if the parser succeeds, its value is returned"
  [p input-str]
  (let [result (run-parser p (tokenize input-str))]
    (condp instance? result
      Ok (:item result)
      Err (let [msg (:errmsg result)]
            (throw (RuntimeException. ^String
                                      (str (:reason msg)
                                           " at "
                                           (:lineno msg)
                                           ":"
                                           (:columnno msg))))))))
