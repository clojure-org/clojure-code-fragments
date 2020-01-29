;; From: segmentation.util.format
;; URL: https://gist.github.com/marioaquino/10790120
(ns clojure-org.fragments.edn
  (:require [clojure.pprint :refer [pprint with-pprint-dispatch code-dispatch]]
            [rewrite-clj.parser :as p]
            [rewrite-clj.printer :as prn] ))

(defn is-token-sandwich [triplet]
  (let [first-token (-> triplet first first)
        second-token (-> triplet second first)
        result (and (not= first-token :newline) (= second-token :whitespace))]
    result))

(defn is-missing-whitespace [triplet]
  (let [first-token (-> triplet first first)
        second-token (-> triplet second first)
        result (and (not (contains? #{:newline :whitespace} first-token)) (not (contains? #{:newline :whitespace} second-token)))]
    result))

(defn drop-whitespace-from-ast [ast]
  (let [descriptor (first ast)
        tokens (rest ast)
        dropped (drop-while #(contains? #{:newline :whitespace} (first %1)) tokens)
        reversed-tokens (reverse dropped)
        dropped (reverse (drop-while #(contains? #{:newline :whitespace} (first %1)) reversed-tokens))
        my-range (dec (count dropped))
        token-sandwiches (reduce (fn [accum x] (assoc accum (inc x) (is-token-sandwich (subvec (vec dropped) x (+ x 2))))) {} (range my-range))

        missing-whitespaces (reduce (fn [accum x] (assoc accum (inc x) (is-missing-whitespace (subvec (vec dropped) x (+ x 2))))) {} (range my-range))

        eaten (vec (for [x (range (count dropped)) :let [token (nth dropped x)]] (if (get token-sandwiches x) [(first token) " "] token)))

        bloated (reduce (fn [accum x]
                          (let [token (nth eaten x)]
                            (if (get missing-whitespaces x)
                              (conj accum [:whitespace " "] token)
                              (conj accum token)))) [] (range (count eaten)))


        recursed-tokens (map (fn [token]
                               (if (contains? #{:set :seq :vector :list :map} (first token))
                                 (drop-whitespace-from-ast token)
                                 token))
                             bloated)
        new-ast (vec (cons descriptor recursed-tokens))]
    new-ast))

(defn code-format [code]
  (let [ast (p/parse-string-all code)
        new-ast (drop-whitespace-from-ast ast)]
    (prn/->string new-ast)))

(defn write-file [filename content]
  (with-open [w (clojure.java.io/writer filename :append false)]
    (.write w content)))

(defn format-file [filename]
  (let [code (slurp filename)
        formatted-content(code-format code)]
    (write-file filename formatted-content)))

(defn -main
  "If called with args, expects args to be file paths and will write format output to each path.
  Otherwise, reads from standard input and writes output to standard output"
  [& args]
  (if (not-empty args)
    (doseq [file args]
      (format-file file))
    (let [code (slurp *in*)]
      (print (code-format code)))))
