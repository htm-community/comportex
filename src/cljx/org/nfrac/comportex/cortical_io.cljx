(ns org.nfrac.comportex.cortical-io
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]
            [clj-http.client :as http]
            [clojure.core.async :refer [go >! <! >!! <!! timeout thread]]))

(def base-uri "http://api.cortical.io/rest")

(def retina-size [128 128])

(def query-params {:retina_name "en_associative"})

(defn get-fingerprint
  [api-key term]
  (http/post (str base-uri "/expressions")
             {:query-params query-params
              :content-type :json
              :as :json
              :form-params {:term term}
              :with-credentials? false
              :throw-exceptions false
              :headers {"api-key" api-key}}))

(defn get-similar-terms
  [api-key bits max-n]
  (http/post (str base-uri "/expressions/similar_terms")
             {:query-params (assoc query-params
                              :get_fingerprint true
                              :max_results max-n)
              :content-type :json
              :as :json
              :form-params {:positions bits}
              :with-credentials? false
              :throw-exceptions false
              :headers {"api-key" api-key}}))

(defn apply-offset
  [xs offset]
  (into (empty xs) (map #(+ % offset))))

(defn random-sdr
  []
  (let [size (apply * retina-size)]
   (set (repeatedly (* size 0.02)
                    #(util/rand-int 0 (dec size))))))

(defn look-up-fingerprint
  [api-key cache term]
  (let [term (str/lower-case term)]
    (or (get @cache term)
        (get (swap! cache assoc term
                    (let [result (get-fingerprint api-key term)]
                      (if (http/success? result)
                        (set (get-in result [:body :positions]))
                        (do (println "cortical.io lookup of term failed:" term)
                            (println result)
                            (random-sdr)))))
             term))))

(defn cortical-io-encoder
  [api-key cache min-votes]
  (let [topo (topology/make-topology retina-size)]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncodable
      (encode
        [_ offset term]
        (if (seq term)
          (cond->
           (look-up-fingerprint api-key cache term)
           (not (zero? offset)) (apply-offset offset))
          #{}))
      (decode
        [_ bit-votes n]
        (let [bits (keep (fn [[i votes]]
                           (when (>= votes min-votes) i))
                         bit-votes)]
          (if (empty? bits)
            []
            (let [total-votes (apply + (vals bit-votes))
                  result (get-similar-terms api-key bits n)]
              (if (http/success? result)
                (->> (:body result)
                     (map (fn [item]
                            (let [x-bits (set (get item :positions))
                                  o-votes (select-keys bit-votes x-bits)
                                  total-o-votes (apply + (vals o-votes))
                                  o-bits (keys o-votes)]
                              {:value (get item :term)
                               :bit-coverage (/ (count o-bits)
                                                (max 1 (count x-bits)))
                               :bit-precision (/ (count o-bits)
                                                 (max 1 (count bit-votes)))
                               :votes-frac (/ total-o-votes
                                              (max 1 total-votes))
                               :votes-per-bit (/ total-o-votes
                                                 (max 1 (count x-bits)))}))
                          )
                     (take n))
                (println result)))))))))
