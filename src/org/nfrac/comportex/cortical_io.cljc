(ns org.nfrac.comportex.cortical-io
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]
            #?(:clj [clj-http.client :as http]
               :cljs [cljs-http.client :as http])
            #?(:cljs [cljs.core.async :refer [<!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(def base-uri "http://api.cortical.io/rest")
(def query-params {:retina_name "en_associative"})
(def retina-dim [128 128])
(def retina-size (apply * retina-dim))
(def max-bits 512)
(def min-votes 2)

(defn request-fingerprint
  [api-key term]
  (http/post (str base-uri "/expressions")
             {:query-params query-params
              :content-type "application/json"
              :as :json
              #?(:clj :form-params, :cljs :json-params) {:term term}
              :with-credentials? false
              :throw-exceptions false
              :headers {"api-key" api-key}}))

(defn request-similar-terms
  [api-key bits max-n]
  (http/post (str base-uri "/expressions/similar_terms")
             {:query-params (assoc query-params
                              :get_fingerprint true
                              :max_results max-n)
              :content-type "application/json"
              :as :json
              #?(:clj :form-params, :cljs :json-params) {:positions (sort bits)}
              :with-credentials? false
              :throw-exceptions false
              :headers {"api-key" api-key}}))

(defn random-sdr
  [term]
  (enc/unique-sdr term retina-size (* retina-size 0.02)))

(defn scramble-bit
  "Maps a retina fingerprint index to another index which is spatially
   scrambled. Meaning that direct neighbours in the retina are now
   offset from each other by around 17 units. This is a complete
   mapping, i.e. the following holds:

   `
   (= (set (map scramble-bit (range retina-size)))
      (set (range retina-size)))`."
  [i]
  (mod (* i 17) retina-size))

;; don't ask me how i got this to work
(defn unscramble-bit
  [j]
  (-> (rem j 17)
      (* 13) ;; == (rem retina-size 17)
      (mod 17)
      (* retina-size)
      (+ j)
      (quot 17)))

(defn scramble-bitset
  [bits]
  (map scramble-bit bits))

(defn ?assoc
  "assoc, but not if the key already has a (truthy) value."
  [m k v]
  (if (m k) m (assoc m k v)))

(defn cache-fingerprint!
  "Makes a request to cortical.io to look up the fingerprint for the
   term, and stores it in the given cache atom. In Clojure this is a
   synchonous call and returns the fingerprint bit-set. In
   Clojurescript this is an asynchronous call and returns a channel."
  [api-key cache term]
  (let [term (str/lower-case term)
        handle (fn [result]
                 (if (http/unexceptional-status? (:status result))
                   (set (get-in result [:body :positions]))
                   (do (println "cortical.io lookup of term failed:" term)
                       (println result)
                       (random-sdr term))))]
    #?(:clj   ;; clj - synchronous
       (let [result (request-fingerprint api-key term)]
         (swap! cache ?assoc term (handle result)))
       :cljs  ;; cljs - asynchronous
       (go
        (let [result (<! (request-fingerprint api-key term))]
          (swap! cache ?assoc term (handle result)))))))

(defn get-fingerprint
  "Looks up a fingerprint for the term, being a set of active indices,
   in the cache. If it is not found (which should not happen, it
   should be preloaded from the web service), the term is assigned a
   new random SDR."
  [cache term]
  (let [term (str/lower-case term)]
    (or (get @cache term)
        (get (swap! cache ?assoc term
                    (do (println "no fingerprint in cache for term:" term
                                 "- generating a random one")
                        (random-sdr term)))
             term))))

(defn elect-bits
  [bit-votes min-votes max-bits]
  (loop [min-votes min-votes]
    (let [bits (keep (fn [[i n]]
                       (when (>= n min-votes) i))
                     bit-votes)]
      (if (> (count bits) max-bits)
        (recur (inc min-votes))
        bits))))

(defn cortical-io-encoder
  [api-key cache & {:keys [decode-locally? spatial-scramble?]}]
  (let [topo (topology/make-topology retina-dim)]
    (reify
      p/PTopological
      (topology [_]
        topo)
      p/PEncoder
      (encode
        [_ term]
        (if (seq term)
          (cond->
           (get-fingerprint cache term)
           spatial-scramble? (scramble-bitset))
          (sequence nil)))
      (decode
        [this bit-votes n]
        (let [bit-votes (if spatial-scramble?
                          (zipmap (map unscramble-bit (keys bit-votes))
                                  (vals bit-votes))
                          bit-votes)]
          (if decode-locally?
            (->> (enc/decode-by-brute-force this (keys @cache) bit-votes)
                 (take n))
            ;; otherwise - remote request for similar terms
            (let [bits (elect-bits bit-votes min-votes max-bits)]
              (if (empty? bits)
                []
                (let [total-votes (apply + (vals bit-votes))
                      handle
                      (fn [result]
                        (if (http/unexceptional-status? (:status result))
                          (->> (:body result)
                               (map (fn [item]
                                      (let [x-bits (set (get-in item [:fingerprint
                                                                      :positions]))]
                                        (println "received prediction results.")
                                        (println "x-bits =" x-bits)
                                        (println "bits =" bits)
                                        (-> (enc/prediction-stats x-bits bit-votes
                                                                  total-votes)
                                            (assoc :value (get item :term))))))
                               (take n))
                          (println result)))]
                  #?(:clj  ;; clj - synchronous
                     (handle (request-similar-terms api-key bits n))
                     :cljs ;; cljs - asynchronous
                     {:channel
                     (go
                      (handle (<! (request-similar-terms api-key bits n))))}))
                ))))))))
