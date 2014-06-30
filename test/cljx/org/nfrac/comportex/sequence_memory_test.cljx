(ns org.nfrac.comportex.sequence-memory-test
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.parameters]
            #+clj [clojure.test :as t
                   :refer (is deftest testing run-tests)]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.cljs.test
                           :refer (is deftest testing run-tests)]))

(def bit-width 200)
(def items [:a :b :c :d :e :f :g :h])

(def patterns
  [[:a :b :c :d]
   [:a :b :f :g]
   [:a :b :b :a]
   [:h :g :f :e]
   [:d :e :a :d]])

(def initial-input [0 0])

(defn input-transform
  [[i j]]
  (let [patt (get patterns i)]
    (if (< (inc j) (count patt))
      [i (inc j)]
      ;; finished pattern, choose another one
      [(util/rand-int 0 (dec (count patterns)))
       0])))

(def efn
  (let [f (enc/category-encoder bit-width items)]
    (fn [[i j]]
      (f (get-in patterns [i j])))))

(defn model
  []
  (let [gen (core/generator initial-input input-transform efn
                            {:bit-width bit-width})
        spec (assoc org.nfrac.comportex.parameters/small
               :ncol 200
               :input-size bit-width
               :potential-radius (quot bit-width 2))]
    (core/cla-model gen spec)))

(deftest sm-test
  (util/set-seed! 0)
  (testing "CLA with sequence memory runs"
   (let [m1 (-> (iterate core/step (model))
                (nth 500))
         r (:region m1)]
     (let [depth (:depth (:spec r))
           ncells (map (comp count :cells) (:columns r))]
       (is (every? #(= depth %) ncells)
           "All columns have the specified number of cells."))
     (let [nsegs (map (fn [c] (count (mapcat :segments (:cells c))))
                      (:columns r))]
       (is (pos? (util/quantile nsegs 0.6))
           "At least 40% of columns have grown dendrite segments."))
     (let [ncol (count (:columns r))
           freqs (core/column-state-freqs r)]
       (is (> (:active freqs) (:unpredicted freqs))
           "Most column activations are predicted.")
       (is (> (:predicted freqs) 0)
           "Some columns were predicted but are not active.")
       (is (< (+ (:active freqs)
                 (:unpredicted freqs)
                 (:predicted freqs))
              (* ncol 0.2))
           "Less than 20% of columns are active or predicted.")))))
