; This is the code used to generate the graphs and data used to look into
; the "Sell in May" stock investment strategy. The write-up of this can be
; found on http://www.agoraopus.com/blog/2015/sell-in-may-and-go-away
(ns sell-in-may.analysis-1
  (:import (org.joda.time LocalDate))
  (:require [incanter.core :as ic]
            [incanter.stats :as is]
            [incanter.charts :as icharts]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.pprint :as pp]))

; Load raw CSV data
(def raw-data (with-open [in-file (io/reader "../../resources/monthly_sp500.csv")]
                (doall (csv/read-csv in-file))))

; Skip header
(def raw-no-header (rest raw-data))

(defn string-date->local-date [s]
  "Convert a yyyy-MM-dd string pattern to a LocalDate"
  (let [parts (.split s "-")
        year (Integer/parseInt (aget parts 0))
        month (Integer/parseInt (aget parts 1))
        day (Integer/parseInt (aget parts 2))]
    (LocalDate. year month day)))

; Format raw data into a sorted map keyed by unix millis and a double price value
(def time-price (into (sorted-map) (map #(vec [(-> % first string-date->local-date .toDateMidnight .getMillis) (-> % last read-string)]) raw-no-header)))

; View above input data
(ic/view (icharts/time-series-plot (keys time-price) (vals time-price)))

; Convert price to log-returns, keyed by millis in a sorted map
(def time-returns (into (sorted-map) (map #(vec [(first %2) (Math/log (/ (second %2) (second %1)))]) time-price (rest time-price))))

; View above returns by time
(ic/view (icharts/time-series-plot (keys time-returns) (vals time-returns)))

; Group returns my month..
(def returns-by-month (group-by #(-> % first LocalDate. .getMonthOfYear) time-returns))

; Get average returns per month and view in bar chart
(def avg-returns-by-month (into (sorted-map) (map #(vec [(first %) (is/mean (map second (second %)))]) returns-by-month)))
(ic/view (icharts/bar-chart (keys avg-returns-by-month) (vals avg-returns-by-month)))

; Get median returns per month and view in bar chart
(def med-returns-by-month (into (sorted-map) (map #(vec [(first %) (is/median (map second (second %)))]) returns-by-month)))
(ic/view (icharts/bar-chart (keys med-returns-by-month) (vals med-returns-by-month)))

; To apply the "Sell in May" strategy with above data we stay out of the market when the month
; is one of May (5), June (6), July (7), August (8), September (9), and October (10).
; Ideally we'd use some historical "risk free" rate for when we're out of the market,
; but we'll keep it simple here and apply a fixed annual geometric rate as defined below.
(def risk-free 0.05)

(defn get-active-return [oom-fn risk-free [millis returns]]
  "Get the active return from either market or risk-free depending on month"
  (let [local-date (LocalDate. millis)
        month (.getMonthOfYear local-date)
        out-of-market (oom-fn month)]
    (if out-of-market (/ risk-free 12) returns)))

; As the time-returns are indexed by the following month, April returns are shown on month 5, etc..
(defn oom-may-october [month]
  "Returns true when we're staying out of market"
  (let [m-1 (dec month)]
    (get {5 true, 6 true, 7 true, 8 true, 9 true, 10 true} m-1 false)))

; Our active returns, keyed by millis as earlier..
(def active-time-returns (into (sorted-map) (map #(vec [(first %) ((partial get-active-return oom-may-october risk-free) %)]) time-returns)))

(defn accumulate-returns [{output :m r-sum :r} i]
  "A helper function to convert accumulated returns back into a normalized price"
  (let [millis (first i)
        returns (second i)
        new-r-sum (+ r-sum returns)
        new-output (assoc output millis (* 100 (Math/exp new-r-sum)))]
    {:m new-output :r new-r-sum}))

; Get passive and active normalized prices based on passive and active returns
(def passive-time-price (:m (reduce accumulate-returns {:m (sorted-map) :r 0.0} time-returns)))
(def active-time-price (:m (reduce accumulate-returns {:m (sorted-map) :r 0.0} active-time-returns)))

(defn add-to-chart [chart [label data]]
  "Helper function to add data series to a time series plot"
  (if (nil? chart)
    (icharts/time-series-plot (keys data) (vals data) :series-label label :legend true)
    (icharts/add-lines chart (keys data) (vals data) :series-label label :legend true)))

; Plot passive and active together..
(ic/view (add-to-chart (add-to-chart nil (vec ["Active" active-time-price])) (vec ["Passive" passive-time-price])))

; Get some simple alpha/beta stats:
(def lm-stats (is/linear-model (vals active-time-returns) (vals time-returns)))
(println (str "lm-stats coefs: " (:coefs lm-stats)))
(println (str "lm-stats t-probs: " (:t-probs lm-stats)))

; So is there really something special about May-October? Or is it just a freak random occurance?
; We can try to see how all other 6 month periods do compared to May-October..
(defn oom-january-june [month]
  (let [m-1 (dec month)]
    (get {1 true, 2 true, 3 true, 4 true, 5 true, 6 true} m-1 false)))
(defn oom-february-july [month]
  (let [m-1 (dec month)]
    (get {2 true, 3 true, 4 true, 5 true, 6 true, 7 true} m-1 false)))
(defn oom-march-august [month]
  (let [m-1 (dec month)]
    (get {3 true, 4 true, 5 true, 6 true, 7 true, 8 true} m-1 false)))
(defn oom-april-september [month]
  (let [m-1 (dec month)]
    (get {4 true, 5 true, 6 true, 7 true, 8 true, 9 true} m-1 false)))
(defn oom-june-november [month]
  (let [m-1 (dec month)]
    (get {6 true, 7 true, 8 true, 9 true, 10 true, 11 true} m-1 false)))
(defn oom-july-december [month]
  (let [m-1 (dec month)]
    (get {7 true, 8 true, 9 true, 10 true, 11 true, 0 true} m-1 false)))
(defn oom-august-january [month]
  (let [m-1 (dec month)]
    (get {8 true, 9 true, 10 true, 11 true, 0 true, 1 true} m-1 false)))
(defn oom-september-february [month]
  (let [m-1 (dec month)]
    (get {9 true, 10 true, 11 true, 0 true, 1 true, 2 true} m-1 false)))
(defn oom-october-march [month]
  (let [m-1 (dec month)]
    (get {10 true, 11 true, 0 true, 1 true, 2 true, 3 true} m-1 false)))
(defn oom-november-april [month]
  (let [m-1 (dec month)]
    (get {11 true, 0 true, 1 true, 2 true, 3 true, 4 true} m-1 false)))
(defn oom-december-may [month]
  (let [m-1 (dec month)]
    (get {0 true, 1 true, 2 true, 3 true, 4 true, 5 true} m-1 false)))

(def all-oom-fns [["January-June" oom-january-june]
                  ["February-July" oom-february-july]
                  ["March-August" oom-march-august]
                  ["April-September" oom-april-september]
                  ["May-October" oom-may-october]
                  ["June-November" oom-june-november]
                  ["July-December" oom-july-december]
                  ["August-January" oom-august-january]
                  ["September-February" oom-september-february]
                  ["October-March" oom-october-march]
                  ["November-April" oom-november-april]
                  ["December-May" oom-december-may]])

; Apply the above 12 different strategies..
(defn apply-oom [oom-fn] (into (sorted-map) (map #(vec [(first %) ((partial get-active-return oom-fn risk-free) %)]) time-returns)))
(def all-active-returns (map #(vec [(first %) (apply-oom (second %))]) all-oom-fns))
(def all-active-prices (map #(vec [(first %) (:m (reduce accumulate-returns {:m (sorted-map) :r 0.0} (second %)))]) all-active-returns))

; Plot all active prices together..
(ic/view (reduce add-to-chart nil all-active-prices))

; Look at alpha distribution
(defn get-alpha [passive-time-returns active-time-returns]
  "Helper function to return alpha and t-prob for a given pair of passive/active returns"
  (let [lm-stats (is/linear-model (vals active-time-returns) (vals passive-time-returns))
        alpha (-> lm-stats :coefs first)
        t-prob (-> lm-stats :t-probs first)]
    (vec [alpha t-prob])))

; Find and print all alphas from the above 12 strategies
(def all-alphas (map #(vec [(first %) (get-alpha time-returns (second %))]) all-active-returns))
(pp/pprint all-alphas)

; Build the dataset used to plot alpha/out-of-market timelines..
(def xy-alphas (let [alpha-map (into {} (map #(vec [(first %) (-> % second first (* 12))]) all-alphas))]
                 (-> (transient {})
                     (assoc! "January-June"         [[0  6]  [(get alpha-map "January-June")       (get alpha-map "January-June")]])
                     (assoc! "February-July"        [[1  7]  [(get alpha-map "February-July")      (get alpha-map "February-July")]])
                     (assoc! "March-August"         [[2  8]  [(get alpha-map "March-August")       (get alpha-map "March-August")]])
                     (assoc! "April-September"      [[3  9]  [(get alpha-map "April-September")    (get alpha-map "April-September")]])
                     (assoc! "May-October"          [[4  10] [(get alpha-map "May-October")        (get alpha-map "May-October")]])
                     (assoc! "June-November"        [[5  11] [(get alpha-map "June-November")      (get alpha-map "June-November")]])
                     (assoc! "July-December"        [[6  12] [(get alpha-map "July-December")      (get alpha-map "July-December")]])
                     (assoc! "August-January-1"     [[7  12] [(get alpha-map "August-January")     (get alpha-map "August-January")]])
                     (assoc! "August-January-2"     [[0  1]  [(get alpha-map "August-January")     (get alpha-map "August-January")]])
                     (assoc! "September-February-1" [[8  12] [(get alpha-map "September-February") (get alpha-map "September-February")]])
                     (assoc! "September-February-2" [[0  2]  [(get alpha-map "September-February") (get alpha-map "September-February")]])
                     (assoc! "October-March-1"      [[9  12] [(get alpha-map "October-March")      (get alpha-map "October-March")]])
                     (assoc! "October-March-2"      [[0  3]  [(get alpha-map "October-March")      (get alpha-map "October-March")]])
                     (assoc! "November-April-1"     [[10 12] [(get alpha-map "November-April")     (get alpha-map "November-April")]])
                     (assoc! "November-April-2"     [[0  4]  [(get alpha-map "November-April")     (get alpha-map "November-April")]])
                     (assoc! "December-May-1"       [[11 12] [(get alpha-map "December-May")       (get alpha-map "December-May")]])
                     (assoc! "December-May-2"       [[0  5]  [(get alpha-map "December-May")       (get alpha-map "December-May")]])
                     (persistent!))))

;(pp/pprint xy-alphas)

(defn add-to-xy-plot [{chart :chart count :count} [_ [x y]]]
  "Helper function to add data series to a xy plot"
  {:count (inc count),
   :chart (if (nil? chart)
              (-> (icharts/xy-plot x y)
                  (icharts/set-stroke :width 3)
                  (icharts/set-stroke-color java.awt.Color/black))
              (-> (icharts/add-lines chart x y)
                  (icharts/set-stroke :width 3 :dataset count)
                  (icharts/set-stroke-color java.awt.Color/black :dataset count)))})

; Plot above dataset
(ic/view (:chart (reduce add-to-xy-plot {:chart nil, :count 0} xy-alphas)))
