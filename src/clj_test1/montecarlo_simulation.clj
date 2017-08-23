(ns clj-test1.montecarlo-simulation
  (:require incanter.stats yahoofinance histquotes quotes.stock)
  (:import (yahoofinance YahooFinance Stock)
           (histquotes HistoricalQuote)
           (quotes.stock StockQuote)))


;;(def coll (.getClose (.getHistory (YahooFinance/get "dax"))))

(def h (.getHistory (YahooFinance/get "dax")))
(def close (.getClose h))

(def closePrices (for [x h] (cons (.getClose x) closePrices)))


;;(defn calculate-mean [coll] (not-empty [coll] (/ (reduce + [coll]) (count [coll]))))

;;(def)

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway) ; (1)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn standard-deviation [coll]
  (let [avg (mean coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (-> (/ (apply + squares)
           (- total 1))
        (Math/sqrt))))


(defn calc-daily-volatility [coll] (standard-deviation [coll]))
(defn calc-annual-volatility [coll] (* (Math/sqrt 252) calc-daily-volatility [coll]))




(defn calc-mcs-price [prob mean price]
  (* price (+ 1
              (incanter.stats/quantile-normal prob mean calc-daily-volatility [closePrices]))))

;;ovo je komentar
(defn calc-mcs-prices [startPrice] (
                                     (calc-mcs-price rand 0 startPrice)))
