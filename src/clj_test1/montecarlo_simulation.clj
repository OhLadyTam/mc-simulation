(ns clj-test1.montecarlo-simulation
  (:require incanter.stats yahoofinance histquotes quotes.stock)
  (:import (yahoofinance YahooFinance Stock)
           (histquotes HistoricalQuote)
           (quotes.stock StockQuote)))


;;(def coll (.getClose (.getHistory (YahooFinance/get "dax"))))

(def h (.getHistory (yahoofinance.YahooFinance/get "MSFT")))
(def price (.getPrice (.getQuote (yahoofinance.YahooFinance/get "MSFT"))))
(println price)

(def closePrices (for [x h] (conj closePrices (.getClose x))))
(println closePrices)
(count closePrices)
;;(defn calculate-mean [coll] (not-empty [coll] (/ (reduce + [coll]) (count [coll]))))

;;(def)
;;

(def closePricesSum ((for [x closePrices] (+ (get closePrices (.indexOf x closePrices) closePricesSum)))))


(defn mean [coll]
;  (/ (sum)))
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(mean closePrices)


(def vecClosePrices (vec '(closePrices)))
(println vecClosePrices)
(apply + vecClosePrices)

(def vecCP (vector))
(count vecClosePrices)
((for [x h] (conj vecCP (.getClose x))))

(conj vecCP (for [x h] (.getClose x)))

(vecCP)
(count vecCP)

(def vecCP1 (vector (conj (for [x h] (.getClose x)))))

(conj (for [x h] (.getClose x)))

(def moj-vektor (conj (for [x h] (.getClose x)))) ;;OVO RADI - VEKTOR IMA 13 ELEMENATA
(count moj-vektor)

(apply + moj-vektor) ;; OVO RADI - SABIRA SVIH 13 ELEMENATA
(mean moj-vektor) ;; NE RADI





(count vecCP1)

(def vecTT [1 2 3])
(for [x vecCP1] (println 1))





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

(incanter.stats/quantile-normal 0.5 0 0.5)
(standard-deviation closePrices)
(mean closePrices)

(defn calc-mcs-price [prob mn price]
  (* price (+ 1
              (incanter.stats/quantile-normal prob mn (calc-daily-volatility closePrices)))))

;;ovo je komentar
(defn calc-mcs-prices [startPrice] (
                                     (calc-mcs-price rand 0 startPrice)))
(calc-mcs-prices price)