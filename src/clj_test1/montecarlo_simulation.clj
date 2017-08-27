(ns clj-test1.montecarlo-simulation
  (:require incanter.stats yahoofinance histquotes quotes.stock)
  (:import (yahoofinance YahooFinance Stock)
           (histquotes HistoricalQuote)
           (quotes.stock StockQuote)
           (incanter.stats incanter.stats)))


;;(def coll (.getClose (.getHistory (YahooFinance/get "dax"))))

(def h (.getHistory (yahoofinance.YahooFinance/get "MSFT")))
(def price (.getPrice (.getQuote (yahoofinance.YahooFinance/get "MSFT"))))
(println price)

;(def closePrices (for [x h] (conj closePrices (.getClose x))))
;(println closePrices)
;(count closePrices)
;;(defn calculate-mean [coll] (not-empty [coll] (/ (reduce + [coll]) (count [coll]))))

;;(def)
;;

;(def closePricesSum ((for [x closePrices] (+ (get closePrices (.indexOf x closePrices) closePricesSum)))))


(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ (double sum) (double count))
      0)))

(def moj-vektor (conj (for [x h] (.getClose x)))) ;;OVO RADI - VEKTOR IMA 13 ELEMENATA
(count moj-vektor)

(apply + moj-vektor) ;; OVO RADI - SABIRA SVIH 13 ELEMENATA
(mean moj-vektor) ;; NE RADI

(let [sum (apply + moj-vektor) count (count moj-vektor)] (if (pos? count) (/ (double sum) (double count)) 0));; RADI MEAN pogledaj with-precision u clojure.docs za zaokruzivanje

(println moj-vektor)





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

(standard-deviation moj-vektor)

(defn calc-d-v-normalized [coll] (loop [iter 1 norm []] (if (< iter (count coll)) (recur (conj norm (nth coll iter)) (inc iter)) (standard-deviation norm))))

(loop [iter 1 norm []] (if (< iter (count moj-vektor)) (recur
                                                         (conj norm
                                                                    (nth moj-vektor iter)
                                                              (inc iter)))
                                                       (standard-deviation norm)))

(defn normalize-stddev [] (loop [iter 1 norm []] (if (< iter 13) (recur (inc iter)  (conj norm (- (/ (double (nth moj-vektor iter)) (double (nth moj-vektor (- iter 1) )) 1)))) (standard-deviation norm))))

(nth moj-vektor (- 1 1))









(calc-d-v-normalized moj-vektor)

(defn calc-daily-volatility [coll] (standard-deviation coll))
(defn calc-annual-volatility [coll] (* (Math/sqrt 252) (calc-daily-volatility coll)))

(calc-daily-volatility moj-vektor)
(calc-annual-volatility moj-vektor)

(defn calc-mcs-price [prob mn price]
  (* price (+ 1
              (incanter.stats/quantile-normal prob mn (normalize-stddev)))))
(calc-mcs-price 0.021362922108393856 0 start)

(def start (.getPrice (.getQuote (yahoofinance.YahooFinance/get "MSFT"))))

(defn calc-mcs-prices
  [price prob]
  (loop [i 0 result-set [(calc-mcs-price prob 0 price)]]
    (if (< i 21)
      (recur (inc i) (conj result-set (calc-mcs-price prob 0 (last result-set))))
      result-set)))

(calc-mcs-prices start (rand))

(defn check-what-happens [] (let [probab 0.021362922108393856] (println "***** res: "(calc-mcs-prices start probab) "***** prob: " probab)))

(check-what-happens)

(incanter.stats/quantile-normal 0.6 0 5.230451235351243)
