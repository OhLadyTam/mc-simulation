(ns clj-test1.core
  (:require yahoofinance histquotes quotes.stock)
  (:import
    (yahoofinance YahooFinance Stock)
    (histquotes HistoricalQuote)
    (quotes.stock StockQuote)))

(defn get-stock []
  (YahooFinance/get "dax"))                   ;; uzimanje Stock za dax

;;(def history (.getHistory get-stock))                        ;; uzimanje history za Stock


;;(defn prn-hist-quote [x] (.getClose x))                     ;;iteriranje kroz listu historical quote

;;(for [x history] (println (prn-hist-quote x)))              ;; uzimanje close

(get-stock)

(def h (.getHistory (YahooFinance/get "dax")))
(defn prn-hist-quote [x] (.getClose x))
(for [x h] (println (prn-hist-quote x)))


(def price (.getPrice (.getQuote (YahooFinance/get "dax"))))

(println price)

