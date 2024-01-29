(ns compress
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn create-frequency-mapping [file-path]
  (with-open [file (io/reader file-path)]
    (let [word-list (string/split (slurp file) #"\s+")
          frequency-map (reduce (fn [frequency-map current-word]
                              (if (contains? frequency-map current-word)
                                frequency-map
                                (assoc frequency-map current-word (count frequency-map))))
                            {}
                            word-list)]
      (vec (keys (sort-by val frequency-map))))))

(defn frequency-ignore-case [vector word]
  (let [lower-word (string/lower-case word)
        frequency-values (keep-indexed
                         (fn [idx val]
                           (when (= (string/lower-case val) lower-word)
                             idx))
                         vector)]
    (if (seq frequency-values)
      (first frequency-values)
      -1)))

(defn remove-extra-spaces [file-path]
  (with-open [reader (io/reader file-path)]
    (let [text (apply str (line-seq reader))
          reduced-text (string/replace text #"\s+" " ")
          trimmed-text (string/trimr reduced-text)]
      (spit file-path trimmed-text))))

(defn compress-file [input-file frequency-vector]
  (let [compressed-file (str input-file ".ct")]
    (with-open [input (io/reader input-file)
                output (io/writer compressed-file)]
      (let [result (->> (line-seq input)
                        (mapcat #(string/split % #"(?<=\w)(?=\W)|(?<=\W)(?=\w)|(?<=\W)(?=\W)"))
                        (map #(let [word %]
                                (cond
                                  (re-matches #"\W+" word) (str " " word " ")
                                  (re-matches #"\d+" word) (str "@" word "@")
                                  :else (let [lower-word (string/lower-case word)
                                              frequency (frequency-ignore-case frequency-vector lower-word)]
                                          (if (>= frequency 0)
                                            (str frequency)
                                            word))))))
            result-str (reduce str result)]
        (spit output result-str)
        (remove-extra-spaces compressed-file)))))

(defn capitalize-characters-formatting [s]
  (string/join " "
            (map #(str (Character/toUpperCase (first %)) (subs % 1))
                 (string/split s #"(?<=[\.\?!])\s+"))))

(defn uncompress-file [input-file frequency-vector]
  (println)
  (let [output (->> (string/split-lines (slurp input-file))
                    (mapcat #(string/split % #"\s+"))
                    (map #(if (re-matches #"\d+" %)
                            (nth frequency-vector (Integer/parseInt %))
                            %))
                    (map #(if (re-matches #"^@@(\d+)@@" %)
                            (let [frequency (Integer/parseInt (subs % 2 (dec (count %))))]
                              (if (and (>= frequency 0) (< frequency (count frequency-vector)))
                                (str (nth frequency-vector frequency))
                                %))
                            %)))
        result-str (reduce str (interpose " " output))
        digit-formatting (string/replace result-str #"@(\d+)@" "$1")
        punctuation-left-space-formatting (string/replace digit-formatting #"\s*([\],.?!})])" "$1")
        punctuation-right-space-formatting (string/replace punctuation-left-space-formatting #"([\[{($@])\s+" "$1")]
    (println (capitalize-characters-formatting punctuation-right-space-formatting))))