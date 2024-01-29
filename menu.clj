(ns menu
  (:require [clojure.java.io :as io]
            [compress]))

(def frequency-vector (compress/create-frequency-mapping "frequency.txt"))

(defn display-file-list []
  (println "File List:")
  (let [files (->> (file-seq (io/file "."))
                   (filter #(not (.isDirectory %)))
                   (map #(.getName %)) (sort))]
    (doseq [file files]
      (println (str "* ./" file)))))

(defn display-file-contents []
  (print "Please enter a file name => ")
  (flush)
  (let [filename (read-line)
        file (io/file filename)]
    (println)
    (if (.exists file)
      (let [content (slurp file)]
        (println content))
      (println "Oops: specified file does not exist"))))

(defn compress-file []
  (print "Please enter a file name => ")
  (flush)
  (let [filename (read-line)
        file (io/file filename)]
    (if (.exists file)
      (compress/compress-file file frequency-vector)
      (println "Oops: specified file does not exist"))))

(defn uncompress-file []
  (print "Please enter a file name => ")
  (flush)
  (let [filename (read-line)
        file (io/file filename)]
    (if (.exists file)
      (compress/uncompress-file file frequency-vector)
      (println "Oops: specified file does not exist"))))

(defn menu []
  (loop []
    (println)
    (println "*** Compression Menu ***")
    (println "-------------------------")
    (println "1. Display list of files")
    (println "2. Display file contents")
    (println "3. Compress a file")
    (println "4. Uncompress a file")
    (println "5. Exit")
    (println)
    (print "Enter an option? ")
    (flush)
    (let [choice (read-line)]
      (println)
      (cond
        (= choice "1")
        (do
          (display-file-list)
          (recur))

        (= choice "2")
        (do
          (display-file-contents)
          (recur))

        (= choice "3")
        (do
          (compress-file)
          (recur))

          (= choice "4")
          (do
            (uncompress-file)
            (recur))

          (= choice "5")
          (do
            (println "Exiting from the program!\n")
            (System/exit 0))

          :else
          (do
            (println "Invalid choice. Please try again.")
            (recur))))))

(menu)