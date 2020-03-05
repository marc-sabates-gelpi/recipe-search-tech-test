(ns rf-search.core
  (:require [clj-memory-meter.core :as mm]
            [clojure.edn :as edn]
            [clojure.java.io :refer [file as-file]]
            [clojure.string :as string]
            #_[digest :as digest]
            [juxt.dirwatch :refer [watch-dir close-watcher]]
            [taoensso.timbre :refer [debug spy]]))

#_(defonce ^:private register (atom {}))
(defonce ^:private index (atom {}))
(def ^:private max-entries 10)

(defn file->index
  "Return a map consisting of words as keys and
  their frequency within a file."
  [{:keys [id content] :as _file-details}]
  (->> content
       (re-seq #"[a-zA-Z\-]{4,}")
       (map string/lower-case)
       frequencies
       (map (juxt key (comp #(list {:freq % :id id}) val)))
       (into {})))

#_(defn file->md5
  "Return the md5sum of a given file `id` when it differs
  from the registered one."
  [id]
  (let [md5 (digest/md5 (as-file id))]
    (when-not (= (get @register id) md5)
      {:id id :md5 md5})))

(defn dir->files
  "Return the files in a directory."
  [s]
  (->> s
       file
       file-seq
       (map str)
       rest                                                 ;; first element is the dir
       #_(keep file->md5)
       (map (partial hash-map :id))))

(defn get-index-register
  [s]
  (let [files    (dir->files s)
        index    (->> files
                      (pmap (comp file->index #(assoc % :content (slurp (:id %)))))
                      (reduce (partial merge-with into) {})
                      (map (juxt key (comp reverse (partial sort-by :freq) val)))
                      (into {}))
        #_#_register (into {} (map (juxt :id :md5) files))]
    {:index index #_#_:register register}))

#_(defn merge-freqs
  "Return a merged list with `new` replacing `old` by `:id`."
  [old new]
  (let [new-ids (map :id new)
        _ (debug "---> new" new)
        _ (debug "---> old" old)
        _ (debug "---> new-ids" new-ids)]
    (spy (into (remove #(some (-> % :id hash-set) new-ids) old) new))))

#_(defn update-index
  "Return `old` replacing the word frequency on a file for the one in `new`.
  TODO: Remove words and remove files within words."
  [old new files]
  (merge-with merge-freqs old new))

(defn partial-index!
  [args]
  ;;TODO: This needs to be debounced
  (println "Updating index..")
  (debug "---> updated files:" (->> args
                                    (filter (comp #{:modify :delete} :action))
                                    (map (juxt (comp str :file) :action))
                                    (into {})))
  ;; TODO: Legacy process. Please implement partial update
  (let [{new-index :index} (get-index-register "resources/recipes")]
    (reset! index new-index)
    #_(let [{new-index :index new-register :register} (get-index-register "resources/recipes")]
        (swap! register merge new-register)
        (swap! index update-index new-index (keys register))))
  (println "Updating done.."))

(defn full-index!
  []
  (println "Indexing..")
  (let [{new-index :index} (get-index-register "resources/recipes")]
    (reset! index new-index))
  (println "Indexing done.."))

(defn index!
  "Replace current index with a new one."
  [& args]
  (if (seq args)
    (partial-index! args)
    (full-index!)))

(defn get-option!
  "Return the user input."
  []
  (println "Options:\ne or q\t-> exit\ns words\t-> search")
  (print "$ ")
  (flush)
  (let [input (string/split (read-line) #"\s")]
    (cond-> {:option (first input)}
            (seq (rest input)) (assoc :args (rest input)))))

(defn search
  [words]
  (or (some->> words
               (map string/lower-case)
               first
               (get @index)
               (map :id)
               (take max-entries))
      [">>> No results <<<"]))

(defn search!
  [args]
  (if (seq args)
    (printf "Search results:\n\n%s\n\n" (apply str (interpose "\n" (search args))))
    (println ">>> No search terms introduced <<<")))

(def exit? #{"e" "q"})
(def search? #{"s"})
(def index? #{"i"})

(defn -main
  []
  (let [w (watch-dir index! (file "resources/recipes"))]
    (println "Starting..")
    (index!)
    (loop [option nil args nil]
      (when-not (exit? option)
        (cond
          (search? option) (search! args)
          (index? option) (index!))
        (let [input (get-option!)]
          (recur (:option input) (:args input)))))
    (println "Exiting!")
    (close-watcher w)
    nil))


(comment (mm/measure (-main)))
