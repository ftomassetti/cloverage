(ns leiningen.cloverage
  (:require [leiningen.run :as run]
            [bultitude.core :as blt]
            [clojure.java.io :as io]
            [bultitude.core :as b]
            [leiningen.core.main :as main]))

(defn ns-names-for-dirs [dirs]
  (map name (mapcat blt/namespaces-in-dir dirs)))

(defn get-lib-version []
  (or (System/getenv "CLOVERAGE_VERSION") "RELEASE"))

(def form-for-suppressing-unselected-tests
  "A function that figures out which vars need to be suppressed based on the
  given selectors, moves their :test metadata to :leiningen/skipped-test (so
  that clojure.test won't think they are tests), runs the given function, and
  then sets the metadata back."
  `(fn [namespaces# selectors# func#]
     (let [copy-meta# (fn [var# from-key# to-key#]
                        (if-let [x# (get (meta var#) from-key#)]
                          (alter-meta! var# #(-> % (assoc to-key# x#) (dissoc from-key#)))))
           vars# (if (seq selectors#)
                   (->> namespaces#
                     (mapcat (comp vals ns-interns))
                     (remove (fn [var#]
                               (some (fn [[selector# args#]]
                                       (let [sfn# (if (vector? selector#)
                                                    (second selector#)
                                                    selector#)]
                                         (apply sfn#
                                           (merge (-> var# meta :ns meta)
                                             (assoc (meta var#) ::var var#))
                                           args#)))
                                 selectors#)))))
           copy# #(doseq [v# vars#] (copy-meta# v# %1 %2))]
       (copy# :test :leiningen/skipped-test)
       (try (func#)
         (finally
           (copy# :leiningen/skipped-test :test))))))

(defn- split-selectors [args]
  (let [[nses selectors] (split-with (complement keyword?) args)]
    [nses
     (loop [acc {} [selector & selectors] selectors]
       (if (seq selectors)
         (let [[args next] (split-with (complement keyword?) selectors)]
           (recur (assoc acc selector (list 'quote args))
             next))
         (if selector
           (assoc acc selector ())
           acc)))]))

(defn- partial-selectors [project-selectors selectors]
  (for [[k v] selectors
        :let [selector-form (k project-selectors)]
        :when selector-form]
    [selector-form v]))

(def ^:private only-form
  ['(fn [ns & vars]
      ((set (for [v vars]
              (-> (str v)
                (.split "/")
                (first)
                (symbol))))
        ns))
   '(fn [m & vars]
      (some #(let [var (str "#'" %)]
               (if (some #{\/} var)
                 (= var (-> m ::var str))
                 (= % (ns-name (:ns m)))))
        vars))])

(defn- convert-to-ns [possible-file]
  (if (and (.endsWith possible-file ".clj") (.exists (io/file possible-file)))
    (str (second (b/ns-form-for-file possible-file)))
    possible-file))

(defn ^:internal read-args [args project]
  (let [args (->> args (map convert-to-ns) (map read-string))
        [nses given-selectors] (split-selectors args)
        nses (or (seq nses)
               (sort (b/namespaces-on-classpath
                       :classpath (map io/file (distinct (:test-paths project)))
                       :ignore-unreadable? false)))
        selectors (partial-selectors (merge {:all '(constantly true)}
                                       {:only only-form}
                                       (:test-selectors project))
                    given-selectors)
        selectors (if (and (empty? selectors)
                        (:default (:test-selectors project)))
                    [[(:default (:test-selectors project)) ()]]
                    selectors)]
    (when (and (empty? selectors)
            (seq given-selectors))
      (main/abort "Please specify :test-selectors in project.clj"))
    [nses selectors]))

(defn cloverage
  "Run code coverage on the project.

  To specify cloverage version, set the CLOVERAGE_VERSION environment variable.
  Specify -o OUTPUTDIR for output directory, for other options see cloverage."
  [project & args]
  (let [[nses selectors] (read-args args project)
         source-namespaces (ns-names-for-dirs (:source-paths project))
        test-namespace    (ns-names-for-dirs (:test-paths project))]
    (println "nses" nses)
    (println "selectors" selectors)
    (System/exit 0)
    (apply run/run (update-in project [:dependencies]
                              conj    ['cloverage (get-lib-version)])
           "-m" "cloverage.coverage"
           (concat (mapcat  #(list "-x" %) test-namespace)
                   args
                   source-namespaces))))
