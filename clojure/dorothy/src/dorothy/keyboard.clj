(ns dorothy.keyboard
  (:import [org.jline.terminal TerminalBuilder]))

(defn listen-keypress []
  (let [terminal (-> (TerminalBuilder/builder)
                     (.system true)
                     (.build))]
    ;;(println "Press a key:")
    (.enterRawMode terminal)
    (let [key (.read (.reader terminal))]
      (.close terminal)
      (char key))))
