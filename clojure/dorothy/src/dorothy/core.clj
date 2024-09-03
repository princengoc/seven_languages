(ns dorothy.core
  (:require
    [dorothy.board :refer [initial-game-state found-treasure? display-grid move-dorothy update-monsters game-state monster-collision?]]
    [dorothy.keyboard :refer [listen-keypress]]))

;; Run monster movement on a separate thread
(defn start-monster-thread []
  (future
    (while true
      (Thread/sleep 300) ;; Monsters move every 500ms
      (update-monsters))))

(start-monster-thread)

;; Add watch. Watch is like a listener triggered on change
(add-watch game-state :on-state-change
  (fn [key ref old-state new-state]
    (display-grid)
    ;; check or collisions
    (cond
      (monster-collision?) (do 
          (println "Dorothy encountered a monster! Game over!")
          (System/exit 0)
          )
      (found-treasure?) (do
          (println "Wizard found! Game over!")
          (System/exit 0)
          )
    )
  )
)


(defn -main []
  "The main entry point for the dorothy game."
  (println (:dorothy @game-state))
  (display-grid)
  (loop []
    (let [key (listen-keypress)]
      (if (= key \q)
        (do
        (println "Quitting...")
        (System/exit 0)
        )
        (do
          (move-dorothy key)
          (recur)
        )))))
