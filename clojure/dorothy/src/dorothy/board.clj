;; Display a grid as the board. 
(ns dorothy.board)

;; Upgrade visited positions etc to game state
(defn initial-game-state []
  {
    :dorothy [0 0]
    :monsters #{[4 4] [-4 -4] [-4 4] [4 -4]}
    :visited #{[0 0]}
    :grid-size 10
    :tree-sparsity 7 ;; higher number is easier (less trees)
    :treasure-sparsity 2144 ;; higher numbers is hard (less treasures)
  }
)

(def game-state (atom (initial-game-state)))

;; Auxiliary functions
(defn position-visited? [position]
  (contains? (:visited @game-state) position))

(defn monster-at? [position] 
  (contains? (:monsters @game-state) position))
  ;;(some #(= % coord) (:monsters @game-state)))

(defn clear-screen []
  (print "\033[H\033[2J"))

(defn make-trees [x y]
  (mod (+ (hash x) (hash y)) (:tree-sparsity @game-state)))  ; Adjust modulus for desired density

(defn make-treasure [x y]
  (mod (+ (hash x) (hash y)) (:treasure-sparsity @game-state))
)

;;generate-board will determine what's in each square
(defn generate-board [x y]
  (let [[cx cy] (:dorothy @game-state)]
    (cond
      (and (= x cx) (= y cy)) 'D ; Dorothy's position
      (monster-at? [x y]) 'M ; Monster is here
      (= 0 (make-trees x y)) 'T   ; 'T' for Tree
      (= 7 (make-treasure x y)) 'W ; W for treasure
      (position-visited? [x y]) \space ; visited already
      :else '.)))                          ; '.' for empty squares


(defn display-grid []
  (clear-screen)
  (let [[cx cy] (:dorothy @game-state)
        size (:grid-size @game-state)  ; if size is 2, this will create a 5x5 grid (2 to each side of current-position)
        positions (for [y (range (- cy size) (+ cy size 1))
                        x (range (- cx size) (+ cx size 1))]
                    [x y])]
    (doseq [row (partition (inc (* 2 size)) positions)] ; partition positions into rows of 5
      (println (apply str (map (fn [[x y]] (generate-board x y)) row))))))

;; Can move onto the new position x y if it's not blocked by a tree
(defn can-move? [target-position]
    (let [[x y] target-position
          target (generate-board x y)]
    (not= 'T target))
)


;; Move dorothy
(defn move-dorothy [key-pressed]
    (let [[x y] (:dorothy @game-state)
        target-position (case key-pressed
                        \w [x (dec y)]  ; move up
                        \s [x (inc y)]  ; move down
                        \a [(dec x) y]  ; move left
                        \d [(inc x) y]  ; move right
                        (:dorothy @game-state))] ; no change if key is not recognized
    (if (can-move? target-position) 
        (swap! game-state
              (fn [state]
                (-> state
                    (assoc :dorothy target-position)
                    (update :visited conj target-position))))
        )        
    )  
)

(defn monster-collision? []
  (contains? (:monsters @game-state) (:dorothy @game-state))
)

(defn found-treasure? []
  (let [[x y] (:dorothy @game-state)]
    (= 7 (make-treasure x y))
  )
)

;; Move monsters towards dorothy
(defn get-monster-new-position [dorothy-position monster-position]
  (let [
        [mx my] monster-position
        [cx cy] dorothy-position
        ;; update either the x or y coordinate
        target-position (if (= :x (rand-nth [:x :y]))
            (if (> cx mx) [(inc mx) my] [(dec mx) my])
            (if (> cy my) [mx (inc my)] [mx (dec my)]))        
        monster-too-far (or (> (abs (- cx mx)) 20) (> (abs (- cy my)) 20))
       ]
  ;; if monster is too far away, re-spawn monster within 10 steps of Dorothy
  (if monster-too-far [(+ (rand-nth [6 7 8 9 10 -6 -7 -8 -9 -10]) cx) (+ (rand-nth [6 7 8 9 10 -6 -7 -8 -9 -10]) cy)]
    ;; otherwise default moves
    ;; only move there if it can move there (no trees)
    (do
      (if (can-move? target-position) target-position monster-position)
    )
  )
  )
)

(defn update-monsters []
  (let [
          move-monster-to-dorothy (partial get-monster-new-position (:dorothy @game-state))
        ]
    ;; for each monster move towards dorothy
    (swap! game-state update :monsters 
          (fn [monsters]
            (set (map move-monster-to-dorothy monsters)))))
)
