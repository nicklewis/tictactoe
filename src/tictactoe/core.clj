(ns tictactoe.core
  (:require [clojure.string :refer (split trim)])
  (:gen-class))

(defn new-board
  []
  (vec (repeat 9 " ")))

(defn print-board
  [board]
  (let [rows (partition 3 board)]
    (doseq [row rows]
      (doseq [cell row]
        (print cell ""))
      (println))))

(defn valid?
  [board move]
  (and (<= 1 move (count board))
       (= (board (dec move)) " ")))

(defn make-move
  [board move player]
  (assoc board (dec move) player) )

(defn parse-move
  [move]
  {:pre [(string? move)]}
  (Integer/parseInt move)
  )

(defn change-player
  [player]
  {:pre [(#{"X" "O"} player)]}
  (if (= player "X")
    "O"
    "X"))

(def ^:const winning-moves
  #{#{0 1 2}
    #{3 4 5}
    #{6 7 8}
    #{0 3 6}
    #{1 4 7}
    #{2 5 8}
    #{2 4 6}
    #{0 4 8}})

(defn won?
  [board move]
  (->> winning-moves
       (filter #(% (dec move)))
       (some #(apply = (map board %)))))

(defn stalemate?
  [board]
  (not (some #(= % " ") board)))

(defn game-over
  [player]
  (println "Player" player "won!")
  (System/exit 0))

(defn play-game
  []
  (println "Welcome to tic-tac-toe")
  (loop [board (new-board)
         player "X"]
    (when (stalemate? board)
      (println "YOU ALL LOSE")
      (System/exit 0))
    (print-board board)
    (println "Enter move for player" player)
    (let [move (parse-move (read-line))]
      (if (valid? board move)
        (let [new-board (make-move board move player)]
          (if (won? new-board move)
            (game-over player)
            (recur new-board (change-player player))))
        (recur board player)))))

(defn -main
  []
  (play-game))
