(ns life.core
  (:gen-class))

(require 'clojure.set)


(defrecord Cell [x-coord y-coord is-alive])



(defn xy-tuple-to-map [xy-tuple]
  {:x-coord (nth xy-tuple 0) :y-coord (nth xy-tuple 1)})



(defn create-generation [gridsize]
   (vec (for [x (range 0 gridsize)]
       (vec (for [y (range 0 gridsize)] (->Cell x y false))))))


(defn get-cell [xymap grid]
  (nth (nth grid (:x-coord xymap)) (:y-coord xymap)))



(defn show-cell [xymap grid]
  (let [cell (get-cell xymap grid)]
    (cond (.is-alive cell)  (println (format "Cell with coordinates %d, %d is alive." (.x-coord cell) (.y-coord cell))))
    (cond (not (.is-alive cell))  (println (format "Cell with coordinates %d, %d is dead." (.x-coord cell) (.y-coord cell))))))


 
(defn cell-status [xymap grid]
  (.is-alive (get-cell xymap grid)))


(defn seed-cell [xymap grid]
  (let [x (:x-coord xymap)
        y (:y-coord xymap)]

    (assoc-in grid [x y] (->Cell x y true))))


(defn seed-cells [xy-tuples grid]
  (let [xymaps (map xy-tuple-to-map xy-tuples)]
    (reduce (fn [g xymap] (seed-cell xymap g)) grid xymaps)))

  

(defn coordinates-match [cell xymap]
  (if (and (= (:x-coord xymap) (.x-coord cell)) (= (:y-coord xymap) (.y-coord cell)))
    true
    false))


(defn live-cells [grid]
  (flatten (for [slice grid] (filter (fn [cell] (.is-alive cell)) slice))))



(defn box-surrounding [cell xstart x-extent ystart y-extent ]

  (def xcoords (vec (range xstart (+ xstart x-extent))))
  (def xcoords (flatten (repeat y-extent xcoords)))
  (def ycoords (vec (map (fn [y] (repeat x-extent y)) (range ystart (+ ystart y-extent)))))
  (def ycoords (flatten ycoords))  
  (def neighbor-cells (map xy-tuple-to-map (map vector xcoords ycoords) ))

  (filter (fn [xymap] (not (coordinates-match cell xymap))) neighbor-cells))



(defn neighbors-of [xymap grid]

  (let [gridsize (count grid)
        cell (get-cell xymap grid)]
    
    (def x-neg-offset 1)
    (def y-neg-offset 1)
    (def x-pos-offset 1)
    (def y-pos-offset 1)
    (def x-extent 3)
    (def y-extent 3)
    
    (when (= (.x-coord cell) 0)      
      (def x-neg-offset 0)
      (def x-extent 2))
    
    (when (= (.y-coord cell) 0)
      (def y-neg-offset 0)
      (def y-extent 2))

    (when (>= (.x-coord cell) (- gridsize 1))
      (def x-pos-offset 0)
      (def x-extent 2))
    
    (when (>= (.y-coord cell) (- gridsize 1))
      (def y-pos-offset 0)
      (def y-extent 2))

    (def xstart (- (.x-coord cell) x-neg-offset))
    (def ystart (- (.y-coord cell) y-neg-offset))

    (box-surrounding cell xstart x-extent ystart y-extent)))



(defn live-neighbors-of [cell grid]
  "return the collection of living neighbors
  of the cell"

  (filter (fn [c] (cell-status c grid)) (neighbors-of cell grid)))


  
(defn cell-lives-in-next-gen? [xymap grid]
      "determines whether a cell in a CA
      lives or dies in the next generation"
           
      (let [cell (get-cell xymap grid)
            neighbors (live-neighbors-of cell grid)
            ncount (count neighbors)]

       (if (.is-alive cell)
         (cond
           (< ncount 2)
            false
           (or (= ncount 2) (= ncount 3))
            true)
         ; and if the cell is not alive...
         (cond  
           (> ncount 3)
            false
           (= ncount 3)
            true))))
      

(defn printgrid [grid gridsize]
  (doseq [n (range 0 gridsize)] (println (nth grid n))))


(defn -main
  "Conway's game of Life"
  [& args]
  
  (println "Cellular Automata by binarymachines")

  (def cellgrid (seed-cells [[1 1] [0 0] [1 0] [1 2]] (create-generation 3)))
  (printgrid cellgrid 3)
  
  
  (def current-gen (live-cells cellgrid))                                      ; assemble a list of all seeds
  (println current-gen)
  (println "------------------")

  ; assemble a list of the neighbors of all seeds
  (def current-neighbors (distinct (flatten (map (fn [cell] (neighbors-of cell cellgrid)) current-gen))))

  
  (println "current neighbors: ")  
  (println current-neighbors)
  (println "------------------")

  (println ">> generating kill list...")
  ; generate a kill-list from seeds
  (def kill-list (filter (fn [xymap] (not (cell-lives-in-next-gen? xymap cellgrid))) current-gen))
  (println kill-list)
  (println "------------------")
 
  (println ">> generating revive list...") ; generate a revive-list from all neighbors
  (def revive-list (filter (fn [xymap] (cell-lives-in-next-gen? xymap cellgrid)) current-neighbors))                              
  (println revive-list)
  (println "------------------")
  
  ; next generation is every seed NOT on the kill list + every neighbor on the revive list
  (def next-gen-survivors (clojure.set/difference (set current-gen) (set kill-list)))
  (def next-gen-revived (clojure.set/intersection (set current-neighbors) (set revive-list)))  
  (def next-gen (clojure.set/union (set next-gen-survivors) (set next-gen-revived))))
  
  


  
