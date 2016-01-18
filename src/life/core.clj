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
  ;(println (format "invoking get-cell on %s..." xymap))
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

(comment
(defn seed-cell [xymap grid]
  (let [x (:x-coord xymap)
        y (:y-coord xymap)]
    (assoc grid x
           (assoc (nth grid x) y (Cell. x y true))))))


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

  ;(println (format "calculating box surrounding cell [%d, %d]" (:x-coord cell) (:y-coord cell)))
  (def xcoords (vec (range xstart (+ xstart x-extent))))
  (def xcoords (flatten (repeat y-extent xcoords)))
  
  (def ycoords (vec (map (fn [y] (repeat x-extent y)) (range ystart (+ ystart y-extent)))))
  (def ycoords (flatten ycoords))
  
  (def neighbor-cells (map xy-tuple-to-map (map vector xcoords ycoords) ))

  ;(println "+++ about to apply MAGIC FILTER...")
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
      ;(println "cell is an outer edge cell")
      (def x-pos-offset 0)
      (def x-extent 2))
    
    (when (>= (.y-coord cell) (- gridsize 1))
      ;(println "cell is an outer edge cell.")
      (def y-pos-offset 0)
      (def y-extent 2))

    (def xstart (- (.x-coord cell) x-neg-offset))
    (def ystart (- (.y-coord cell) y-neg-offset))

    ;(println (format "calling box-surrounding with x = %s, y = %s, y-start %s, y-extent %s" (:x-coord cell) (:y-coord cell) ystart y-extent))
    
    (box-surrounding cell xstart x-extent ystart y-extent)))



(defn live-neighbors-of [cell grid]
  "return the collection of living neighbors
  of the cell"

  ;(println (format "calculating live neighbors of cell %s, %s" (:x-coord cell) (:y-coord cell)))
  (filter (fn [c] (cell-status c grid)) (neighbors-of cell grid)))


  
(defn cell-lives-in-next-gen? [xymap grid]
      "determines whether a cell in a CA
      lives or dies in the next generation"
    
     (let [cell (get-cell xymap grid)] 
       (if (and (.is-alive cell) (< (count (live-neighbors-of cell grid)) 2))
                (boolean false))
       (if (and (.is-alive cell)  (or (= (count (live-neighbors-of cell grid)) 2) (= (count (live-neighbors-of cell grid)) 3)))
                (boolean true))
       (if (and (.is-alive cell)  (> (count (live-neighbors-of cell grid)) 3))
                (boolean false))
       (if (and (not (.is-alive cell)) (= (count (live-neighbors-of cell grid)) 3))
              (boolean true))))



(defn -main
  "Conway's game of Life"
  [& args]
  
  (println "Cellular Automata by binarymachines")
  ;(def cellgrid (seed-cell {:x-coord 1 :y-coord 1} (create-generation 3)))


  (def cellgrid (seed-cells [[1 1] [0 0] [1 0] [1 2]] (create-generation 3)))
  
  (doseq [n (range 0 3)] (println (nth cellgrid n)))
  
  (def current-gen (live-cells cellgrid))                                      ; assemble a list of all seeds

  (println current-gen)

  ; assemble a list of the neighbors of all seeds
  ; (def current-neighbors (set (for [cell current-gen] (neighbors-of cell cellgrid))))
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


  (def samplecell (get-cell {:x-coord 0 :y-coord 0} cellgrid))
  (println (format "cell %s, %s has %s live neighbors" (.x-coord samplecell) (.y-coord samplecell)  (count (live-neighbors-of samplecell cellgrid))))
  (println (format "cell is alive? %s" (.is-alive (get-cell samplecell cellgrid))))
  (println (format "cell lives in next gen? %s" (cell-lives-in-next-gen? samplecell cellgrid)))
  
  ; next generation is every seed NOT on the kill list + every neighbor on the revive list
  (def next-gen-survivors (clojure.set/difference (set current-gen) (set kill-list)))


  
  (def next-gen-revived (clojure.set/intersection (set current-neighbors) (set revive-list)))
  (def next-gen (clojure.set/union (set next-gen-survivors) (set next-gen-revived)))
  
  
)

  
