(ns life.core
  (:gen-class))


(defrecord Cell [x-coord y-coord is-alive])



(defn xy-tuple-to-map [xy-tuple]
  {:x-coord (nth xy-tuple 0) :y-coord (nth xy-tuple 1)}
)



(defn create-generation [gridsize]
   (vec (for [x (range 0 gridsize)]
       (vec (for [y (range 0 gridsize)] (->Cell x y false)))
       )
   )
)


(defn get-cell [xymap grid]
  (nth (nth grid (:x-coord xymap)) (:y-coord xymap)))



(defn show-cell [xymap grid]
  (let [cell (get-cell xymap grid)]
    (cond (.is-alive cell)  (println (format "Cell with coordinates %d, %d is alive." (.x-coord cell) (.y-coord cell))))
    (cond (not (.is-alive cell))  (println (format "Cell with coordinates %d, %d is dead." (.x-coord cell) (.y-coord cell))))
    )
  )


 
(defn cell-status [xymap grid]
  (.is-alive (get-cell xymap grid)))




(defn seed-cell [xymap grid]
  (let [x (:x-coord xymap)
        y (:y-coord xymap)]

    (assoc-in grid [x y] (->Cell x y true))
)


(defn seed-cells [xy-tuples grid]

  (let [xymaps (map xy-tuple-to-map xy-tuples)]
    (reduce (fn [g xymap] (seed-cell xymap g)) grid xymaps)))

  

(defn coordinates-match [cell xymap]

  (if (and (= (:x-coord xymap) (.x-coord cell)) (= (:y-coord xymap) (.y-coord cell)))
    true
    false
  )
 )


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
      (def x-extent 2)
      )
    (when (= (.y-coord cell) 0)
      (def y-neg-offset 0)
      (def y-extent 2)
      )

    (when (= (.x-coord cell) (- gridsize 1))
      (println "cell is an outer edge cell")
      (def x-pos-offset 0)
      (def x-extent 2)
      )
    (when (= (.y-coord cell) (- gridsize 1))
      (println "cell is an outer edge cell.")
      (def y-pos-offset 0)
      (def yextent 2)
      )

    (def xstart (- (.x-coord cell) x-neg-offset))
    (def ystart (- (.y-coord cell) y-neg-offset))

    (println (format "cell coordinates are %d,%d" (.x-coord cell) (.y-coord cell)))
    (println (format "grid size is %d x %d." gridsize gridsize))
    
    (println (format "x starting point is %d." xstart))
    (println (format "y starting point is %d." ystart))
    
    (println (format "reading frame y extent is %d." y-extent))
    (println (format "reading frame x extent is %d." x-extent))

    (box-surrounding cell xstart x-extent ystart y-extent)
   )
)



(defn live-neighbors-of [cell grid]
  "return the collection of living neighbors
  of the cell"

  (let [xymap {:x-coord (.x-coord cell) :y-coord (.y-coord cell)}]
    (filter (fn [xymap] (cell-status xymap grid)) (neighbors-of xymap grid))))


  
(defn cell-lives-in-next-gen [xymap grid]
      "determines whether a cell in a CA
      lives or dies in the next generation"
  
     (let [cell (get-cell xymap grid)] 
      (cond (.is-alive cell)
            (if (< (count (live-neighbors-of cell grid)) 2)
              false)
            (if (or (= (count (live-neighbors-of cell grid)) 2) (= (count (live-neighbors-of cell grid)) 3))
              true)
            (if (> (count (live-neighbors-of cell grid)) 3)
              false))
      (cond (.is-dead cell)
            (if (= (count (live-neighbors-of cell grid)) 3)
              true))))   



(defn -main
  "Conway's game of Life"
  [& args]
  
  (println "Cellular Automata by binarymachines")


  (def cellgrid (seed-cells [[2 2] [3 2] [3 3] [3 4] [10 5] [11 4] [10 6]] (create-generation 15)))

  
  (show-cell {:x 2 :y 2} cellgrid)
  (println (neighbors-of {:x 2 :y 2} cellgrid))

  
  (def current-gen (live-cells cellgrid))                                      ; assemble a list of all seeds
  (def current-neighbors (for [cell current-gen] (neighbors-of cell)))         ; assemble a list of the neighbors of all seeds
  (def kill-list (filter () current-gen))                                      ; generate a kill-list from seeds
  (def revive-list (filter () current-neighbors))                              ; generate a revive-list from all neighbors
  (def next-gen                                      ; next generation is every seed not on the kill list + every neighbor on the revive list
  
  
  )

  
