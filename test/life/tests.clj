(ns life.tests
  (:use clojure.data)
  (:use clojure.test)
  (:use clojure.set)
  (:use life.core))



(deftest all-grids-are-square
  (def deadgrid (create-generation 5))  
  (def gridcount (reduce (fn [gsum column] (+ gsum (count column))) 0 deadgrid))
  (is (= gridcount (* 5 5))))



(deftest live-cell-count-is-correct
  (let [seeds [[0 1] [1 2] [3 2] [4 4]]
        livegrid (seed-cells seeds (create-generation 5))
        livingcells (live-cells livegrid)
        living-cell-coords (cells-to-xy-tuples livingcells)]

    (is (= 0 (count (difference (set living-cell-coords) (set seeds)))))))



(deftest live-cell-with-less-than-two-live-neighbors-dies
  (let [seeds [[0 1] [1 2] [3 2] [4 4]]
        livegrid (seed-cells seeds (create-generation 5))
        testcell (get-cell {:x-coord 0 :y-coord 1} livegrid)
        neighbors (live-neighbors-of testcell livegrid)]

    (is (= 1 (count neighbors)))
    (is (= false (cell-lives-in-next-gen? testcell livegrid)))))



(deftest live-cell-with-two-or-three-live-neighbors-lives
  (let [seeds [[0 0] [0 1] [1 1] [4 4]]
        livegrid (seed-cells seeds (create-generation 5))
        testcell (get-cell {:x-coord 0 :y-coord 0} livegrid)
        neighbors (live-neighbors-of testcell livegrid)]

    (is (= 2 (count neighbors)))
    (is (= true (cell-lives-in-next-gen? testcell livegrid)))))



(deftest dead-cell-with-more-than-three-live-neighbors-no-change
  (let [seeds [[0 0] [0 1] [1 0] [2 0] [3 0]]
        livegrid (seed-cells seeds (create-generation 5))
        testcell (get-cell {:x-coord 1 :y-coord 1} livegrid)
        neighbors (live-neighbors-of testcell livegrid)]

    (is (> (count neighbors) 3))
    (is (= false (cell-lives-in-next-gen? testcell livegrid)))))



(deftest dead-cell-with-three-live-neighbors-lives
  (let [seeds [[0 1] [1 0] [2 0] [3 0]]
        livegrid (seed-cells seeds (create-generation 5))
        testcell (get-cell {:x-coord 1 :y-coord 1} livegrid)
        neighbors (live-neighbors-of testcell livegrid)]

    (is (= 3 (count neighbors)))
    (is (= true (cell-lives-in-next-gen? testcell livegrid)))))



(deftest aggregate-neighbor-count-is-correct
    (let [seeds [[0 3] [2 3]]
        livegrid (seed-cells seeds (create-generation 5))
        testcell (get-cell {:x-coord 1 :y-coord 1} livegrid)
        neighbors (aggregate-neighbors seeds livegrid)]

    (is (= 10 (count neighbors)))))

  





