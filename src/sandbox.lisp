
(defun kmeans++ (data k)
  "K-means++ initialization for clustering."
  (let ((centroids (list)))
    ;; Randomly choose the first centroid
    (push (random-choice (data-rows data)) centroids)
    ;; Choose the remaining k-1 centroids
    (loop repeat (1- k)
          do (let ((distances (mapcar #'(lambda (row)
                                          (apply #'min
                                                 (mapcar #'(lambda (c)
                                                             (distance row c))
                                                         centroids)))
                                      (data-rows data))))
               (let ((prob (normalize (mapcar #'(lambda (d) (* d d)) distances))))
                 (push (weighted-random-choice (data-rows data) prob) centroids))))
    centroids))


(defun distance (point1 point2)
  "Calculate Euclidean distance between two points."
  (sqrt (reduce #'+ (mapcar #'(lambda (x y) (expt (- x y) 2)) point1 point2))))

(defun normalize (lst)
  "Normalize a list to sum to 1."
  (let ((sum (reduce #'+ lst)))
    (mapcar #'(lambda (x) (/ x sum)) lst)))

(defun weighted-random-choice (items weights)
  "Select an item based on weighted probabilities."
  (let* ((cumulative (reduce #'(lambda (acc x) (append acc (list (+ (or (car (last acc)) 0) x)))) weights :initial-value nil))
         (rand (random (car (last cumulative)))))
    (loop for item in items
          for cum in cumulative
          when (> cum rand) return item)))

(defun random-choice (items)
  "Select a random item from a list."
  (nth (random (length items)) items))
