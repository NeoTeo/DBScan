(ns dbscan.core)

(defn average [lst] (/ (reduce + lst) (count lst)))


(defn avg [xs]
	(loop [xs xs count 0 sum 0]
		(if-not (seq xs)
			(/ sum count)
			(recur (rest xs) (inc count) (+ sum (first xs))))))

(defn addIfNew [x xs]
	(if-not (some #{x} xs)
		(into [] (cons x xs))
		xs))

(defn regionQuery [data p eps]
	; return all points in data within p's eps-neighbourhood (including p)
	(if (empty? data)
		'[]
		; adding the closing parens after typing "(first data" after datval hangs the editor
		;(if-let [datval (cons (first data) (regionQuery (rest data) p eps))
		(let [datval (first data)]
			(if (<= (Math/abs (- datval p)) eps)
				(cons datval (regionQuery (rest data) p eps))
				(regionQuery (rest data) p eps)))))

(defn expandCluster [data p neighbourPts eps minpts visited clusters]
	(loop [neighbourPts neighbourPts visited visited cluster []]
		(if-not (seq neighbourPts)
			[cluster visited]
			(let [pp (first neighbourPts) 
				ppBeenVisited (some #{pp} visited)
				newVisited (addIfNew pp visited)
				newNeighbourPts (if-not ppBeenVisited
									(let [np (regionQuery data pp eps)]
										(if (>= (count np) minpts)
											(distinct (concat neighbourPts np)) ;investigate speed implications of distinct
											neighbourPts))
									neighbourPts)
				newCluster (if-not (some #{pp} (flatten (conj clusters cluster))) ;investigate speed implications of flatten
								(cons pp cluster)
								cluster)]
				(recur (rest newNeighbourPts) newVisited newCluster)))))



(defn dbscan [data eps minpts]
	(loop [unvisited data visited [] clusters []]
		(if-not (seq unvisited)
			(remove empty? clusters)
			(let [p (first unvisited)
				neighbourPts (regionQuery data p eps)
				[cluster newVisited] (if (< (count neighbourPts) minpts)
					[[] (addIfNew p visited)]
					(expandCluster data p neighbourPts eps minpts (addIfNew p visited) clusters) )]
				(recur (rest unvisited) newVisited (cons cluster clusters)))))
	)
