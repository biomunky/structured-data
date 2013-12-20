(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0 0) (get v 2 0)))

(defn cutify [v] (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ c & others] v] (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [ [[w1 h1] [w2 h2]] rectangle]
    (- w2 w1)))

(defn height [rectangle]
  (let [[[w1 h1] [w2 h2]] rectangle]
    (- h2 h1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[w1 h1] [w2 h2]] rectangle
        [pw ph] point
        in-width  (<= w1 pw w2)
        in-height (<= h1 ph h2)]
    (and in-width in-height)))

(defn contains-rectangle? [outer inner]
  (let [[[iw1 ih1] [iw2 ih2]] inner]
    (and (contains-point? outer [iw1 ih1])
         (contains-point? outer [iw2 ih2]))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1 ) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [coll]
  (map count coll))

(defn second-elements [coll]
 (let [snd (fn [x] (get x 1 nil))]
  (map snd coll)))

(defn titles [coll]
   (map :title coll))

(defn stars [n]
  (apply str (repeat n "*")))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [coll]
  (not= (count coll) (count (set coll))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (let [names (map :name (authors books))]
  (set names)))

(defn author->string [author]
  (let [auth-name  (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if birth
      (str auth-name " (" birth " - " death ")")
      auth-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [author-string (authors->string (:authors book))]
    (str (:title book) ", written by " author-string)))

(defn books->string [books]
  (let [book-strings (apply str (interpose ". " (map book->string books)))
        numb-books (count books)
        book-count (cond
                    (== numb-books 1) "1 book."
                    (>  numb-books 1) (str numb-books " books."))]
    (if (== 0 numb-books)
      "No books."
      (str book-count " " book-strings "."))))


(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (filter alive? (:authors book)))
    false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%


