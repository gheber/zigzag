;;;; zigzag without CLOS

;;; Direction

(defun direction-p (d)
  "
  A predicate for the direction type.
  "
  (and (symbolp d)
       (or (eq d 'pos) (eq d 'neg))))

(deftype direction ()
  "
  A direction type.
  "
  '(satisfies direction-p))

(defun other (dir)
  "
  Reverses a given direction.
  "
  (declare (type direction dir))
  (if (eq dir 'pos) 'neg 'pos))

;;; Cell
;;;
;;; If there weren't too many (< 10?) dimensions per cell,
;;; we could use an a-list as :connector.

(defun make-cell (key value)
  "
  Make a new cell.
  Cells are represented as property lists.
  Connectors are represented a hash tables with the dimension name
  as the (string) key and a-list [ ((POS . *) (NEG . *)) ] value.
  "
  (declare (type integer key))
  `(:key ,key :value ,value :connectors ,(make-hash-table :test #'equal)))

(defun get-connectors (cell dim)
  "
  Get a cell's connectors. (Initialize if absent.)
  "
  (declare (type string dim))
  (let ((conn (getf cell :connectors)))
    (multiple-value-bind (value present-p)
        (gethash dim conn)
      (if (not present-p)
          (setf (gethash dim conn) (list (cons 'pos nil) (cons 'neg nil)))
          value))))

(defun connect (cell other-cell dim dir &optional (new t))
  "
  Connect CELL and OTHER-CELL along dimension DIM in direction DIR.
  Signals an error if there is an existing connection.
  Replaces an existing connection if (NOT NEW).
  "
  (let* ((connectors (get-connectors cell dim))
         (step-fwd (cdr (assoc dir connectors)))
         (other-connectors (get-connectors other-cell dim))
         (step-back (cdr (assoc (other dir) other-connectors))))
    ;; Make sure to keep consistency if a connection is being replaced
    (unless (or (null step-fwd)
                (eq step-fwd other-cell))
      (if new
          (error "Connection already exists.")
          (rplacd (assoc (other dir) (get-connectors step-fwd dim)) nil)))
    (unless (or (null step-back)
                (eq step-back cell))
      (if new
          (error "Connection already exists.")
          (rplacd (assoc dir (get-connectors step-back dim)) nil)))
    (rplacd (assoc dir connectors) other-cell)
    (rplacd (assoc (other dir) other-connectors) cell)
    (values)))

(defun add (cell key value dim &optional (dir 'pos))
  "
  Returns a new cell with KEY/VALUE connected to CELL along dimension
  DIM in direction DIR.
  "
  (let ((new-cell (make-cell key value)))
    (connect cell new-cell dim dir)
    new-cell))

(defun cross-connect (cell)
  "
  Cross connect neighbors in all dimensions.
  "
  (maphash (lambda (dim conn)
             (let ((neg (cdr (assoc 'neg conn))))
               (connect neg (cdr (assoc 'pos conn)) dim 'pos nil)))
           (getf cell :connectors)))

(defun next-step (cell dim &optional (dir 'pos))
  "
  Returns the next cell along dimension DIM in direction DIR.
  "
  (cdr (assoc dir (get-connectors cell dim))))

(defun path (cell dim dir)
  "
  Returns a list of cells following CELL in dimension DIM and direction DIR.
  "
  (loop while (next-step cell dim dir)
        do (setf cell (next-step cell dim dir))
          collect cell))

(defun get-row (cell dim)
  "
  Returns all cells in dimension DIM passing through CELL.
  "
  (append (reverse (path cell dim 'neg))
          (list cell)
          (path cell dim 'pos)))

;;; Tissue

(defun make-tissue ()
  "
  Returns a new tissue.
  "
  (let ((key 0))
    (list :cells (make-hash-table) :next-key (lambda () (1- (setq key (1+ key)))))))

(defun start (tissue value)
  "
  Creates and returns the first cell of a tissue.
  The cell is initialized with VALUE.
  "
  (let ((cells (getf tissue :cells))
        (key (funcall (getf tissue :next-key))))
    (setf (gethash key cells) (make-cell key value))))

(defun add-cell (tissue cell dim value &optional (dir 'POS))
  "
  Returns a newly created cell with VALUE that is connected to CELL
  along dimension DIM in direction DIR.
  "
  (let* ((key (funcall (getf tissue :next-key)))
         (cells (getf tissue :cells))
         (new-cell (add (gethash (getf cell :key) cells) key value dim dir)))
    (setf (gethash key cells) new-cell)
    new-cell))

(defun delete-cell (tissue cell)
  "
  Deletes a cell.
  "
  (let ((key (getf cell :key)))
    (cross-connect cell)
    (remhash key (getf tissue :cells))))

;;; If you are into the whole monarchy thing...

(let* ((genealogy (make-tissue))
       (liz (start genealogy "Elizabeth II"))
       (charles (add-cell genealogy liz "generation" "Charles"))
       (ann (add-cell genealogy charles "sibling" "Ann"))
       (andrew (add-cell genealogy ann "sibling" "Andrew"))
       (george (add-cell genealogy liz "generation" "George VI" 'neg)))
  (connect ann liz "generation" 'neg nil)
  (add-cell genealogy andrew "sibling" "Edward")
  (add-cell genealogy george "generation" "Victoria" 'neg)
  (delete-cell genealogy andrew)
  (format t "Row with Ann in generation dimension: ~{~a~^, ~}~%"
          (mapcar #'(lambda (x) (getf x :value)) (get-row ann "generation")))
  (format t "Row with Ann in sibling dimension: ~{~a~^, ~}"
          (mapcar #'(lambda (x) (getf x :value)) (get-row ann "sibling"))))
