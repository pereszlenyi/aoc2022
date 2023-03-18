;; Solution for Day 3: Rucksack Reorganization
(require "asdf")

;; Checking for command line arguments
(unless (equal (length ext:*args*) 1)
	(princ "The single argument must be the name of the input file.")
	(princ #\newline)
	(quit 1))

;; Reads input from file
(defun get-input (filename)
	(remove-if (lambda (s) (string-equal s ""))    ;; Removes empty strings
		(uiop:split-string    ;; Splits the input on new lines
			(uiop:read-file-string filename)    ;; Reads file into a string
			:separator '(#\newline))))

;; Global variable storing the input as a list of strings
(defparameter *input-list* (get-input (car ext:*args*)))

;; Calculates the priority of an item
(defun item-priority (char)
	(1+
		(if (char<= #\a char #\z)
			(- (char-code char) (char-code #\a))    ;; lowercase
			(+ (- (char-code char) (char-code #\A)) (item-priority #\z)))))    ;; uppercase

;; Calculates the priority of the first item in the intersection
;; of a list of strings
(defun calculate-intersection-priority (strings)
	(item-priority (car    ;; Returns the priority of the first item
		(reduce #'intersection    ;; Calculates the intersection
			(mapcar (lambda (s) (coerce s 'list))    ;; Converts strings to lists
				(mapcar #'remove-duplicates strings))))))    ;; Removes duplicate characters

;; Calculates the priority of the item common to both compartments of the sack
(defun calculate-sack-priority (sack)
	(let ((half-length (/ (length sack) 2)))
		(calculate-intersection-priority
			(list (subseq sack 0 half-length) (subseq sack half-length)))))

(defun solve-part-one ()
	(reduce '+ (mapcar #'calculate-sack-priority *input-list*)))

;; Creates groups of three from the input list
(defun create-groups (input-list)
	(if input-list
		(cons (list (car input-list) (cadr input-list) (caddr input-list))
			(create-groups (cdddr input-list)))
		()
	))

(defun solve-part-two ()
	(reduce '+
		(mapcar #'calculate-intersection-priority
			(create-groups *input-list*))))

(princ "Sum of the priorities of the items common to both compartments: ")
(princ (solve-part-one))
(princ #\newline)
(princ "Sum of the priorities of the badges of the three-Elf groups: ")
(princ (solve-part-two))
(princ #\newline)
