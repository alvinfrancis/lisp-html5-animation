;;;; utility.lisp
(in-package :lisp-html5-animation)

(<:augment-with-doctype "html" "" :auto-emit-p t)

(defun slug (string)
  (substitute #\- #\Space
              (string-downcase
               (string-trim '(#\Space #\Tab #\Newline) string))))

;;; easier access to the hash table data
(defun get-part (key)
  (gethash key *parts*))
(defun (setf get-part) (value key)
  (setf (gethash key *parts*) value))

(defun get-chapter (key)
  (gethash key *chapters*))
(defun (setf get-chapter) (value key)
  (setf (gethash key *chapters*) value))

(defun get-example (key)
  (gethash (slug key) *examples*))
(defun (setf get-example) (value key)
  (setf (gethash (slug key) *examples*) value))

(defun get-script (key)
  (gethash (slug key) *scripts*))
(defun (setf get-script) (value key)
  (setf (gethash (slug key) *scripts*) value))

(defun make-gethash (hash &optional (key-func (lambda (key) key)))
  #'(lambda (key)
      (gethash (funcall key-func key) hash)))

(defun canvas-page (&key script links pre-canvas post-canvas title)
  (<:html
   (<:head
    (<:meta :charset "utf-8")
    (<:title title)
    links
    (<:link :rel "stylesheet" :href "/static/css/style.css"))
   (<:body
    pre-canvas
    (<:canvas :id "canvas" :width "400" :height "400")
    post-canvas
    (<:script script))))
