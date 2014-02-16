;;;; classes.lisp
(in-package #:lisp-html5-animation)

;;; Classes
(defclass book-component ()
  ((title
    :documentation "Title or header of this book component"
    :initarg :title
    :initform (error "Must supply a title.")
    :accessor title)
   (index
    :documentation "Index of this book component (e.g. Part 1 or Chapter 1)"
    :initarg :index
    :initform 0
    :accessor index)))

(defclass part (book-component)
  ((chapters
    :initarg :chapters
    :initform (lambda () nil)
    :accessor chapters)))

(defclass chapter (book-component)
  ((examples
    :initarg :examples
    :initform (lambda () nil)
    :accessor examples)))

(defclass example (book-component)
  ((href
    :initarg :href
    :initform "404"
    :accessor href)
   (identifier
    :initarg :identifier
    :initform (error "Must supply an identifier.")
    :accessor identifier)
   (page
    :initarg :page
    :initform (<:h1 "Missing Page")
    :accessor page)))

;;;; Methods
(defgeneric render-in-list-index (book-component)
  (:documentation "Render in index page"))

(defmethod render-in-list-index (book-component)
  (<:h2 (title book-component)))

(defmethod render-in-list-index ((part part))
  (<:li
   (<:h2 (format nil "Part ~@(~R~): " (index part)) (title part))
   (<:ol
    (let ((counter 1))
      (mapcar
       #'(lambda (chapter)
           (setf (index chapter) counter)
           (incf counter)
           (render-in-list-index chapter))
       (funcall (chapters part)))))))

(defmethod render-in-list-index ((chapter chapter))
  (<:li
   (<:h3 (format nil "Chapter ~d: " (index chapter))
         (title chapter))
   (<:ol (mapcar
          #'(lambda (example)
              (setf (href example)
                    (restas:genurl 'example
                                   :chapter (slug (title chapter))
                                   :example (identifier example)))
              (render-in-list-index example))
          (funcall (examples chapter))))))

(defmethod render-in-list-index ((example example))
  (<:li (<:a :href (href example)
             (title example))))

(defmethod print-object ((object book-component) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Title: ~s" (title object))))

(defmethod print-object ((object part) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "Title: ~s" (title object))
    (loop for chapter in (funcall (chapters object))
         do (format stream " | Chapter: ~s" (title chapter)))))
