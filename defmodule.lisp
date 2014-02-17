;;;; defmodule.lisp

(restas:define-module #:lisp-html5-animation
  (:use #:cl #:cl-who #:hunchentoot #:parenscript #:sexml))

(in-package #:lisp-html5-animation)

(defparameter *template-directory* ; will be unused
  (merge-pathnames #P"templates/" lisp-html5-animation-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" lisp-html5-animation-config:*base-directory*))

(defparameter *paren-directory*
  (merge-pathnames #P"paren/" *static-directory*))

(sexml:with-compiletime-active-layers
    (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
   (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
   :<))

(setf ps:*js-target-version* 1.8)

;;; Global Variables
(defparameter *parts*
  (make-hash-table))
(defparameter *chapters*
  (make-hash-table))
(defparameter *examples*
  (make-hash-table :test 'equalp))
(defparameter *paren-examples*
  (make-hash-table :test 'equalp))
(defparameter *paren-classes*
  (make-hash-table :test 'equalp))
(defparameter *paren-utils* nil)
(defparameter *paren-keycode* nil)
