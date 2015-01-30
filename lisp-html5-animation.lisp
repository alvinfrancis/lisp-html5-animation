;;;; lisp-html5-animation.lisp
(in-package #:lisp-html5-animation)

;;; "lisp-html5-animation" goes here. Hacks and glory await!

;;; Static
(restas:mount-module -homedir- (#:restas.directory-publisher)
  (:url "/static/")
  (restas.directory-publisher:*directory*
   (merge-pathnames "static/"
                    (asdf:system-source-directory
                     (string-downcase (package-name *package*)))))
  (restas.directory-publisher:*autoindex* t))

;;; Routes
(restas:define-route index-page ("")
 (index-page))
(restas:define-route example ("example/:chapter/:example")
  (funcall (page (gethash example *examples*))))
(restas:define-route paren-utils
    ("paren/utils" :content-type "text/javascript")
  *paren-utils*)
(restas:define-route paren-keycode
    ("paren/keycode" :content-type "text/javascript")
  *paren-keycode*)
(restas:define-route paren-class
    ("paren/class/:paren" :content-type "text/javascript")
  (gethash paren *paren-classes*))
(restas:define-route paren-example
    ("paren/example/:paren" :content-type "text/javascript")
  (gethash paren *paren-examples*))

;;; html stuffs
(setf (html-mode) :html5)
(setf *html-no-indent-tags* '(:a :span :textarea :pre :em))

(defun index-page ()
  (<:html
   (<:head
    (<:meta :charset "utf-8")
    (<:title "Foundation HTML5 Animation with Lisp")
    (<:link :rel "stylesheet" :href "static/css/style.css")
    (<:link :rel "stylesheet" :href "http://fonts.googleapis.com/css?family=Vollkorn")
    (<:meta :name "author" :content "Alvin Francis Dumalus")
    (<:meta :name "description" :content "Foundation HTML5 Animation with Javascript, by Billy Lamberta and Keith Peters, covers everything that you need to know to create dynamic, scripted animations using the HTML5 canvas."))
   (<:body
    :id "index-body"
    (<:section
     :id "description"
     (<:p
      (<:em "This is a Common Lisp recreation of the Foundation HTML5
      Animation with Javascript webpage")))
    (<:section
     :id "description"
     (<:p "This page contains all the samples, exercises and demo code from")
     (<:h1
      (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
           "Foundation HTML5 Animation with JavaScript"))
     (<:p
      "Everything you need to know about moving things around in the web browser using JavaScript. "
      "This is " (<:em "the") " starting place for creating HTML5 games, animations, and UX effects. "
      "Check out the "
      (<:a :href "http://www.amazon.com/Foundation-HTML5-Animation-JavaScript-Lamberta/product-reviews/1430236655?tag=html5anim-20"
           "Amazon reviews")
      ", I promise I didn't make these up :)"
      (<:ul
       (<:li (<:em
              "“One of the best programming books I've read in a long time. Writing is clear, straightforward, and interesting.”"
              (<:span :style "white-space: nowrap;" "—David Kroenke")))
       (<:li (<:em
              "“It has been 35 years since I had to even spell sine or cosine yet this book makes perfect sense and has really opened my eyes to what is possible.”"
              (<:span :style "white-space: nowrap;" "—Peter Hoyt")))
       (<:li (<:em
              "“MUST OWN for any HTML5 game developer.”"
              (<:span :style "white-space: nowrap;" "—Amazon Customer")))))
     (<:h2 "Buy the Book!")
     (<:div
      :class "container"
      (<:div
       :class "text"
       "My "
       (<:a :href "http://www.apress.com/9781430236658"
            "publisher")
       " has graciously allowed me to post " (<:em "all") " the
         sample code and exercises below to make it easier to browse
         and incorporate in your own work. Each example is explained
         in detail within the book, so check it out if you really want
         to " (<:em "understand") " how all this code works. If you're
         just browsing around and looking to solve a problem,that's
         cool, but if it helps you out of a jam,please consider buying
         the "
       (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
            "book (or ebook)")
       " so that I can continue publishing. I promise, it's good!"
       (<:br) "And if you enjoy the book, please spread the word and "
       (<:a :href "http://www.amazon.com/review/create-review?ie=UTF8&asin=1430236655&store=books"
            "write a review") ".")
      (<:a :href "http://www.amazon.com/dp/1430236655?tag=html5anim-20"
           :target "_blank"
           (<:img :src "./examples/include/book-thumb.jpg"
                  :alt "HTML5 Animation with JavaScript"))
      (<:div :class "clear"))
     (<:h2 "The Blurb")
     (<:p
      (<:em "Foundation HTML5 Animation with Javascript")
      ", by "
      (<:a :href "http://lamberta.org" "Billy Lamberta")
      " and "
      (<:a :href "http://www.bit-101.com" "Keith Peters")
      ", covers everything that you need to know to create dynamic,
      scripted animations using the HTML5 canvas. It provides
      information on all the relevant math you'll need, before moving
      on to physics concepts like acceleration, velocity, easing,
      springs, collision detection, conservation of momentum, 3D, and
      forward and inverse kinematics. <em>Foundation HTML5 Animation
      with JavaScript</em> is a fantastic resource for all web
      developers working in HTML5 or switching over from Flash to
      create standards-compliant games, applications, and animations
      that will work across all modern browsers and most mobile
      devices, including iPhones,iPads, and Android devices."))
    (<:section
     :id "exercises"
     (<:h1 "Table of Contents and Exercises")
     (<:p "The examples below cover the following:"
          (<:ul
           (<:li
            "All the JavaScript and HTML5 code (including math and
             trigonometry functions) you'll need to start animating
             with code.")
           (<:li
            "Basic motion principles like velocity, acceleration,
             friction, easing, and bouncing.")
           (<:li
            "How to handle user interactions via the keyboard,mouse,
             and touchscreen.")
           (<:li
            "Advanced motion techniques like springs, coordinate
             rotation, conservation of momentum, and forward and
             inverse kinematics.")
           (<:li
            "All the basic 3D concepts you'll need for 3D in HTML5
             (without WebGL)—from simple perspective to full 3D
             solids, complete with backface culling and dynamic
             lighting.")))
     (<:ol
      (loop
         for val being each hash-value in *parts*
         for index from 1
         collect (progn (setf (index val) index) (render-in-list-index val))))))))
