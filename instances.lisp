(in-package #:lisp-html5-animation)

;; Examples
(let* ((identifier (slug "skeleton"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Our Basic HTML5 Canvas Setup")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :script (ps (setf (@ window onload)
                             (lambda () t))))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "event-demo"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "JavaScript Event Demonstration")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Event Demo"
           :post-canvas (<:aside "Open debugging console in web browser and click mouse.")
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "mouse-events"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Listening for Mouse Events")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Mouse Events"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Open debugging console in web browser and move/click mouse.")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "mouse-position"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Capture the Mouse Position")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Mouse Position"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Open debugging console in web browser and move/click mouse.")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "touch-events"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Listening for Touch Events")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Touch Events"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Open debugging console in web browser and move/click mouse.")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "keyboard-events"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Listening for Keyboard Events")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Keyboard Events"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Open debugging console in web browser and move/click mouse.")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "key-codes"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Capture a Keyboard Event by Key Code")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Key Codes"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Open debugging console in web browser and move/click mouse.")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "key-names"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Capture a Keyboard Event by Name")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Key Names"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Open debugging console in web browser and move/click mouse.")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "rotate-to-mouse"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Rotate an Object Towards a Point")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Rotate to Mouse"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Move mouse on canvas element")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "bobbing-1"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Smooth Up and Down Motion")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Bobbing 1"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "bobbing-2"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Set the Range, Speed, and Center of the Motion")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Bobbing 2"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "wave-1"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Add Linear Vertical Motion to Make a Wave")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Wave 1"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "pulse"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Create a Pulsing Motion by Adjusting Scale")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Pulse"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "random"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Wave Motion Using Two Angles")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Random"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "wave-2"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Draw a Wave to the Canvas Element")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Wave 2"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "circle"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Circular Movement")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Circle"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "oval"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Elliptical Movement")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Oval"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "distance"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Using the Pythagorean Theorem to Calculate Distance")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Distance"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (list (<:textarea :id "log")
                              (<:aside "Refresh page for another calculation"))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "mouse-distance"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Find the Distance Between the Center Point and Mouse")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Mouse Distance"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (list (<:textarea :id "log")
                              (<:aside "Move mouse on canvas element."))
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "drawing-app"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Drawing App")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Drawing App"
           :pre-canvas (<:header "Example from "
                                 (<:a :href "http://amzn.com/1430236655?tag=html5anim-20"
                                      (<:em "Foundation HTML5 Animation with JavaScript")))
           :post-canvas (<:aside "Click and draw with the mouse.")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))

(let* ((identifier (slug "skewer-testing"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Skewer Testing")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Skewer Testing"
           :pre-canvas (<:script :src "http://localhost:8080/skewer"))))
  (setf (gethash identifier *examples*) new-example))
(let* ((identifier (slug "slider-experiment"))
       (new-example
        (make-instance 'example
                       :identifier identifier
                       :title "Slider Experiment")))
  (setf (page new-example)
        (lambda ()
          (canvas-page
           :title "Slider Experiment"
           :pre-canvas (<:script :src "http://localhost:8080/skewer")
           :script (gethash identifier *paren-examples*))))
  (setf (gethash identifier *examples*) new-example))

;; Chapters
(setf
 (get-chapter :basic-animation-concepts)
 (make-instance 'chapter
                :title "Basic Animation Concepts")
 (get-chapter :basics-javascript-animation)
 (make-instance 'chapter
                :title "Basics of JavaScript Animation"
                :examples #'(lambda ()
                              (remove-if #'null
                                         (mapcar
                                          (make-gethash *examples* #'slug)
                                          (list "skeleton"
                                                "event demo"
                                                "mouse events"
                                                "mouse position"
                                                "touch events"
                                                "keyboard events"
                                                "key codes"
                                                "key names")))))
 (get-chapter :trigonometry-animation)
 (make-instance 'chapter
                :title "Trigonometry for Animation"
                :examples #'(lambda ()
                              (remove-if #'null
                                         (mapcar
                                          (make-gethash *examples* #'slug)
                                          (list "rotate to mouse"
                                                "bobbing-1"
                                                "bobbing-2"
                                                "wave-1"
                                                "pulse"
                                                "random"
                                                "wave-2"
                                                "circle"
                                                "oval"
                                                "distance"
                                                "mouse-distance")))))
 (get-chapter :rendering-techniques)
 (make-instance 'chapter
                :title "Rendering Techniques"
                :examples #'(lambda ()
                              (remove-if #'null
                                         (mapcar
                                          (make-gethash *examples* #'slug)
                                          (list "drawing app"
                                                "drawing curves"
                                                "curve through point"
                                                "multi curve 1"
                                                "multi curve 2"
                                                "multi curve 3"
                                                "gradient fill 1"
                                                "gradient fill 2"
                                                "gradient fill radial"
                                                "load image"
                                                "embed image"
                                                "video frames"
                                                "inver color"
                                                "grayscale"
                                                "pixel move"
                                                "spray paint")))))
 (get-chapter :experiments)
 (make-instance 'chapter
                :title "Experimental Stuff"
                :examples #'(lambda ()
                              (remove-if #'null
                                         (mapcar
                                          (make-gethash *examples* #'slug)
                                          (list "skewer testing"
                                                "slider experiment"))))))

;; Parts
(setf (gethash :animation-basics *parts*)
      (make-instance 'part
                     :title "JavaScript Animation Basics"
                     :chapters #'(lambda () 
                                   (remove-if #'null
                                              (mapcar (make-gethash *chapters*)
                                                      '(:basic-animation-concepts
                                                        :basics-javascript-animation
                                                        :trigonometry-animation
                                                        :rendering-techniques)))))
      (gethash :experiments *parts*)
      (make-instance 'part
                     :title "Testing and Experimenting"
                     :chapters #'(lambda ()
                                   (remove-if #'null
                                              (mapcar (make-gethash *chapters*)
                                                      '(:experiments))))))
