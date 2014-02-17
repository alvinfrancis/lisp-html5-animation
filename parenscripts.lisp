;;;; parenscripts
(in-package :lisp-html5-animation)

;;; namespaces
;; Use parenscript namespace prefixing to create namespace
;; objects. This requires the namespace objects to be created first.
(defpackage :utils
  (:use :parenscript))
(setf (ps-package-prefix :utils) "utils.")

;;; utility scripts
(setf                                   ; paren-utils
 *paren-utils*
 (ps (setf
      (@ window utils)
      (create
       capture-mouse
       (lambda (element)
         (with-make-object
             mouse (create x 0 y 0 event null)
             (let ((body-scroll-left (@ document body scroll-left))
                   (element-scroll-left (@ document document-element scroll-left))
                   (body-scroll-top (@ document body scroll-top))
                   (element-scroll-top (@ document document-element scroll-top))
                   (offset-left (@ element offset-left))
                   (offset-top (@ element offset-top)))
               (progn
                 (chain element
                        (add-event-listener
                         "mousemove"
                         (lambda (event)
                           (let* ((x (- (if (@ event page-x) (@ event page-x)
                                            (+ (@ event client-x)
                                               body-scroll-left element-scroll-left))
                                        offset-left))
                                  (y (- (if (@ event page-y) (@ event page-y)
                                            (+ (@ event client-y)
                                               body-scroll-top element-scroll-top))
                                        offset-top)))
                             (setf (@ mouse x) x)
                             (setf (@ mouse y) y)
                             (setf (@ mouse event) event)))
                         false))))))
       capture-touch
       (lambda (element)
         (with-make-object touch
           (create x null
                   y null
                   is-pressed false
                   event null)
           (let ((body-scroll-left (@ document body scroll-left))
                 (element-scroll-left (@ document document-element scroll-left))
                 (body-scroll-top (@ document body scroll-top))
                 (element-scroll-top (@ document document-element scroll-top))
                 (offset-left (@ element offset-left))
                 (offset-top (@ element offset-top)))
             (with-pchain element
               (add-event-listener
                "touchstart"
                (lambda (event)
                  (with-object-setf touch
                    is-pressed t
                    event event)))
               (add-event-listener
                "touchend"
                (lambda (event)
                  (with-object-setf touch
                    is-pressed false
                    x null
                    y null
                    event event)))
               (add-event-listener
                "touchmove"
                (lambda (event)
                  (let* ((touch-event (@ event touches 0))
                         (x (- (if (@ touch-event page-x)
                                   (@ touch-event page-x)
                                   (+ (@ touch-event client-x)
                                      (@ body-scroll-left)
                                      (@ element-scroll-left)))
                               offset-left))
                         (y (- (if (@ touch-event page-y)
                                   (@ touch-event page-y)
                                   (+ (@ touch-event client-y)
                                      (@ body-scroll-top)
                                      (@ element-scroll-top)))
                               offset-left)))
                    (with-object-setf touch
                      x x
                      y y
                      event event)))
                false)))))
       parse-color
       (lambda (color &optional (to-number nil))
         (if to-number
             (cond
               ((numberp color)
                (logior color 0))
               ((stringp color)
                (parse-integer (subseq color 1) :radix 16))
               (t (parse-integer color :radix 16)))
             (if (numberp color)
                 (concatenate
                  'string
                  "#"
                  (let* ((suffix (concatenate
                                  'string
                                  "00000" (write-to-string (logior color 0) 16)))
                         (length (length suffix)))
                    (subseq suffix (if (>= length 6)
                                       (- length 6)
                                       0))))
                 color)))
       color-to-r-g-b
       (lambda (color &optional (alpha 1))
         (when (and (= (typeof color) "string")
                    (= (@ color 0) "#"))
           (setf color (parse-integer (subseq (@ color) 1) :radix 16)))
         (let ((r (logand (ash color -16) #xff))
               (g (logand (ash color -8) #xff))
               (b (logand color #xff))
               (a (cond ((< alpha 0) 0)
                        ((> alpha 1) 1) (t alpha))))
           (if (= a 1)
               (concatenate 'string
                            "rgb(" r "," g "," b ")")
               (concatenate 'string
                            "rgba(" r "," g "," b "," a ")"))))
       contains-point
       (lambda (rect x y)
         (let ((rx (@ rect x))
               (ry (@ rect y))
               (rwidth (@ rect width))
               (rheight (@ rect height))))
         (not (or (< x rx)
                  (> x (+ rx rwidth))
                  (< y ry)
                  (> y (+ ry rheight)))))
       intersects
       (lambda (rect-a rect-b)
         (let ((comp (lambda (x y z)
                       (< (+ x y)
                          z))))
           (not (or (comp (@ rect-a x) (@ rect-a width)
                          (@ rect-b x))
                    (comp (@ rect-b x) (@ rect-b width)
                          (@ rect-a x))
                    (comp (@ rect-a y) (@ rect-a height)
                          (@ rect-b y))
                    (comp (@ rect-b y) (@ rect-b height)
                          (@ rect-a y))))))))))
(setf                                   ; paren-keycode
 ;; favor ps-keycode macro over this
 *paren-keycode*
 (ps (defvar keycode
       (create
        +BACKSPACE+ 8
        +TAB+ 9
        +ENTER+ 13
        +COMMAND+ 15
        +SHIFT+ 16
        +CONTROL+ 17
        +ALTERNATE+ 18
        +PAUSE+ 19
        +CAPS_LOCK+ 20
        +NUMPAD+ 21
        +ESCAPE+ 27
        +SPACE+ 32
        +PAGE_UP+ 33
        +PAGE_DOWN+ 34
        +END+ 35
        +HOME+ 36

        +LEFT+ 37
        +UP+ 38
        +RIGHT+ 39
        +DOWN+ 40

        +INSERT+ 45
        +DELETE+ 46

        +NUMBER_0+ 48
        +NUMBER_1+ 49
        +NUMBER_2+ 50
        +NUMBER_3+ 51
        +NUMBER_4+ 52
        +NUMBER_5+ 53
        +NUMBER_6+ 54
        +NUMBER_7+ 55
        +NUMBER_8+ 56
        +NUMBER_9+ 57

        +A+ 65
        +B+ 66
        +C+ 67
        +D+ 68
        +E+ 69
        +F+ 70
        +G+ 71
        +H+ 72
        +I+ 73
        +J+ 74
        +K+ 75
        +L+ 76
        +M+ 77
        +N+ 78
        +O+ 79
        +P+ 80
        +Q+ 81
        +R+ 82
        +S+ 83
        +T+ 84
        +U+ 85
        +V+ 86
        +W+ 87
        +X+ 88
        +Y+ 89
        +Z+ 90

        +LEFT_WINDOW_KEY+ 91
        +RIGHT_WINDOW_KEY+ 92
        +SELECT_KEY+ 93

        +NUMPAD_0+ 96
        +NUMPAD_1+ 97
        +NUMPAD_2+ 98
        +NUMPAD_3+ 99
        +NUMPAD_4+ 100
        +NUMPAD_5+ 101
        +NUMPAD_6+ 102
        +NUMPAD_7+ 103
        +NUMPAD_8+ 104
        +NUMPAD_9+ 105
        +NUMPAD_MULTIPLY+ 106
        +NUMPAD_ADD+ 107
        +NUMPAD_ENTER+ 108
        +NUMPAD_SUBTRACT+ 109
        +NUMPAD_DECIMAL+ 110
        +NUMPAD_DIVIDE+ 111

        +F1+ 112
        +F2+ 113
        +F3+ 114
        +F4+ 115
        +F5+ 116
        +F6+ 117
        +F7+ 118
        +F8+ 119
        +F9+ 120
        +F10+ 121
        +F11+ 122
        +F12+ 123
        +F13+ 124
        +F14+ 125
        +F15+ 126

        +NUM_LOCK+ 144
        +SCROLL_LOCK+ 145

        +SEMICOLON+ 186
        +EQUAL+ 187
        +COMMA+ 188
        +MINUS+ 189
        +PERIOD+ 190
        +SLASH+ 191
        +BACKQUOTE+ 192
        +LEFTBRACKET+ 219
        +BACKSLASH+ 220
        +RIGHTBRACKET+ 221
        +QUOTE+ 22))))

;;; paren classes
(setf                                   ; arrow
 (gethash "arrow" *paren-classes*)
 (ps
   (defun -arrow ()
     (with-make-object
         this this
         (with-object-setf
             this
           x 0
           y 0
           color "#ffff00"
           rotation 0)))
   (setf (prototype -arrow draw)
         (lambda (ctx)
           (with-pchain ctx
             (save)
             (translate (this x) (this y))
             (rotate (this rotation)))
           (with-object-setf ctx
             line-width 2
             fill-style (this color))
           (with-pchain ctx
             (begin-path)
             (move-to -50 -25))
           (with-function-serial-call
               ctx line-to
               (0 -25)
               (0 -50)
               (50 0)
               (0 50)
               (0 25)
               (-50 25)
               (-50 -25))
           (with-pchain ctx
             (close-path)
             (fill)
             (stroke)
             (restore))))))
(setf                                   ; ball
 (gethash "ball" *paren-classes*)
 (ps
   (defun -ball (&optional (radius 40) (color "#ff0000"))
     (with-object-setf this
       x 0
       y 0
       radius radius
       rotation 0
       scale-x 1
       scale-y 1
       color (parse-color color)
       line-width 1))
   (setf (prototype -ball draw)
         (lambda (ctx)
           (with-pchain ctx
             (save)
             (translate (this x) (this y))
             (rotate (this rotation))
             (scale (this scale-x) (this scale-y)))
           (with-object-setf ctx
             line-width (this line-width)
             fill-style (this color))
           (with-pchain ctx
             (begin-path)
             (arc
              0 0
              (this radius) 0
              (* pi 2) t)
             (close-path)
             (fill))
           (when (> (this line-width) 0)
             (chain ctx (stroke)))
           (chain ctx (restore))))))
(setf                                   ; rect
 (gethash "rect" *paren-classes*)
 (ps
   (defun -rect (&optional (height 4) (width 4) (color "#000000"))
     (with-object-setf this
       x 0
       y 0
       height height
       width width
       rotation 0
       scale-x 1
       scale-y 1
       color (parse-color color)))
   (setf (prototype -rect draw)
         (lambda (ctx)
           (with-pchain ctx
             (save)
             (translate (this x) (this y))
             (rotate (this rotation))
             (scale (this scale-x) (this scale-y)))
           (setf (@ ctx fill-style) (this color))
           (chain ctx
                  (fill-rect (- (half (this width)))
                             (- (half (this height)))
                             (this width)
                             (this height)))
           (chain ctx (restore))))))
(setf                                   ; slider
 (gethash "slider" *paren-classes*)
 (ps
   (defun -slider (&optional (min 0) (max 100) (value 100))
     (with-object-setf this
       min min
       max max
       value value
       on-change nil
       x 0
       y 0
       width 16
       height 100
       back-color "#cccccc"
       back-border-colder "#999999"
       back-width 4
       back-x (- (/ (this width) 2) (/ (this back-width) 2))
       handle-color "#eeeeee"
       handle-border-color "#cccccc"
       handle-height 6
       handle-y 0
       (chain this (update-position))))
   (setf (@ -slider prototype)
         (create
          draw (lambda (ctx)
                 (chain ctx (save))
                 (setf (@ ctx fill-style) (this back-color))
                 ;; draw back
                 (with-pchain ctx 
                   (translate (this x) (this y))
                   (begin-path)
                   (fill-rect (this back-x) 0
                              (this back-width) (this height))
                   (close-path))
                 ;; draw handle
                 (with-object-setf ctx
                   stroke-style (this handle-border-color)
                   fill-style (this handle-color))
                 (with-pchain ctx
                   (begin-path)
                   (rect 0 (this handle-y)
                         (this width) (this handle-height))
                   (close-path)
                   (fill)
                   (stroke))
                 (chain ctx (restore)))
          update-value (lambda ()
                         (let ((old-value (this value))
                               (handle-range (- (this height) (this handle-height)))
                               (value-range (- (this max) (this min))))
                           (setf (this value)
                                 (+ (/ (- handle-range (this handle-y))
                                       (* handle-range value-range))
                                    (this min)))
                           ;; hook
                           (when (and (= (typeof (this on-change)) "function")
                                      (not (= (this value) old-value)))
                             (chain this (on-change)))))
          update-position (lambda ()
                            (let ((handle-range (- (this height) (this handle-height)))
                                  (value-range (- (this max) (this min))))
                              (setf (this handle-y)
                                    (- handle-range
                                       (* (/ (- (this value) (this min))
                                             value-range)
                                          handle-range)))))
          capture-mouse (lambda (element)
                          (let ((self this)
                                (mouse (chain utils (capture-mouse element)))
                                (bounds (create)))
                            (set-handle-bounds)
                            (chain element
                                   (add-event-listener
                                    "mousedown"
                                    (lambda ()
                                      (when (chain utils
                                                   (contains-point bounds
                                                                   (@ mouse x)
                                                                   (@ mouse y)))
                                        (with-pchain element
                                          (add-event-listener "mouseup"
                                                              on-mouse-up
                                                              false)
                                          (add-event-listener "mousemove"
                                                              on-mouse-move
                                                              false))))))
                            (defun on-mouse-up ()
                              (with-pchain element
                                (remove-event-listener
                                 "mousemove" on-mouse-move false)
                                (remove-event-listener
                                 "mouseup" on-mouse-up false))
                              (set-handle-bounds))
                            (defun on-mouse-move ()
                              (let ((pos-y (- (@ mouse y) (@ self y))))
                                (setf (@ self handle-y)
                                      (min (- (@ self height)
                                              (@ self handle-height))
                                           (max pos-y 0)))
                                (chain self (update-value))))
                            (defun set-handle-bounds ()
                              (with-object-setf bounds
                                x (@ self x)
                                y (+ (@ self y) (@ self handle-y))
                                width (@ self width)
                                height (@ self handle-height)))))))))

;;; scripts for the examples
(setf                                   ; event-demo
 (gethash (slug "event-demo") *paren-examples*)
 (ps (setf
      (@ window onload)
      (lambda ()
        (with-pchain (chain document (get-element-by-id "canvas"))
          (add-event-listener
           "mousedown"
           (lambda (event) (console-log "mouse down"))
           false)
          (add-event-listener
           "mouseup"
           (lambda (event) (console-log "mouse up"))
           false))))))
(setf                                   ; mouse-events
 (gethash (slug "mouse-events") *paren-examples*)
 (ps (setf
      (@ window onload)
      (lambda ()
        (let ((on-mouse-event
               (lambda (event) (console-log (@ event type)))))
          (with-pchain (chain document (get-element-by-id "canvas"))
            (add-event-listener "mousedown" on-mouse-event false)
            (add-event-listener "mouseup" on-mouse-event false)
            (add-event-listener "click" on-mouse-event false)
            (add-event-listener "dblclick" on-mouse-event false)
            (add-event-listener "mousewheel" on-mouse-event false)
            (add-event-listener "mouseover" on-mouse-event false)
            (add-event-listener "mouseout" on-mouse-event false)))))))
(setf                                   ; mouse-position
 (gethash (slug "mouse-position") *paren-examples*)
 (list
  *paren-utils*
  (ps (setf
       (@ window onload)
       (lambda ()
         (let* ((canvas (chain document (get-element-by-id "canvas")))
                (mouse (chain utils (capture-mouse canvas))))
           (chain canvas (add-event-listener
                          "mousedown"
                          (lambda ()
                            (console-log
                             (concatenate 'string
                                          "x: "
                                          (@ mouse x)
                                          ", y: "
                                          (@ mouse y))))
                          false))))))))
(setf                                   ; touch-events
 (gethash (slug "touch-events") *paren-examples*)
 (ps (setf
      (@ window onload)
      (lambda ()
        (let ((on-touch-event (lambda (event)
                                (console-log (@ event type)))))
          (with-pchain (chain document (get-element-by-id "canvas"))
            (add-event-listener "touchstart" on-touch-event false)
            (add-event-listener "touchend" on-touch-event false)
            (add-event-listener "touchmove" on-touch-event false)))))))
(setf                                   ; keyboard-events
 (gethash (slug "keyboard-events") *paren-examples*)
 (ps (setf
      (@ window onload)
      (lambda ()
        (let ((on-keyboard-event (lambda (event)
                                   (console-log (@ event type)))))
          (with-pchain window
            (add-event-listener "keydown" on-keyboard-event false)
            (add-event-listener "keyup" on-keyboard-event false)))))))
(setf                                   ; key-codes
 (gethash (slug "key-codes") *paren-examples*)
 (ps (setf
      (@ window onload)
      (lambda ()
        (let ((on-keyboard-event
               (lambda (event)
                 (case (@ event key-code)
                   (38 (console-log "up"))
                   (40 (console-log "down"))
                   (37 (console-log "left"))
                   (39 (console-log "right"))
                   (otherwise (console-log (@ event key-code)))))))
          (chain window (add-event-listener "keydown" on-keyboard-event false)))))))
(setf                                   ; key-names
 (gethash (slug "key-names") *paren-examples*)
 (list
  *paren-keycode*
  (ps (setf
       (@ window onload)
       (lambda ()
         (let ((on-keyboard-event
                (lambda (event)
                  (switch (@ event key-code)
                    ((@ keycode +up+) (console-log "up") break)
                    ((@ keycode +down+) (console-log "down") break)
                    ((@ keycode +left+) (console-log "left") break)
                    ((@ keycode +right+) (console-log "right") break)
                    (default (console-log (@ event key-code)))))))
           (chain window (add-event-listener "keydown" on-keyboard-event false))))))))
(setf                                   ; rotate-to-mouse
 (gethash (slug "rotate-to-mouse") *paren-examples*)
 (list
  *paren-utils*
  (gethash "arrow" *paren-classes*)
  (ps (setf
       (@ window onload)
       (lambda ()
         (let* ((canvas (chain document (get-element-by-id "canvas")))
                (ctx (chain canvas (get-context "2d")))
                (mouse (chain utils (capture-mouse canvas)))
                (arrow (new (-arrow))))
           (with-object-setf arrow
             x (/ (@ canvas width) 2)
             y (/ (@ canvas height) 2))
           (funcall
            (deflambda draw-frame ()
              (chain window (request-animation-frame draw-frame))
              (chain ctx (clear-rect
                          0 0
                          (@ canvas width) (@ canvas height)))
              (let ((dx (- (@ mouse x)
                           (@ arrow x)))
                    (dy (- (@ mouse y)
                           (@ arrow y))))
                (setf (@ arrow rotation) (atan dy dx))
                (chain arrow (draw ctx)))))))))))
(setf                                   ; bobbing-1
 (gethash (slug "bobbing-1") *paren-examples*)
 (list
  *paren-utils*
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle 0))
         (with-object-setf ball
           x (/ (@ canvas width) 2)
           y (/ (@ canvas height) 2))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame))
            (chain ctx (clear-rect
                        0 0
                        (@ canvas width) (@ canvas height)
                        ))
            (setf (@ ball y)
                  (+ (/ (@ canvas height) 2) (* (sin angle) 50)))
            (incf angle 0.1)
            (chain ball (draw ctx))))))))))
(setf                                   ; bobbing-2
 (gethash (slug "bobbing-2") *paren-examples*)
 (list
  *paren-utils*
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle 0)
              (center-y 200)
              (range 50)
              (speed 0.05))
         (setf (@ ball x) (/ (@ canvas width) 2))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame))
            (chain ctx (clear-rect
                        0 0
                        (@ canvas width) (@ canvas height)
                        ))
            (setf (@ ball y)
                  (+ center-y (* (sin angle) range)))
            (incf angle speed)
            (chain ball (draw ctx))))))))))
(setf                                   ; wave-1
 (gethash (slug "wave-1") *paren-examples*)
 (list
  *paren-utils*
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle 0)
              (center-y 200)
              (range 50)
              (x-speed 1)
              (y-speed 0.05))
         (setf (@ ball x) (/ (@ canvas width) 2))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame))
            (chain ctx (clear-rect
                        0 0
                        (@ canvas width) (@ canvas height)
                        ))
            (incf (@ ball x) x-speed)
            (setf (@ ball y)
                  (+ center-y (* (sin angle) range)))
            (incf angle y-speed)
            (chain ball (draw ctx))))))))))
(setf                                   ; pulse
 (gethash (slug "pulse") *paren-examples*)
 (list
  *paren-utils*
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle 0)
              (center-scale 1)
              (range 0.5)
              (speed 0.05))
         (with-object-setf ball
           x (/ (@ canvas width) 2)
           y (/ (@ canvas height) 2))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame))
            (clear-2d-canvas canvas)
            (multiple-assign
             (+ center-scale (* (sin angle) range))
             (@ ball scale-x)
             (@ ball scale-y))
            (incf angle speed)
            (chain ball (draw ctx))))))))))
(setf                                   ; random
 (gethash (slug "random") *paren-examples*)
 (list
  *paren-utils*
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle-x 0)
              (angle-y 0)
              (range 50)
              (center-x (half (@ canvas width)))
              (center-y (half (@ canvas height)))
              (x-speed 0.08)
              (y-speed 0.11))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame))
            (clear-2d-canvas canvas)
            (with-object-setf ball
              x (+ center-x (* (sin angle-x) range))
              y (+ center-y (* (sin angle-y) range)))
            (incf angle-x x-speed)
            (incf angle-y y-speed)
            (chain ball (draw ctx))))))))))
(setf                                   ; wave-2
 (gethash (slug "wave-2") *paren-examples*)
 (list
  *paren-utils*
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (angle 0)
              (range 50)
              (center-y (half (@ canvas height)))
              (x-speed 1)
              (y-speed 0.05)
              (x-pos 0)
              (y-pos center-y))
         (setf (@ ctx line-width) 2)
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame))
            (with-pchain ctx
              (begin-path)
              (move-to x-pos y-pos))
            (incf x-pos x-speed)
            (incf angle y-speed)
            (setf y-pos (+ center-y (* (sin angle) range)))
            (with-pchain ctx
              (line-to x-pos y-pos)
              (stroke)
              (close-path))))))))))
(setf                                   ; circle
 (gethash (slug "circle") *paren-examples*)
 (list
  *paren-utils*
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle 0)
              (center-x (half (@ canvas width)))
              (center-y (half (@ canvas height)))
              (radius 50)
              (speed 0.05))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame canvas))
            (clear-2d-canvas canvas)
            (with-object-setf ball
              x (+ center-x (* (cos angle) radius))
              y (+ center-y (* (sin angle) radius)))
            (incf angle speed) 
            (chain ball (draw ctx))))))))))
(setf                                   ; oval
 (gethash (slug "oval") *paren-examples*)
 (list
  *paren-utils*
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle 0)
              (center-x (half (@ canvas width)))
              (center-y (half (@ canvas height)))
              (radius-x 150)
              (radius-y 100)
              (speed 0.05))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame canvas))
            (clear-2d-canvas canvas)
            (with-object-setf ball
              x (+ center-x (* (cos angle) radius-x))
              y (+ center-y (* (sin angle) radius-y)))
            (incf angle speed) 
            (chain ball (draw ctx))))))))))
(setf                                   ; distance
 (gethash (slug "distance") *paren-examples*)
 (list
  *paren-utils*
  (gethash "rect" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (log (chain document (get-element-by-id "log"))))
         (let ((rect1 (new (-rect)))
               (rect2 (new (-rect))))
           (with-object-setf rect1
             x (* (random) (@ canvas width))
             y (* (random) (@ canvas height)))
           (chain rect1 (draw ctx))
           (with-object-setf rect2
             x (* (random) (@ canvas width))
             y (* (random) (@ canvas height)))
           (chain rect2 (draw ctx))
           (setf (@ log value)
                 (concatenate 'string
                              "distance: " (distance-2d rect1 rect2))))))))))
(setf                                   ; mouse-distance
 (gethash (slug "mouse-distance") *paren-examples*)
 (list
  *paren-utils*
  (gethash "rect" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (mouse (chain utils (capture-mouse canvas)))
              (log (chain document (get-element-by-id "log")))
              (rect (new (-rect))))
         (with-object-setf rect
           x (half (@ canvas width))
           y (half (@ canvas height)))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame canvas))
            (clear-2d-canvas canvas)
            (setf (@ ctx fill-style) "#000000")
            (chain rect (draw ctx))
            (with-pchain ctx
              (begin-path)
              (move-to (@ rect x) (@ rect y))
              (line-to (@ mouse x) (@ mouse y))
              (close-path)
              (stroke))
            (setf (@ log value)
                  (concatenate 'string
                               "distance "
                               (distance-2d rect mouse)))))))))))

;;; experiments
(setf                                   ; slider-experiment
 (gethash (slug "slider-experiment") *paren-examples*)
 (list
  *paren-utils*
  (funcall (lambda () (gethash "slider" *paren-classes*)))
  (gethash "ball" *paren-classes*)
  (ps
    (setf
     (@ window onload)
     (lambda ()
       (let* ((canvas (chain document (get-element-by-id "canvas")))
              (ctx (chain canvas (get-context "2d")))
              (ball (new (-ball)))
              (angle 0)
              (center-x (half (@ canvas width)))
              (center-y (half (@ canvas height)))
              (radius 50)
              (slider (new (-slider 0 10 0)))
              (speed (@ slider value)))
         (with-object-setf slider
           x 20
           y 20
           on-change (lambda () (setf speed (@ slider value))))
         (chain slider (capture-mouse canvas))
         (funcall
          (deflambda draw-frame ()
            (chain window (request-animation-frame draw-frame canvas))
            ;; (setf (@ ctx fill-style)
            ;;       "rgba(255, 255, 255, 0.01)")
            ;; (chain ctx (fill-rect 0 0 (@ canvas width) (@ canvas height)))
            (clear-2d-canvas canvas)
            (with-object-setf ball
              x (+ center-x (* (cos angle) radius))
              y (+ center-y (* (sin angle) radius)))
            (incf angle speed) 
            (chain slider (draw ctx))
            (chain ball (draw ctx))))))))))
