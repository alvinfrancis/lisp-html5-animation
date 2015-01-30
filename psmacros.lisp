;;;; psmacros.lisp
;; Taken mostly from arvid's extensions:
;; https://github.com/aarvid/arvid-parenscript-extensions

(in-package #:lisp-html5-animation)

;; Taken from arvid extensions
(defpsmacro with-pchain (object &rest chains)
  "generate pseudo-chain of method calls of object. use in iterative code only.
for use of js prototypes that do not allow chaining like j-query."
  (ps-once-only (object)
    `(progn
       ,@(mapcar (lambda (x) `(chain ,object ,x))
                 chains))))

;;  should be called something like with-object-chain or with-prototype-chain
;;  but this name allows use similar to ps:chain.
(defpsmacro fchain (object &rest chains)
  "generate a pseudo-chain of method calls of given object
and return the object. for use of js prototypes that do not
allow chaining like j-query."
  (with-ps-gensyms (obj)
    `(funcall (lambda (,obj)
                ,@(mapcar (lambda (x) `(chain ,obj ,x))
                          chains)
                ,obj)
              ,object)))

(defpsmacro fpchain (object &rest chains)
  "generate a pseudo-chain of method calls of given object
and return the object. for use of js prototypes that do not
allow chaining like j-query."
  (ps-once-only (object)
    (append '(progn)
            (mapcar (lambda (x) `(chain ,object ,x))
                    chains)
            `(,object))))

;; may be should be named with-object-setf
(defpsmacro slot-setf (object &rest slot-pairs)
  "setf several slot-value pairs of the same object"
  (ps-once-only (object)
    `(setf ,@(mapcan (lambda (x) `((@ ,object ,(car x))
                                   ,(cdr x)))
                     (alexandria:plist-alist slot-pairs)))))
(defpsmacro with-object-setf (object &rest slot-pairs)
  "setf several slot-value pairs of the same object"
  (ps-once-only (object)
    `(setf ,@(mapcan (lambda (x) `((@ ,object ,(car x))
                                   ,(cdr x)))
                     (alexandria:plist-alist slot-pairs)))))
;;; my stuff

(defpsmacro -> (&rest chain)
  `(chain ,@chain))

(defpsmacro console-log (object)
  `(chain console (log ,object)))

;; obsolete
(defpsmacro setf-with-symbol (symbol &rest list)
  (ps-once-only (symbol)
    `(progn
       ,@(mapcar (lambda (elem)
                   `(setf (chain ,symbol ,(car elem)) ,@(cdr elem)))
                 list))))

(defpsmacro general-with-symbol (symbol list)
  `(progn
     ,@(mapcar (lambda (elem)
                 (cond
                   ((equalp (car elem) :setf)
                    `(setf (chain ,symbol ,(cadr elem)) ,@(cddr elem)))
                   ((equalp (car elem) :run)
                    `(chain ,symbol ,@(cdr elem)))
                   (t (error "Unexpected directive."))))
               list)))

(defpsmacro with-make-object (symbol symvalue &body body)
  `(let ((,symbol ,symvalue))
     (progn
       ,@body
       ,symbol)))

(defpsmacro prototype (symbol &rest rest)
  `(@ ,symbol prototype ,@rest))

(defpsmacro this (&rest rest)
  `(@ this ,@rest))

(defpsmacro keycode (symbol)
  (case symbol
    (+BACKSPACE+ 8)
    (+TAB+ 9)
    (+ENTER+ 13)
    (+COMMAND+ 15)
    (+SHIFT+ 16)
    (+CONTROL+ 17)
    (+ALTERNATE+ 18)
    (+PAUSE+ 19)
    (+CAPS_LOCK+ 20)
    (+NUMPAD+ 21)
    (+ESCAPE+ 27)
    (+SPACE+ 32)
    (+PAGE_UP+ 33)
    (+PAGE_DOWN+ 34)
    (+END+ 35)
    (+HOME+ 36)

    (+LEFT+ 37)
    (+UP+ 38)
    (+RIGHT+ 39)
    (+DOWN+ 40)

    (+INSERT+ 45)
    (+DELETE+ 46)

    (+NUMBER_0+ 48)
    (+NUMBER_1+ 49)
    (+NUMBER_2+ 50)
    (+NUMBER_3+ 51)
    (+NUMBER_4+ 52)
    (+NUMBER_5+ 53)
    (+NUMBER_6+ 54)
    (+NUMBER_7+ 55)
    (+NUMBER_8+ 56)
    (+NUMBER_9+ 57)

    (+A+ 65)
    (+B+ 66)
    (+C+ 67)
    (+D+ 68)
    (+E+ 69)
    (+F+ 70)
    (+G+ 71)
    (+H+ 72)
    (+I+ 73)
    (+J+ 74)
    (+K+ 75)
    (+L+ 76)
    (+M+ 77)
    (+N+ 78)
    (+O+ 79)
    (+P+ 80)
    (+Q+ 81)
    (+R+ 82)
    (+S+ 83)
    (+T+ 84)
    (+U+ 85)
    (+V+ 86)
    (+W+ 87)
    (+X+ 88)
    (+Y+ 89)
    (+Z+ 90)

    (+LEFT_WINDOW_KEY+ 91)
    (+RIGHT_WINDOW_KEY+ 92)
    (+SELECT_KEY+ 93)

    (+NUMPAD_0+ 96)
    (+NUMPAD_1+ 97)
    (+NUMPAD_2+ 98)
    (+NUMPAD_3+ 99)
    (+NUMPAD_4+ 100)
    (+NUMPAD_5+ 101)
    (+NUMPAD_6+ 102)
    (+NUMPAD_7+ 103)
    (+NUMPAD_8+ 104)
    (+NUMPAD_9+ 105)
    (+NUMPAD_MULTIPLY+ 106)
    (+NUMPAD_ADD+ 107)
    (+NUMPAD_ENTER+ 108)
    (+NUMPAD_SUBTRACT+ 109)
    (+NUMPAD_DECIMAL+ 110)
    (+NUMPAD_DIVIDE+ 111)

    (+F1+ 112)
    (+F2+ 113)
    (+F3+ 114)
    (+F4+ 115)
    (+F5+ 116)
    (+F6+ 117)
    (+F7+ 118)
    (+F8+ 119)
    (+F9+ 120)
    (+F10+ 121)
    (+F11+ 122)
    (+F12+ 123)
    (+F13+ 124)
    (+F14+ 125)
    (+F15+ 126)

    (+NUM_LOCK+ 144)
    (+SCROLL_LOCK+ 145)

    (+SEMICOLON+ 186)
    (+EQUAL+ 187)
    (+COMMA+ 188)
    (+MINUS+ 189)
    (+PERIOD+ 190)
    (+SLASH+ 191)
    (+BACKQUOTE+ 192)
    (+LEFTBRACKET+ 219)
    (+BACKSLASH+ 220)
    (+RIGHTBRACKET+ 221)
    (+QUOTE+ 22)
    (otherwise undefined)))

(defpsmacro with-function-serial-call (object func &rest rest)
  "Series of same function calls on the list"
  (ps-once-only (object)
    `(progn
       ,@(mapcar (lambda (x) `(chain ,object (,func ,@x)))
                 rest))))

(defpsmacro parse-integer (to-parse &key (radix 10))
  "Map CL parse-integer to JS parseInt"
  ;; TODO: Match JS behaviour when radix is null
  `(chain window (parse-int ,to-parse ,radix)))

(defpsmacro subseq (seq start &optional (end 0 end-supplied-p))
  "Map CL subseq to JS slice"
  ;; TODO: Match JS behaviour when arguments are negative
  `(chain ,seq (slice ,@(remove-if #'null (list start (when end-supplied-p end))))))

(defpsmacro write-to-string (number &optional (radix 10))
  "Map CL write-to-string to JS toString"
  `(chain ,number (to-string ,radix)))

(defpsmacro parse-color (color)
  (with-ps-gensyms (color-in))
  `(funcall
    (lambda (color-in)
      (if (= (typeof color-in) "number")
          (concatenate
           'string "#"
           (chain (concatenate
                   'string
                   "00000"
                   (chain (logior color-in 0)
                          (to-string 16)))
                  (substr -6))) 
          color-in))
    ,color))

(defpsmacro clear-2d-canvas (canvas)
  `(chain (chain ,canvas (get-context "2d"))
          (clear-rect 0 0 (@ ,canvas width) (@ ,canvas height))))

(defpsmacro multiple-assign (value &rest vars)
  (ps-once-only (value)
    `(progn
       ,@(mapcar (lambda (x) `(setf ,x ,value))
                 vars))))

(defpsmacro half (value)
  `(/ ,value 2))

(defpsmacro square (x)
  (ps-once-only (x)
    `(* ,x ,x)))

(defpsmacro distance-2d (object1 object2)
  (ps-once-only (object1 object2)
    `(sqrt (+ (square (- (@ ,object1 x) (@ ,object2 x)))
              (square (- (@ ,object1 y) (@ ,object2 y)))))))

(defpsmacro deflambda (name lambda-list &body body)
  `(lambda ()
     (funcall (flet ((,name ,lambda-list
                       ,@body))))))

(defpsmacro request-animation-frame (&rest rest)
  `(-> window (request-animation-frame ,@rest)))
