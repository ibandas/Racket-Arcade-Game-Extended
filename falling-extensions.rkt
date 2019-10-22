;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname falling-extensions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;
;                                                      ;
;     ;;;;                                             ;
;   ;;    ;
;   ;       ;    ;  ; ;;;    ;;;;   ; ;;;    ;;;;    ;;;     ;;;;
;   ;        ;   ;  ;;   ;  ;;  ;;  ;;  ;   ;    ;     ;    ;    ;
;    ;;      ;  ;   ;    ;  ;    ;  ;    ;  ;          ;    ;
;      ;;;   ;  ;   ;    ;  ;    ;  ;    ;  ;;;        ;    ;;;
;         ;   ; ;   ;    ;  ;    ;  ;    ;     ;;;     ;       ;;;
;         ;   ;;    ;    ;  ;    ;  ;    ;       ;     ;         ;
;   ;    ;;   ;;    ;    ;  ;;  ;;  ;;  ;   ;    ;     ;    ;    ;
;    ;;;;;     ;    ;    ;   ;;;;   ; ;;;    ;;;;   ;;;;;;;  ;;;;
;              ;                    ;
;             ;                     ;
;            ;;                     ;
;

#|
For this exercise you will design and implement a
minimalistic, one finger input game. The player
controls a paddle that moves back and forth at the
bottom of the screen. Falling from the heavens are
some items that you're trying to capture on your
paddle. The paddle never stays still; it
continuously moves left and right along the bottom
of the screen.
There is only a single kind of input accepted
(think like a thumb tap on a phone); the tap
reverses the direction of the paddle. That is, if
there is no input, then the paddle moves from the
left edge to the right edge and then back to the
left edge, over and over. When the user taps, then
the paddle reverses direction even when it isn’t
at one of the edges. So, if the user wishes to
keep the paddle in one spot, they can tap
repeatedly.
The player gets 10 points for each falling item
that the paddle catches and loses one point each
time they tap to reverse direction, but the score
never goes below zero.
Use the world data definition given below; note
that there is some ambiguity in this definition.
For example, do the `Posn`s of the fallers
represent their centers or upper-left corners? You
will need to figure out issues like this one and
make sure your code is consistent.
Either way, you should use the center of the
faller to determine if it has fallen off of the
bottom or if it has hit the paddle.
|#

(require 2htdp/image)
(require 2htdp/universe)


;
;
;
;   ;;;;;             ;
;   ;    ;            ;
;   ;     ;   ;;;   ;;;;;;    ;;;
;   ;     ;  ;   ;    ;      ;   ;
;   ;     ;      ;    ;          ;
;   ;     ;  ;;;;;    ;      ;;;;;
;   ;     ; ;    ;    ;     ;    ;
;   ;     ; ;    ;    ;     ;    ;
;   ;    ;  ;   ;;    ;     ;   ;;
;   ;;;;;    ;;; ;     ;;;   ;;; ;
;
;
;
;

; A Faller is:
;   (make-faller Type Posn)
; interp.: if `a-faller` is a Faller then all of:
; - (faller-type a-faller) is a Type
; - (faller-position a-faller) is a Posn of the Faller
(define-struct faller [type position])

; A Type is one of:
; - "Shrink"
; - "Grow"
; - "Normal"



; A Faller-world is
;   (make-fw Number Direction List-of-Faller Natural)
; interp.: if `a-fw` is a Faller-world then all of:
; - (fw-paddle a-fw) is the x coordinate of the paddle,
; - (fw-direction a-fw) gives which direction the paddle is moving,
; - (fw-fallers a-fw) is a list of Faller, and
; - (fw-score a-fw) is the score.
(define-struct fw (paddle direction fallers score))

; A Direction is one of:
; - "left"
; - "right"

; A List-of-Faller is one of:
; - '()
; - (cons Faller List-of-Faller)

; A Posn is (make-posn Real Real)
; (Note: `Real` means a real number, which excludes complex numbers.)


;
;
;
;     ;;;;                            ;                       ;
;    ;    ;                           ;                       ;
;   ;        ;;;;   ; ;;;    ;;;;   ;;;;;;    ;;;   ; ;;;   ;;;;;;   ;;;;
;   ;       ;;  ;;  ;;   ;  ;    ;    ;      ;   ;  ;;   ;    ;     ;    ;
;   ;       ;    ;  ;    ;  ;         ;          ;  ;    ;    ;     ;
;   ;       ;    ;  ;    ;  ;;;       ;      ;;;;;  ;    ;    ;     ;;;
;   ;       ;    ;  ;    ;     ;;;    ;     ;    ;  ;    ;    ;        ;;;
;   ;       ;    ;  ;    ;       ;    ;     ;    ;  ;    ;    ;          ;
;    ;    ; ;;  ;;  ;    ;  ;    ;    ;     ;   ;;  ;    ;    ;     ;    ;
;     ;;;;   ;;;;   ;    ;   ;;;;      ;;;   ;;; ;  ;    ;     ;;;   ;;;;
;
;
;
;

;; You will use these named constants in the
;; definitions of your functions to determine the
;; world’s dimensions and when fallers are created.
;; Your program should still work—with no other
;; changes—when these constants are adjusted (within
;; a reasonable range).
(define WORLD-WIDTH 200)   ; window width
(define WORLD-HEIGHT 300)  ; window height
(define MAX-FALLERS 20)    ; maximum faller count 20
(define INV-P-FALLER 25)   ; inverse of per-tick probability of new faller

#|
For the first step, give your game some flavor.
Find or design an image to show as the falling
items and design an image to use as the paddle.
For the paddle, use `2htdp/image` to make an
image, but for the fallers you may find an image
online to include in your program (or you may
compose your own one using `2htdp/image`).
Make your falling image about 20 pixels tall and
20 pixels wide and make your paddle about 12
pixels tall and 50 pixels wide. Use `image-width`
and `image-height` to confirm the sizes.
Please DO NOT paste the image that you find
directly into your code because that makes version
control (Git) not work very well on the resulting
file. Instead, you should save the image as a file
in this directory and load it in your program
using the `bitmap/file` function. For example, if
you save your faller image as `faller.jpg` (in the
same directory as this file), then you can load it
like this:
  (define FALLER-IMAGE (bitmap/file "faller.jpg"))
In order to a new file like `faller.jpg` to be
committed to Git and uploaded to GitHub (so that
we can see it when grading), you need to use the
`git add` command, like so:
  $ git add faller.jpg
When you commit after `git add`, the file that you
added will be included in the commit.
|#
(define PADDLE-WIDE 50)
(define PADDLE-TALL 12)
(define FALLER-SIZE 20)
; Paddle Image
; TO-DO: Make the paddle-image somehow be dynamic too
; Maybe creating a struct for the paddle:
; Paddle is:
; (make-paddle Number Number Image)
; interp.: if `a-paddle` is a Paddle then all of:
; (paddle-wide a-paddle) is the width of the paddle for the image
; (paddle-tall a-paddle) is the height of the paddle for the image
; (paddle-image a-paddle) is the image of the paddle
; Potentially only do wide + tall and pass those into the definition below
(define PADDLE-IMAGE (rectangle PADDLE-WIDE PADDLE-TALL "solid" "black"))
; Faller Images
(define FALLER-IMAGE (star (/ FALLER-SIZE 2) "solid" "yellow"))
(define FALLER-WILL-TOUCH-IMAGE (star (/ FALLER-SIZE 2) "solid" "blue"))
(define FALLER-SHRINK-IMAGE (star (/ FALLER-SIZE 2) "solid" "red"))
(define FALLER-SHRINK-WILL-TOUCH-IMAGE (star (/ FALLER-SIZE 2) "solid" "black"))
(define FALLER-GROW-IMAGE (star (/ FALLER-SIZE 2) "solid" "green"))
(define FALLER-GROW-WILL-TOUCH-IMAGE (star (/ FALLER-SIZE 2) "solid" "orange"))

(define BACKGROUD-IMAGE (rectangle WORLD-WIDTH WORLD-HEIGHT "solid" "white"))
(define LEFT-SIDE 25)
(define RIGHT-SIDE 175)

;
;
;                                              ;
;    ;;;;;;                           ;        ;
;    ;                                ;
;    ;      ;    ;  ; ;;;     ;;;   ;;;;;;   ;;;     ;;;;   ; ;;;    ;;;;
;    ;      ;    ;  ;;   ;   ;   ;    ;        ;    ;;  ;;  ;;   ;  ;    ;
;    ;;;;;  ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;
;    ;      ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;  ;;;
;    ;      ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;     ;;;
;    ;      ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;       ;
;    ;      ;   ;;  ;    ;   ;   ;    ;        ;    ;;  ;;  ;    ;  ;    ;
;    ;       ;;; ;  ;    ;    ;;;      ;;;  ;;;;;;;  ;;;;   ;    ;   ;;;;
;
;
;
;

#|
There are three separate pieces of the game to be
implemented: rendering the world to an image to be
shown on the screen, handling user inputs, and
adjusting the world as time passes.
Here are the sigantures of the three functions:
  draw : Faller-world -> Scene
  key : Faller-world Key-event -> Faller-world
  tick : Faller-world -> Faller-world
`draw` and `key` are fairly well-described by the
description above, but `tick` needs a little more
explanation. Conceptually, it performs several
different tasks:
 1. Move all the fallers down the screen by one
pixel.
 2. If a faller touches the bottom of the screen,
remove it from the world; if it overlaps with the
paddle, also increment the score.
 3. Possibly add a new faller to the list,
starting from the top of the screen.
 4. Move the paddle.
Be sure to compose several different functions to
accomplish these tasks, and not just one
monolithic function that does it all! (And think
carefully about how you might break up the other
two main functions, too.)
Don't forget: the coordinate system has the
origin in the upper-left, with `x` coordinates
increasing rightward and `y` coordinates
increasing *downward*.
(fw-score tw)
|#

; draw: Faller-world -> Image
; to show the images on screen(fallers and the paddle)
; examples:
(check-expect (draw (make-fw 175 "right" '() 10))
              (place-image
               (text "10" 20 "black")
               25 25
               (place-image
                PADDLE-IMAGE
                175 294
                BACKGROUD-IMAGE)))
; Strategy: structure decomposition
(define (draw tw)
  (place-image
   (text (number->string (fw-score tw)) 20 "black")
   25 25
   (place-image
    PADDLE-IMAGE
    (fw-paddle tw) (- WORLD-HEIGHT (/ PADDLE-TALL 2))
    (draw-faller (fw-fallers tw) tw))))


; draw-faller: [a List-of-Posn] -> Image
; a function to draw fallers on background
; examples:
#|
(check-expect (draw-faller (list (make-posn 80 80))
                           (make-fw 175 "right" '() 10))
              (place-image
               FALLER-IMAGE
               80 80
               BACKGROUD-IMAGE))
|#
; Strategy: structure decomposition
(define (draw-faller faller tw)
  (cond
    [(empty? faller) BACKGROUD-IMAGE]
    [else
     (if (will-touch? (fw-paddle tw) (posn-x (faller-position (first faller))))
         (place-image
          FALLER-WILL-TOUCH-IMAGE
          (posn-x (faller-position (first faller)))
          (posn-y (faller-position (first faller)))
          (draw-faller (rest faller) tw))  
         (place-image
          FALLER-IMAGE
          (posn-x (faller-position (first faller)))
          (posn-y (faller-position (first faller)))
          (draw-faller (rest faller) tw)))]))

; will-touch?: Num Num -> Boolean
; Checks the x position of paddle and x position of the faller
; if faller is above paddle, return true, else false
; Strategy: Function Composition
(define (will-touch? paddle-x faller-x)
  (<= (abs (- paddle-x faller-x)) (/ PADDLE-WIDE 2)))



; key: Faller-world KeyEvent -> Faller-world
; change direction and minus points when you tap the screen
; examples:
(check-expect (key (make-fw 100 "right" '() 10) "left")
              (make-fw 100 "left" '() 9))
(check-expect (key (make-fw 100 "left" '() 10) "right")
              (make-fw 100 "right" '() 9))
(check-expect (key (make-fw 100 "right" '() 10) "right")
              (make-fw 100 "right" '() 10))
(check-expect (key (make-fw 100 "left" '() 10) "left")
              (make-fw 100 "left" '() 10))
; Strategy: structure decomposition
(define (key tw cur-key)
  (cond
    [(key=? cur-key (fw-direction tw)) tw]
    [(key=? cur-key "left")
     (make-fw
      (fw-paddle tw)
      "left"
      (fw-fallers tw)
      (if (>= 0 (fw-score tw)) 0 (- (fw-score tw) 1)))]
    [(key=? cur-key "right")
     (make-fw
      (fw-paddle tw)
      "right"
      (fw-fallers tw)
      (if (>= 0 (fw-score tw)) 0 (- (fw-score tw) 1)))]
    [else tw]))

; tick: Faller-world -> Faller-world
; update the status after every tick
; examples:
; (check-expect (tick (make-fw 28 "left" '() 10)) (make-fw 27 "left" '() 10)) 
(define (tick tw)
  (make-fw
   (paddle-pos tw)
   (paddle-direct tw)
   (fallers-down (maybe-add-faller (fw-fallers tw)) (fw-paddle tw))
   (point-count tw)))



; paddle-direct: Faller-world -> Direction
; Change the direction if paddle reach the left edge or the right edge
; examples:
(check-expect (paddle-direct (make-fw 175 "right" '() 10)) "left")
(check-expect (paddle-direct (make-fw 25 "left" '() 10)) "right")
(check-expect (paddle-direct (make-fw 100 "right" '() 10)) "right")
; Strategy: structure decomposition
(define (paddle-direct tw)
  (cond
    [( equal? LEFT-SIDE (fw-paddle tw)) "right"]
    [( equal? RIGHT-SIDE (fw-paddle tw)) "left"]
    [else (fw-direction tw)]))

; paddle-pos: Faller-world -> Number
; Move the paddle
; examples:
(check-expect (paddle-pos (make-fw 100 "right" '() 10)) 101)
(check-expect (paddle-pos (make-fw 100 "left" '() 10)) 99)
; Strategy: structure decomposition
(define (paddle-pos tw)
  (cond
    [ (equal? (fw-direction tw) "left")
      (- (fw-paddle tw) 1)]
    [ (equal? (fw-direction tw) "right")
      (+ (fw-paddle tw) 1)]))


; fallers-down: [a List-of-Posn] number -> [a list of posn]
; Move all the fallers down the screen by one pixel and remove it from the world
; if fallers touch the botton or overlap with paddle
; examples:
#|
(check-expect (fallers-down (list (make-posn 100 100)(make-posn 80 80)) 45)
              (list (make-posn 100 101)(make-posn 80 81)))
(check-expect (fallers-down (list (make-posn 100 300)(make-posn 80 80)) 45)
              (list (make-posn 80 81)))
(check-expect (fallers-down (list (make-posn 45 298)(make-posn 80 80)) 45)
              (list (make-posn 80 81)))
|#
; Strategy: structure decomposition
(define (fallers-down items paddle)
  (cond
    [ (empty? items) '() ]
    [else (if (or (faller-hit-bottom? (faller-position (first items)) paddle)
                  (faller-hit-paddle? (faller-position (first items)) paddle))
              (fallers-down(rest items) paddle)
              (cons (make-faller (faller-type (first items)) (make-posn
                     (posn-x (faller-position (first items)))
                     (+ (posn-y (faller-position (first items))) 1)))
                    (fallers-down(rest items) paddle)))]))


; point-count Faller-world -> number
; update scores
; examples:
#|
(check-expect (point-count
               (make-fw
                100
                "right"
                (list (make-posn 100 295)(make-posn 80 81))
                10)) 20)
(check-expect (point-count
               (make-fw
                100
                "right"
                (list (make-posn 100 295)(make-posn 160 300))
                10)) 19)
|#
(define (point-count tw)
  (less-zero? (- (+ (fw-score tw)
                    (* 10 (count-hitting-fallers (fw-fallers tw) (fw-paddle tw))))
                 (count-hitting-bottom (fw-fallers tw) (fw-paddle tw)))))

; less-zero?: number -> number
; judge score is less than zero
;examples:
(check-expect (less-zero? 1) 1)
(check-expect (less-zero? -1) 0)
(define (less-zero? score)
  (if (> score 0) score 0))


; count-hitting-fallers: [a List-of-Posn] number -> number
; count how many fallers hit paddle
; examples:
;(check-expect
; (count-hitting-fallers (list (make-posn 27 300) (make-posn 60 300)) 75) 1)
; Strategy: structure decomposition
(define (count-hitting-fallers fallers paddle-x)
  (cond
    [(empty? fallers) 0]
    [else (if (faller-hit-paddle? (faller-position (first fallers)) paddle-x)
              (+ 1 (count-hitting-fallers (rest fallers) paddle-x))
              (count-hitting-fallers (rest fallers) paddle-x))]))

; faller-hit-paddle? Posn Number -> Boolean
; judge whether faller hit paddle
; example
#|
(check-expect (faller-hit-paddle? (make-posn 60 298) 85) #true)
(check-expect (faller-hit-paddle? (make-posn 60 298) 94) #true)
(check-expect (faller-hit-paddle? (make-posn 80 298) 47) #true)
(check-expect (faller-hit-paddle? (make-posn 80 298) 140) #false)
|#
(define (faller-hit-paddle? faller paddle-x)
  (and
   (< (- paddle-x (/ PADDLE-WIDE 2))
      (+ (posn-x faller) (/ FALLER-SIZE 2)))
   (< (- (posn-x faller) (/ FALLER-SIZE 2))
      (+ paddle-x (/ PADDLE-WIDE 2)))
   (< (- WORLD-HEIGHT PADDLE-TALL (/ FALLER-SIZE 2)) (posn-y faller))))

; count-hitting-bottom [a List-of-Posn] number -> number
; count how many fallers hit bottom
; examples:
;(check-expect
; (count-hitting-bottom (list (make-posn 10 300) (make-posn 50 300)) 60) 1)
; Strategy: structure decomposition
(define (count-hitting-bottom fallers paddle-x)
  (cond
    [(empty? fallers) 0]
    [else (if (faller-hit-bottom? (faller-position (first fallers)) paddle-x)
              (+ 1 (count-hitting-bottom (rest fallers) paddle-x))
              (count-hitting-bottom (rest fallers) paddle-x))]))

; faller-hit-bottom? Posn Number -> Boolean
; judge whether faller hit bottom
; examples:
;(check-expect (faller-hit-bottom? (make-posn 60 298) 85) #false)
; (check-expect (faller-hit-bottom? (make-posn 60 300) 110) #true)
(define (faller-hit-bottom? faller paddle-x)
  (and (equal? WORLD-HEIGHT (posn-y faller))
       (not (faller-hit-paddle? faller paddle-x))))


;
;
;                    ;;;
;   ;    ;             ;
;   ;    ;             ;
;   ;    ;   ;;;;      ;    ; ;;;
;   ;    ;   ;  ;;     ;    ;;  ;
;   ;;;;;;  ;    ;     ;    ;    ;
;   ;    ;  ;;;;;;     ;    ;    ;
;   ;    ;  ;          ;    ;    ;
;   ;    ;  ;          ;    ;    ;
;   ;    ;   ;         ;    ;;  ;
;   ;    ;   ;;;;;      ;;; ; ;;;
;                           ;
;                           ;
;                           ;
;

;; In your `tick` function, you need to
;; *sometimes* add a faller to the list of
;; fallers. Use a function like `maybe-add-faller`
;; (below) to (randomly) add a faller to the
;; current list. You may wish to adjust it based
;; on gameplay factors or the way you interpret
;; `Posn`s as fallers. Note that because of the
;; randomness, this function is difficult to test
;; using `check-expect`, so the test example given
;; below just checks the length of the resulting
;; list.

; maybe-add-faller : List-of-Posn -> List-of-Posn
; Adds a random faller with probabilty
; `1/INV-P-FALLERS`, but only if there are fewer than `MAX-FALLERS`
; fallers aleady.
;
; Example:
(check-expect
 (<= 4
     (length
      (maybe-add-faller
       (list (make-posn 0 0)
             (make-posn 1 1)
             (make-posn 2 2)
             (make-posn 3 3))))
     5)
 #true)

; Strategy: decision tree
#;
(define (maybe-add-faller fallers)
  (cond
    [(< (length fallers) MAX-FALLERS)
     (cond
       [(zero? (random INV-P-FALLER))
        (cons (make-posn (random WORLD-WIDTH) 0)
              fallers)]
       [else fallers])]
    [else fallers]))


; maybe-add-faller : List-of-Faller -> List-of-Faller
; Adds a random faller with probabilty
; `1/INV-P-FALLERS`, but only if there are fewer than `MAX-FALLERS`
; fallers aleady.
; TODO : Adjust for Shrink and Grow fallers
(define (maybe-add-faller fallers)
  (cond
    [(< (length fallers) MAX-FALLERS)
     (cond
       [(zero? (random INV-P-FALLER))
        (cons (make-faller "Normal"
                           (make-posn (random WORLD-WIDTH) 0))
              fallers)]
       [else fallers])]
    [else fallers]))


;; You'll use this `start` function to start your
;; faller game once you’ve completed designing the
;; three main handler functions that it depends
;; on.

; start : Any -> Faller-world
; Starts the faller game. (Argument is ignored.)
;
; Example:
;  - (start 0)

(define (start _dummy)
  (big-bang (make-fw (/ WORLD-WIDTH 2) "right" '() 0)
    [on-tick tick 1/200]
    [on-key  key]
    [to-draw draw]))

(start 0)