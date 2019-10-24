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
;   (make-faller Type Posn Natural)
; interp.: if `a-faller` is a Faller then all of:
; - (faller-type a-faller) is a Type
; - (faller-position a-faller) is a Posn of the Faller
(define-struct faller [type position])

; A Type is one of:
; - "Shrink"
; - "Grow"
; - "Normal"

; A Paddle is:
;   (make-paddle Number Number)
; - interp.: if `a-paddle` is a Paddle then all of:
; - (paddle-xposition a-paddle) is the x coordinate of the paddle
; - (paddle-width a-paddle) is the width of the paddle
(define-struct paddle (xposition width))

; A Faller-world is
;   (make-fw Number Direction List-of-Faller Natural Natural)
; interp.: if `a-fw` is a Faller-world then all of:
; - (fw-paddle a-fw) is the x coordinate of the paddle,
; - (fw-direction a-fw) gives which direction the paddle is moving,
; - (fw-fallers a-fw) is a list of Faller, and
; - (fw-score a-fw) is the score.
; - (fw-faller-count a-fw) is the number of fallers that have spawned
(define-struct fw (paddle direction fallers score faller-count))

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
; To show the images on screen (fallers and the paddle)
(check-expect (draw (make-fw (make-paddle 175 50) "right" '() 10 1))
              (place-image
               (text "10" 20 "black")
               25 25
               (place-image
                PADDLE-IMAGE
                175 294
                BACKGROUD-IMAGE)))
; Strategy: Structural Decomposition
(define (draw tw)
  (place-image
   (text (number->string (fw-score tw)) 20 "black")
   25 25
   (place-image
    (rectangle (paddle-width (fw-paddle tw)) PADDLE-TALL "solid" "black") 
    (paddle-xposition (fw-paddle tw)) (- WORLD-HEIGHT (/ PADDLE-TALL 2))
    (draw-faller (fw-fallers tw) tw))))

; draw-faller: List-of-Faller Faller-world -> Image
; Draw fallers on background
(check-expect (draw-faller (list (make-faller "Normal" (make-posn 80 80)))
                           (make-fw (make-paddle 175 50)
                                    "right" '() 10 1))
              (place-image
               FALLER-IMAGE
               80 80
               BACKGROUD-IMAGE))
(check-expect (draw-faller (list (make-faller "Grow" (make-posn 80 80)))
                           (make-fw (make-paddle 175 50)
                                    "right" '() 10 1))
              (place-image
               FALLER-GROW-IMAGE
               80 80
               BACKGROUD-IMAGE))
(check-expect (draw-faller (list (make-faller "Shrink" (make-posn 80 80)))
                           (make-fw (make-paddle 175 50)
                                    "right" '() 10 1))
              (place-image
               FALLER-SHRINK-IMAGE
               80 80
               BACKGROUD-IMAGE))
(check-expect (draw-faller (list (make-faller "Normal" (make-posn 80 80)))
                           (make-fw (make-paddle 80 50)
                                    "right" '() 10 1))
              (place-image
               FALLER-WILL-TOUCH-IMAGE
               80 80
               BACKGROUD-IMAGE))
(check-expect (draw-faller (list (make-faller "Grow" (make-posn 80 80)))
                           (make-fw (make-paddle 80 50)
                                    "right" '() 10 1))
              (place-image
               FALLER-GROW-WILL-TOUCH-IMAGE
               80 80
               BACKGROUD-IMAGE))
(check-expect (draw-faller (list (make-faller "Shrink" (make-posn 80 80)))
                           (make-fw (make-paddle 80 50)
                                    "right" '() 10 1))
              (place-image
               FALLER-SHRINK-WILL-TOUCH-IMAGE
               80 80
               BACKGROUD-IMAGE))
; Strategy: Structural Decomposition
(define (draw-faller faller tw)
  (cond
    [(empty? faller) BACKGROUD-IMAGE]
    [else
     (if (will-touch? (fw-paddle tw)
                      (posn-x (faller-position (first faller))))
         (place-image
          (draw-faller-helper-will-touch (faller-type (first faller)))
          (posn-x (faller-position (first faller)))
          (posn-y (faller-position (first faller)))
          (draw-faller (rest faller) tw))  
         (place-image
          (draw-faller-helper (faller-type (first faller)))
          (posn-x (faller-position (first faller)))
          (posn-y (faller-position (first faller)))
          (draw-faller (rest faller) tw)))]))

; draw-faller-helper: Type -> Image
; Passes the type of the faller and returns the associated faller image
(check-expect (draw-faller-helper "Grow") FALLER-GROW-IMAGE)
(check-expect (draw-faller-helper "Shrink") FALLER-SHRINK-IMAGE)
(check-expect (draw-faller-helper "Normal") FALLER-IMAGE)
; Strategy: Function Composition
(define (draw-faller-helper type)
  (cond
    [(string=? type "Grow") FALLER-GROW-IMAGE]
    [(string=? type "Shrink") FALLER-SHRINK-IMAGE]
    [else FALLER-IMAGE]))

; draw-faller-helper-will-touch: Type -> Image
; Passes the type of the faller and returns the associated faller image
; for a faller that's directly above the paddle
(check-expect (draw-faller-helper-will-touch "Grow")
              FALLER-GROW-WILL-TOUCH-IMAGE)
(check-expect (draw-faller-helper-will-touch "Shrink")
              FALLER-SHRINK-WILL-TOUCH-IMAGE)
(check-expect (draw-faller-helper-will-touch "Normal")
              FALLER-WILL-TOUCH-IMAGE)
; Strategy: Function Composition
(define (draw-faller-helper-will-touch type)
  (cond
    [(string=? type "Grow") FALLER-GROW-WILL-TOUCH-IMAGE]
    [(string=? type "Shrink") FALLER-SHRINK-WILL-TOUCH-IMAGE]
    [else FALLER-WILL-TOUCH-IMAGE]))

; will-touch?: Paddle Number -> Boolean
; Checks the x position of paddle and x position of the faller
; if faller is above paddle, return true, else false
(check-expect (will-touch? (make-paddle 50 50) 50) #true)
(check-expect (will-touch? (make-paddle 50 50) 80) #false)
; Strategy: Function Composition
(define (will-touch? paddle faller-x)
  (<= (abs (- (paddle-xposition paddle) faller-x))
      (/ (paddle-width paddle) 2)))

; key: Faller-world KeyEvent -> Faller-world
; Change direction and minus points when you tap the screen
(check-expect (key (make-fw 100 "right" '() 10 1) "left")
              (make-fw 100 "left" '() 9 1))
(check-expect (key (make-fw 100 "left" '() 10 1) "right")
              (make-fw 100 "right" '() 9 1))
(check-expect (key (make-fw 100 "right" '() 10 1) "right")
              (make-fw 100 "right" '() 10 1))
(check-expect (key (make-fw 100 "left" '() 10 1) "left")
              (make-fw 100 "left" '() 10 1))
; Strategy: Structural Decomposition
(define (key tw cur-key)
  (cond
    [(key=? cur-key (fw-direction tw)) tw]
    [(key=? cur-key "left")
     (make-fw
      (fw-paddle tw)
      "left"
      (fw-fallers tw)
      (if (>= 0 (fw-score tw)) 0 (- (fw-score tw) 1))
      (fw-faller-count tw))]
    [(key=? cur-key "right")
     (make-fw
      (fw-paddle tw)
      "right"
      (fw-fallers tw)
      (if (>= 0 (fw-score tw)) 0 (- (fw-score tw) 1))
      (fw-faller-count tw))]
    [else tw]))

; tick: Faller-world -> Faller-world
; Update the status after every tick
(check-expect (tick (make-fw (make-paddle 28 50) "left" '() 10 1))
              (make-fw (make-paddle 27 50) "left" '() 10 1))
(check-expect (tick (make-fw (make-paddle 28 50) "left"
                             (list (make-faller "Grow" (make-posn 28 298)))
                             10 1))
              (make-fw (make-paddle 27 60) "left" '() 20 1))
; Strategy: Structural Decomposition
(define (tick tw)
  (cond
    [(add-faller? tw)
     (make-fw
      (super-paddle tw)
      (paddle-direct tw)
      (fallers-down (add-faller (fw-fallers tw)
                                (fw-faller-count tw)) (fw-paddle tw))
      (point-count tw)
      (add-faller-count tw))]
    [else
     (make-fw
      (super-paddle tw)
      (paddle-direct tw)
      (fallers-down (fw-fallers tw) (fw-paddle tw))
      (point-count tw)
      (fw-faller-count tw))]))

; add-faller-count: Faller-world -> Number
; Reverts faller-count to 1 when at 25, else adds 1
(check-expect (add-faller-count (make-fw (make-paddle 175 50)
                                         "right" '() 10 25)) 1)
(check-expect (add-faller-count (make-fw (make-paddle 175 50)
                                         "right" '() 10 24)) 25)
; Strategy: Function Composition
(define (add-faller-count tw)
  (cond
    [(= (fw-faller-count tw) 25) 1]
    [else (add1 (fw-faller-count tw))]))

; paddle-direct: Faller-world -> Direction
; Change the direction if paddle reach the left edge or the right edge
(check-expect (paddle-direct
               (make-fw (make-paddle 175 50) "right" '() 10 1)) "left")
(check-expect (paddle-direct
               (make-fw (make-paddle 25 50) "left" '() 10 1)) "right")
(check-expect (paddle-direct
               (make-fw (make-paddle 100 50) "right" '() 10 1)) "right")
(check-expect (paddle-direct
               (make-fw (make-paddle 150 100) "right" '() 10 1)) "left")
; Strategy: Structural Decomposition + Function Composition
(define (paddle-direct tw)
  (cond
    [(>= (/ (paddle-width (fw-paddle tw)) 2)
         (paddle-xposition (fw-paddle tw))) "right"]
    [(<= (- WORLD-WIDTH (/ (paddle-width (fw-paddle tw)) 2))
         (paddle-xposition (fw-paddle tw))) "left"]
    [else (fw-direction tw)]))

; paddle-pos: Direction Number -> Number
; Takes in a direction and paddle-xposition and returns a number
; for the x-position of the paddle
(check-expect (paddle-pos "left" 100) 99)
(check-expect (paddle-pos "right" 100) 101)
; Strategy: Function Composition
(define (paddle-pos direction paddle-x)
  (cond
    [ (equal? direction "left")
      (- paddle-x 1)] 
    [ (equal? direction "right")
      (add1 paddle-x)]))

; super-paddle: Faller-world -> Paddle
; Create the paddle for the world
; using grow-paddle-helper and paddle-pos
(check-expect (super-paddle (make-fw (make-paddle
                      (/ WORLD-WIDTH 2) PADDLE-WIDE)
                       "right" (list (make-faller "Grow" (make-posn 100 298)))
                                     0 1))
              (make-paddle 101 60))
; Structural Decomposition + Function Composition
(define (super-paddle tw)
  (make-paddle (paddle-pos (fw-direction tw) (paddle-xposition (fw-paddle tw)))
               (grow-paddle-helper (fw-fallers tw)
                                   (paddle-xposition (fw-paddle tw))
                                   (paddle-width (fw-paddle tw)))))


; grow-paddle-helper: List-of-Faller Number Number -> Number
; Determines the new width of the paddle
(check-expect (grow-paddle-helper (list (make-faller "Normal"
                                                     (make-posn 50 298))
                                        (make-faller "Grow"
                                                     (make-posn 50 298))
                                        (make-faller "Shrink"
                                                     (make-posn 50 298)))
                                  50 50) 40)
; Strategy: Structural Decomposition + Function Composition
(define (grow-paddle-helper fallers paddle-x paddle-width)
  (cond
    [(empty? fallers) paddle-width]
    [else (if (and (faller-hit-paddle? (faller-position (first fallers))
                                       paddle-x paddle-width)
                   (enable-grow-or-shrink? (faller-type (first fallers))))
              (grow-paddle-helper (rest fallers) paddle-x
                                  (grow-or-shrink?
                                   (faller-type (first fallers))
                                   paddle-width))
              (grow-paddle-helper (rest fallers) paddle-x
                                  paddle-width))]))

; enable-grow-or-shrink?: Type -> Boolean
; If it's a grow or shrink faller, return true
(check-expect (enable-grow-or-shrink? "Grow") #true)
(check-expect (enable-grow-or-shrink? "Shrink") #true)
(check-expect (enable-grow-or-shrink? "Normal") #false)
; Strategy: Function Composition
(define (enable-grow-or-shrink? type)
  (or (string=? type "Grow") (string=? type "Shrink")))

; grow-or-shrink?: Boolean -> Number
; If it's a grow, it adds 10
; If it's a shrink, it deducts 20
(check-expect (grow-or-shrink? "Grow" 50) 60)
(check-expect (grow-or-shrink? "Shrink" 50) 30)
(check-expect (grow-or-shrink? "Normal" 50) 50)
(define (grow-or-shrink? type paddle-width)
  (cond
    [(string=? type "Grow") (+ paddle-width 10)]
    [(string=? type "Shrink") (- paddle-width 20)]
    [else paddle-width]))

; fallers-down: List-of-Faller Paddle -> List-of-Faller
; Move all the fallers down the screen by one pixel
; and remove it from the world
; if fallers touch the botton or overlap with paddle
(check-expect (fallers-down (list (make-faller "Normal" (make-posn 100 100))
                                  (make-faller "Normal" (make-posn 80 80)))
                            (make-paddle 45 50))
              (list (make-faller "Normal" (make-posn 100 101))
                    (make-faller "Normal" (make-posn 80 81))))
(check-expect (fallers-down (list (make-faller "Normal" (make-posn 100 300))
                                  (make-faller "Normal" (make-posn 80 80)))
                            (make-paddle 45 50))
              (list (make-faller "Normal" (make-posn 80 81))))
(check-expect (fallers-down (list (make-faller "Normal" (make-posn 45 298))
                                  (make-faller "Normal" (make-posn 80 80)))
                            (make-paddle 45 50))
              (list (make-faller "Normal" (make-posn 80 81))))
; Strategy: Structural Decomposition
(define (fallers-down items paddle)
  (cond
    [ (empty? items) '() ]
    [else (if (or (faller-hit-bottom? (faller-position (first items)) paddle)
                  (faller-hit-paddle? (faller-position (first items))
                                      (paddle-xposition paddle)
                                      (paddle-width paddle)))
              (fallers-down (rest items) paddle)
              (cons (make-faller
                     (faller-type (first items)) (make-posn
                                                  (posn-x (faller-position
                                                           (first items)))
                                                  (+ (posn-y (faller-position
                                                              (first items)))
                                                     1)))
                    (fallers-down(rest items) paddle)))]))

; point-count: Faller-world -> Number
; Update scores
(check-expect (point-count
               (make-fw
                (make-paddle 100 50)
                "right"
                (list (make-faller "Normal" (make-posn 100 295))
                      (make-faller "Normal" (make-posn 80 81)))
                10
                3)) 20)
(check-expect (point-count
               (make-fw
                (make-paddle 100 50)
                "right"
                (list (make-faller "Normal" (make-posn 100 295))
                      (make-faller "Normal" (make-posn 160 300)))
                10
                1)) 19)
; Strategy: Structural Decomposition
(define (point-count tw)
  (less-zero? (- (+ (fw-score tw)
                    (* 10 (count-hitting-fallers (fw-fallers tw)
                                                 (fw-paddle tw))))
                 (count-hitting-bottom (fw-fallers tw) (fw-paddle tw)))))

; less-zero?: Number -> Number
; Judge score is less than zero
(check-expect (less-zero? 1) 1)
(check-expect (less-zero? -1) 0)
; Strategy: Function Composition
(define (less-zero? score)
  (if (> score 0) score 0))

; count-hitting-fallers: List-of-Faller Paddle -> Number
; Count how many fallers hit paddle
(check-expect
 (count-hitting-fallers (list (make-faller "Normal" (make-posn 27 300))
                              (make-faller "Normal" (make-posn 60 300)))
                        (make-paddle 75 50)) 1)
; Strategy: Structural Decomposition
(define (count-hitting-fallers fallers paddle)
  (cond
    [(empty? fallers) 0]
    [else (if (faller-hit-paddle? (faller-position (first fallers))
                                  (paddle-xposition paddle)
                                  (paddle-width paddle))
              (+ 1 (count-hitting-fallers (rest fallers)
                                          paddle))
              (count-hitting-fallers (rest fallers)
                                     paddle))]))

; faller-hit-paddle? Posn Number Number -> Boolean
; Judge whether faller hit paddle
(check-expect (faller-hit-paddle? (make-posn 60 298)
                                  85 50) #true)
(check-expect (faller-hit-paddle? (make-posn 60 298)
                                  94 50) #true)
(check-expect (faller-hit-paddle? (make-posn 80 298)
                                  47 50) #true)
(check-expect (faller-hit-paddle? (make-posn 80 298)
                                  140 50) #false)
(check-expect (faller-hit-paddle? (make-posn 80 298)
                                  130 100) #true)
(check-expect (faller-hit-paddle? (make-posn 180 298)
                                  130 100) #true)
(check-expect (faller-hit-paddle? (make-posn 190 298)
                                  130 100) #false)
; Strategy: Function Composition
(define (faller-hit-paddle? faller paddle-x paddle-width)
  (and
   (< (- paddle-x (/ paddle-width 2))
      (+ (posn-x faller) (/ FALLER-SIZE 2)))
   (< (- (posn-x faller) (/ FALLER-SIZE 2))
      (+ paddle-x (/ paddle-width 2)))
   (< (- WORLD-HEIGHT PADDLE-TALL
         (/ FALLER-SIZE 2)) (posn-y faller))))

; count-hitting-bottom: List-of-Faller Paddle -> Number
; Count how many fallers hit bottom
(check-expect (count-hitting-bottom '() (make-paddle 60 50)) 0)
(check-expect
 (count-hitting-bottom (list (make-faller "Normal" (make-posn 10 300))
                             (make-faller "Normal" (make-posn 50 300)))
                       (make-paddle 60 50)) 1)
; Strategy: Structural Decomposition
(define (count-hitting-bottom fallers paddle)
  (cond
    [(empty? fallers) 0]
    [else (if (faller-hit-bottom? (faller-position (first fallers)) paddle)
              (+ 1 (count-hitting-bottom (rest fallers) paddle))
              (count-hitting-bottom (rest fallers) paddle))]))

; faller-hit-bottom? Posn Paddle -> Boolean
; Judge whether faller hit bottom
(check-expect (faller-hit-bottom? (make-posn 60 298)
                                  (make-paddle 85 60)) #false)
(check-expect (faller-hit-bottom? (make-posn 60 300)
                                  (make-paddle 110 80)) #true)
; Strategy: Function Composition
(define (faller-hit-bottom? faller paddle)
  (and (equal? WORLD-HEIGHT (posn-y faller))
       (not (faller-hit-paddle? faller (paddle-xposition paddle)
                                (paddle-width paddle)))))


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


; add-faller: Fallers Number -> List-of-Fallers
; Takes in a list of fallers and creates a new faller based
; on the faller count
(check-expect (length (add-faller (list (make-faller "Normal"
                                                     (make-posn 50 0))) 1)) 2)
; Strategy: Structural Decomposition + Function Composition
(define (add-faller fallers faller-count)
  (cons (make-faller (find-faller-type faller-count)
                     (make-posn (random WORLD-WIDTH) 0)) fallers))

; add-faller?: Faller-world -> Boolean
; Adds a random faller with probabilty
; `1/INV-P-FALLERS`, but only if there are fewer than `MAX-FALLERS`
; fallers aleady.
(check-expect (add-faller? (make-fw (make-paddle
                      (/ WORLD-WIDTH 2) PADDLE-WIDE) "right" '() 0 1)) #false)
; Strategy: Structural Decomposition + Function Composition
(define (add-faller? tw)
  (and (< (length (fw-fallers tw)) MAX-FALLERS)(zero? (random INV-P-FALLER))))

; find-faller-type: Faller -> String
; Finds the type of the faller
(check-expect (find-faller-type 1) "Normal")
(check-expect (find-faller-type 3) "Normal")
(check-expect (find-faller-type 6) "Grow")
(check-expect (find-faller-type 12) "Grow")
(check-expect (find-faller-type 25) "Shrink")
(define (find-faller-type faller-count)
  (cond
    [(= 0 (modulo faller-count 6)) "Grow"]
    [(= 0 (modulo faller-count 25)) "Shrink"]
    [else "Normal"]))

(define (start _dummy)
  (big-bang (make-fw (make-paddle
                      (/ WORLD-WIDTH 2) PADDLE-WIDE) "right" '() 0 1)
    [on-tick tick 1/200]
    [on-key  key]
    [to-draw draw]))

; (start 0)