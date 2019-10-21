;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname falling-extensions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|

The goal of this assignment is to extend your falling
game.

In addition to the extensions listed below, find all
opportunities to abstract the functions that you have
already written using map, filter, and other such
higher-order functions.

1) make objects that would be touching the paddle, if
   they were at the bottom of the screen, look different.
   That is, help the player to understand the extent of the
   paddle with a subtly different faller image. (This
   applies to the new types of fallers you add for the
   second part of the homework too.)

2) make a new type of faller, such that, when it touches
   the paddle, the paddle gets wider and another such that,
   when it touches the paddle, the paddle gets narrower.
   These fallers should appear at regular intervals (not
   randomly) in your game. For example, every 10th faller
   could be a shrinking faller, say.

In order to avoid being overwhelmed by the changes to
the game, first be sure you have a good test suite for
your existing game. Second, pick only one small aspect
of the above bullets to work on first. Break the changes
up into two phases: a refactoring phase and then a
semantics-breaking change. That is, first change the
program in such a way that the game plays the same as it
played before but that prepares you for the new game's
behavior. Once that change is implemented and tested,
then change the behavior in a second (usually much
easier) step. At each stage, make sure you have a
complete set of passing tests and, even better, check
each change in to git so you can exploit the 'git diff'
command.

Note that changing the data definition of Fw-World is a
good idea. Think carefully about the changes, though,
and don't be afraid to change your mind about what the
best changes are.

Note that your final submission should be consistent,
(i.e., make sure your signatures, templates, tests,
bodies, etc etc are all matched up as if they had been
written from scratch with the final data definitions you
end up using).

|#

; Delete this when you have real code instead:
(check-expect (+ 1 1) 2)



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

; A Faller-world is
;   (make-fw Number Direction List-of-Posn Natural)
; interp.: if `a-fw` is a Faller-world then all of:
; - (fw-paddle a-fw) is the x coordinate of the paddle,
; - (fw-direction a-fw) gives which direction the paddle is moving,
; - (fw-fallers a-fw) is a list of the positions of the fallers, and
; - (fw-score a-fw) is the score.
(define-struct fw (paddle direction fallers score))

; A Direction is one of:
; - "left"
; - "right"

; A List-of-Posn is one of:
; - '()
; - (cons Posn List-of-Posn)

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
(define MAX-FALLERS 20)    ; maximum faller count
(define INV-P-FALLER 25)   ; inverse of per-tick probability of new faller
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))


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

(define FALLER-IMAGE (circle 10 "solid" "red"))
(define FALLER-WILL-TOUCH-IMAGE (circle 10 "solid" "blue")
(define PADDLE-LENGTH 50)
(define PADDLE-WIDTH 12)
(define PADDLE-IMAGE (rectangle PADDLE-LENGTH PADDLE-WIDTH "solid" "black")) 


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
|#


;; NOTE ON DRAWING PADDLE:
;; (place-image PADDLE-IMAGE fw-paddle PADDLE-WIDTH BACKGROUND)
;; This will place the paddle image at the correct position


;A Point is:
; - (make-posn real real)

;A Fallers
; -'()
; - (cons point fallers)



;falling-update: Fallers Number -> Fallers
; Returns a list of point by deleting the points when they get caught
; by the paddle or touch the bottom of the scene
; NUM IS SCORE
; Strategy: function composition 
(define (falling-update fallers num)
  (cond
    [(empty? fallers) '()]
    [else
     (if (or (touch-helper (first fallers) num) (reduce-helper (first fallers)))
         (falling-update (rest fallers) num)
         (cons (make-posn (posn-x (first fallers))
                          (+ 1 (posn-y (first fallers))))
                  (falling-update (rest fallers) num)))]))

;tests:
(check-expect (falling-update(list (make-posn 50 50) (make-posn 60 60)) 50)
              (list (make-posn 50 51) (make-posn 60 61)))

(check-expect (falling-update (list (make-posn 50 (- WORLD-HEIGHT 10))
                                   (make-posn 50 60)) 50)
              (list (make-posn 50 61)))



;paddle-moving: Faller-world -> Number
;Returns the x of paddle
(define (paddle-moving fw-example)
  (cond
    [(string=? (fw-direction fw-example) "left") (- (fw-paddle fw-example) 1)]
    [(string=? (fw-direction fw-example) "right") (+ (fw-paddle fw-example) 1)]))

;tests:
(check-expect (paddle-moving (make-fw 100 "left" '() 0)) 99)
(check-expect (paddle-moving (make-fw 100 "right" '() 0)) 101)



;touch-helper: Point Number -> Boolean
;Returns that if the faller touchs the paddle
;;Strategy: function composition
(define (touch-helper faller paddle-x)
  (cond
    [(and (<= (abs (- paddle-x (posn-x faller))) (/ PADDLE-LENGTH 2))
          (>= (posn-y faller) (- WORLD-HEIGHT PADDLE-WIDTH)))
     #true]
    [else #false]))
;tests:
(check-expect (touch-helper (make-posn 50 50) 50) #false)
(check-expect (touch-helper (make-posn 50 (- WORLD-HEIGHT PADDLE-WIDTH)) 50)
              #true)
(check-expect (touch-helper (make-posn 25 (- WORLD-HEIGHT PADDLE-WIDTH)) 50)
              #true)
(check-expect (touch-helper (make-posn 24 (- WORLD-HEIGHT PADDLE-WIDTH)) 50)
              #false)
(check-expect (touch-helper (make-posn 75 (- WORLD-HEIGHT PADDLE-WIDTH)) 50)
              #true)
(check-expect (touch-helper (make-posn 76 (- WORLD-HEIGHT PADDLE-WIDTH)) 50)
              #false)
(check-expect (touch-helper (make-posn 50 (- WORLD-HEIGHT 9)) 50) #true)
(check-expect (touch-helper (make-posn 50 WORLD-HEIGHT) 50) #true)
(check-expect (touch-helper (make-posn 50 (+ WORLD-HEIGHT 5)) 50) #true)
(check-expect (touch-helper (make-posn 50 (- WORLD-HEIGHT 13)) 50) #false)


;reduce-helper: Posn -> Boolean
;Returns a boolean based on if the faller touches the bottom of the scene
;Strategy: function composition
(define (reduce-helper y)
  (cond
    [(<= (- WORLD-HEIGHT 10) (posn-y y)) #true]
    [else #false]))

;teste:
(check-expect (reduce-helper (make-posn 50 50)) #false)
(check-expect (reduce-helper (make-posn 50 (- WORLD-HEIGHT 15))) #false)
(check-expect (reduce-helper (make-posn 100 (- WORLD-HEIGHT 5))) #true)

;touch: Faller-world -> Faller-world
;Returns the fallerworld with the updated score by adding ten points everytime the paddle catches the faller
;Strategy: function composition
(define (touch fw-example)
  (local
    [(define len1 (length (fw-fallers fw-example)))
     (define len2 (length (falling-update (fw-fallers fw-example) (fw-paddle fw-example))))]    
    (cond
      [(empty? (fw-fallers fw-example)) fw-example]
      [else
       (if (> (- len1 len2) 0)
           (make-fw (fw-paddle fw-example) (fw-direction fw-example) (fw-fallers fw-example)
                    ( +(fw-score fw-example) (* 10 (- len1 len2))))
           fw-example)])))
;tests:
(check-expect (touch (make-fw 100 "left" '() 0)) (make-fw 100 "left" '() 0))
(check-expect (touch (make-fw 50 "left"
                              (list (make-posn 50 (- WORLD-HEIGHT PADDLE-WIDTH)) (make-posn 25 (- WORLD-HEIGHT PADDLE-WIDTH))) 0))
              (make-fw 50 "left" (list (make-posn 50 (- WORLD-HEIGHT PADDLE-WIDTH)) (make-posn 25 (- WORLD-HEIGHT PADDLE-WIDTH))) 20))
(check-expect (touch (make-fw 50 "left"
                              (list (make-posn 50 (- WORLD-HEIGHT PADDLE-WIDTH)) (make-posn 15 (- WORLD-HEIGHT PADDLE-WIDTH))) 0))
              (make-fw 50 "left" (list (make-posn 50 (- WORLD-HEIGHT PADDLE-WIDTH)) (make-posn 15 (- WORLD-HEIGHT PADDLE-WIDTH))) 10))
(check-expect (touch (make-fw 50 "left"
                              (list (make-posn 80 (- WORLD-HEIGHT PADDLE-WIDTH)) (make-posn 15 (- WORLD-HEIGHT PADDLE-WIDTH))) 0))
              (make-fw 50 "left" (list (make-posn 80 (- WORLD-HEIGHT PADDLE-WIDTH)) (make-posn 15 (- WORLD-HEIGHT PADDLE-WIDTH))) 0))



;reduce: Faller-world -> Number
;Returns the score by reducing one point everytime the faller touches the bottom of the scene
;Strategy: function composition
(define (reduce fw-example)
  (cond
    [(empty? (fw-fallers fw-example)) (fw-score fw-example)]
    [else
     (if (and (> (fw-score fw-example) 0) (reduce-helper (first (fw-fallers fw-example))))
         (- (fw-score fw-example) 1)
         (reduce (make-fw (fw-paddle fw-example) (fw-direction fw-example)
                          (rest (fw-fallers fw-example)) (fw-score fw-example))))]))
;tests
(check-expect (reduce (make-fw 100 "left" '() 0)) 0)
(check-expect (reduce (make-fw 50 "left"
                              (list (make-posn 50 (- WORLD-HEIGHT 5)) (make-posn 100 WORLD-HEIGHT)) 20)) 19)
(check-expect (reduce (make-fw 50 "left"
                              (list (make-posn 50  WORLD-HEIGHT) (make-posn 100 WORLD-HEIGHT)) 1)) 0)
              
;change-driection: Faller-world -> String
;Returns the direction the paddle will be next tick
(define (change-direction fw-example)
  (cond
    [(and (<= (fw-paddle fw-example) 25) (string=? (fw-direction fw-example) "left")) "right"]
    [(and (>= (fw-paddle fw-example) (- WORLD-WIDTH 25)) (string=? (fw-direction fw-example) "right")) "left"]
    [else (fw-direction fw-example)]))

;tests:
(check-expect (change-direction (make-fw 25 "left" '() 0)) "right")
(check-expect (change-direction (make-fw (- WORLD-WIDTH 25) "right" '() 0)) "left")
(check-expect (change-direction (make-fw 100 "right" '() 0)) "right")

;key : Faller-world Key-event -> Faller-world
;Builds a key event to control the paddle
;Strategy: function composition

(define (key fw-example a-key)
  (cond
    [(key=? a-key "left")
     (make-fw (paddle-moving (make-fw (fw-paddle fw-example) "left" (fw-fallers fw-example) (fw-score fw-example))) "left"
              (fw-fallers fw-example) (fw-score fw-example))]
    [(key=? a-key "right")
     (make-fw (paddle-moving (make-fw (fw-paddle fw-example) "right" (fw-fallers fw-example) (fw-score fw-example))) "right"
              (fw-fallers fw-example) (fw-score fw-example))]))

;tests   
(check-expect (key (make-fw 100 "left" '() 0) "left")
              (make-fw 99 "left" '() 0))
              
(check-expect (key (make-fw 100 "left" '() 0) "right")
              (make-fw 101 "right" '() 0))

;tick : Faller-world -> Faller-world
;Builds a tick to make a new faller-world every tick
(define (tick fw-example)
  (make-fw (paddle-moving fw-example)
           (change-direction fw-example)
           (falling-update (maybe-add-faller (fw-fallers fw-example)) (fw-paddle fw-example))
           (reduce(touch fw-example))))

;tests:
;It's really hard to test this one...


; draw-fallers: Fallers -> Image
; Draws all the fallers on top of the background
; Strategy: Function Composition
(define (draw-fallers fallers)
  (cond
    [(empty? fallers) BACKGROUND]
    [else (place-image FALLER-IMAGE (posn-x (first fallers))
                       (posn-y (first fallers)) (draw-fallers (rest fallers)))]))

;tests:
(check-expect (draw-fallers '() )
              BACKGROUND)

; draw-paddle: Faller-World -> Image
; Draws the paddle on top of the fallers scene
; Strategy: Function Composition
(define (draw-paddle fw-example)
  (place-image PADDLE-IMAGE (fw-paddle fw-example)
               (- WORLD-HEIGHT (/ PADDLE-WIDTH 2))
               (draw-fallers (fw-fallers fw-example))))

;tests:
(check-expect (draw-paddle (make-fw 100 "left" '() 0))
              (place-image PADDLE-IMAGE 100 (- WORLD-HEIGHT (/ PADDLE-WIDTH 2)) (draw-fallers '())))


; draw-score: Faller-World -> Image
; Draws the score on the top of the scene
; Strategy: Function Composition
(define (draw-score fw-example)
  (place-image 
   (text (number->string (fw-score fw-example)) 10 "olive")
   10 8 (draw-paddle fw-example)))

;tests:
(check-expect (draw-score (make-fw 100 "left" '() 0))
              (place-image (text "0" 10 "olive") 10 8 (draw-paddle (make-fw 100 "left" '() 0))))

; draw: Faller-World -> Scene
; Draws the entire scene with the paddle and fallers
; Strategy : Structural Decomposition
(define (draw fw-example)
  (draw-score fw-example))
               
;teste:
(check-expect (draw (make-fw 100 "left" '() 0)) (draw-score (make-fw 100 "left" '() 0)))
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
(define (maybe-add-faller fallers)
  (cond
    [(< (length fallers) MAX-FALLERS)
     (cond
       [(zero? (random INV-P-FALLER))
        (cons (make-posn (random WORLD-WIDTH) 0)
              fallers)]
       [else fallers])]
    [else fallers]))


(define (change-color-faller 
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

; (start 0)

