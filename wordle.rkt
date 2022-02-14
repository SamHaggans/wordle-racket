;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wordle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)
(define ROWS 5) ; 0 to 5


(define DICTIONARY (read-lines "wordlists/allWords.txt"))

(define POSSIBLE (read-lines "wordlists/possibleWords.txt"))

(define (dictsize dict)
  (cond  [(empty? dict) 0]
         [else (+ 1 (dictsize (rest dict)))]))

(define (dictget dict num)
  (cond [(= num 0) (first dict)]
        [else (dictget (rest dict) (- num 1))]))

(define WORD (dictget POSSIBLE (random (dictsize POSSIBLE))))

(check-expect (contains? "aahed" DICTIONARY) #true)
(check-expect (contains? "crane" DICTIONARY) #true)
(check-expect (contains? "monkey" DICTIONARY) #false)


(define (contains? word dict)
  (cond [(empty? dict) #false]
        [(string=? word (first dict)) #true]
        [else (contains? word (rest dict))]))

(define-struct wordle-state [word row prev-lett current-guess won])



(define (letter-box color letter)
  (overlay (place-image (text (string-upcase letter) 60 "white")
                        40 45
                        (rectangle 79 79 "solid" color))
           (rectangle  80 80 "outline" "black")))
  

             
(define BACKGROUND (rectangle 458 648 "solid" "white"))
(define EMPTY-IMAGE (rectangle 80 80 "solid" "black"))

(define (draw-all ws)
  (overlay/xy (draw-rows ws 0)
              -20
              -100
              BACKGROUND))

(define (draw-rows ws count)
  (if (< count 6)
      (overlay/xy
       (cond [(< count (wordle-state-row ws))
              (draw-boxes-prev 0
                               (wordle-state-word ws)
                               (substring (wordle-state-prev-lett ws)
                                          (* 5 count)
                                          (* 5 (+ 1 count))))]
             [(= count (wordle-state-row ws))
              (draw-boxes-active (wordle-state-current-guess ws) 0)]
             [else (generate-boxes 5)])
       0 90
       (draw-rows ws (+ count 1)))
      (rectangle 0 0 "solid" "black")))
 
(define (draw-boxes-prev count ans word)
  (if (string=? word "") (rectangle 0 0 "solid" "black")
      (overlay/xy (letter-box (get-color count (string-ith word 0) ans) (string-ith word 0))
                  85 0
                  (draw-boxes-prev (+ count 1) ans (remove-first word)))))

(define (draw-boxes-active str count)
  (cond [(= count 5) (rectangle 0 0 "solid" "black")]
        [(string=? str "") (overlay/xy EMPTY-IMAGE 85 0 (draw-boxes-active "" (+ count 1)))]; go to 5
        [else (overlay/xy (letter-box "black" (string-ith str 0)) 85 0 (draw-boxes-active (remove-first str) (+ count 1)))]))

(define (get-color index lett ans)
  (cond [(string=? (string-ith ans index) lett) "Lime Green"]
        [(string-contains? lett ans) "Goldenrod"]
        [else "Dim Gray"]))


(define (generate-boxes count)
  (if (= count 0)
      (rectangle 0 0 "solid" "black")
      (overlay/xy EMPTY-IMAGE 85 0 (generate-boxes (- count 1)))))

(define (remove-last str)
  (substring str 0 (- (string-length str) 1)))
(define (remove-first str)
  (substring str 1 (string-length str)))

(define game-state (make-wordle-state WORD 0 "" "" #false))

(define (handle-key ws ke)
  (cond [(string=? ke "\b")
         (if (> (string-length (wordle-state-current-guess ws)) 0)
             (make-wordle-state (wordle-state-word ws)
                                (wordle-state-row ws)
                                (wordle-state-prev-lett ws)
                                (remove-last (wordle-state-current-guess ws)) #false) ws) ]
        [(string=? ke "\r")
         (if (contains? (wordle-state-current-guess ws) DICTIONARY)
             (make-wordle-state (wordle-state-word ws)
                                (add1 (wordle-state-row ws))
                                (string-append (wordle-state-prev-lett ws) (wordle-state-current-guess ws))
                                ""
                                (string=? (wordle-state-current-guess ws) (wordle-state-word ws)))
             ws)]
        [else
         (if (< (string-length (wordle-state-current-guess ws)) 5)
             (make-wordle-state (wordle-state-word ws)
                                (wordle-state-row ws)
                                (wordle-state-prev-lett ws)
                                (string-append (wordle-state-current-guess ws) ke) #false) ws)]))
(define (done? ws)
  (or (wordle-state-won ws) (> (wordle-state-row ws) 5)))


WORD
(define (wordle-game ws)
  (big-bang ws
    [to-draw draw-all]
    [on-key handle-key]
    [stop-when done? draw-all]))

(wordle-game game-state)


