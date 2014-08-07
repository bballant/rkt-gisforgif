#lang racket

(define ee find-executable-path)

; (system* (ee "cowsay") "\"Hello Yyyyou!\"")
; (system* (ee "ffprobe") "0001.mov")
; (system* (ee "ffmpeg")
;          "-i" "MVI_1503.MOV"
;          "-ss" "00:00:06"
;          "-t" "00:00:01"
;          "-acodec" "copy"
;          "-vcodec" "copy"
;          "-async" "1"
;          "-y"  "0001.mov")
; 
; (system* (ee "ffmpeg")
;          "-i" "MVI_1503.MOV"
;          "-ss" "00:00:06"
;          "-t" "00:00:02"
;          "-vf" "scale=480:270"
;          "-y"  "0001.gif")
; 

(define movie-dir
  "/Users/bballantine/Google Drive/iMovie Events.localized")

(define movie-seq
  (in-directory movie-dir))

(define movie-list
  (stream->list
    (stream-filter 
      (lambda (path)
        (regexp-match? #rx"[.]MOV$" path))
      (sequence->stream movie-seq))))

(define (rando-movie)
  (list-ref movie-list (random (length movie-list))))

(define (rando-start-time)
  (~a "00:00:"
    (~a (random 10) #:width 2 #:align 'right #:left-pad-string "0")))

(define (gen-rando-gif filename)
  (system* (ee "ffmpeg")
          "-i" (rando-movie)
          "-ss" (rando-start-time)
          "-t" "00:00:02"
          "-vf" "scale=640:360"
          "-y"  filename))

(define (gen-gif movie-in gif-out)
  (system* (ee "ffmpeg")
          "-i" movie-in
          "-ss" (rando-start-time)
          "-t" "00:00:02"
          "-vf" "scale=640:360"
          "-y"  gif-out))

(define (rando-movie-list n)
  (for/list ([i n]) (rando-movie)))

(define (gen-n-rando-movies n filebase)
  (for ([i (in-naturals 0)]
        [movie (rando-movie-list n)])
    (gen-gif movie (~a filebase i ".gif"))))
