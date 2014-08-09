#lang racket


;-- getting movies ------------------------------

(define movie-dir
  "/Users/bballantine/Google Drive/iMovie Events.localized")

; gets all the movies in movie-dir
(define movie-list
  (stream->list
    (stream-filter 
      (lambda (path)
        (regexp-match? #rx"[.]MOV$" path))
      (sequence->stream (in-directory movie-dir)))))

; get a random movie
(define (rando-movie)
  (list-ref movie-list (random (length movie-list))))


;-- getting information ---------------------------

(define (ffmprobe-lines movie-in)
  (map (lambda (line) (string-trim line))
       (string-split
         (with-output-to-string
           (lambda () (system (~a "ffprobe \"" movie-in "\" -show_format 2>&1"))))
         "\n")))

(define (duration-string info-lines)
  (first (filter
           (lambda (line) (regexp-match? #rx"^duration" line))
           info-lines)))

(define (duration-seconds info-lines)
  (cond
    [(regexp-match
       #rx"[0-9]*\\.[0-9]*"
       (duration-string info-lines)) =>
     (lambda (duration-numstr-list)
       (inexact->exact (floor (string->number (first duration-numstr-list)))))]
    [else 0]))

(define (creation-time-line info-lines)
  (first (filter
           (lambda (line) (regexp-match? #rx"^creation_time" line))
           info-lines)))

(define (creation-time-string info-lines)
  (cond
    [(regexp-match
       #rx"[0-9]*\\-[0-9]*\\-[0-9]*"
       (creation-time-line info-lines)) =>
     (lambda (timestr-match-list) (first timestr-match-list))]
    [else "0000-00-00"]))

;get the info for the movie
(define (movie-info movie-in)
  (let ([info-lines (ffmprobe-lines movie-in)])
    (hash
      'duration (duration-seconds info-lines)
      'datestring (creation-time-string info-lines))))


;-- generating the gifs -------------------------

(define gif-seconds 2)

(define (rando-start-time movie-info-hash)
  (random (- (hash-ref movie-info-hash 'duration) gif-seconds)))

(define (gen-gif movie-in base-filename-out)
  (let ([movie-info-hash (movie-info movie-in)])
    (system* (find-executable-path "ffmpeg")
             "-i" movie-in
             "-ss" (~a (rando-start-time movie-info-hash))
             "-t" (~a gif-seconds)
             "-vf" "scale=640:360"
             "-y"  (~a base-filename-out
                       "-"
                       (hash-ref movie-info-hash 'datestring)
                       ".gif"))))

(define (rando-movie-list n)
  (for/list ([i n]) (rando-movie)))

(define (gen-n-rando-movies n filebase)
  (for ([i (in-naturals 0)]
        [movie (rando-movie-list n)])
    (gen-gif movie (~a filebase i))))
