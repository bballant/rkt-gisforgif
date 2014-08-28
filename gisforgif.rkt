#lang racket

;-- command line -------------------------------

; (define verbose-mode (make-parameter #f))
; 
; (command-line
;   #:once-each
;   [("-v" "--verbose")
;   "Compile with verbose messages"
;   (verbose-mode #t)]
;   #:args (filename)
;   filename)

;-- getting movies ------------------------------

(define movie-dir
  "/Users/bballantine/Google Drive/iMovie Events.localized")

; gets all the movies in movie-dir
(define movie-list
  (stream->list
    (stream-filter 
      (lambda (path)
        (regexp-match? #rx"[.](?i:MOV)$" path)) ; case-insensitive
        ; (regexp-match? #rx"[.]mov$" path)) ; just lower-case
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

(define (duration-seconds info-lines)
  (local
    [(define (durstr->dursecs durstr)
        (inexact->exact (floor (string->number durstr))))]
     (define duration-string
        (findf (lambda (line) (regexp-match? #rx"^duration" line))
               info-lines))
    (cond
      [(not duration-string) 0]
      [(regexp-match #rx"[0-9]*\\.[0-9]*" duration-string) =>
         (lambda (match-list) (durstr->dursecs (first match-list)))]
      [else 0])))

(define (creation-date-string info-lines)
  (local
    [(define creation-date-line
       (findf (lambda (line) (regexp-match? #rx"^creation_time" line))
              info-lines))]
    (cond
      [(not creation-date-line) "0000-00-00"]
      [(regexp-match #rx"[0-9]*\\-[0-9]*\\-[0-9]*" creation-date-line) =>
         (lambda (timestr-match-list) (first timestr-match-list))]
      [else "0000-00-00"])))

;
;get the info for the movie
;
(define (movie-info movie-in)
  (let ([info-lines (ffmprobe-lines movie-in)])
    (if (empty? info-lines)
      '()
      (hash
        'duration (duration-seconds info-lines)
        'datestring (creation-date-string info-lines)))))

;-- generating the gifs -------------------------

(define gif-seconds 2)

(define (rando-start-time movie-info-hash)
  (let ([dur (hash-ref movie-info-hash 'duration)])
        (if (<= dur gif-seconds)
          0
          (random (- dur gif-seconds)))))

(define (gen-gif movie-in base-filename-out)
  (let ([movie-info-hash (movie-info movie-in)])
    (if (empty? movie-info-hash)
      'no-op
      (system* (find-executable-path "ffmpeg")
              "-i" movie-in
              "-ss" (~a (rando-start-time movie-info-hash))
              "-t" (~a gif-seconds)
              "-vf" "scale=640:360"
              "-y"  (~a base-filename-out
                        "-"
                        (hash-ref movie-info-hash 'datestring)
                        ".gif")))))

(define (rando-movie-list n)
  (for/list ([i n]) (rando-movie)))

(define (gen-n-rando-movies n filebase)
  (for ([i (in-naturals 0)]
        [movie (rando-movie-list n)])
    (gen-gif movie (~a filebase i))))

(define (gen-all-movies filebase)
  (for ([i (in-naturals 0)]
        [movie movie-list])
    (gen-gif movie (~a filebase i))))
