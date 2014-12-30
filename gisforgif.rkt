#lang racket

(require racket/string)

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
  "/Users/jb/Google Drive/iMovie Events.localized")

; gets all the movies in movie-dir
(define movie-list
  (stream->list
    (stream-filter
      (lambda (path)
        (and
          (regexp-match? #rx"[.](?i:MOV)$" path) ; case-insensitive
          (not (regexp-match? #rx"Cache-[0-9]+.*mov$" path))))
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

; TODO we can get datestring from the movie name :-)
; (regexp-match #px"^([0-9]{4})([0-9]{2})([0-9]{2})" "20130613-03-MVI_0216.mov")
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

;get the info for the movie
(define (movie-info movie-in)
  (let ([info-lines (ffmprobe-lines movie-in)])
    (if (empty? info-lines)
      '()
      (hash
        'duration (duration-seconds info-lines)
        'datestring (creation-date-string info-lines)))))

;-- generating the gifs -------------------------

(define gif-seconds 2)

(define (rando-start-time dur)
  (if (<= dur gif-seconds)
    0
    (random (- dur gif-seconds))))

(define (during-babys-first-year? datestring)
  (define first-birthday-plus-1 '(2013 10 01))
  ; tail-recursive check to see if one list is less than another
  ; returns #f if lists are different length (good enough behavior)
  (define (list< a b)
    (cond
      [(and (empty? a) (empty? b)) #f]
      [(not (= (length a) (length b))) #f]
      [(< (first a) (first b)) #t]
      [else (list< (rest a) (rest b))]))
  (define datelist
    (regexp-match #px"([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})" datestring))
  (if (or (empty? datelist) (not (= 4 (length datelist))))
      #f
      (list< (map string->number (rest datelist)) first-birthday-plus-1)))

(define (gen-gif movie-in base-filename-out index [limit-to-year #f])
  (let ([movie-info-hash (movie-info movie-in)])
    (display (~a
               "\n\n\n\n" index "\n"
               "=====================\n"
               movie-in "\n\n"
               (hash-ref movie-info-hash 'datestring)
               "\n\n"))
    (cond
      [(empty? movie-info-hash) 'no-op]
      [(= 0 (hash-ref movie-info-hash 'duration)) 'no-op]
      [(and limit-to-year (not (during-babys-first-year? (hash-ref movie-info-hash 'datestring)))) 'no-op]
      [else
        (system* (find-executable-path "ffmpeg")
                "-i" movie-in
                "-ss" (~a (rando-start-time (hash-ref movie-info-hash 'duration)))
                "-t" (~a gif-seconds)
                "-vf" "scale=640:360"
                "-y"  (~a
                          base-filename-out
                          "-" (hash-ref movie-info-hash 'datestring)
                          "-" "i" index
                          ".gif"))])))

;-- thumbnails for gifs -------------------------

; gets all the gifs in gif-dir
(define (gif-list gif-dir)
  (stream->list
    (stream-filter
      (lambda (path)
        (regexp-match? #rx"[.](?i:GIF)$" path)) ; case-insensitive
      (sequence->stream (in-directory gif-dir)))))

(define (base-filename filename)
  (last (string-split (first (string-split filename ".")) "/")))

(define (gen-thumb gif-in dir-out)
  (let ([base-in (base-filename (~a gif-in))])
    (system* (find-executable-path "ffmpeg")
             "-i" gif-in
             "-vf" "scale=30:17"
             "-y" (~a dir-out "/" base-in "-thm.gif"))))

(define (gen-all-thumbs in-dir dir-out)
  (for ([gif (gif-list in-dir)])
    (gen-thumb gif dir-out)))

;-- json ----------------------------------------

(require json)

(define (gifdir->jsexpr gif-dir)
  (for/list ([gif (gif-list gif-dir)])
            (hasheq 'datestring (first
                                  (regexp-match #px"[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}"
                                                (base-filename (~a gif))))
                    'filename (base-filename (~a gif)))))

(define (write-gif-json json-file-name gif-dir)
  (let ([json-file (open-output-file json-file-name #:exists 'replace)])
    (write-json (gifdir->jsexpr gif-dir) json-file)
    (close-output-port json-file)))

;-- controlling it all --------------------------

(define (rando-movie-list n)
  (for/list ([i n]) (rando-movie)))

(define (gen-n-rando-movies n filebase)
  (for ([i (in-naturals 0)]
        [movie (rando-movie-list n)])
    (gen-gif movie filebase i #t)))

(define (gen-all-movies filebase)
  (for ([i (in-naturals 0)]
        [movie movie-list])
    (gen-gif movie filebase i #t)))
