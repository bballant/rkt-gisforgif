#lang web-server/insta

; scrap

(define gif-dir "~/projects/rkt-gisforgif/out")

(define (start request)
  (response/xexpr
    `(html
       (head (title "My Blog"))
       (body (ul ,@(map path->li (gif-path-list gif-dir)))))))

(define (path->li path)
  `(li ,(path->string path)))

(define (path-list dir match-regexp)
  (stream->list
    (stream-filter
      (lambda (path)
        (regexp-match? match-regexp path))
      (sequence->stream (in-directory dir)))))

(define (gif-path-list gif-dir)
  (path-list gif-dir #rx"[.]gif$"))


