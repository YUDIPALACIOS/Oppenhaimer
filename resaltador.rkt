#lang racket
(require racket/format)

(define (write-html output-file output-str)
  (with-output-to-file output-file
    #:mode 'text 
    #:exists 'append
    (lambda ()
      (displayln output-str))))

(define (analize-regexp file)
  (with-input-from-file file
    (lambda ()
      (let loop ((lines (port->lines (current-input-port))) (result '()))
        (cond
          ((null? lines) (reverse result))
          (else
            (let* ((line (car lines))
                   (pair (string-split line ","))
                   (color (format "#~x~x~x" (random 256) (random 256) (random 256))))
              (loop (cdr lines) (cons (cons (car pair) color) result)))))))))

(define (regexp-matcher exp-list line html-file)
  (if (not (string=? line ""))
      (let loop ((list exp-list) (line line))
        (define regex (car (car list)))
        (define color (cdr (car list)))
        
        (cond
          [(null? list) (displayln "Error: Lista vacia, no se detectaron expresiones regulares")]
          [(regexp-match regex line)
            (begin
              (write-html html-file (format "<span style=\"color: ~a;\">~a</span>" color (car (regexp-match regex line))))
              (regexp-matcher exp-list (substring line (string-length (car (regexp-match regex line)))) html-file))]
          [(null? (cdr list)) (write-html html-file (format "<span style=\"color: #FF0000;\">:ERROR, no se reconoce la expresion ~a</span>" line))]
          [else (loop (cdr list) line)]))
    (displayln "")))

(define (syntax-highlight regexp-file input-file output-file)
  (define exp-list (analize-regexp regexp-file))
  
  (define (write-html-body)
    (write-html output-file "<html><body>")
    (write-html output-file "<h1><center>Actividad Integradora 3.4 Resaltador de sintaxis</center></h1>")
    (write-html output-file "<h2><center>Sebastián Rosas Maciel - A01233989</center></h2>")
    (with-input-from-file input-file
      (lambda ()
        (let loop ((line (read-line)))
          (cond
            [(eof-object? line) (void)]
            [else
              (write-html output-file "<div style=\"display: block;\">")
              (regexp-matcher exp-list line output-file)
              (write-html output-file "</div>")
              (write-html output-file " ")
              (loop (read-line))]))))
    (write-html output-file "</body></html>")
    )
  
  (if (file-exists? output-file)
      (write-html-body)
      (begin
        (write-html output-file "<html><body></body></html>")
        (write-html-body))))

(syntax-highlight "expresiones.txt" "codigo_fuente.txt" "output.html")
