;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname resaltador) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Importar módulos necesarios
;(require racket/regexp)
(require racket/file)
(require racket/contract)

; Función auxiliar para generar colores aleatorios
(define/contract (generate-color)
  (-> string?)
  (format "#~x~x~x" (random 256) (random 256) (random 256)))

; Función auxiliar para tokenizar una línea de código
(define (match-token regex line color)
  (let ((match (regexp-match regex line)))
    (if match
        (cons (match:substring match) color)
        (cons (substring line 0 1) "#000000"))))

; Función principal para tokenizar una línea de código
(define (tokenize line regex-list)
  (define (loop line tokens)
    (if (regexp-match? (car regex-list) line)
        (reverse tokens))
        (let* ((token (apply match-token regex-list line (list (generate-color))))
               (next-line (substring line (if token (string-length (car token)) 1))))
          (loop next-line (cons token tokens)))))

; Función auxiliar para generar HTML con la información de los tokens
(define (generate-html tokens regex-list)
  (string-append
   "<!DOCTYPE html>\n<html>\n<head>\n<style>\n"
   ; Generar estilos CSS
   (string-append
    "body {font-family: monospace; white-space: pre;}\n"
    (apply string-append
           (map (lambda (_) (format ".~a {color: ~a;}\n" (generate-color) (generate-color)))
                regex-list)))
   "</style>\n</head>\n<body>\n"
   ; Generar cuerpo del HTML con los tokens
   (string-join (map (lambda (token)
                       (format "<span style=\"color: ~a;\">~a</span>"
                               (cdr token) (car token)))
                     tokens)
                "\n")
   "\n</body>\n</html>"))

; Función auxiliar para leer un archivo y devolver sus líneas
(define (read-lines file)
  (file->lines file))

; Función principal para resaltar léxico
(define (resaltador regex-file source-file output-file)
(define regex-list (read-lines regex-file)))
(define source-code (read-lines source-file))
(define tokens (apply append (map (lambda (line) (tokenize line regex-list)) source-code)))

  ; Escribir el HTML generado en el archivo de salida
  (with-output-to-file output-file
    (lambda ()
      (display (generate-html tokens regex-list))))

; Llamar a la función principal con los archivos de entrada y salida
(resaltador "expresiones.txt" "codigo_fuente.txt" "output.html")

 
