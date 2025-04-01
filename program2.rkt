#lang racket

;; Recursive Descent Parser for a simple calculator language
;; Grammar:
;; program -> {stmt_list} $$
;; stmt_list -> stmt stmt_list | epsilon
;; stmt -> id = expr; | if (expr) stmt_list endif; | read id; | write expr;
;; expr -> id etail | num etail
;; etail -> + expr | - expr | compare expr | epsilon
;; id -> [a-zA-Z]+
;; num -> numsign digit digit*
;; numsign -> + | - | epsilon
;; compare -> < | <= | > | >= | == | !=

;; Define the structure to store the current parsing state
(struct parser-state (input current-line current-pos))

;; Global variables for parser state
(define current-token null)
(define next-char #\space)
(define current-line 1)
(define current-pos 0)
(define input-port null)

;; Function to get the next character from the input
(define (get-char)
  (set! next-char (read-char input-port))
  (when (eof-object? next-char)
    (set! next-char #\space))
  (when (char=? next-char #\newline)
    (set! current-line (add1 current-line))
    (set! current-pos 0))
  (set! current-pos (add1 current-pos)))

;; Function to skip whitespace
(define (skip-whitespace)
  (when (and (not (eof-object? next-char)) (char-whitespace? next-char))
    (get-char)
    (skip-whitespace)))

;; Function to check if a character is a letter
(define (letter? c)
  (and (char? c) (or (char<=? #\a c #\z) (char<=? #\A c #\Z))))

;; Function to check if a character is a digit
(define (digit? c)
  (and (char? c) (char<=? #\0 c #\9)))

;; Function to scan the next token
(define (scan-token)
  (skip-whitespace)
  (cond
    [(eof-object? next-char) '(EOF)]
    [(letter? next-char) (scan-id)]
    [(digit? next-char) (scan-num)]
    [(char=? next-char #\+) (begin (get-char) '(PLUS))]
    [(char=? next-char #\-) (begin (get-char) '(MINUS))]
    [(char=? next-char #\() (begin (get-char) '(LPAREN))]
    [(char=? next-char #\)) (begin (get-char) '(RPAREN))]
    [(char=? next-char #\;) (begin (get-char) '(SEMICOLON))]
    [(char=? next-char #\=) (begin 
                              (get-char)
                              (if (char=? next-char #\=)
                                  (begin (get-char) '(COMPARE ==))
                                  '(ASSIGN)))]
    [(char=? next-char #\<) (begin 
                              (get-char)
                              (if (char=? next-char #\=)
                                  (begin (get-char) '(COMPARE <=))
                                  '(COMPARE <)))]
    [(char=? next-char #\>) (begin 
                              (get-char)
                              (if (char=? next-char #\=)
                                  (begin (get-char) '(COMPARE >=))
                                  '(COMPARE >)))]
    [(char=? next-char #\!) (begin 
                              (get-char)
                              (if (char=? next-char #\=)
                                  (begin (get-char) '(COMPARE !=))
                                  (error (format "Scan Error on line ~a: Unexpected character '!'" current-line))))]
    [(char=? next-char #\$) (begin 
                              (get-char)
                              (if (char=? next-char #\$)
                                  (begin (get-char) '(END))
                                  (error (format "Scan Error on line ~a: Unexpected character '$'" current-line))))]
    [else (error (format "Scan Error on line ~a: Illegal character '~a'" current-line next-char))]))

;; Function to scan an identifier
(define (scan-id)
  (let loop ([id-str (string next-char)])
    (get-char)
    (if (letter? next-char)
        (loop (string-append id-str (string next-char)))
        (cond
          [(string=? id-str "if") '(IF)]
          [(string=? id-str "endif") '(ENDIF)]
          [(string=? id-str "read") '(READ)]
          [(string=? id-str "write") '(WRITE)]
          [else `(ID ,id-str)]))))

;; Function to scan a number
(define (scan-num)
  (let loop ([num-str (string next-char)])
    (get-char)
    (if (digit? next-char)
        (loop (string-append num-str (string next-char)))
        `(NUM ,(string->number num-str)))))

;; Function to get the next token
(define (get-token)
  (set! current-token (scan-token)))

;; Function to match the expected token
(define (match expected-token)
  (if (equal? (car current-token) expected-token)
      (let ([matched-token current-token])
        (get-token)
        matched-token)
      (error (format "Parse Error on line ~a: Expected ~a, got ~a" 
                    current-line 
                    expected-token 
                    (car current-token)))))

;; Parser functions for each non-terminal in the grammar

;; Program -> {stmt_list} $$
(define (parse-program)
  (get-token)
  (let ([tree (parse-stmt-list)])
    (match 'END)
    (if (null? tree)
        '(program)
        `(program ,tree))))

;; stmt_list -> stmt stmt_list | epsilon
(define (parse-stmt-list)
  (if (or (equal? (car current-token) 'ID)
          (equal? (car current-token) 'IF)
          (equal? (car current-token) 'READ)
          (equal? (car current-token) 'WRITE))
      (let ([stmt (parse-stmt)]
            [rest-stmts (parse-stmt-list)])
        (if (null? rest-stmts)
            `(stmt-list ,stmt)
            `(stmt-list ,stmt ,rest-stmts)))
      '()))

;; stmt -> id = expr; | if (expr) stmt_list endif; | read id; | write expr;
(define (parse-stmt)
  (cond
    [(equal? (car current-token) 'ID)
     (let ([id (match 'ID)])
       (match 'ASSIGN)
       (let ([expr (parse-expr)])
         (match 'SEMICOLON)
         `(assign-stmt ,id ,expr)))]
    [(equal? (car current-token) 'IF)
     (match 'IF)
     (match 'LPAREN)
     (let ([expr (parse-expr)])
       (match 'RPAREN)
       (let ([stmt-list (parse-stmt-list)])
         (match 'ENDIF)
         (match 'SEMICOLON)
         `(if-stmt ,expr ,stmt-list)))]
    [(equal? (car current-token) 'READ)
     (match 'READ)
     (let ([id (match 'ID)])
       (match 'SEMICOLON)
       `(read-stmt ,id))]
    [(equal? (car current-token) 'WRITE)
     (match 'WRITE)
     (let ([expr (parse-expr)])
       (match 'SEMICOLON)
       `(write-stmt ,expr))]))

;; expr -> id etail | num etail
(define (parse-expr)
  (let ([term (cond 
                [(equal? (car current-token) 'ID) (match 'ID)]
                [(equal? (car current-token) 'NUM) (match 'NUM)]
                [(equal? (car current-token) 'MINUS)
                 (match 'MINUS)
                 (let ([num (match 'NUM)])
                   `(NUM ,(- (cadr num))))]
                [else (error (format "Parse Error on line ~a: Expected ID or NUM, got ~a" 
                                    current-line 
                                    (car current-token)))])])
    (let ([etail (parse-etail)])
      (if (null? etail)
          `(expr ,term)
          `(expr ,term ,etail)))))

;; etail -> + expr | - expr | compare expr | epsilon
(define (parse-etail)
  (cond
    [(equal? (car current-token) 'COMPARE)
     (let ([compare-op (match 'COMPARE)]
           [expr (parse-expr)])
       `(etail ,compare-op ,expr))]
    [(equal? (car current-token) 'PLUS)
     (match 'PLUS)
     (let ([expr (parse-expr)])
       `(etail + ,expr))]
    [(equal? (car current-token) 'MINUS)
     (match 'MINUS)
     (let ([expr (parse-expr)])
       `(etail - ,expr))]
    [else '()]))

;; Main parse function
(define (parse filename)
  (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
    (set! input-port (open-input-file filename))
    (set! current-line 1)
    (set! current-pos 0)
    (get-char)  ; Initialize by getting the first character
    (let ([parse-tree (parse-program)])
      (close-input-port input-port)
      (format "Accept\n~a" parse-tree))))

;; Test the parser with the sample files
(module+ main
 
  (displayln "\nTesting file2.txt:")
  (displayln (parse "file2.txt"))
  
  (displayln "\nTesting file3.txt:")
  (displayln (parse "file3.txt"))
  
  (displayln "\nTesting file4.txt:")
  (displayln (parse "file4.txt")))