;;; Simple calculator with variables and assignments
;;; Supports basic arithmetic operations and variable assignments

;; Variable storage using an association list
(define variables '())

;; Helper function to set a variable value
(define (set-variable! name value)
  (let ((pair (assoc name variables)))
    (if pair
        (set-cdr! pair value)
        (set! variables (cons (cons name value) variables))))
  value)

;; Helper function to get a variable value
(define (get-variable name)
  (let ((pair (assoc name variables)))
    (if pair
        (cdr pair)
        (error (string-append "Undefined variable: " name)))))

;; Tokenize input string
(define (tokenize input-string)
  (define (is-digit? c)
    (char<=? #\0 c #\9))
  
  (define (is-letter? c)
    (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))
  
  (define (is-operator? c)
    (member c '(#\+ #\- #\* #\/ #\^ #\= #\( #\))))
  
  (define (tokenize-helper chars tokens current-token token-type)
    (if (null? chars)
        (if (null? current-token)
            tokens
            (cons (list token-type (list->string (reverse current-token))) tokens))
        (let ((c (car chars))
              (rest (cdr chars)))
          (cond
            ;; Skip whitespace
            ((char-whitespace? c)
             (if (null? current-token)
                 (tokenize-helper rest tokens '() 'none)
                 (tokenize-helper rest 
                                 (cons (list token-type (list->string (reverse current-token))) tokens)
                                 '()
                                 'none)))
            
            ;; Handle digits
            ((is-digit? c)
             (if (or (null? current-token) (eq? token-type 'number))
                 (tokenize-helper rest tokens (cons c current-token) 'number)
                 (tokenize-helper rest 
                                 (cons (list token-type (list->string (reverse current-token))) tokens)
                                 (list c)
                                 'number)))
            
            ;; Handle letters
            ((is-letter? c)
             (if (or (null? current-token) (eq? token-type 'identifier))
                 (tokenize-helper rest tokens (cons c current-token) 'identifier)
                 (tokenize-helper rest 
                                 (cons (list token-type (list->string (reverse current-token))) tokens)
                                 (list c)
                                 'identifier)))
            
            ;; Handle operators
            ((is-operator? c)
             (if (null? current-token)
                 (tokenize-helper rest 
                                 (cons (list 'operator (string c)) tokens)
                                 '()
                                 'none)
                 (tokenize-helper rest 
                                 (cons (list 'operator (string c))
                                       (cons (list token-type (list->string (reverse current-token))) tokens))
                                 '()
                                 'none)))
            
            ;; Handle other characters (ignore)
            (else
             (tokenize-helper rest tokens current-token token-type))))))
  
  (reverse (tokenize-helper (string->list input-string) '() '() 'none)))

;; Helper function to safely get token value
(define (token-value token)
  (if (and (list? token) (>= (length token) 2))
      (cadr token)
      ""))

;; Helper function to safely get token kind
(define (token-kind token)
  (if (and (list? token) (>= (length token) 1))
      (car token)
      'invalid))

;; Parse and evaluate expressions
(define (parse-and-evaluate tokens)
  ;; Track current position in token list
  (define current-position 0)
  
  ;; Get current token
  (define (current-token)
    (if (< current-position (length tokens))
        (list-ref tokens current-position)
        '(end "")))
  
  ;; Advance to next token
  (define (next-token)
    (set! current-position (+ current-position 1))
    (current-token))
  
  ;; Check if token matches expected kind and value
  (define (match-token kind value)
    (and (eq? (token-kind (current-token)) kind)
         (equal? (token-value (current-token)) value)))
  
  ;; Consume token if it matches expected kind and value
  (define (consume-token kind value)
    (if (match-token kind value)
        (begin
          (next-token)
          #t)
        #f))
  
  ;; Forward declarations for recursive descent parser
  (define (parse-expression) 
    (parse-assignment))
  
  ;; Parse assignment
  (define (parse-assignment)
    (if (and (eq? (token-kind (current-token)) 'identifier)
             (let ((var-name (token-value (current-token))))
               (next-token)
               (if (match-token 'operator "=")
                   (begin
                     (next-token)
                     (let ((value (parse-addition-subtraction)))
                       (set-variable! var-name value)))
                   (begin
                     (set! current-position (- current-position 1))
                     #f))))
        (get-variable (token-value (list-ref tokens 0)))
        (parse-addition-subtraction)))
  
  ;; Parse addition and subtraction
  (define (parse-addition-subtraction)
    (let ((left (parse-multiplication-division)))
      (let loop ((result left))
        (cond
          ((match-token 'operator "+")
           (next-token)
           (loop (+ result (parse-multiplication-division))))
          ((match-token 'operator "-")
           (next-token)
           (loop (- result (parse-multiplication-division))))
          (else result)))))
  
  ;; Parse multiplication and division
  (define (parse-multiplication-division)
    (let ((left (parse-power)))
      (let loop ((result left))
        (cond
          ((match-token 'operator "*")
           (next-token)
           (loop (* result (parse-power))))
          ((match-token 'operator "/")
           (next-token)
           (let ((divisor (parse-power)))
             (if (= divisor 0)
                 (error "Division by zero")
                 (loop (/ result divisor)))))
          (else result)))))
  
  ;; Parse exponentiation
  (define (parse-power)
    (let ((left (parse-primary)))
      (let loop ((result left))
        (if (match-token 'operator "^")
            (begin
              (next-token)
              (loop (expt result (parse-primary))))
            result))))
  
  ;; Parse primary expressions
  (define (parse-primary)
    (cond
      ;; Parse number
      ((eq? (token-kind (current-token)) 'number)
       (let ((value (string->number (token-value (current-token)))))
         (next-token)
         value))
      
      ;; Parse variable
      ((eq? (token-kind (current-token)) 'identifier)
       (let ((var-name (token-value (current-token))))
         (next-token)
         (get-variable var-name)))
      
      ;; Parse parenthesized expression
      ((and (eq? (token-kind (current-token)) 'operator)
            (equal? (token-value (current-token)) "("))
       (next-token)
       (let ((result (parse-expression)))
         (if (consume-token 'operator ")")
             result
             (error "Missing closing parenthesis"))))
      
      ;; Parse unary minus
      ((and (eq? (token-kind (current-token)) 'operator)
            (equal? (token-value (current-token)) "-"))
       (next-token)
       (- (parse-primary)))
      
      ;; Error for unexpected token
      (else
       (error (string-append "Unexpected token: " (token-value (current-token)))))))
  
  ;; Start parsing from the top level
  (parse-expression))

;; Main REPL (Read-Eval-Print Loop)
(define (calculator-repl)
  (display "Scheme Calculator\n")
  (display "Enter expressions with variables and assignments (e.g., \"x = 5\", \"y = x + 3\")\n")
  (display "Type \"exit\" or \"quit\" to quit\n")
  
  (define (exit-command? input)
    (or (string=? input "exit")
        (string=? input "quit")))
  
  (let loop ()
    (display "> ")
    (flush-output)
    (let ((input (read-line)))
      ;; Check for EOF (Ctrl+D or Ctrl+Z)
      (if (eof-object? input)
          (begin
            (display "Goodbye!\n")
            (void)) ; Exit the loop
          (if (exit-command? input)
              (begin
                (display "Goodbye!\n")
                (void)) ; Exit the loop
              (begin
                (when (not (string=? input ""))
                  (with-handlers 
                    ([exn:fail? 
                      (lambda (e) 
                        (display "Error: ")
                        (display (exn-message e))
                        (newline))])
                    (let ((tokens (tokenize input)))
                      (if (= (length tokens) 1) 
                          (let ((token (car tokens)))
                            (if (and (eq? (token-kind token) 'identifier)
                                     (exit-command? (token-value token)))
                                (begin
                                  (display "Goodbye!\n")
                                  (exit 0)) ; Force exit the program
                                (let ((result (parse-and-evaluate tokens)))
                                  (display result)
                                  (newline))))
                          (let ((result (parse-and-evaluate tokens)))
                            (display result)
                            (newline))))))
                (loop)))))))

;; Start the calculator
(calculator-repl)
