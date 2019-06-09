(use file.util)
(use gauche.parseopt)

; 全ファイルのポートをすべて開く
(define (get-port-allfiles modules-path)
    (cond
        ((null? modules-path) '())
        (else
            (cons (open-input-file (car modules-path)) (get-port-allfiles (cdr modules-path)))
        )
    )
)

; ファイルポートを渡したら、引数の述語に合致するS式を返す
(define (filter-fileport pred port)
    (let ((s-expr (read port)))
        (cond
            ; 最後まで走査したら
            ((eof-object? s-expr) (close-input-port port) '())
            ((pair? (filter pred s-expr)) (cons s-expr (filter-fileport pred port)))
            (else
                (filter-fileport pred port)
            )
        )
    )
)

; get-sexpr-from-allportsで使用する述語
(define (define-module? x) (if (equal? 'define-module x) #t #f))
(define (export? x) (if (equal? 'export x) #t #f))

; すべてのポートから述語に合致するS式を取得する。
(define (get-sexpr-from-allports pred ports)
    (cond
        ((null? ports) '())
        (else
            (cons (filter-fileport pred (car ports)) (get-sexpr-from-allports pred (cdr ports)))
        )
    )
)

; 入れ子のS式を分解してアトムのリストに直す処理
(define (flat list)
    (cond 
        ((null? list) '())
        ((list? (car list)) (append (flat (car list)) (flat (cdr list))))
        (else
            (cons (car list) (flat (cdr list)))
        )
    )
)

; module名のみをS式から抽出する
; この手続きは抽象化できるけどいい名前が思いつかない...
(define (get-modulename s-expr)
    (cond 
        ((null? s-expr) '())
        ((equal? 'define-module (car s-expr)) (cons (car (cdr s-expr)) (get-modulename (cdr (cdr s-expr)))))
        (else
            (get-modulename (cdr s-expr))
        )
    )
)

; export名のみをS式から抽出する
; この手続きは抽象化できるけどいい名前が思いつかない...
(define (get-exportname s-expr)
    (cond 
        ((null? s-expr) '())
        ((equal? 'export (car s-expr)) (cons (car (cdr s-expr)) (get-exportname (cdr (cdr s-expr)))))
        (else
            (get-exportname (cdr s-expr))
        )
    )
)

; require.scmの先頭に記述する要素を決定
(define (build-define-module exports)
    (cond 
        ((null? exports) '())
        (else
            (cons (list (string-append "export " (x->string (car exports)))) (build-define-module (cdr exports)))
        )
    )
)

; (require ...) (import ...)を書き込む
(define (write-require-import modules out-port)
    (cond
        ((null? modules))
        (else
            ; S式で再帰しながら書いたほうがきれいだよね...
            ; require
            (display "(require " out-port)
            (display "\"./modules/" out-port)
            (display (car modules) out-port)
            (display "\"" out-port)
            (display ")" out-port)
            ; import
            (display "(import " out-port)
            (display (car modules) out-port)
            (display ")" out-port)
            (newline out-port)
            (write-require-import (cdr modules) out-port)
        )
    )
)

; provideの一文を書き込む
(define (write-provide name  out-port)
    (display "(provide " out-port)
    (display "\"" out-port)
    (display name out-port)
    (display "\")" out-port)
)

(define (main args)
    (let-args (cdr args)
        (
            (outfile-name "o|outfile=s")
            (modules-name "m|module=s")
            (is-recur "r|recure")
            . restargs
        )
        (print outfile-name)
        (print modules-name)
        (print is-recur)
        (print restargs)
    )
)

    ; gosh module-packing.scm packing対象のディレクトリで起動した場合

    ; (let
    ;     (
    ;         (pack-name (car (cdr args)))
    ;         (path (build-path (current-directory) (car (cdr args))))
    ;     )
    ;     (define module-list (directory-list path :add-path? #t :children? #t))
    ;     (define ports (get-port-allfiles module-list))
    ;     (define read-in (flat (get-sexpr-from-allports define-module? ports)))

    ;     (define modules (get-modulename read-in))
    ;     (define exports (build-define-module (get-exportname read-in)))

    ;     (define _define-module (cons (string-append "define-module " pack-name) exports))
    ;     (define _select-module (list (string-append "select-module " pack-name)))
    
    ;     (define out (open-output-file (build-path (current-directory) (string-append pack-name ".scm"))))

    ;     (display _define-module out)
    ;     (newline out)

    ;     (display _select-module out)
    ;     (newline out)

    ;     (write-require-import modules out)
    ;     (write-provide pack-name out)

    ;     (close-output-port out)
    ; )
