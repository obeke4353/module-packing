(use file.util)
(use gauche.parseopt)

; 全ファイルのポートをすべて開く
(define (open-ports modules-path)
    (cond
        ((null? modules-path) '())
        (else
            (cons (open-input-file (car modules-path)) (open-ports (cdr modules-path)))
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

; filter-sexprで使用する述語
(define (define-module? x) (if (equal? 'define-module x) #t #f))
(define (export? x) (if (equal? 'export x) #t #f))

; すべてのポートから述語に合致するS式を取得する。
(define (filter-sexpr pred ports)
    (cond
        ((null? ports) '())
        (else
            (cons (filter-fileport pred (car ports)) (filter-sexpr pred (cdr ports)))
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
; [TODO] names引数の部分がきれいじゃないから直す
(define (write-require-and-import-sexpr-to-pack-file modules names pack-name out-port)
    (cond
        ((null? modules))
        (else
            ; S式で再帰しながら書いたほうがきれいだよね...
            ; require
            (display (string-append "(require \""  (x->string (car modules)) "\")") out-port)
            ; import
            (display (string-append "(import " (x->string (car names)) ")") out-port)
            (newline out-port)
            (write-require-and-import-sexpr-to-pack-file (cdr modules) (cdr names) pack-name out-port)
        )
    )
)

; loadファイルに書き込むmodule部分のテキスト生成
(define (build-modules module-list)
    (get-modulename
        (flat 
            (filter-sexpr define-module? 
                (open-ports module-list)
            )
        )
    )
)

; loadファイルに書き込むexports部分のテキスト生成
(define (build-exports module-list)
    (build-define-module 
        (get-exportname
            (flat 
                (filter-sexpr define-module? 
                    (open-ports module-list)
                )
            )
        )
    )
)

; サブディレクトリを再帰的に検索する。
(define (find-module-recur module-list pack-name)
    (define (_find-module-recur module-list result pack-name)
        (cond
            ((null? module-list) result)
            ; ディレクトリならば再帰的に中のファイルを取得する
            ((file-is-directory? (car module-list))
                (_find-module-recur (directory-list (car module-list) :add-path? #t :children? #t) result pack-name)
            )
            (else
                (_find-module-recur (cdr module-list) (cons (car module-list) result) pack-name)
            )
        )
    )
    (_find-module-recur module-list '() pack-name)
)

(define (main args)
    (let-args (cdr args)
        (
            (outfile-name "o|outfile=s")
            (modules-name "m|module=s")
            (is-recur "r|recur")
            . restargs
        )
        
        ; コマンドラインオプションにモジュールディレクトリの指定がない場合
        (if (and (not (pair? restargs)) (not modules-name))
            (exit 0)
        )

        (let* 
            (
                (pack-name ((lambda (m r) (if (pair? restargs) (car r) m)) modules-name restargs))
                (module-list (directory-list (build-path "./" pack-name) :add-path? #t :children? #t))
                (out (open-output-file (build-path (current-directory) (string-append pack-name ".scm"))))
            )
                ; recurコマンドライン引数を設定しなかった場合、サブディレクトリはmodule-list内から削除
                (cond 
                    ((eq? is-recur #f) (set! module-list (remove file-is-directory? module-list)))
                    (else
                        ; サブディレクトリ内のモジュール名のリスト
                        (set! module-list (find-module-recur module-list pack-name))
                    )
                )

                (display (cons (string-append "define-module " pack-name) (build-exports module-list)) out)
                (newline out)

                (display (list (string-append "select-module " pack-name)) out)
                (newline out)

                (write-require-and-import-sexpr-to-pack-file module-list (build-modules module-list) pack-name out)
                (display (string-append "(provide \"" pack-name "\")") out)

                (close-output-port out)
        )
    )
)