(module awful-woof ()

(import chicken scheme)
(use data-structures extras files posix srfi-1 srfi-13 utils)
(use awful spiffy)

;; --count
(define *max-downloads* 1)

;; Alist mapping files/patterns to the number of times they have been
;; downloaded
(define *download-count* '())

(enable-sxml #t)

(define (download-count file)
  (alist-ref file *download-count* equal? 0))

(define (inc-download-count! file)
  (set! *download-count*
        (alist-update! file
                       (+ 1 (download-count file))
                       *download-count*
                       equal?)))

(define (serve-file uri-file)
  (define-page uri-file
    (lambda ()
      (let* ((path (normalize-pathname
                    (make-pathname (if (absolute-pathname? uri-file)
                                       #f
                                       (current-directory))
                                   uri-file)))
             (file (pathname-strip-directory uri-file)))
        (if (or (< *max-downloads* 0)
                (< (download-count path) *max-downloads*))
            (let ((dir (pathname-directory path)))
              (lambda ()
                (inc-download-count! path)
                (parameterize ((root-path dir))
                  (printf "=== Serving file ~a to ~a\n" path (remote-address))
                  (send-static-file file))))
            (begin
              (printf "=== Download limit for ~a has been reached.  Not serving file.\n"
                      path)
              (lambda ()
                (send-status 404
                             (sprintf "~a has been downloaded ~a ~a already"
                                      file
                                      *max-downloads*
                                      (if (> *max-downloads* 1)
                                          "times"
                                          "time"))))))))))

;; An index page to show the available files
(define (index-page files)
  (define-page (main-page-path)
    (lambda ()
      `((h1 "Available files")
        (ul ,@(map (lambda (f)
                     `(li (a (@ (href ,f)) ,f)))
                   files))))))

(define (ip-match? ip pattern)
  (let ((ip-tokens (string-split ip "."))
        (pat-tokens (string-split pattern ".")))
    (when (not (= (length pat-tokens) 4))
      (fprintf (current-error-port)
               "Error: invalid IP pattern: ~a"
               pattern)
      (exit 1))
    (let loop ((ip-octets ip-tokens)
               (pat-octets pat-tokens))
      (if (null? ip-octets)
          #t
          (let ((pat-octet (car pat-octets)))
            (if (or (equal? pat-octet "*")
                    (equal? pat-octet (car ip-octets)))
                (loop (cdr ip-octets)
                      (cdr pat-octets))
                #f))))))

(define (control-access! ips)
  (page-access-control
   (lambda (_)
     (any (lambda (ip)
            (ip-match? (remote-address) ip))
          ips))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port #<#EOF
Usage: #this [<options>] [files]

<options>:

--port <port>
  Port to listen to.  Default is #(server-port).

--count <integer>
  Maximum number of times files can be downloaded.  Negative values
  indicate no limit.  The default value is 1.

--allow <ip>
  Only allow hosts whose IP match <ip>.  This parameter may be provided
  multiple times. `*' can be used to specify any number in an octet (e.g.,
  "192.168.0.*" matches any IP number in the range
  192.168.0.0 - 192.168.0.255).  By default, serve files to any address.

--ip <ip>
  IP address to bind too.  If not provided, will bind to all interfaces
  and their corresponding addresses.

EOF
))
  (when exit-code
    (exit exit-code)))

(define (die! message)
  (fprintf (current-error-port) (string-append message "\n"))
  (exit 1))

(let* ((args (command-line-arguments))
       (files '())
       (port (server-port))
       (ip-address #f)
       (allowed '())
       (dev-mode? #f))
  (let loop ((args args))
    (unless (null? args)
      (let ((arg (string->symbol (car args))))
        (case arg
          ((-h -help --help)
           (usage 0))
          ((--port)
           (if (null? (cdr args))
               (die! "--port requires an argument")
               (or (and-let* ((p (string->number (cadr args))))
                     (set! port p))
                   (die! (sprintf "--port: invalid argument: ~a" (cadr args)))))
           (loop (cddr args)))
          ((--ip)
           (if (null? (cdr args))
               (die! "--ip requires an argument")
               (set! ip-address (cadr args)))
           (loop (cddr args)))
          ((--allow)
           (if (null? (cdr args))
               (die! "--allow requires an argument")
               (set! allowed (cons (cadr args) allowed)))
           (loop (cddr args)))
          ((--count)
           (if (null? (cdr args))
               (die! "--count requires an argument")
               (or (and-let* ((c (string->number (cadr args))))
                     (set! *max-downloads* c))
                   (die! (sprintf "--count: invalid argument: ~a" (cadr args)))))
           (loop (cddr args)))
          ((--development-mode)
           (set! dev-mode? #t)
           (loop (cdr args)))
          (else
           (set! files (cons (car args) files))
           (loop (cdr args))))))

    ;; Don't serve files unless they are specified on the command line
    (root-path (if (eq? (software-type) 'windows)
                   "NUL"
                   "/dev/null"))

    (index-page files)
    (for-each serve-file files)

    (unless (null? allowed)
      (control-access! allowed))

    (awful-start void
                 ip-address: ip-address
                 port: port
                 dev-mode?: dev-mode?)
    ))

) ;; end module
