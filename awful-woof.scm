(module awful-woof ()

(import chicken scheme)
(use data-structures extras files irregex posix srfi-1 srfi-13 utils)
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

(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port #<#EOF
Usage: #this <options> files

<options>:

--port=<port>
  Port to listen to.  Default is 8080.

--count=<integer>
  Maximum number of times files can be downloaded.  Negative values
  indicate no limit.  The default value is 1.

--allow=<ip>
  Only allow hosts whose IP match <ip>.  <ip> may be a comma-separated
  list of IP numbers. `*' can be used to specify any number in an
  octet (e.g., "192.168.0.*" matches any IP number in the range
  192.168.0.0 - 192.168.0.255).  By default, serve files to address.

--ip=<ip>
  IP address to bind too.  If not provided, will bind to all interfaces
  and their corresponding addresses.

EOF
))
  (when exit-code
    (exit exit-code)))


(let* ((args (command-line-arguments))
       (files (remove (lambda (opt)
                        (string-prefix? "--" opt))
                      args)))

  (when (null? files)
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((port (cond ((cmd-line-arg '--port args)
                     => (lambda (port)
                          (or (string->number port)
                              (server-port))))
                    (else (server-port))))
        (ip-address (cmd-line-arg '--ip args))
        (allowed (cmd-line-arg '--allow args))
        (dev-mode? (and (member "--development-mode" args) #t)))
    (cond ((cmd-line-arg '--count args)
           => (lambda (count)
                (set! *max-downloads* (or (string->number count) 1)))))

    ;; Don't serve files unless they are specified on the command line
    (root-path (if (eq? (software-type) 'windows)
                   "NUL"
                   "/dev/null"))

    (index-page files)
    (for-each serve-file files)

    (when allowed
      (control-access! (string-split allowed ",")))

    (awful-start void
                 ip-address: ip-address
                 port: port
                 dev-mode?: dev-mode?)
    ))

) ;; end module
