#lang racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         racket/cmdline)

(define _DWORD _uint)
(define _ULONG _uint)
(define _UCHAR _ubyte)

(define FT_OK 0)
(define FT_DEVICE_NOT_FOUND 2)

(define _FT_STATUS _uint)
(define _FT_HANDLE (_cpointer 'ft-handler))
(define _char _int8)

(define FT_BITMODE_RESET #x00)
(define FT_BITMODE_SYNC_BITBANG #x04)
(define FT_BITMODE_CBUS_BITBANG #x20)

(define test-mode (make-parameter #f))

(define-ffi-definer define-libftd2xx (ffi-lib "libftd2xx" '("1.4.8")))
;; (define-ffi-definer define-libftd2xx (ffi-lib "libtestftd2xx"))

(define-cstruct _FT_DEVICE_LIST_INFO_NODE
  ([flags _ULONG]
   [type _ULONG]
   [id _ULONG]
   [loc-id _DWORD]
   [serial-number (_array/list _char 16)]
   [description (_array/list _char 64)]
   [ft-handle _FT_HANDLE])
  )

(define (_array/list->string lis)
  (define index (index-of lis 0))
  (apply string (map integer->char (take lis index))))

(module+ test
  (require rackunit)
  (check-equal? (_array/list->string '(#x74 #x65 #x73 #x74 #x00)) "test"))

;; FT_STATUS FT_CreateDeviceInfoList (LPDWORD lpdwNumDevs)
(define-libftd2xx FT_CreateDeviceInfoList
  (_fun (lpdwNumDevs : (_ptr o _DWORD)) -> (status : _FT_STATUS) -> (values status lpdwNumDevs)))

(module+ test
  (require rackunit)
  (define-values (status0 num) (FT_CreateDeviceInfoList))

  (check-equal? status0 FT_OK)
  (check-equal? num 2))

;; FT_STATUS FT_GetDeviceInfoList (FT_DEVICE_LIST_INFO_NODE *pDest, LPDWORD lpdwNumDevs)
(define-libftd2xx FT_GetDeviceInfoList
  (_fun (pDest : (_list o _FT_DEVICE_LIST_INFO_NODE 10)) (lpdwNumDevs : (_ptr o _DWORD)) ->
        (status : _FT_STATUS) -> (values status lpdwNumDevs pDest)))

(module+ test
  (define-values (status1 nums infos) (FT_GetDeviceInfoList))

  (check-equal? status1 FT_OK)
  (check-equal? nums 2)

  (struct test-data (flag desc))
  (define tests (list->vector
                 (list (test-data #x0001 "test1")
                       (test-data #x0002 "test2"))))

  (for ([i nums])
    (define info (vector-ref (list->vector infos) i))
    (define flag (FT_DEVICE_LIST_INFO_NODE-flags info))
    (define desc
      (_array/list->string (FT_DEVICE_LIST_INFO_NODE-description info)))

    (check-equal? flag (test-data-flag (vector-ref tests i)))
    (check-equal? desc (test-data-desc (vector-ref tests i)))))

;; FT_STATUS FT_Open (int iDevice, FT_HANDLE *ftHandle)
(define-libftd2xx FT_Open
  (_fun _int (ftHandle : (_ptr o _FT_HANDLE)) -> (status : _FT_STATUS) -> (values status ftHandle)))

(module+ test
  (define-values (status2 handler) (FT_Open 0))
  (check-equal? status2 FT_OK))

;; FT_STATUS FT_OpenEx (PVOID pvArg1, DWORD dwFlags, FT_HANDLE *ftHandle)
(define-libftd2xx FT_OpenEx
  (_fun _string _DWORD (ftHandle : (_ptr o _FT_HANDLE)) -> (status : _FT_STATUS) -> (values status ftHandle)))

(define FT_OPEN_BY_SERIAL_NUMBER 1)
(define FT_OPEN_BY_DESCRIPTION 2)

(module+ test
  (let-values (((status _) (FT_OpenEx "desc_ok" FT_OPEN_BY_DESCRIPTION)))
    (check-equal? status FT_OK))

  (let-values (((status _) (FT_OpenEx "desc_ng" FT_OPEN_BY_DESCRIPTION)))
    (check-equal? status FT_DEVICE_NOT_FOUND))

  (let-values (((status _) (FT_OpenEx "000001" FT_OPEN_BY_SERIAL_NUMBER)))
    (check-equal? status FT_OK))

  (let-values (((status _) (FT_OpenEx "serial_ng" FT_OPEN_BY_SERIAL_NUMBER)))
    (check-equal? status FT_DEVICE_NOT_FOUND)))

;; FT_STATUS FT_GetBitMode (FT_HANDLE ftHandle, PUCHAR pucMode)
(define-libftd2xx FT_GetBitMode
  (_fun (ftHandle : _FT_HANDLE) (pucMode : (_ptr o _UCHAR)) -> (status : _FT_STATUS)
        -> (values status pucMode)))

;; FT_STATUS FT_SetBitMode(FT_HANDLE ftHandle, UCHAR ucMask, UCHAR ucMode)
(define-libftd2xx FT_SetBitMode
  (_fun (ftHandle : _FT_HANDLE) _UCHAR _UCHAR -> (status : _FT_STATUS)))

(module+ test
  (let-values (((_ handler) (FT_Open 0)))
    (define set-status (FT_SetBitMode handler #xcc #xff))
    (check-equal? set-status FT_OK)

    (define-values (get-status mode) (FT_GetBitMode handler))
    (check-equal? get-status FT_OK)
    (check-equal? mode #xff)))

;; FT_STATUS FT_Close (FT_HANDLE ftHandle)
(define-libftd2xx FT_Close
  (_fun  (ftHandle : _FT_HANDLE) -> (status : _FT_STATUS)))

(module+ test
  (let-values (((_ handler) (FT_Open 0)))
    (check-equal? (FT_Close handler) FT_OK))

  (let-values (((_ handler) (FT_OpenEx "desc_ok" FT_OPEN_BY_DESCRIPTION)))
    (check-equal? (FT_Close handler) FT_OK)))

(define (ftdi-serial-numbers)
  (define-values (status0 devices) (FT_CreateDeviceInfoList))
  (when (not (eq? status0 FT_OK))
    (error (format "FT_CreateDeviceInfoList error. ret code=~a" status1)))

  (define-values (status1 num device-info-list) (FT_GetDeviceInfoList))
  (when (not (eq? status1 FT_OK))
    (error (format "FT_GetDeviceInfoList error. ret code=~a" status1)))

  (map (lambda (device-info)
         (define serial-number (FT_DEVICE_LIST_INFO_NODE-serial-number device-info))
         (_array/list->string serial-number))
       (take device-info-list num)))

(define (ftdi-open-by-serial-number serial)
  (define-values (status handler) (FT_OpenEx serial FT_OPEN_BY_SERIAL_NUMBER))
  (when (not (eq? status FT_OK))
    (error (format "FT_OpenEx error. ret code=~a" status)))
  handler)

(define (ftdi-close handler)
  (define status (FT_Close handler))
  (when (not (eq? status FT_OK))
    (error (format "FT_Close error. ret code=~a" status))))

(define (ftdi-reset handler)
  (define status (FT_SetBitMode handler #x00 FT_BITMODE_RESET))
  (when (not (eq? status FT_OK))
    (error (format "FT_SetBitmode error. ret code=~a" status))))

(define (ftdi-set-cbus handler mode out)
  (define status (FT_SetBitMode handler
                                (bitwise-xor (arithmetic-shift mode 4) out) FT_BITMODE_CBUS_BITBANG))
  (when (not (eq? status FT_OK))
    (error (format "FT_SetBitmode error. ret code=~a" status))))

(module+ test
  (check-equal? (ftdi-serial-numbers) '("000001" "000002")))

(require racket/class)

(define-logger app)
(define app-log-rceiver (make-log-receiver app-logger 'debug))

(define main-frame%
  (class frame%
    (field [handler ""]
           [ftdi-status 'close])

    (super-new
     [label "cbus controller"]
     [width 300]
     [height 400])

    (define (enable-cbus-choices status)
      (for-each (lambda (choice)
                  (send choice enable status))
                cbus-choices))

    (define (enable-cbus-sliders status)
      (for-each (lambda (slider)
                  (send slider enable status))
                cbus-sliders))

    (define (on-open)
      (set! ftdi-status 'open)

      (send close-button enable #t)
      (send cbus-reset-button enable #t)
      ;; (send cbus-status-button enable #t)
      (send cbus-update-button enable #t)
      (enable-cbus-choices #t)
      (enable-cbus-sliders #t)

      (send open-button enable #f)
      (send update-button enable #f)
      (send ftdi-choice enable #f))

    (define (on-close)
      (set! ftdi-status 'close)

      (send close-button enable #f)
      (send cbus-reset-button enable #f)
      ;; (send cbus-status-button enable #f)
      (send cbus-update-button enable #f)
      (enable-cbus-choices #f)
      (enable-cbus-sliders #f)

      (send open-button enable #t)
      (send update-button enable #t)
      (send ftdi-choice enable #t))

    (define (on-open-clicked button event)
      (log-app-info "on-open-clicked called")
      (define serial (send ftdi-choice get-string (send ftdi-choice get-selection)))
      (set! handler (ftdi-open-by-serial-number  serial))
      (on-open))

    (define (on-close-clicked button event)
      (log-app-info "on-close-clicked called")
      (ftdi-close handler)
      (on-close))

    (define (on-update-clicked button event)
      (log-app-info "on-update-clicked called")

      (define serial-numbers (ftdi-serial-numbers))
      (send ftdi-choice clear)
      (for-each (lambda (serial-number)
                  (send ftdi-choice append serial-number))
                serial-numbers))

    (define (on-cbus-status-clicked button event)
      (log-app-info "on-cbus-status-clicked called"))

    (define (on-cbus-update-clicked button event)
      (log-app-info "on-cbus-update-clicked called")
      (define mode
        (foldr (lambda (choice acc)
                 (bitwise-ior (arithmetic-shift acc 1)
                              (if (eq? (send choice get-string-selection) "Output") 1 0)))
               0
               cbus-choices))
      (define out
        (foldr (lambda (slider acc)
                 (bitwise-ior (arithmetic-shift acc 1)
                              (send slider get-value)))
               0
               cbus-sliders))
      (ftdi-set-cbus handler mode out))

    (define (on-cbus-reset-clicked button event)
      (log-app-info "on-cbus-reset-clicked called")
      (ftdi-reset handler))

    (define top-panel (new vertical-panel%
                           [parent this]))

    (define ftdi-group (new group-box-panel%
                            [parent top-panel]
                            [label "FTDI"]
                            [border 2]
                            [alignment '(center center)]
                            [stretchable-height #f]
                            [stretchable-width #t]))

    (define ftdi-panel (new horizontal-panel%
                            [parent ftdi-group]
                            [border 2]
                            [alignment '(center center)]))

    (define ftdi-choice (new choice%
                             [parent ftdi-panel]
                             [label "IC : "]
                             [choices (ftdi-serial-numbers)]))

    (define update-button (new button%
                               [parent ftdi-panel]
                               [label "update"]
                               [callback on-update-clicked]))

    (define ftdi-button-panel (new vertical-panel%
                                   [parent ftdi-panel]))

    (define open-button (new button%
                             [parent ftdi-button-panel]
                             [label "open"]
                             [callback on-open-clicked]))

    (define close-button (new button%
                              [parent ftdi-button-panel]
                              [label "close"]
                              [callback on-close-clicked]))

    (define cbus-group (new group-box-panel%
                            [parent top-panel]
                            [label "CBUS"]
                            [border 2]
                            [stretchable-height #f]
                            [stretchable-width #t]
                            [alignment '(center center)]))

    (define cbus-panel (new vertical-panel%
                            [parent cbus-group]))

    (define choice-slider
      (map (lambda (i)
             (define cbus-output-panel
               (new horizontal-panel%
                    [parent cbus-group]))

             (define choice (new choice%
                                 [label (format "CBUS~s" i)]
                                 [parent cbus-output-panel]
                                 [choices '("Input" "Output")]
                                 [selection 1]
                                 [callback (lambda (choice event)
                                             (when (eq?  ftdi-status 'open)
                                               (define io (send choice get-string-selection))
                                               (send slider enable (eq? io "Output"))))]))

             (define slider (new slider%
                                 [label "L/H"]
                                 [parent cbus-output-panel]
                                 [min-value 0]
                                 [max-value 1]
                                 [init-value 0]))
             (list choice slider))
           (range 4)))

    (define cbus-choices (map (lambda (lis) (car lis)) choice-slider))
    (define cbus-sliders (map (lambda (lis) (cadr lis)) choice-slider))

    (define cbus-buttons-panel (new horizontal-panel%
                                    [parent cbus-group]))

    (define cbus-reset-button (new button%
                                   [parent cbus-buttons-panel]
                                   [label "reset"]
                                   [stretchable-width #t]
                                   [callback on-cbus-reset-clicked]))

    ;; (define cbus-status-button (new button%
    ;;                                 [parent cbus-buttons-panel]
    ;;                                 [label "status"]
    ;;                                 [stretchable-width #t]
    ;;                                 [callback on-cbus-status-clicked]))

    (define cbus-update-button (new button%
                                    [parent cbus-buttons-panel]
                                    [label "update"]
                                    [stretchable-width #t]
                                    [callback on-cbus-update-clicked]))

    (new message%
         [parent top-panel]
         [label "L:0, H:1"])

    (on-close)))

(define (run-gui)
  (define main-frame (new main-frame%))
  (send main-frame show #t)
  'ok )

(module+ main
  (run-gui))
