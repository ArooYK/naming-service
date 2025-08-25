;; naming-service.clar simple expiry-based name registry
;; - Names are strings (string-ascii 64)
;; - Payment is in STX; price-per-block is configurable
;; - Names expire (block-height based) and become claimable

(define-constant ERR_NAME_TAKEN u100)
(define-constant ERR_NOT_OWNER u101)
(define-constant ERR_BAD_DURATION u102)
(define-constant ERR_UNAUTHORIZED u103)
(define-constant ERR_NO_FUNDS u104)
(define-constant ERR_EVENT_ERROR u105)

(define-data-var price-per-block uint u1)
(define-data-var owner (optional principal) none)
(define-data-var event-nonce uint u0)

(define-map names 
  {name: (string-ascii 64)} 
  {owner: principal, expires: uint, resolver: (string-utf8 128)})

(define-map events uint {
  event-type: (string-ascii 16),
  name: (string-ascii 64),
  owner: principal,
  expires: (optional uint),
  price: (optional uint),
  old-owner: (optional principal),
  new-owner: (optional principal),
  resolver: (optional (string-utf8 128))
})

;; Event emission functions with proper response types
(define-private (emit-name-registered 
    (name-val (string-ascii 64)) 
    (owner-val principal) 
    (expires-val uint) 
    (price-val uint))
  (begin
    (map-set events (var-get event-nonce) {
      event-type: "registered",
      name: name-val,
      owner: owner-val,
      expires: (some expires-val),
      price: (some price-val),
      old-owner: none,
      new-owner: none,
      resolver: none
    })
    (var-set event-nonce (+ (var-get event-nonce) u1))
    (ok true)))

(define-private (emit-name-renewed
    (name-val (string-ascii 64)) 
    (owner-val principal) 
    (expires-val uint) 
    (price-val uint))
  (begin
    (map-set events (var-get event-nonce) {
      event-type: "renewed",
      name: name-val,
      owner: owner-val,
      expires: (some expires-val),
      price: (some price-val),
      old-owner: none,
      new-owner: none,
      resolver: none
    })
    (var-set event-nonce (+ (var-get event-nonce) u1))
    (ok true)))

(define-private (emit-name-transferred
    (name-val (string-ascii 64)) 
    (old-owner-val principal) 
    (new-owner-val principal))
  (begin
    (map-set events (var-get event-nonce) {
      event-type: "transferred",
      name: name-val,
      owner: old-owner-val,
      expires: none,
      price: none,
      old-owner: (some old-owner-val),
      new-owner: (some new-owner-val),
      resolver: none
    })
    (var-set event-nonce (+ (var-get event-nonce) u1))
    (ok true)))

(define-private (emit-resolver-set 
    (name-val (string-ascii 64)) 
    (owner-val principal) 
    (resolver-val (string-utf8 128)))
  (begin
        (map-set events (var-get event-nonce) {
          event-type: "resolver",
          name: name-val,
          owner: owner-val,
          expires: none,
          price: none,
          old-owner: none,
          new-owner: none,
          resolver: (some resolver-val)
        })
        (var-set event-nonce (+ (var-get event-nonce) u1))
        (ok true)))
    
    (define-private (create-name-record 
    (owner-val principal) 
    (expires-val uint) 
    (resolver-val (string-utf8 128)))
  {owner: owner-val, expires: expires-val, resolver: resolver-val})

(define-private (get-price-for-duration (duration-val uint))
  (let ((price-per-block-val (var-get price-per-block)))
    (* duration-val price-per-block-val)))

(define-public (bootstrap (admin principal))
  (begin 
    (asserts! (is-none (var-get owner)) (err ERR_UNAUTHORIZED))
    (var-set owner (some admin))
    (ok true)))

(define-read-only (get-owner) 
  (var-get owner))

(define-public (set-price (ppb uint))
  (let ((price-checked (begin 
                        (asserts! (> ppb u0) (err ERR_BAD_DURATION))
                        ppb)))
    (begin
      (asserts! (is-some (var-get owner)) (err ERR_UNAUTHORIZED))
      (asserts! (is-eq tx-sender (unwrap-panic (var-get owner))) (err ERR_UNAUTHORIZED))
      (var-set price-per-block price-checked)
      (ok true))))

(define-public (register-name (name-input (string-ascii 64)) (duration uint))
  (begin
    (asserts! (> duration u0) (err ERR_BAD_DURATION))
    (let ((name-key {name: name-input})
          (entry (map-get? names name-key))
          (current-height u0)
          (price (get-price-for-duration duration)))
      (if (is-some entry)
        (let ((current-entry (unwrap! entry (err ERR_NOT_OWNER))))
          (asserts! (<= (get expires current-entry) current-height) (err ERR_NAME_TAKEN))
          (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
          (let ((expires-val (+ current-height duration))
                (new-record {owner: tx-sender, expires: expires-val, resolver: u""}))
            (ok (begin
              (map-set names name-key new-record)
              (unwrap! (emit-name-registered name-input tx-sender expires-val price)
                      (err ERR_EVENT_ERROR))))))
        (begin
          (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
          (let ((expires-val (+ current-height duration))
                (new-record {owner: tx-sender, expires: expires-val, resolver: u""}))
            (ok (begin
              (map-set names name-key new-record)
              (unwrap! (emit-name-registered name-input tx-sender expires-val price)
                      (err ERR_EVENT_ERROR))))))))))

(define-public (renew-name (name (string-ascii 64)) (duration uint))
  (begin
    (asserts! (> duration u0) (err ERR_BAD_DURATION))
    (let ((entry (unwrap! (map-get? names {name: name}) (err ERR_NOT_OWNER)))
          (price (get-price-for-duration duration)))
      (asserts! (is-eq (get owner entry) tx-sender) (err ERR_NOT_OWNER))
      (let ((new-exp (+ (get expires entry) duration)))
        (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
        (let ((new-record {owner: tx-sender, expires: new-exp, resolver: (get resolver entry)}))
          (ok (begin
            (map-set names {name: name} new-record)
            (unwrap! (emit-name-renewed name tx-sender new-exp price)
                    (err ERR_EVENT_ERROR)))))))))

(define-public (transfer-name (name (string-ascii 64)) (to principal))
  (begin
    (let ((entry (unwrap! (map-get? names {name: name}) (err ERR_NOT_OWNER))))
      (asserts! (is-eq (get owner entry) tx-sender) (err ERR_NOT_OWNER))
      (let ((new-record {owner: to, expires: (get expires entry), resolver: (get resolver entry)}))
        (ok (begin 
          (map-set names {name: name} new-record)
          (unwrap! (emit-name-transferred name tx-sender to)
                  (err ERR_EVENT_ERROR))))))))

(define-public (set-resolver (name (string-ascii 64)) (resolver (string-utf8 128)))
  (begin
    (let ((entry (unwrap! (map-get? names {name: name}) (err ERR_NOT_OWNER))))
      (asserts! (is-eq (get owner entry) tx-sender) (err ERR_NOT_OWNER))
      (let ((new-record {owner: tx-sender, expires: (get expires entry), resolver: resolver}))
        (ok (begin
          (map-set names {name: name} new-record)
          (unwrap! (emit-resolver-set name tx-sender resolver)
                  (err ERR_EVENT_ERROR))))))))

(define-read-only (resolve (name (string-ascii 64)))
  (map-get? names {name: name}))

(define-public (withdraw-fees (amount uint))
  (begin
    (let ((current-owner (unwrap! (var-get owner) (err ERR_UNAUTHORIZED))))
      (asserts! (is-eq tx-sender current-owner) (err ERR_UNAUTHORIZED))
      (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) (err ERR_NO_FUNDS))
      (try! (stx-transfer? amount (as-contract tx-sender) tx-sender))
      (ok true))))