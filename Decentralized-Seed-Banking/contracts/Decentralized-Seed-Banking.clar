;; Decentralized Seed Banking Contract
;; Preserve agricultural biodiversity with community participation

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SEED_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_ALREADY_EXISTS (err u104))
(define-constant ERR_INVALID_DATA (err u105))
(define-constant ERR_NOT_APPROVED (err u106))

;; Data Variables
(define-data-var next-seed-id uint u1)
(define-data-var total-seeds-preserved uint u0)
(define-data-var community-fund uint u0)

;; Data Maps
(define-map seeds
  uint ;; seed-id
  {
    name: (string-ascii 100),
    variety: (string-ascii 50),
    origin: (string-ascii 100),
    depositor: principal,
    quantity: uint,
    preservation-date: uint,
    genetic-info: (string-ascii 200),
    verified: bool,
    active: bool
  }
)

(define-map seed-requests
  uint ;; request-id
  {
    requester: principal,
    seed-id: uint,
    quantity: uint,
    purpose: (string-ascii 200),
    approved: bool,
    fulfilled: bool
  }
)

(define-map user-contributions
  principal
  {
    seeds-deposited: uint,
    seeds-requested: uint,
    reputation-score: uint,
    total-contribution: uint
  }
)

(define-map seed-exchanges
  uint ;; exchange-id
  {
    from-user: principal,
    to-user: principal,
    seed-id: uint,
    quantity: uint,
    exchange-date: uint,
    status: (string-ascii 20)
  }
)

(define-map community-votes
  {seed-id: uint, voter: principal}
  {vote: bool, timestamp: uint}
)

(define-map seed-ratings
  uint ;; seed-id
  {
    total-ratings: uint,
    average-score: uint,
    rating-count: uint
  }
)

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (update-user-stats (user principal) (seeds-count uint))
  (let (
    (current-stats (default-to 
      {seeds-deposited: u0, seeds-requested: u0, reputation-score: u0, total-contribution: u0}
      (map-get? user-contributions user)
    ))
  )
    (map-set user-contributions user
      (merge current-stats {
        seeds-deposited: (+ (get seeds-deposited current-stats) seeds-count),
        total-contribution: (+ (get total-contribution current-stats) u1),
        reputation-score: (+ (get reputation-score current-stats) u10)
      })
    )
  )
)

(define-private (calculate-preservation-reward (quantity uint))
  (if (>= quantity u100)
    u1000000 ;; 1 STX for large deposits
    (if (>= quantity u50)
      u500000  ;; 0.5 STX for medium deposits
      u100000  ;; 0.1 STX for small deposits
    )
  )
)

;; Public Functions

;; Deposit seeds into the bank
(define-public (deposit-seeds 
  (name (string-ascii 100))
  (variety (string-ascii 50))
  (origin (string-ascii 100))
  (quantity uint)
  (genetic-info (string-ascii 200))
)
  (let (
    (seed-id (var-get next-seed-id))
    (reward (calculate-preservation-reward quantity))
  )
    (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
    (asserts! (> (len name) u0) ERR_INVALID_DATA)
    
    ;; Store seed information
    (map-set seeds seed-id {
      name: name,
      variety: variety,
      origin: origin,
      depositor: tx-sender,
      quantity: quantity,
      preservation-date: block-height,
      genetic-info: genetic-info,
      verified: false,
      active: true
    })
    
    ;; Initialize seed ratings
    (map-set seed-ratings seed-id {
      total-ratings: u0,
      average-score: u0,
      rating-count: u0
    })
    
    ;; Update statistics
    (var-set next-seed-id (+ seed-id u1))
    (var-set total-seeds-preserved (+ (var-get total-seeds-preserved) u1))
    (update-user-stats tx-sender u1)
    
    ;; Transfer reward to depositor
    (try! (stx-transfer? reward (as-contract tx-sender) tx-sender))
    
    (ok seed-id)
  )
)

;; Request seeds from the bank
(define-public (request-seeds 
  (seed-id uint)
  (quantity uint)
  (purpose (string-ascii 200))
)
  (let (
    (seed-info (unwrap! (map-get? seeds seed-id) ERR_SEED_NOT_FOUND))
    (request-id (+ (* seed-id u1000) (var-get next-seed-id)))
  )
    (asserts! (get active seed-info) ERR_SEED_NOT_FOUND)
    (asserts! (<= quantity (get quantity seed-info)) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> quantity u0) ERR_INVALID_AMOUNT)
    
    ;; Create seed request
    (map-set seed-requests request-id {
      requester: tx-sender,
      seed-id: seed-id,
      quantity: quantity,
      purpose: purpose,
      approved: false,
      fulfilled: false
    })
    
    ;; Update user stats
    (let (
      (current-stats (default-to 
        {seeds-deposited: u0, seeds-requested: u0, reputation-score: u0, total-contribution: u0}
        (map-get? user-contributions tx-sender)
      ))
    )
      (map-set user-contributions tx-sender
        (merge current-stats {
          seeds-requested: (+ (get seeds-requested current-stats) u1)
        })
      )
    )
    
    (ok request-id)
  )
)

;; Approve seed request (by seed owner or contract owner)
(define-public (approve-request (request-id uint))
  (let (
    (request-info (unwrap! (map-get? seed-requests request-id) ERR_SEED_NOT_FOUND))
    (seed-info (unwrap! (map-get? seeds (get seed-id request-info)) ERR_SEED_NOT_FOUND))
  )
    (asserts! 
      (or (is-eq tx-sender (get depositor seed-info)) (is-contract-owner))
      ERR_UNAUTHORIZED
    )
    
    ;; Update request status
    (map-set seed-requests request-id
      (merge request-info {approved: true})
    )
    
    (ok true)
  )
)

;; Fulfill approved seed request
(define-public (fulfill-request (request-id uint))
  (let (
    (request-info (unwrap! (map-get? seed-requests request-id) ERR_SEED_NOT_FOUND))
    (seed-info (unwrap! (map-get? seeds (get seed-id request-info)) ERR_SEED_NOT_FOUND))
    (exchange-id (+ request-id (var-get next-seed-id)))
  )
    (asserts! (get approved request-info) ERR_NOT_APPROVED)
    (asserts! (not (get fulfilled request-info)) ERR_ALREADY_EXISTS)
    
    ;; Update seed quantity
    (map-set seeds (get seed-id request-info)
      (merge seed-info {
        quantity: (- (get quantity seed-info) (get quantity request-info))
      })
    )
    
    ;; Mark request as fulfilled
    (map-set seed-requests request-id
      (merge request-info {fulfilled: true})
    )
    
    ;; Record exchange
    (map-set seed-exchanges exchange-id {
      from-user: (get depositor seed-info),
      to-user: (get requester request-info),
      seed-id: (get seed-id request-info),
      quantity: (get quantity request-info),
      exchange-date: block-height,
      status: "completed"
    })
    
    (ok exchange-id)
  )
)

;; Verify seed authenticity (by contract owner or community vote)
(define-public (verify-seed (seed-id uint))
  (let (
    (seed-info (unwrap! (map-get? seeds seed-id) ERR_SEED_NOT_FOUND))
  )
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (map-set seeds seed-id
      (merge seed-info {verified: true})
    )
    
    ;; Reward depositor for verified seed
    (try! (stx-transfer? u2000000 (as-contract tx-sender) (get depositor seed-info)))
    
    (ok true)
  )
)

;; Community voting for seed verification
(define-public (vote-for-seed (seed-id uint) (approve bool))
  (let (
    (seed-info (unwrap! (map-get? seeds seed-id) ERR_SEED_NOT_FOUND))
    (vote-key {seed-id: seed-id, voter: tx-sender})
  )
    (asserts! (get active seed-info) ERR_SEED_NOT_FOUND)
    (asserts! (is-none (map-get? community-votes vote-key)) ERR_ALREADY_EXISTS)
    
    ;; Record vote
    (map-set community-votes vote-key {
      vote: approve,
      timestamp: block-height
    })
    
    (ok true)
  )
)

;; Rate seed quality
(define-public (rate-seed (seed-id uint) (rating uint))
  (let (
    (seed-info (unwrap! (map-get? seeds seed-id) ERR_SEED_NOT_FOUND))
    (current-rating (default-to 
      {total-ratings: u0, average-score: u0, rating-count: u0}
      (map-get? seed-ratings seed-id)
    ))
  )
    (asserts! (get verified seed-info) ERR_NOT_APPROVED)
    (asserts! (<= rating u5) ERR_INVALID_AMOUNT)
    (asserts! (>= rating u1) ERR_INVALID_AMOUNT)
    
    (let (
      (new-total (+ (get total-ratings current-rating) rating))
      (new-count (+ (get rating-count current-rating) u1))
      (new-average (/ new-total new-count))
    )
      (map-set seed-ratings seed-id {
        total-ratings: new-total,
        average-score: new-average,
        rating-count: new-count
      })
    )
    
    (ok true)
  )
)

;; Contribute to community fund
(define-public (contribute-to-fund (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set community-fund (+ (var-get community-fund) amount))
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-seed-info (seed-id uint))
  (map-get? seeds seed-id)
)

(define-read-only (get-seed-rating (seed-id uint))
  (map-get? seed-ratings seed-id)
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-contributions user)
)

(define-read-only (get-request-info (request-id uint))
  (map-get? seed-requests request-id)
)

(define-read-only (get-total-seeds-preserved)
  (var-get total-seeds-preserved)
)

(define-read-only (get-community-fund-balance)
  (var-get community-fund)
)

(define-read-only (get-exchange-info (exchange-id uint))
  (map-get? seed-exchanges exchange-id)
)