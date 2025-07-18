(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_BOUNTY_EXPIRED (err u105))
(define-constant ERR_BOUNTY_NOT_ACTIVE (err u106))
(define-constant ERR_ALREADY_SUBMITTED (err u107))
(define-constant ERR_INVALID_RATING (err u108))
(define-constant ERR_CANNOT_RATE_SELF (err u109))
(define-constant ERR_ALREADY_RATED (err u110))
(define-constant ERR_NO_INTERACTION (err u111))

(define-data-var next-bounty-id uint u1)
(define-data-var next-submission-id uint u1)
(define-data-var next-rating-id uint u1)

(define-map bounties
  { bounty-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward: uint,
    deadline: uint,
    status: (string-ascii 20),
    winner: (optional principal)
  }
)

(define-map submissions
  { submission-id: uint }
  {
    bounty-id: uint,
    submitter: principal,
    description: (string-ascii 500),
    submitted-at: uint,
    status: (string-ascii 20)
  }
)

(define-map bounty-submissions
  { bounty-id: uint, submitter: principal }
  { submission-id: uint }
)

(define-map user-bounties
  { creator: principal, bounty-id: uint }
  { exists: bool }
)

(define-map bounty-funds
  { bounty-id: uint }
  { amount: uint }
)

(define-map user-reputation
  { user: principal }
  {
    total-score: uint,
    completed-bounties: uint,
    successful-submissions: uint,
    average-rating: uint,
    total-ratings: uint
  }
)

(define-map user-ratings
  { rating-id: uint }
  {
    rater: principal,
    rated-user: principal,
    bounty-id: uint,
    rating: uint,
    comment: (string-ascii 200),
    timestamp: uint,
    rating-type: (string-ascii 20)
  }
)

(define-map user-rating-history
  { rater: principal, rated-user: principal, bounty-id: uint }
  { rating-id: uint }
)

(define-map user-interactions
  { user1: principal, user2: principal }
  { bounty-ids: (list 50 uint) }
)

(define-public (create-bounty (title (string-ascii 100)) (description (string-ascii 500)) (reward uint) (duration uint))
  (let
    (
      (bounty-id (var-get next-bounty-id))
      (deadline (+ stacks-block-height duration))
    )
    (asserts! (> reward u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
    (map-set bounties
      { bounty-id: bounty-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        reward: reward,
        deadline: deadline,
        status: "active",
        winner: none
      }
    )
    (map-set bounty-funds { bounty-id: bounty-id } { amount: reward })
    (map-set user-bounties { creator: tx-sender, bounty-id: bounty-id } { exists: true })
    (var-set next-bounty-id (+ bounty-id u1))
    (unwrap-panic (update-user-reputation-on-bounty-create tx-sender))
    (ok bounty-id)
  )
)

(define-public (submit-bug-report (bounty-id uint) (description (string-ascii 500)))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR_NOT_FOUND))
      (submission-id (var-get next-submission-id))
    )
    (asserts! (is-eq (get status bounty) "active") ERR_BOUNTY_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get deadline bounty)) ERR_BOUNTY_EXPIRED)
    (asserts! (is-none (map-get? bounty-submissions { bounty-id: bounty-id, submitter: tx-sender })) ERR_ALREADY_SUBMITTED)
    (map-set submissions
      { submission-id: submission-id }
      {
        bounty-id: bounty-id,
        submitter: tx-sender,
        description: description,
        submitted-at: stacks-block-height,
        status: "pending"
      }
    )
    (map-set bounty-submissions
      { bounty-id: bounty-id, submitter: tx-sender }
      { submission-id: submission-id }
    )
    (var-set next-submission-id (+ submission-id u1))
    (ok submission-id)
  )
)

(define-public (approve-submission (bounty-id uint) (submission-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR_NOT_FOUND))
      (submission (unwrap! (map-get? submissions { submission-id: submission-id }) ERR_NOT_FOUND))
      (bounty-fund (unwrap! (map-get? bounty-funds { bounty-id: bounty-id }) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status bounty) "active") ERR_BOUNTY_NOT_ACTIVE)
    (asserts! (is-eq (get bounty-id submission) bounty-id) ERR_NOT_FOUND)
    (try! (as-contract (stx-transfer? (get reward bounty) tx-sender (get submitter submission))))
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "completed", winner: (some (get submitter submission)) })
    )
    (map-set submissions
      { submission-id: submission-id }
      (merge submission { status: "approved" })
    )
    (map-delete bounty-funds { bounty-id: bounty-id })
    (unwrap-panic (update-user-reputation-on-submission-approve (get submitter submission) (get creator bounty)))
    (unwrap-panic (record-user-interaction (get creator bounty) (get submitter submission) bounty-id))
    (ok true)
  )
)

(define-public (reject-submission (bounty-id uint) (submission-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR_NOT_FOUND))
      (submission (unwrap! (map-get? submissions { submission-id: submission-id }) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get bounty-id submission) bounty-id) ERR_NOT_FOUND)
    (asserts! (is-eq (get status submission) "pending") ERR_NOT_FOUND)
    (map-set submissions
      { submission-id: submission-id }
      (merge submission { status: "rejected" })
    )
    (ok true)
  )
)

(define-public (cancel-bounty (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR_NOT_FOUND))
      (bounty-fund (unwrap! (map-get? bounty-funds { bounty-id: bounty-id }) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status bounty) "active") ERR_BOUNTY_NOT_ACTIVE)
    (try! (as-contract (stx-transfer? (get reward bounty) tx-sender (get creator bounty))))
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "cancelled" })
    )
    (map-delete bounty-funds { bounty-id: bounty-id })
    (ok true)
  )
)

(define-public (claim-expired-bounty (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR_NOT_FOUND))
      (bounty-fund (unwrap! (map-get? bounty-funds { bounty-id: bounty-id }) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status bounty) "active") ERR_BOUNTY_NOT_ACTIVE)
    (asserts! (>= stacks-block-height (get deadline bounty)) ERR_BOUNTY_EXPIRED)
    (try! (as-contract (stx-transfer? (get reward bounty) tx-sender (get creator bounty))))
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "expired" })
    )
    (map-delete bounty-funds { bounty-id: bounty-id })
    (ok true)
  )
)

(define-read-only (get-bounty (bounty-id uint))
  (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-submission (submission-id uint))
  (map-get? submissions { submission-id: submission-id })
)

(define-read-only (get-user-submission (bounty-id uint) (submitter principal))
  (match (map-get? bounty-submissions { bounty-id: bounty-id, submitter: submitter })
    entry (map-get? submissions { submission-id: (get submission-id entry) })
    none
  )
)

(define-read-only (get-bounty-fund (bounty-id uint))
  (map-get? bounty-funds { bounty-id: bounty-id })
)

(define-read-only (get-next-bounty-id)
  (var-get next-bounty-id)
)

(define-read-only (get-next-submission-id)
  (var-get next-submission-id)
)

(define-read-only (is-bounty-creator (bounty-id uint) (user principal))
  (match (map-get? bounties { bounty-id: bounty-id })
    bounty (is-eq user (get creator bounty))
    false
  )
)

(define-read-only (is-bounty-active (bounty-id uint))
  (match (map-get? bounties { bounty-id: bounty-id })
    bounty (and 
      (is-eq (get status bounty) "active")
      (< stacks-block-height (get deadline bounty))
    )
    false
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-public (rate-user (rated-user principal) (bounty-id uint) (rating uint) (comment (string-ascii 200)) (rating-type (string-ascii 20)))
  (let
    (
      (rating-id (var-get next-rating-id))
      (rater tx-sender)
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)
    (asserts! (not (is-eq rater rated-user)) ERR_CANNOT_RATE_SELF)
    (asserts! (is-none (map-get? user-rating-history { rater: rater, rated-user: rated-user, bounty-id: bounty-id })) ERR_ALREADY_RATED)
    (asserts! (has-user-interaction rater rated-user bounty-id) ERR_NO_INTERACTION)
    (map-set user-ratings
      { rating-id: rating-id }
      {
        rater: rater,
        rated-user: rated-user,
        bounty-id: bounty-id,
        rating: rating,
        comment: comment,
        timestamp: stacks-block-height,
        rating-type: rating-type
      }
    )
    (map-set user-rating-history
      { rater: rater, rated-user: rated-user, bounty-id: bounty-id }
      { rating-id: rating-id }
    )
    (var-set next-rating-id (+ rating-id u1))
    (unwrap-panic (update-user-reputation-rating rated-user rating))
    (ok rating-id)
  )
)

(define-private (update-user-reputation-on-bounty-create (user principal))
  (let
    (
      (current-rep (default-to 
        { total-score: u0, completed-bounties: u0, successful-submissions: u0, average-rating: u0, total-ratings: u0 }
        (map-get? user-reputation { user: user })
      ))
    )
    (map-set user-reputation
      { user: user }
      (merge current-rep { total-score: (+ (get total-score current-rep) u5) })
    )
    (ok true)
  )
)

(define-private (update-user-reputation-on-submission-approve (submitter principal) (creator principal))
  (let
    (
      (submitter-rep (default-to 
        { total-score: u0, completed-bounties: u0, successful-submissions: u0, average-rating: u0, total-ratings: u0 }
        (map-get? user-reputation { user: submitter })
      ))
      (creator-rep (default-to 
        { total-score: u0, completed-bounties: u0, successful-submissions: u0, average-rating: u0, total-ratings: u0 }
        (map-get? user-reputation { user: creator })
      ))
    )
    (map-set user-reputation
      { user: submitter }
      (merge submitter-rep { 
        total-score: (+ (get total-score submitter-rep) u10),
        successful-submissions: (+ (get successful-submissions submitter-rep) u1)
      })
    )
    (map-set user-reputation
      { user: creator }
      (merge creator-rep { 
        total-score: (+ (get total-score creator-rep) u3),
        completed-bounties: (+ (get completed-bounties creator-rep) u1)
      })
    )
    (ok true)
  )
)

(define-private (update-user-reputation-rating (user principal) (rating uint))
  (let
    (
      (current-rep (default-to 
        { total-score: u0, completed-bounties: u0, successful-submissions: u0, average-rating: u0, total-ratings: u0 }
        (map-get? user-reputation { user: user })
      ))
      (new-total-ratings (+ (get total-ratings current-rep) u1))
      (new-average (/ (+ (* (get average-rating current-rep) (get total-ratings current-rep)) rating) new-total-ratings))
    )
    (map-set user-reputation
      { user: user }
      (merge current-rep { 
        average-rating: new-average,
        total-ratings: new-total-ratings
      })
    )
    (ok true)
  )
)

(define-private (record-user-interaction (user1 principal) (user2 principal) (bounty-id uint))
  (let
    (
      (current-interactions (default-to 
        { bounty-ids: (list) }
        (map-get? user-interactions { user1: user1, user2: user2 })
      ))
      (updated-list (unwrap! (as-max-len? (append (get bounty-ids current-interactions) bounty-id) u50) (ok true)))
    )
    (map-set user-interactions
      { user1: user1, user2: user2 }
      { bounty-ids: updated-list }
    )
    (map-set user-interactions
      { user1: user2, user2: user1 }
      { bounty-ids: updated-list }
    )
    (ok true)
  )
)

(define-private (has-user-interaction (user1 principal) (user2 principal) (bounty-id uint))
  (match (map-get? user-interactions { user1: user1, user2: user2 })
    interactions (is-some (index-of (get bounty-ids interactions) bounty-id))
    false
  )
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-user-rating (rating-id uint))
  (map-get? user-ratings { rating-id: rating-id })
)

(define-read-only (get-user-rating-between (rater principal) (rated-user principal) (bounty-id uint))
  (match (map-get? user-rating-history { rater: rater, rated-user: rated-user, bounty-id: bounty-id })
    entry (map-get? user-ratings { rating-id: (get rating-id entry) })
    none
  )
)

(define-read-only (get-user-interactions (user1 principal) (user2 principal))
  (map-get? user-interactions { user1: user1, user2: user2 })
)

(define-read-only (calculate-trust-score (user principal))
  (match (map-get? user-reputation { user: user })
    rep (+ 
      (get total-score rep) 
      (* (get average-rating rep) u10)
      (* (get successful-submissions rep) u5)
      (* (get completed-bounties rep) u3)
    )
    u0
  )
)

(define-read-only (get-reputation-tier (user principal))
  (let ((trust-score (calculate-trust-score user)))
    (if (>= trust-score u1000) "legendary"
    (if (>= trust-score u500) "expert"
    (if (>= trust-score u200) "advanced"
    (if (>= trust-score u50) "intermediate"
    "novice"))))
  )
)

(define-read-only (get-next-rating-id)
  (var-get next-rating-id)
)