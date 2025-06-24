(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_BOUNTY_EXPIRED (err u105))
(define-constant ERR_BOUNTY_NOT_ACTIVE (err u106))
(define-constant ERR_ALREADY_SUBMITTED (err u107))

(define-data-var next-bounty-id uint u1)
(define-data-var next-submission-id uint u1)

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