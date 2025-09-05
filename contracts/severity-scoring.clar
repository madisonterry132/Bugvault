;; Bug Severity Classification and Scoring System
;; Categorizes vulnerabilities by severity and calculates risk scores for better bounty management

(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_SEVERITY (err u200))
(define-constant ERR_INVALID_SCORE (err u201))
(define-constant ERR_ALREADY_CLASSIFIED (err u202))
(define-constant ERR_BOUNTY_NOT_ACTIVE (err u203))

;; Severity level constants
(define-constant SEVERITY_CRITICAL u4)
(define-constant SEVERITY_HIGH u3)
(define-constant SEVERITY_MEDIUM u2)
(define-constant SEVERITY_LOW u1)
(define-constant SEVERITY_INFO u0)

;; Vulnerability categories
(define-constant VULN_RCE u0)           ;; Remote Code Execution
(define-constant VULN_SQLI u1)          ;; SQL Injection
(define-constant VULN_XSS u2)           ;; Cross-Site Scripting
(define-constant VULN_AUTHBYPASS u3)    ;; Authentication Bypass
(define-constant VULN_PRIVESC u4)       ;; Privilege Escalation
(define-constant VULN_DISCLOSURE u5)    ;; Information Disclosure
(define-constant VULN_DOS u6)           ;; Denial of Service
(define-constant VULN_CRYPTO u7)        ;; Cryptographic Issues
(define-constant VULN_LOGIC u8)         ;; Logic Flaws
(define-constant VULN_OTHER u9)         ;; Other

(define-data-var classification-counter uint u0)

;; Bug severity classifications
(define-map bug-classifications
  { submission-id: uint }
  {
    severity-level: uint,
    vulnerability-type: uint,
    cvss-base-score: uint,
    impact-score: uint,
    exploitability-score: uint,
    risk-score: uint,
    classifier: principal,
    classification-timestamp: uint,
    auto-classification: bool
  }
)

;; Severity multipliers for rewards
(define-map severity-multipliers
  { severity-level: uint }
  {
    reward-multiplier: uint,
    priority-score: uint,
    description: (string-ascii 50)
  }
)

;; Vulnerability type scoring
(define-map vulnerability-scoring
  { vuln-type: uint }
  {
    base-impact: uint,
    base-exploitability: uint,
    common-severity: uint,
    type-name: (string-ascii 30)
  }
)

;; Bounty severity requirements
(define-map bounty-severity-requirements
  { bounty-id: uint }
  {
    min-severity-level: uint,
    severity-bonus: uint,
    accepts-all-levels: bool,
    created-by: principal
  }
)

;; Classification history for appeals
(define-map classification-history
  { submission-id: uint, version: uint }
  {
    previous-severity: uint,
    new-severity: uint,
    changed-by: principal,
    change-reason: (string-ascii 200),
    timestamp: uint
  }
)

;; Initialize severity multipliers
(define-public (initialize-severity-system)
  (begin
    ;; Set severity multipliers
    (map-set severity-multipliers { severity-level: SEVERITY_CRITICAL }
      { reward-multiplier: u300, priority-score: u100, description: "Critical - Immediate action required" })
    (map-set severity-multipliers { severity-level: SEVERITY_HIGH }
      { reward-multiplier: u200, priority-score: u80, description: "High - Fix within 24-48 hours" })
    (map-set severity-multipliers { severity-level: SEVERITY_MEDIUM }
      { reward-multiplier: u150, priority-score: u60, description: "Medium - Fix within 1-2 weeks" })
    (map-set severity-multipliers { severity-level: SEVERITY_LOW }
      { reward-multiplier: u100, priority-score: u40, description: "Low - Fix when convenient" })
    (map-set severity-multipliers { severity-level: SEVERITY_INFO }
      { reward-multiplier: u50, priority-score: u20, description: "Informational - Best practice" })
    
    ;; Set vulnerability type scoring
    (map-set vulnerability-scoring { vuln-type: VULN_RCE }
      { base-impact: u90, base-exploitability: u85, common-severity: SEVERITY_CRITICAL, type-name: "Remote Code Execution" })
    (map-set vulnerability-scoring { vuln-type: VULN_AUTHBYPASS }
      { base-impact: u85, base-exploitability: u75, common-severity: SEVERITY_HIGH, type-name: "Authentication Bypass" })
    (map-set vulnerability-scoring { vuln-type: VULN_PRIVESC }
      { base-impact: u80, base-exploitability: u70, common-severity: SEVERITY_HIGH, type-name: "Privilege Escalation" })
    (map-set vulnerability-scoring { vuln-type: VULN_SQLI }
      { base-impact: u75, base-exploitability: u80, common-severity: SEVERITY_HIGH, type-name: "SQL Injection" })
    (map-set vulnerability-scoring { vuln-type: VULN_XSS }
      { base-impact: u60, base-exploitability: u70, common-severity: SEVERITY_MEDIUM, type-name: "Cross-Site Scripting" })
    (map-set vulnerability-scoring { vuln-type: VULN_DISCLOSURE }
      { base-impact: u50, base-exploitability: u40, common-severity: SEVERITY_MEDIUM, type-name: "Information Disclosure" })
    (map-set vulnerability-scoring { vuln-type: VULN_CRYPTO }
      { base-impact: u70, base-exploitability: u50, common-severity: SEVERITY_MEDIUM, type-name: "Cryptographic Issues" })
    (map-set vulnerability-scoring { vuln-type: VULN_DOS }
      { base-impact: u40, base-exploitability: u60, common-severity: SEVERITY_LOW, type-name: "Denial of Service" })
    (map-set vulnerability-scoring { vuln-type: VULN_LOGIC }
      { base-impact: u55, base-exploitability: u45, common-severity: SEVERITY_MEDIUM, type-name: "Logic Flaws" })
    (map-set vulnerability-scoring { vuln-type: VULN_OTHER }
      { base-impact: u30, base-exploitability: u30, common-severity: SEVERITY_LOW, type-name: "Other" })
    
    (ok true)
  )
)

;; Classify a bug submission
(define-public (classify-bug (submission-id uint) (severity-level uint) (vuln-type uint) (impact-score uint) (exploitability-score uint))
  (let
    (
      (submission (unwrap! (contract-call? .Bugvault get-submission submission-id) ERR_NOT_FOUND))
      (bounty-id (get bounty-id submission))
      (bounty (unwrap! (contract-call? .Bugvault get-bounty bounty-id) ERR_NOT_FOUND))
      (cvss-score (calculate-cvss-base-score impact-score exploitability-score))
      (risk-score (calculate-risk-score severity-level vuln-type impact-score exploitability-score))
    )
    ;; Only bounty creator or contract owner can classify
    (asserts! (or (is-eq tx-sender (get creator bounty)) 
                  (contract-call? .Bugvault is-bounty-creator bounty-id tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (<= severity-level SEVERITY_CRITICAL) ERR_INVALID_SEVERITY)
    (asserts! (<= vuln-type VULN_OTHER) ERR_INVALID_SEVERITY)
    (asserts! (and (<= impact-score u100) (<= exploitability-score u100)) ERR_INVALID_SCORE)
    (asserts! (is-none (map-get? bug-classifications { submission-id: submission-id })) ERR_ALREADY_CLASSIFIED)
    
    (map-set bug-classifications
      { submission-id: submission-id }
      {
        severity-level: severity-level,
        vulnerability-type: vuln-type,
        cvss-base-score: cvss-score,
        impact-score: impact-score,
        exploitability-score: exploitability-score,
        risk-score: risk-score,
        classifier: tx-sender,
        classification-timestamp: stacks-block-height,
        auto-classification: false
      }
    )
    
    (var-set classification-counter (+ (var-get classification-counter) u1))
    (ok risk-score)
  )
)

;; Auto-classify based on vulnerability type
(define-public (auto-classify-by-type (submission-id uint) (vuln-type uint))
  (let
    (
      (submission (unwrap! (contract-call? .Bugvault get-submission submission-id) ERR_NOT_FOUND))
      (vuln-data (unwrap! (map-get? vulnerability-scoring { vuln-type: vuln-type }) ERR_INVALID_SEVERITY))
      (suggested-severity (get common-severity vuln-data))
      (base-impact (get base-impact vuln-data))
      (base-exploitability (get base-exploitability vuln-data))
      (cvss-score (calculate-cvss-base-score base-impact base-exploitability))
      (risk-score (calculate-risk-score suggested-severity vuln-type base-impact base-exploitability))
    )
    (asserts! (<= vuln-type VULN_OTHER) ERR_INVALID_SEVERITY)
    (asserts! (is-none (map-get? bug-classifications { submission-id: submission-id })) ERR_ALREADY_CLASSIFIED)
    
    (map-set bug-classifications
      { submission-id: submission-id }
      {
        severity-level: suggested-severity,
        vulnerability-type: vuln-type,
        cvss-base-score: cvss-score,
        impact-score: base-impact,
        exploitability-score: base-exploitability,
        risk-score: risk-score,
        classifier: tx-sender,
        classification-timestamp: stacks-block-height,
        auto-classification: true
      }
    )
    
    (ok suggested-severity)
  )
)

;; Set bounty severity requirements
(define-public (set-bounty-severity-requirements (bounty-id uint) (min-severity uint) (severity-bonus uint) (accepts-all bool))
  (let
    (
      (bounty (unwrap! (contract-call? .Bugvault get-bounty bounty-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR_UNAUTHORIZED)
    (asserts! (<= min-severity SEVERITY_CRITICAL) ERR_INVALID_SEVERITY)
    
    (map-set bounty-severity-requirements
      { bounty-id: bounty-id }
      {
        min-severity-level: min-severity,
        severity-bonus: severity-bonus,
        accepts-all-levels: accepts-all,
        created-by: tx-sender
      }
    )
    (ok true)
  )
)

;; Calculate severity-adjusted reward
(define-public (calculate-severity-reward (bounty-id uint) (submission-id uint))
  (let
    (
      (bounty (unwrap! (contract-call? .Bugvault get-bounty bounty-id) ERR_NOT_FOUND))
      (classification (unwrap! (map-get? bug-classifications { submission-id: submission-id }) ERR_NOT_FOUND))
      (severity-multiplier (unwrap! (map-get? severity-multipliers { severity-level: (get severity-level classification) }) ERR_NOT_FOUND))
      (base-reward (if (get is-multi-submission bounty) (get reward-per-winner bounty) (get reward bounty)))
      (multiplier (get reward-multiplier severity-multiplier))
      (adjusted-reward (/ (* base-reward multiplier) u100))
    )
    (ok {
      base-reward: base-reward,
      severity-multiplier: multiplier,
      adjusted-reward: adjusted-reward,
      risk-score: (get risk-score classification)
    })
  )
)

;; Helper functions
(define-private (calculate-cvss-base-score (impact uint) (exploitability uint))
  (let
    (
      ;; Simplified CVSS calculation
      (impact-subscore (/ (* impact u395) u100))
      (exploitability-subscore (/ (* exploitability u821) u100))
      (sum-score (+ impact-subscore exploitability-subscore))
      (capped-sum (if (> sum-score u10000) u10000 sum-score))
      (base-score (if (is-eq impact-subscore u0) 
                    u0
                    (+ capped-sum (/ (* impact-subscore u15) u100))))
      (final-score (if (> base-score u10000) u10000 base-score))
    )
    final-score ;; Cap at 100.00
  )
)

(define-private (calculate-risk-score (severity uint) (vuln-type uint) (impact uint) (exploitability uint))
  (let
    (
      (severity-weight (get-severity-weight severity))
      (type-weight (get-vulnerability-weight vuln-type))
      (combined-score (+ (* severity-weight u40) (* type-weight u30) (* impact u2) (* exploitability u3)))
    )
    (/ combined-score u100)
  )
)

(define-private (get-severity-weight (severity uint))
  (if (is-eq severity SEVERITY_CRITICAL) u100
    (if (is-eq severity SEVERITY_HIGH) u80
      (if (is-eq severity SEVERITY_MEDIUM) u60
        (if (is-eq severity SEVERITY_LOW) u40 u20))))
)

(define-private (get-vulnerability-weight (vuln-type uint))
  (if (is-eq vuln-type VULN_RCE) u100
    (if (is-eq vuln-type VULN_AUTHBYPASS) u95
      (if (is-eq vuln-type VULN_PRIVESC) u90
        (if (is-eq vuln-type VULN_SQLI) u85
          (if (is-eq vuln-type VULN_CRYPTO) u70
            (if (is-eq vuln-type VULN_XSS) u60
              (if (is-eq vuln-type VULN_DISCLOSURE) u50
                (if (is-eq vuln-type VULN_DOS) u40
                  (if (is-eq vuln-type VULN_LOGIC) u55 u30)))))))))
)

;; Read-only functions
(define-read-only (get-bug-classification (submission-id uint))
  (map-get? bug-classifications { submission-id: submission-id })
)

(define-read-only (get-severity-multiplier (severity-level uint))
  (map-get? severity-multipliers { severity-level: severity-level })
)

(define-read-only (get-vulnerability-info (vuln-type uint))
  (map-get? vulnerability-scoring { vuln-type: vuln-type })
)

(define-read-only (get-bounty-severity-requirements (bounty-id uint))
  (map-get? bounty-severity-requirements { bounty-id: bounty-id })
)

(define-read-only (get-severity-name (severity-level uint))
  (if (is-eq severity-level SEVERITY_CRITICAL) "Critical"
    (if (is-eq severity-level SEVERITY_HIGH) "High"
      (if (is-eq severity-level SEVERITY_MEDIUM) "Medium"
        (if (is-eq severity-level SEVERITY_LOW) "Low" "Info"))))
)

(define-read-only (get-vulnerability-name (vuln-type uint))
  (match (map-get? vulnerability-scoring { vuln-type: vuln-type })
    data (get type-name data)
    "Unknown"
  )
)

(define-read-only (is-submission-classified (submission-id uint))
  (is-some (map-get? bug-classifications { submission-id: submission-id }))
)

(define-read-only (get-classification-stats)
  (ok {
    total-classifications: (var-get classification-counter),
    current-block: stacks-block-height
  })
)

(define-read-only (meets-bounty-requirements (bounty-id uint) (submission-id uint))
  (match (map-get? bounty-severity-requirements { bounty-id: bounty-id })
    requirements
      (match (map-get? bug-classifications { submission-id: submission-id })
        classification
          (or 
            (get accepts-all-levels requirements)
            (>= (get severity-level classification) (get min-severity-level requirements))
          )
        false
      )
    true ;; No requirements set, accept all
  )
)

(define-read-only (rank-submissions-by-severity (submission-ids (list 20 uint)))
  (let
    (
      (classified-submissions (filter is-submission-classified submission-ids))
    )
    (ok classified-submissions) ;; Simplified - would normally sort by risk score
  )
)
