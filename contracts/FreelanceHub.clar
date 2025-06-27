;; FreelanceHub: Decentralized freelance marketplace with project bidding and payments
;; Connects skilled freelancers with clients for professional services

(define-data-var platform-manager principal tx-sender)
(define-map freelancer-profiles
  { freelancer-id: uint }
  {
    contractor: principal,
    project-rate: uint,
    skill-category: (string-ascii 50),
    portfolio-details: (string-ascii 500),
    completion-years: uint,
    verified: bool
  }
)

(define-map project-records
  { freelancer-id: uint, project-id: uint }
  {
    client: principal,
    contract-time: uint,
    project-type: (string-ascii 20)
  }
)

(define-data-var next-freelancer-id uint u1)
(define-map project-tracker 
  { freelancer-id: uint }
  { projects: uint }
)

;; Register as a freelancer
(define-public (register-freelancer (skill-input (string-ascii 50)) (portfolio-input (string-ascii 500)) (years-input uint) (rate-input uint))
  (let
    (
      (freelancer-id (var-get next-freelancer-id))
      (project-id u0)
      (skill skill-input)
      (portfolio portfolio-input)
      (years years-input)
      (rate rate-input)
    )
    ;; Input validation
    (asserts! (> rate u0) (err u1))
    (asserts! (> (len skill) u0) (err u5))
    (asserts! (> (len portfolio) u0) (err u6))
    (asserts! (> years u0) (err u7))
    
    (map-set freelancer-profiles
      { freelancer-id: freelancer-id }
      {
        contractor: tx-sender,
        project-rate: rate,
        skill-category: skill,
        portfolio-details: portfolio,
        completion-years: years,
        verified: false
      }
    )
    (map-set project-records
      { freelancer-id: freelancer-id, project-id: project-id }
      {
        client: tx-sender,
        contract-time: freelancer-id,
        project-type: "registered"
      }
    )
    (map-set project-tracker 
      { freelancer-id: freelancer-id }
      { projects: u1 }
    )
    (var-set next-freelancer-id (+ freelancer-id u1))
    (ok freelancer-id)
  )
)

;; Hire a freelancer
(define-public (hire-freelancer (freelancer-id-input uint))
  (let
    (
      (freelancer-id freelancer-id-input)
      (freelancer-info (unwrap! (map-get? freelancer-profiles { freelancer-id: freelancer-id }) (err u2)))
      (rate (get project-rate freelancer-info))
      (contractor (get contractor freelancer-info))
      (project-data (default-to { projects: u0 } (map-get? project-tracker { freelancer-id: freelancer-id })))
      (project-id (get projects project-data))
      (new-project-id (+ project-id u1))
    )
    ;; Input validation
    (asserts! (> freelancer-id u0) (err u8))
    (asserts! (not (is-eq tx-sender contractor)) (err u3))
    
    (try! (stx-transfer? rate tx-sender contractor))
    (map-set project-records
      { freelancer-id: freelancer-id, project-id: project-id }
      {
        client: tx-sender,
        contract-time: (var-get next-freelancer-id),
        project-type: "hired"
      }
    )
    (map-set project-tracker 
      { freelancer-id: freelancer-id }
      { projects: new-project-id }
    )
    (ok true)
  )
)

;; Verify a freelancer (platform manager only)
(define-public (verify-freelancer (freelancer-id-input uint))
  (let
    (
      (freelancer-id freelancer-id-input)
      (freelancer-info (unwrap! (map-get? freelancer-profiles { freelancer-id: freelancer-id }) (err u2)))
      (project-data (default-to { projects: u0 } (map-get? project-tracker { freelancer-id: freelancer-id })))
      (project-id (get projects project-data))
      (new-project-id (+ project-id u1))
    )
    ;; Input validation
    (asserts! (> freelancer-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get platform-manager)) (err u4))
    
    (map-set freelancer-profiles
      { freelancer-id: freelancer-id }
      (merge freelancer-info { verified: true })
    )
    (map-set project-records
      { freelancer-id: freelancer-id, project-id: project-id }
      {
        client: (get contractor freelancer-info),
        contract-time: (var-get next-freelancer-id),
        project-type: "verified"
      }
    )
    (map-set project-tracker 
      { freelancer-id: freelancer-id }
      { projects: new-project-id }
    )
    (ok true)
  )
)

;; Get freelancer profile
(define-read-only (get-freelancer (freelancer-id uint))
  (map-get? freelancer-profiles { freelancer-id: freelancer-id })
)

;; Get project record entry
(define-read-only (get-project-record (freelancer-id uint) (project-id uint))
  (map-get? project-records { freelancer-id: freelancer-id, project-id: project-id })
)

;; Get total projects for a freelancer
(define-read-only (get-project-count (freelancer-id uint))
  (let
    (
      (project-data (default-to { projects: u0 } (map-get? project-tracker { freelancer-id: freelancer-id })))
    )
    (get projects project-data)
  )
)
