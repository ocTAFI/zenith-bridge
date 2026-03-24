;; Zenith Bridge - Enterprise Blockchain Supply Chain Platform
;; Clarity Version 2, Epoch 2.1
;; Implements Chain-of-Custody DNA and Trust Gradient mechanisms

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-CHECKPOINT (err u102))
(define-constant ERR-INVALID-TRUST-SCORE (err u103))
(define-constant ERR-PRODUCT-ALREADY-EXISTS (err u104))
(define-constant ERR-STAKEHOLDER-NOT-FOUND (err u105))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Data structures

;; Product with Chain-of-Custody DNA
(define-map products
  { product-id: (string-ascii 64) }
  {
    dna-fingerprint: (buff 32),
    current-owner: principal,
    created-at: uint,
    current-checkpoint: uint,
    total-checkpoints: uint,
    industry-type: (string-ascii 32),
    is-active: bool
  }
)

;; Supply chain checkpoints
(define-map checkpoints
  { product-id: (string-ascii 64), checkpoint-id: uint }
  {
    location: (string-ascii 128),
    timestamp: uint,
    verifier: principal,
    dna-hash: (buff 32),
    iot-data-hash: (optional (buff 32)),
    geolocation: (optional (string-ascii 64)),
    compliance-status: (string-ascii 32)
  }
)

;; Stakeholder Trust Gradient scores
(define-map stakeholders
  { stakeholder: principal }
  {
    trust-score: uint,
    total-verifications: uint,
    successful-verifications: uint,
    registration-date: uint,
    industry-certifications: (list 5 (string-ascii 32))
  }
)

;; Compliance templates by industry
(define-map compliance-templates
  { industry: (string-ascii 32) }
  {
    required-checkpoints: uint,
    verification-threshold: uint,
    documentation-requirements: (list 10 (string-ascii 64))
  }
)

;; Read-only functions

;; Get product details
(define-read-only (get-product (product-id (string-ascii 64)))
  (ok (map-get? products { product-id: product-id }))
)

;; Get checkpoint details
(define-read-only (get-checkpoint (product-id (string-ascii 64)) (checkpoint-id uint))
  (ok (map-get? checkpoints { product-id: product-id, checkpoint-id: checkpoint-id }))
)

;; Get stakeholder trust score
(define-read-only (get-stakeholder-trust (stakeholder principal))
  (ok (map-get? stakeholders { stakeholder: stakeholder }))
)

;; Calculate verification requirements based on trust score
(define-read-only (get-verification-requirements (stakeholder principal))
  (let (
    (stakeholder-data (unwrap! (map-get? stakeholders { stakeholder: stakeholder }) 
                               (ok { enhanced-scrutiny: true, required-docs: u10 })))
  )
    (if (>= (get trust-score stakeholder-data) u80)
      (ok { enhanced-scrutiny: false, required-docs: u3 })
      (if (>= (get trust-score stakeholder-data) u50)
        (ok { enhanced-scrutiny: false, required-docs: u5 })
        (ok { enhanced-scrutiny: true, required-docs: u10 })
      )
    )
  )
)

;; Get compliance template
(define-read-only (get-compliance-template (industry (string-ascii 32)))
  (ok (map-get? compliance-templates { industry: industry }))
)

;; Check if product DNA is valid at checkpoint
(define-read-only (verify-dna-chain (product-id (string-ascii 64)) (checkpoint-id uint) (provided-hash (buff 32)))
  (let (
    (checkpoint-data (unwrap! (map-get? checkpoints { product-id: product-id, checkpoint-id: checkpoint-id })
                              (ok false)))
  )
    (ok (is-eq (get dna-hash checkpoint-data) provided-hash))
  )
)

;; Public functions

;; Register a new stakeholder
(define-public (register-stakeholder (certifications (list 5 (string-ascii 32))))
  (let (
    (caller tx-sender)
  )
    (ok (map-set stakeholders
      { stakeholder: caller }
      {
        trust-score: u50,
        total-verifications: u0,
        successful-verifications: u0,
        registration-date: block-height,
        industry-certifications: certifications
      }
    ))
  )
)

;; Create a new product with Chain-of-Custody DNA
(define-public (create-product 
                (product-id (string-ascii 64))
                (initial-dna (buff 32))
                (industry (string-ascii 32)))
  (let (
    (caller tx-sender)
  )
    (asserts! (is-none (map-get? products { product-id: product-id })) ERR-PRODUCT-ALREADY-EXISTS)
    (ok (map-set products
      { product-id: product-id }
      {
        dna-fingerprint: initial-dna,
        current-owner: caller,
        created-at: block-height,
        current-checkpoint: u0,
        total-checkpoints: u0,
        industry-type: industry,
        is-active: true
      }
    ))
  )
)

;; Add a supply chain checkpoint with DNA evolution
(define-public (add-checkpoint
                (product-id (string-ascii 64))
                (location (string-ascii 128))
                (new-dna-hash (buff 32))
                (iot-data (optional (buff 32)))
                (geo-location (optional (string-ascii 64)))
                (compliance-status (string-ascii 32)))
  (let (
    (caller tx-sender)
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
    (stakeholder-data (unwrap! (map-get? stakeholders { stakeholder: caller }) ERR-STAKEHOLDER-NOT-FOUND))
    (new-checkpoint-id (+ (get current-checkpoint product) u1))
  )
    (asserts! (get is-active product) ERR-INVALID-CHECKPOINT)
    
    ;; Record checkpoint
    (map-set checkpoints
      { product-id: product-id, checkpoint-id: new-checkpoint-id }
      {
        location: location,
        timestamp: block-height,
        verifier: caller,
        dna-hash: new-dna-hash,
        iot-data-hash: iot-data,
        geolocation: geo-location,
        compliance-status: compliance-status
      }
    )
    
    ;; Update product with evolved DNA
    (map-set products
      { product-id: product-id }
      (merge product {
        dna-fingerprint: new-dna-hash,
        current-checkpoint: new-checkpoint-id,
        total-checkpoints: (+ (get total-checkpoints product) u1)
      })
    )
    
    ;; Update stakeholder verification count
    (map-set stakeholders
      { stakeholder: caller }
      (merge stakeholder-data {
        total-verifications: (+ (get total-verifications stakeholder-data) u1)
      })
    )
    
    (ok new-checkpoint-id)
  )
)

;; Update Trust Gradient score for stakeholder
(define-public (update-trust-score (stakeholder principal) (successful bool))
  (let (
    (caller tx-sender)
    (stakeholder-data (unwrap! (map-get? stakeholders { stakeholder: stakeholder }) ERR-STAKEHOLDER-NOT-FOUND))
    (new-successful (if successful 
                       (+ (get successful-verifications stakeholder-data) u1)
                       (get successful-verifications stakeholder-data)))
    (new-total (+ (get total-verifications stakeholder-data) u1))
    (new-score (/ (* new-successful u100) new-total))
  )
    (asserts! (is-eq caller (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-score u100) ERR-INVALID-TRUST-SCORE)
    
    (ok (map-set stakeholders
      { stakeholder: stakeholder }
      (merge stakeholder-data {
        trust-score: new-score,
        successful-verifications: new-successful,
        total-verifications: new-total
      })
    ))
  )
)

;; Set compliance template for an industry
(define-public (set-compliance-template
                (industry (string-ascii 32))
                (required-checkpoints uint)
                (verification-threshold uint)
                (docs (list 10 (string-ascii 64))))
  (let (
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (map-set compliance-templates
      { industry: industry }
      {
        required-checkpoints: required-checkpoints,
        verification-threshold: verification-threshold,
        documentation-requirements: docs
      }
    ))
  )
)

;; Transfer product ownership
(define-public (transfer-product (product-id (string-ascii 64)) (new-owner principal))
  (let (
    (caller tx-sender)
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
  )
    (asserts! (is-eq caller (get current-owner product)) ERR-NOT-AUTHORIZED)
    (ok (map-set products
      { product-id: product-id }
      (merge product { current-owner: new-owner })
    ))
  )
)

;; Deactivate product (e.g., end of lifecycle)
(define-public (deactivate-product (product-id (string-ascii 64)))
  (let (
    (caller tx-sender)
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
  )
    (asserts! (is-eq caller (get current-owner product)) ERR-NOT-AUTHORIZED)
    (ok (map-set products
      { product-id: product-id }
      (merge product { is-active: false })
    ))
  )
)

;; Admin function to update contract owner
(define-public (set-contract-owner (new-owner principal))
  (let (
    (caller tx-sender)
  )
    (asserts! (is-eq caller (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)
