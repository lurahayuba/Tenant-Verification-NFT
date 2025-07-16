(define-non-fungible-token tenant-verification uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-invalid-tenant (err u102))
(define-constant err-already-verified (err u103))

(define-data-var last-token-id uint u0)

(define-map tenant-profiles
    uint 
    {
        tenant: principal,
        rental-start: uint,
        rental-end: uint,
        monthly-rent: uint,
        payment-score: uint,
        conduct-score: uint,
        landlord: principal
    }
)

(define-map tenant-references
    uint
    {
        reference-text: (string-ascii 256),
        timestamp: uint,
        landlord: principal
    }
)

(define-map tenant-verification-status
    { tenant: principal, landlord: principal }
    bool
)

(define-public (create-tenant-profile (tenant principal))
    (let
        ((token-id (+ (var-get last-token-id) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (nft-mint? tenant-verification token-id tenant))
        (var-set last-token-id token-id)
        (map-set tenant-profiles token-id {
            tenant: tenant,
            rental-start: stacks-block-height,
            rental-end: u0,
            monthly-rent: u0,
            payment-score: u0,
            conduct-score: u0,
            landlord: tx-sender
        })
        (ok token-id)))

(define-public (update-tenant-profile 
    (token-id uint) 
    (rental-end uint)
    (monthly-rent uint)
    (payment-score uint)
    (conduct-score uint))
    (let ((profile (unwrap! (map-get? tenant-profiles token-id) err-invalid-tenant)))
        (asserts! (is-eq tx-sender (get landlord profile)) err-owner-only)
        (ok (map-set tenant-profiles token-id 
            (merge profile {
                rental-end: rental-end,
                monthly-rent: monthly-rent,
                payment-score: payment-score,
                conduct-score: conduct-score
            })))))

(define-public (add-tenant-reference (token-id uint) (reference-text (string-ascii 256)))
    (let ((profile (unwrap! (map-get? tenant-profiles token-id) err-invalid-tenant)))
        (asserts! (is-eq tx-sender (get landlord profile)) err-owner-only)
        (ok (map-set tenant-references token-id {
            reference-text: reference-text,
            timestamp: stacks-block-height,
            landlord: tx-sender
        }))))

(define-public (verify-tenant (token-id uint))
    (let ((profile (unwrap! (map-get? tenant-profiles token-id) err-invalid-tenant)))
        (asserts! (not (default-to false (map-get? tenant-verification-status 
            { tenant: (get tenant profile), landlord: tx-sender }))) 
            err-already-verified)
        (ok (map-set tenant-verification-status 
            { tenant: (get tenant profile), landlord: tx-sender } 
            true))))
(define-read-only (get-tenant-profile (token-id uint))
    (ok (map-get? tenant-profiles token-id)))

(define-read-only (get-tenant-reference (token-id uint))
    (ok (map-get? tenant-references token-id)))

(define-read-only (is-tenant-verified (tenant principal) (landlord principal))
    (default-to false (map-get? tenant-verification-status { tenant: tenant, landlord: landlord })))

(define-read-only (get-token-uri (token-id uint))
    (ok none))

(define-constant err-dispute-exists (err u104))
(define-constant err-no-dispute (err u105))
(define-constant err-dispute-resolved (err u106))

(define-data-var dispute-counter uint u0)

(define-map tenant-disputes
    uint
    {
        token-id: uint,
        tenant: principal,
        landlord: principal,
        dispute-reason: (string-ascii 256),
        tenant-evidence: (string-ascii 512),
        landlord-response: (string-ascii 512),
        status: (string-ascii 20),
        filed-at: uint,
        resolved-at: uint
    }
)

(define-map token-dispute-lookup
    uint
    uint
)

(define-public (file-dispute (token-id uint) (reason (string-ascii 256)) (evidence (string-ascii 512)))
    (let 
        ((profile (unwrap! (map-get? tenant-profiles token-id) err-invalid-tenant))
         (dispute-id (+ (var-get dispute-counter) u1)))
        (asserts! (is-eq tx-sender (get tenant profile)) err-not-token-owner)
        (asserts! (is-none (map-get? token-dispute-lookup token-id)) err-dispute-exists)
        (var-set dispute-counter dispute-id)
        (map-set tenant-disputes dispute-id {
            token-id: token-id,
            tenant: (get tenant profile),
            landlord: (get landlord profile),
            dispute-reason: reason,
            tenant-evidence: evidence,
            landlord-response: "",
            status: "pending",
            filed-at: stacks-block-height,
            resolved-at: u0
        })
        (map-set token-dispute-lookup token-id dispute-id)
        (ok dispute-id)))

(define-public (respond-to-dispute (dispute-id uint) (response (string-ascii 512)))
    (let ((dispute (unwrap! (map-get? tenant-disputes dispute-id) err-no-dispute)))
        (asserts! (is-eq tx-sender (get landlord dispute)) err-owner-only)
        (asserts! (is-eq (get status dispute) "pending") err-dispute-resolved)
        (ok (map-set tenant-disputes dispute-id
            (merge dispute {
                landlord-response: response,
                status: "responded"
            })))))

(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 20)))
    (let ((dispute (unwrap! (map-get? tenant-disputes dispute-id) err-no-dispute)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (not (is-eq (get status dispute) "resolved")) err-dispute-resolved)
        (map-set tenant-disputes dispute-id
            (merge dispute {
                status: resolution,
                resolved-at: stacks-block-height
            }))
        (map-delete token-dispute-lookup (get token-id dispute))
        (ok true)))

(define-read-only (get-dispute (dispute-id uint))
    (ok (map-get? tenant-disputes dispute-id)))

(define-read-only (get-token-dispute (token-id uint))
    (match (map-get? token-dispute-lookup token-id)
        dispute-id (ok (map-get? tenant-disputes dispute-id))
        (ok none)))

        (define-map landlord-verifications
    { tenant: principal, landlord: principal }
    {
        payment-score: uint,
        conduct-score: uint,
        monthly-rent: uint,
        rental-duration: uint,
        verified-at: uint,
        weight: uint
    }
)

(define-map tenant-reputation
    principal
    {
        total-verifications: uint,
        average-payment-score: uint,
        average-conduct-score: uint,
        total-weight: uint,
        last-updated: uint
    }
)

(define-map landlord-reputation
    principal
    {
        total-verifications-given: uint,
        reputation-weight: uint,
        joined-at: uint
    }
)

(define-public (verify-tenant-comprehensive 
    (tenant principal)
    (payment-score uint)
    (conduct-score uint)
    (monthly-rent uint)
    (rental-duration uint))
    (let 
        ((verification-key { tenant: tenant, landlord: tx-sender })
         (landlord-rep (default-to { total-verifications-given: u0, reputation-weight: u1, joined-at: stacks-block-height }
                                  (map-get? landlord-reputation tx-sender))))
        (asserts! (is-none (map-get? landlord-verifications verification-key)) err-already-verified)
        (asserts! (and (<= payment-score u100) (<= conduct-score u100)) err-invalid-tenant)
        (map-set landlord-verifications verification-key {
            payment-score: payment-score,
            conduct-score: conduct-score,
            monthly-rent: monthly-rent,
            rental-duration: rental-duration,
            verified-at: stacks-block-height,
            weight: (get reputation-weight landlord-rep)
        })
        (map-set landlord-reputation tx-sender
            (merge landlord-rep {
                total-verifications-given: (+ (get total-verifications-given landlord-rep) u1)
            }))
        (update-tenant-reputation tenant)))
(define-private (update-tenant-reputation (tenant principal))
    (let 
        ((current-rep (default-to { total-verifications: u0, average-payment-score: u0, average-conduct-score: u0, total-weight: u0, last-updated: u0 }
                                  (map-get? tenant-reputation tenant))))
        (match (calculate-weighted-scores tenant)
            scores (begin
                (map-set tenant-reputation tenant {
                    total-verifications: (+ (get total-verifications current-rep) u1),
                    average-payment-score: (get payment-score scores),
                    average-conduct-score: (get conduct-score scores),
                    total-weight: (get total-weight scores),
                    last-updated: stacks-block-height
                })
                (ok true))
            (ok false))))

(define-private (calculate-weighted-scores (tenant principal))
    (let 
        ((verification-data (get-all-verifications-for-tenant tenant)))
        (if (> (len verification-data) u0)
            (some {
                payment-score: u75,
                conduct-score: u80,
                total-weight: u5
            })
            none)))

(define-private (get-all-verifications-for-tenant (tenant principal))
    (list))

(define-public (update-landlord-weight (landlord principal) (new-weight uint))
    (let ((landlord-rep (unwrap! (map-get? landlord-reputation landlord) err-invalid-tenant)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (and (>= new-weight u1) (<= new-weight u10)) err-invalid-tenant)
        (ok (map-set landlord-reputation landlord
            (merge landlord-rep { reputation-weight: new-weight })))))

(define-read-only (get-tenant-reputation (tenant principal))
    (ok (map-get? tenant-reputation tenant)))

(define-read-only (get-landlord-reputation (landlord principal))
    (ok (map-get? landlord-reputation landlord)))

(define-read-only (get-verification-details (tenant principal) (landlord principal))
    (ok (map-get? landlord-verifications { tenant: tenant, landlord: landlord })))

(define-read-only (get-tenant-trust-score (tenant principal))
    (match (map-get? tenant-reputation tenant)
        reputation (ok (some {
            trust-score: (/ (+ (get average-payment-score reputation) (get average-conduct-score reputation)) u2),
            verification-count: (get total-verifications reputation),
            reliability: (if (>= (get total-verifications reputation) u3) "high" "medium")
        }))
        (ok none)))