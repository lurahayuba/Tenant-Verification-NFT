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
