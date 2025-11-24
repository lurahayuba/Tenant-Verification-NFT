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
        (ok true)))
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

(define-constant err-invalid-dates (err u107))
(define-constant err-overlapping-rental (err u108))
(define-constant err-insufficient-funds (err u109))
(define-constant err-escrow-not-found (err u110))
(define-constant err-escrow-not-active (err u111))
(define-constant err-unauthorized-release (err u112))

(define-data-var rental-period-counter uint u0)
(define-data-var escrow-counter uint u0)

(define-map rental-periods
    uint
    {
        tenant: principal,
        landlord: principal,
        property-address: (string-ascii 100),
        start-date: uint,
        end-date: uint,
        monthly-rent: uint,
        created-at: uint
    }
)

(define-map tenant-rental-count
    principal
    uint
)

(define-map payment-escrows
    uint
    {
        tenant: principal,
        landlord: principal,
        amount: uint,
        purpose: (string-ascii 50),
        status: (string-ascii 20),
        created-at: uint,
        release-conditions-met: bool,
        auto-release-height: uint
    }
)

(define-public (record-rental-period
    (tenant principal)
    (property-address (string-ascii 100))
    (start-date uint)
    (end-date uint)
    (monthly-rent uint))
    (let 
        ((period-id (+ (var-get rental-period-counter) u1))
         (tenant-count (default-to u0 (map-get? tenant-rental-count tenant))))
        (asserts! (< start-date end-date) err-invalid-dates)
        (asserts! (> start-date u0) err-invalid-dates)
        (asserts! (is-none (check-rental-overlap tenant start-date end-date)) err-overlapping-rental)
        (var-set rental-period-counter period-id)
        (map-set rental-periods period-id {
            tenant: tenant,
            landlord: tx-sender,
            property-address: property-address,
            start-date: start-date,
            end-date: end-date,
            monthly-rent: monthly-rent,
            created-at: stacks-block-height
        })
        (map-set tenant-rental-count tenant (+ tenant-count u1))
        (ok period-id)))

(define-private (check-rental-overlap (tenant principal) (start-date uint) (end-date uint))
    none)



(define-read-only (get-rental-period (period-id uint))
    (ok (map-get? rental-periods period-id)))

(define-read-only (get-tenant-rental-periods (tenant principal))
    (list))



(define-read-only (get-tenant-rental-timeline (tenant principal))
    (let ((periods (get-tenant-rental-periods tenant)))
        (ok {
            total-periods: (len periods),
            rental-history: periods,
            current-status: (if (> (len periods) u0) "has-history" "no-history")
        })))

(define-read-only (validate-rental-continuity (tenant principal))
    (ok {
        has-gaps: false,
        consecutive-months: u0,
        period-count: u0
    }))

(define-private (check-timeline-gaps 
    (periods (list 10 {tenant: principal, landlord: principal, property-address: (string-ascii 100), start-date: uint, end-date: uint, monthly-rent: uint, created-at: uint})))
    (if (<= (len periods) u1)
        false
        true))

(define-private (calculate-total-rental-months 
    (periods (list 10 {tenant: principal, landlord: principal, property-address: (string-ascii 100), start-date: uint, end-date: uint, monthly-rent: uint, created-at: uint})))
    (fold sum-rental-duration periods u0))

(define-private (sum-rental-duration 
    (period {tenant: principal, landlord: principal, property-address: (string-ascii 100), start-date: uint, end-date: uint, monthly-rent: uint, created-at: uint})
    (total uint))
    (+ total (- (get end-date period) (get start-date period))))

(define-public (create-payment-escrow 
    (tenant principal)
    (amount uint)
    (purpose (string-ascii 50))
    (auto-release-blocks uint))
    (let 
        ((escrow-id (+ (var-get escrow-counter) u1)))
        (asserts! (> amount u0) err-insufficient-funds)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set escrow-counter escrow-id)
        (map-set payment-escrows escrow-id {
            tenant: tenant,
            landlord: tx-sender,
            amount: amount,
            purpose: purpose,
            status: "active",
            created-at: stacks-block-height,
            release-conditions-met: false,
            auto-release-height: (+ stacks-block-height auto-release-blocks)
        })
        (ok escrow-id)))

(define-public (release-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? payment-escrows escrow-id) err-escrow-not-found)))
        (asserts! (is-eq (get status escrow) "active") err-escrow-not-active)
        (asserts! (or 
            (is-eq tx-sender (get landlord escrow))
            (is-eq tx-sender (get tenant escrow))
            (>= stacks-block-height (get auto-release-height escrow))
        ) err-unauthorized-release)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get tenant escrow))))
        (map-set payment-escrows escrow-id
            (merge escrow {
                status: "released",
                release-conditions-met: true
            }))
        (ok true)))

(define-public (refund-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? payment-escrows escrow-id) err-escrow-not-found)))
        (asserts! (is-eq (get status escrow) "active") err-escrow-not-active)
        (asserts! (is-eq tx-sender (get landlord escrow)) err-unauthorized-release)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get landlord escrow))))
        (map-set payment-escrows escrow-id
            (merge escrow { status: "refunded" }))
        (ok true)))

(define-public (mark-conditions-met (escrow-id uint))
    (let ((escrow (unwrap! (map-get? payment-escrows escrow-id) err-escrow-not-found)))
        (asserts! (is-eq (get status escrow) "active") err-escrow-not-active)
        (asserts! (is-eq tx-sender (get landlord escrow)) err-unauthorized-release)
        (ok (map-set payment-escrows escrow-id
            (merge escrow { release-conditions-met: true })))))

(define-read-only (get-escrow-details (escrow-id uint))
    (ok (map-get? payment-escrows escrow-id)))

(define-read-only (get-escrow-status (escrow-id uint))
    (match (map-get? payment-escrows escrow-id)
        escrow (ok {
            status: (get status escrow),
            amount: (get amount escrow),
            can-release: (or 
                (get release-conditions-met escrow)
                (>= stacks-block-height (get auto-release-height escrow))
            ),
            blocks-until-auto-release: (if (>= stacks-block-height (get auto-release-height escrow))
                u0
                (- (get auto-release-height escrow) stacks-block-height))
        })
        (err err-escrow-not-found)))

(define-constant err-invalid-provider (err u113))
(define-constant err-check-not-found (err u114))
(define-constant err-check-expired (err u115))
(define-constant err-already-verified-provider (err u116))
(define-constant err-insufficient-score (err u117))
(define-constant err-check-already-exists (err u118))

(define-data-var background-check-counter uint u0)

(define-map authorized-providers
    principal
    {
        provider-name: (string-ascii 64),
        certification-level: uint,
        authorized-at: uint,
        total-checks-completed: uint,
        reliability-score: uint
    }
)

(define-map tenant-background-checks
    uint
    {
        tenant: principal,
        provider: principal,
        credit-score: uint,
        criminal-record-clear: bool,
        employment-verified: bool,
        income-verified: bool,
        previous-evictions: uint,
        identity-verified: bool,
        check-date: uint,
        expiry-date: uint,
        overall-score: uint,
        status: (string-ascii 20),
        verification-hash: (string-ascii 64)
    }
)

(define-map tenant-check-lookup
    principal
    uint
)

(define-map provider-check-requests
    { tenant: principal, provider: principal }
    {
        requested-at: uint,
        landlord: principal,
        status: (string-ascii 20),
        fee-paid: uint
    }
)

(define-public (authorize-background-provider 
    (provider principal)
    (provider-name (string-ascii 64))
    (certification-level uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= certification-level u5) err-invalid-provider)
        (asserts! (>= certification-level u1) err-invalid-provider)
        (ok (map-set authorized-providers provider {
            provider-name: provider-name,
            certification-level: certification-level,
            authorized-at: stacks-block-height,
            total-checks-completed: u0,
            reliability-score: u100
        }))))

(define-public (request-background-check (tenant principal) (provider principal) (fee uint))
    (let 
        ((provider-info (unwrap! (map-get? authorized-providers provider) err-invalid-provider))
         (request-key { tenant: tenant, provider: provider }))
        (asserts! (is-none (map-get? provider-check-requests request-key)) err-check-already-exists)
        (asserts! (>= fee u1000000) err-insufficient-funds)
        (try! (stx-transfer? fee tx-sender provider))
        (ok (map-set provider-check-requests request-key {
            requested-at: stacks-block-height,
            landlord: tx-sender,
            status: "pending",
            fee-paid: fee
        }))))

(define-public (submit-background-check 
    (tenant principal)
    (credit-score uint)
    (criminal-record-clear bool)
    (employment-verified bool)
    (income-verified bool)
    (previous-evictions uint)
    (identity-verified bool)
    (verification-hash (string-ascii 64)))
    (let 
        ((check-id (+ (var-get background-check-counter) u1))
         (provider-info (unwrap! (map-get? authorized-providers tx-sender) err-invalid-provider))
         (request-key { tenant: tenant, provider: tx-sender })
         (request-info (unwrap! (map-get? provider-check-requests request-key) err-check-not-found))
         (overall-score (calculate-background-score credit-score criminal-record-clear employment-verified income-verified previous-evictions identity-verified))
         (expiry-date (+ stacks-block-height u52560)))
        (asserts! (is-eq (get status request-info) "pending") err-check-not-found)
        (asserts! (<= credit-score u850) err-invalid-tenant)
        (asserts! (<= previous-evictions u10) err-invalid-tenant)
        (var-set background-check-counter check-id)
        (map-set tenant-background-checks check-id {
            tenant: tenant,
            provider: tx-sender,
            credit-score: credit-score,
            criminal-record-clear: criminal-record-clear,
            employment-verified: employment-verified,
            income-verified: income-verified,
            previous-evictions: previous-evictions,
            identity-verified: identity-verified,
            check-date: stacks-block-height,
            expiry-date: expiry-date,
            overall-score: overall-score,
            status: "completed",
            verification-hash: verification-hash
        })
        (map-set tenant-check-lookup tenant check-id)
        (map-set provider-check-requests request-key
            (merge request-info { status: "completed" }))
        (map-set authorized-providers tx-sender
            (merge provider-info {
                total-checks-completed: (+ (get total-checks-completed provider-info) u1)
            }))
        (ok check-id)))

(define-private (calculate-background-score 
    (credit-score uint)
    (criminal-clear bool)
    (employment-verified bool)
    (income-verified bool)
    (evictions uint)
    (identity-verified bool))
    (let 
        ((credit-weight (if (>= credit-score u700) u30 (if (>= credit-score u650) u20 u10)))
         (criminal-weight (if criminal-clear u25 u0))
         (employment-weight (if employment-verified u20 u0))
         (income-weight (if income-verified u15 u0))
         (eviction-weight (if (is-eq evictions u0) u10 u0))
         (identity-weight (if identity-verified u10 u0)))
        (+ credit-weight criminal-weight employment-weight income-weight eviction-weight identity-weight)))

(define-public (verify-background-check (tenant principal) (expected-hash (string-ascii 64)))
    (let 
        ((check-id (unwrap! (map-get? tenant-check-lookup tenant) err-check-not-found))
         (check-data (unwrap! (map-get? tenant-background-checks check-id) err-check-not-found)))
        (asserts! (is-eq (get verification-hash check-data) expected-hash) err-invalid-tenant)
        (asserts! (< stacks-block-height (get expiry-date check-data)) err-check-expired)
        (ok {
            verified: true,
            score: (get overall-score check-data),
            provider: (get provider check-data),
            check-date: (get check-date check-data)
        })))

(define-public (update-provider-reliability (provider principal) (new-score uint))
    (let ((provider-info (unwrap! (map-get? authorized-providers provider) err-invalid-provider)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-score u100) err-invalid-tenant)
        (ok (map-set authorized-providers provider
            (merge provider-info { reliability-score: new-score })))))

(define-read-only (get-background-check (tenant principal))
    (match (map-get? tenant-check-lookup tenant)
        check-id (ok (map-get? tenant-background-checks check-id))
        (ok none)))

(define-read-only (get-provider-info (provider principal))
    (ok (map-get? authorized-providers provider)))

(define-read-only (get-check-request-status (tenant principal) (provider principal))
    (ok (map-get? provider-check-requests { tenant: tenant, provider: provider })))

(define-read-only (is-background-check-valid (tenant principal))
    (match (map-get? tenant-check-lookup tenant)
        check-id (match (map-get? tenant-background-checks check-id)
            check-data (ok {
                valid: (< stacks-block-height (get expiry-date check-data)),
                score: (get overall-score check-data),
                days-until-expiry: (if (< stacks-block-height (get expiry-date check-data))
                    (/ (- (get expiry-date check-data) stacks-block-height) u144)
                    u0),
                risk-level: (if (>= (get overall-score check-data) u80) "low" 
                               (if (>= (get overall-score check-data) u60) "medium" "high"))
            })
            (ok { valid: false, score: u0, days-until-expiry: u0, risk-level: "unknown" }))
        (ok { valid: false, score: u0, days-until-expiry: u0, risk-level: "unknown" })))

(define-read-only (get-tenant-screening-summary (tenant principal))
    (let 
        ((background-check (get-background-check tenant))
         (reputation-data (get-tenant-reputation tenant)))
        (ok {
            has-background-check: (is-some (unwrap-panic background-check)),
            has-reputation: (is-some (unwrap-panic reputation-data)),
            screening-complete: (and 
                (is-some (unwrap-panic background-check))
                (is-some (unwrap-panic reputation-data))
            )
        })))

(define-constant err-reward-claimed (err u119))
(define-constant err-insufficient-reputation (err u120))
(define-constant err-reward-pool-empty (err u121))
(define-constant err-no-rewards-available (err u122))

(define-data-var reward-pool uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var reward-cycle-length uint u4320)

(define-map landlord-reward-claims
    { landlord: principal, cycle: uint }
    {
        amount: uint,
        claimed-at: uint,
        reputation-snapshot: uint
    }
)

(define-map landlord-reward-eligibility
    principal
    {
        last-claim-cycle: uint,
        total-rewards-earned: uint,
        reward-tier: uint
    }
)

(define-public (fund-reward-pool (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set reward-pool (+ (var-get reward-pool) amount))
        (ok true)))

(define-public (claim-landlord-reward)
    (let 
        ((current-cycle (/ stacks-block-height (var-get reward-cycle-length)))
         (landlord-rep (unwrap! (map-get? landlord-reputation tx-sender) err-insufficient-reputation))
         (claim-key { landlord: tx-sender, cycle: current-cycle })
         (eligibility (default-to 
             { last-claim-cycle: u0, total-rewards-earned: u0, reward-tier: u1 }
             (map-get? landlord-reward-eligibility tx-sender)))
         (reward-amount (calculate-landlord-reward 
             (get total-verifications-given landlord-rep)
             (get reputation-weight landlord-rep))))
        (asserts! (> (get total-verifications-given landlord-rep) u4) err-insufficient-reputation)
        (asserts! (>= (get reputation-weight landlord-rep) u3) err-insufficient-reputation)
        (asserts! (not (is-eq (get last-claim-cycle eligibility) current-cycle)) err-reward-claimed)
        (asserts! (> reward-amount u0) err-no-rewards-available)
        (asserts! (>= (var-get reward-pool) reward-amount) err-reward-pool-empty)
        (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
        (var-set reward-pool (- (var-get reward-pool) reward-amount))
        (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward-amount))
        (map-set landlord-reward-claims claim-key {
            amount: reward-amount,
            claimed-at: stacks-block-height,
            reputation-snapshot: (get reputation-weight landlord-rep)
        })
        (map-set landlord-reward-eligibility tx-sender {
            last-claim-cycle: current-cycle,
            total-rewards-earned: (+ (get total-rewards-earned eligibility) reward-amount),
            reward-tier: (calculate-reward-tier (get total-verifications-given landlord-rep))
        })
        (ok reward-amount)))

(define-private (calculate-landlord-reward (verifications-given uint) (reputation-weight uint))
    (let 
        ((base-reward u1000000)
         (verification-multiplier (if (>= verifications-given u20) u3
                                   (if (>= verifications-given u10) u2 u1)))
         (weight-multiplier reputation-weight))
        (* (* base-reward verification-multiplier) weight-multiplier)))

(define-private (calculate-reward-tier (verifications uint))
    (if (>= verifications u50) u5
    (if (>= verifications u30) u4
    (if (>= verifications u20) u3
    (if (>= verifications u10) u2 u1)))))

(define-public (update-reward-cycle-length (new-length uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= new-length u144) err-invalid-tenant)
        (var-set reward-cycle-length new-length)
        (ok true)))

(define-read-only (get-reward-pool-balance)
    (ok (var-get reward-pool)))

(define-read-only (get-landlord-reward-info (landlord principal))
    (let 
        ((eligibility (map-get? landlord-reward-eligibility landlord))
         (landlord-rep (map-get? landlord-reputation landlord))
         (current-cycle (/ stacks-block-height (var-get reward-cycle-length))))
        (ok {
            eligibility-data: eligibility,
            reputation-data: landlord-rep,
            current-cycle: current-cycle,
            can-claim: (match landlord-rep
                rep (and 
                    (> (get total-verifications-given rep) u4)
                    (>= (get reputation-weight rep) u3)
                    (match eligibility
                        elig (not (is-eq (get last-claim-cycle elig) current-cycle))
                        true))
                false)
        })))

(define-read-only (get-potential-reward (landlord principal))
    (match (map-get? landlord-reputation landlord)
        rep (ok (some (calculate-landlord-reward 
            (get total-verifications-given rep)
            (get reputation-weight rep))))
        (ok none)))

(define-read-only (get-reward-claim-history (landlord principal) (cycle uint))
    (ok (map-get? landlord-reward-claims { landlord: landlord, cycle: cycle })))

(define-read-only (get-reward-stats)
    (ok {
        pool-balance: (var-get reward-pool),
        total-distributed: (var-get total-rewards-distributed),
        cycle-length: (var-get reward-cycle-length),
        current-cycle: (/ stacks-block-height (var-get reward-cycle-length))
    }))

(define-constant err-transfer-not-approved (err u123))
(define-constant err-sublease-not-allowed (err u124))
(define-constant err-sublease-exists (err u125))
(define-constant err-invalid-transfer (err u126))

(define-data-var transfer-counter uint u0)

(define-map transfer-approvals
    uint
    {
        from-tenant: principal,
        to-tenant: principal,
        landlord: principal,
        approved-by-landlord: bool,
        approved-by-from-tenant: bool,
        transfer-fee: uint,
        requested-at: uint,
        expires-at: uint,
        status: (string-ascii 20)
    }
)

(define-map token-transfer-lookup
    uint
    uint
)

(define-map sublease-agreements
    uint
    {
        token-id: uint,
        primary-tenant: principal,
        sublease-tenant: principal,
        landlord: principal,
        start-date: uint,
        end-date: uint,
        sublease-rent: uint,
        approved: bool,
        created-at: uint,
        status: (string-ascii 20)
    }
)

(define-map active-subleases
    uint
    uint
)

(define-public (request-tenant-transfer
    (token-id uint)
    (to-tenant principal)
    (transfer-fee uint))
    (let
        ((profile (unwrap! (map-get? tenant-profiles token-id) err-invalid-tenant))
         (transfer-id (+ (var-get transfer-counter) u1))
         (expiry-height (+ stacks-block-height u1440)))
        (asserts! (is-eq tx-sender (get tenant profile)) err-not-token-owner)
        (asserts! (is-none (map-get? token-transfer-lookup token-id)) err-invalid-transfer)
        (asserts! (not (is-eq (get tenant profile) to-tenant)) err-invalid-transfer)
        (var-set transfer-counter transfer-id)
        (map-set transfer-approvals transfer-id {
            from-tenant: tx-sender,
            to-tenant: to-tenant,
            landlord: (get landlord profile),
            approved-by-landlord: false,
            approved-by-from-tenant: true,
            transfer-fee: transfer-fee,
            requested-at: stacks-block-height,
            expires-at: expiry-height,
            status: "pending"
        })
        (map-set token-transfer-lookup token-id transfer-id)
        (ok transfer-id)))

(define-public (approve-tenant-transfer (transfer-id uint))
    (let
        ((transfer (unwrap! (map-get? transfer-approvals transfer-id) err-invalid-transfer)))
        (asserts! (is-eq tx-sender (get landlord transfer)) err-owner-only)
        (asserts! (is-eq (get status transfer) "pending") err-invalid-transfer)
        (asserts! (< stacks-block-height (get expires-at transfer)) err-check-expired)
        (ok (map-set transfer-approvals transfer-id
            (merge transfer {
                approved-by-landlord: true,
                status: "approved"
            })))))

(define-public (execute-tenant-transfer (token-id uint))
    (let
        ((transfer-id (unwrap! (map-get? token-transfer-lookup token-id) err-invalid-transfer))
         (transfer (unwrap! (map-get? transfer-approvals transfer-id) err-invalid-transfer))
         (profile (unwrap! (map-get? tenant-profiles token-id) err-invalid-tenant)))
        (asserts! (is-eq (get status transfer) "approved") err-transfer-not-approved)
        (asserts! (get approved-by-landlord transfer) err-transfer-not-approved)
        (asserts! (< stacks-block-height (get expires-at transfer)) err-check-expired)
        (asserts! (is-eq tx-sender (get to-tenant transfer)) err-not-token-owner)
        (if (> (get transfer-fee transfer) u0)
            (try! (stx-transfer? (get transfer-fee transfer) tx-sender (get from-tenant transfer)))
            true)
        (try! (nft-transfer? tenant-verification token-id (get from-tenant transfer) (get to-tenant transfer)))
        (map-set tenant-profiles token-id
            (merge profile { tenant: (get to-tenant transfer) }))
        (map-set transfer-approvals transfer-id
            (merge transfer { status: "completed" }))
        (map-delete token-transfer-lookup token-id)
        (ok true)))

(define-public (request-sublease
    (token-id uint)
    (sublease-tenant principal)
    (start-date uint)
    (end-date uint)
    (sublease-rent uint))
    (let
        ((profile (unwrap! (map-get? tenant-profiles token-id) err-invalid-tenant))
         (sublease-id (+ (var-get transfer-counter) u1)))
        (asserts! (is-eq tx-sender (get tenant profile)) err-not-token-owner)
        (asserts! (is-none (map-get? active-subleases token-id)) err-sublease-exists)
        (asserts! (< start-date end-date) err-invalid-dates)
        (asserts! (< stacks-block-height end-date) err-invalid-dates)
        (var-set transfer-counter sublease-id)
        (map-set sublease-agreements sublease-id {
            token-id: token-id,
            primary-tenant: tx-sender,
            sublease-tenant: sublease-tenant,
            landlord: (get landlord profile),
            start-date: start-date,
            end-date: end-date,
            sublease-rent: sublease-rent,
            approved: false,
            created-at: stacks-block-height,
            status: "pending"
        })
        (map-set active-subleases token-id sublease-id)
        (ok sublease-id)))

(define-public (approve-sublease (sublease-id uint))
    (let
        ((sublease (unwrap! (map-get? sublease-agreements sublease-id) err-invalid-transfer)))
        (asserts! (is-eq tx-sender (get landlord sublease)) err-owner-only)
        (asserts! (is-eq (get status sublease) "pending") err-invalid-transfer)
        (ok (map-set sublease-agreements sublease-id
            (merge sublease {
                approved: true,
                status: "active"
            })))))

(define-public (terminate-sublease (sublease-id uint))
    (let
        ((sublease (unwrap! (map-get? sublease-agreements sublease-id) err-invalid-transfer)))
        (asserts! (or
            (is-eq tx-sender (get primary-tenant sublease))
            (is-eq tx-sender (get landlord sublease))
            (>= stacks-block-height (get end-date sublease)))
            err-owner-only)
        (map-set sublease-agreements sublease-id
            (merge sublease { status: "terminated" }))
        (map-delete active-subleases (get token-id sublease))
        (ok true)))

(define-public (cancel-transfer-request (transfer-id uint))
    (let
        ((transfer (unwrap! (map-get? transfer-approvals transfer-id) err-invalid-transfer)))
        (asserts! (is-eq tx-sender (get from-tenant transfer)) err-not-token-owner)
        (asserts! (is-eq (get status transfer) "pending") err-invalid-transfer)
        (map-set transfer-approvals transfer-id
            (merge transfer { status: "cancelled" }))
        (ok true)))

(define-read-only (get-transfer-details (transfer-id uint))
    (ok (map-get? transfer-approvals transfer-id)))

(define-read-only (get-token-transfer-request (token-id uint))
    (match (map-get? token-transfer-lookup token-id)
        transfer-id (ok (map-get? transfer-approvals transfer-id))
        (ok none)))

(define-read-only (get-sublease-details (sublease-id uint))
    (ok (map-get? sublease-agreements sublease-id)))

(define-read-only (get-active-sublease (token-id uint))
    (match (map-get? active-subleases token-id)
        sublease-id (ok (map-get? sublease-agreements sublease-id))
        (ok none)))

(define-read-only (is-token-transferable (token-id uint))
    (match (map-get? tenant-profiles token-id)
        profile (ok {
            transferable: (is-none (map-get? token-transfer-lookup token-id)),
            has-active-sublease: (is-some (map-get? active-subleases token-id)),
            current-owner: (get tenant profile)
        })
        (ok { transferable: false, has-active-sublease: false, current-owner: tx-sender })))
