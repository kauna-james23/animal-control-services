;; Animal Control Services - Pet Registry Contract
;; A comprehensive pet registration and licensing system for municipal animal services

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-UNAUTHORIZED (err u103))
(define-constant ERR-INVALID-STATUS (err u104))

;; Data Variables
(define-data-var next-pet-id uint u1)
(define-data-var next-license-id uint u1)
(define-data-var next-report-id uint u1)
(define-data-var next-adoption-id uint u1)

;; Data Maps

;; Pet registration
(define-map pets
    uint ;; pet-id
    {
        owner: principal,
        name: (string-ascii 50),
        species: (string-ascii 20),
        breed: (string-ascii 50),
        color: (string-ascii 30),
        age: uint,
        microchip-id: (optional (string-ascii 20)),
        registration-date: uint,
        is-active: bool
    }
)

;; Pet licensing
(define-map licenses
    uint ;; license-id
    {
        pet-id: uint,
        license-number: (string-ascii 20),
        issue-date: uint,
        expiry-date: uint,
        fee-paid: uint,
        is-valid: bool
    }
)

;; Vaccination records
(define-map vaccinations
    {pet-id: uint, vaccine-type: (string-ascii 30)} ;; composite key
    {
        vaccination-date: uint,
        veterinarian: (string-ascii 100),
        next-due-date: uint,
        batch-number: (string-ascii 20)
    }
)

;; Lost pet reports
(define-map lost-pets
    uint ;; report-id
    {
        pet-id: uint,
        reporter: principal,
        location-last-seen: (string-ascii 100),
        date-lost: uint,
        description: (string-ascii 200),
        contact-info: (string-ascii 100),
        is-found: bool,
        report-date: uint
    }
)

;; Adoption records
(define-map adoptions
    uint ;; adoption-id
    {
        pet-id: uint,
        previous-owner: principal,
        new-owner: principal,
        adoption-date: uint,
        adoption-fee: uint,
        is-completed: bool
    }
)

;; Helper Maps
(define-map owner-pets principal (list 100 uint)) ;; owner -> pet-ids
(define-map pet-licenses uint (list 10 uint)) ;; pet-id -> license-ids

;; Read-only functions

(define-read-only (get-pet (pet-id uint))
    (map-get? pets pet-id)
)

(define-read-only (get-license (license-id uint))
    (map-get? licenses license-id)
)

(define-read-only (get-vaccination (pet-id uint) (vaccine-type (string-ascii 30)))
    (map-get? vaccinations {pet-id: pet-id, vaccine-type: vaccine-type})
)

(define-read-only (get-lost-pet-report (report-id uint))
    (map-get? lost-pets report-id)
)

(define-read-only (get-adoption (adoption-id uint))
    (map-get? adoptions adoption-id)
)

(define-read-only (get-owner-pets (owner principal))
    (default-to (list) (map-get? owner-pets owner))
)

(define-read-only (get-pet-licenses (pet-id uint))
    (default-to (list) (map-get? pet-licenses pet-id))
)

(define-read-only (is-pet-licensed (pet-id uint))
    (let ((licenses-list (get-pet-licenses pet-id)))
        (> (len licenses-list) u0)
    )
)

;; Public functions

;; Register a new pet
(define-public (register-pet 
    (name (string-ascii 50))
    (species (string-ascii 20))
    (breed (string-ascii 50))
    (color (string-ascii 30))
    (age uint)
    (microchip-id (optional (string-ascii 20))))
    (let 
        (
            (pet-id (var-get next-pet-id))
            (current-pets (get-owner-pets tx-sender))
        )
        (map-set pets pet-id
            {
                owner: tx-sender,
                name: name,
                species: species,
                breed: breed,
                color: color,
                age: age,
                microchip-id: microchip-id,
                registration-date: stacks-block-height,
                is-active: true
            }
        )
        (map-set owner-pets tx-sender (unwrap! (as-max-len? (append current-pets pet-id) u100) (err u999)))
        (var-set next-pet-id (+ pet-id u1))
        (ok pet-id)
    )
)

;; Issue a license for a registered pet
(define-public (issue-license 
    (pet-id uint)
    (license-number (string-ascii 20))
    (validity-months uint)
    (fee uint))
    (let 
        (
            (pet (unwrap! (get-pet pet-id) ERR-NOT-FOUND))
            (license-id (var-get next-license-id))
            (current-licenses (get-pet-licenses pet-id))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get is-active pet) ERR-INVALID-STATUS)
        (map-set licenses license-id
            {
                pet-id: pet-id,
                license-number: license-number,
                issue-date: stacks-block-height,
                expiry-date: (+ stacks-block-height (* validity-months u4320)), ;; ~30 days per month
                fee-paid: fee,
                is-valid: true
            }
        )
        (map-set pet-licenses pet-id (unwrap! (as-max-len? (append current-licenses license-id) u10) (err u999)))
        (var-set next-license-id (+ license-id u1))
        (ok license-id)
    )
)

;; Record vaccination
(define-public (record-vaccination 
    (pet-id uint)
    (vaccine-type (string-ascii 30))
    (veterinarian (string-ascii 100))
    (next-due-months uint)
    (batch-number (string-ascii 20)))
    (let 
        (
            (pet (unwrap! (get-pet pet-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (map-set vaccinations {pet-id: pet-id, vaccine-type: vaccine-type}
            {
                vaccination-date: stacks-block-height,
                veterinarian: veterinarian,
                next-due-date: (+ stacks-block-height (* next-due-months u4320)),
                batch-number: batch-number
            }
        )
        (ok true)
    )
)

;; Report a lost pet
(define-public (report-lost-pet 
    (pet-id uint)
    (location-last-seen (string-ascii 100))
    (date-lost uint)
    (description (string-ascii 200))
    (contact-info (string-ascii 100)))
    (let 
        (
            (pet (unwrap! (get-pet pet-id) ERR-NOT-FOUND))
            (report-id (var-get next-report-id))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (map-set lost-pets report-id
            {
                pet-id: pet-id,
                reporter: tx-sender,
                location-last-seen: location-last-seen,
                date-lost: date-lost,
                description: description,
                contact-info: contact-info,
                is-found: false,
                report-date: stacks-block-height
            }
        )
        (var-set next-report-id (+ report-id u1))
        (ok report-id)
    )
)

;; Mark lost pet as found
(define-public (mark-pet-found (report-id uint))
    (let 
        (
            (report (unwrap! (get-lost-pet-report report-id) ERR-NOT-FOUND))
            (pet (unwrap! (get-pet (get pet-id report)) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (map-set lost-pets report-id (merge report {is-found: true}))
        (ok true)
    )
)

;; Initiate pet adoption process
(define-public (initiate-adoption 
    (pet-id uint)
    (new-owner principal)
    (adoption-fee uint))
    (let 
        (
            (pet (unwrap! (get-pet pet-id) ERR-NOT-FOUND))
            (adoption-id (var-get next-adoption-id))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (map-set adoptions adoption-id
            {
                pet-id: pet-id,
                previous-owner: tx-sender,
                new-owner: new-owner,
                adoption-date: stacks-block-height,
                adoption-fee: adoption-fee,
                is-completed: false
            }
        )
        (var-set next-adoption-id (+ adoption-id u1))
        (ok adoption-id)
    )
)

;; Complete adoption (transfer ownership)
(define-public (complete-adoption (adoption-id uint))
    (let 
        (
            (adoption (unwrap! (get-adoption adoption-id) ERR-NOT-FOUND))
            (pet (unwrap! (get-pet (get pet-id adoption)) ERR-NOT-FOUND))
            (current-pets-new (get-owner-pets (get new-owner adoption)))
        )
        (asserts! (is-eq (get new-owner adoption) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (not (get is-completed adoption)) ERR-INVALID-STATUS)
        
        ;; Update pet ownership
        (map-set pets (get pet-id adoption) (merge pet {owner: (get new-owner adoption)}))
        
        ;; Add pet to new owner's list (simplified - not removing from old owner list)
        (map-set owner-pets (get new-owner adoption)
            (unwrap! (as-max-len? (append current-pets-new (get pet-id adoption)) u100) (err u999))
        )
        
        ;; Mark adoption as completed
        (map-set adoptions adoption-id (merge adoption {is-completed: true}))
        (ok true)
    )
)

;; Update pet information
(define-public (update-pet-info 
    (pet-id uint)
    (name (string-ascii 50))
    (color (string-ascii 30))
    (age uint))
    (let 
        (
            (pet (unwrap! (get-pet pet-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (map-set pets pet-id (merge pet {name: name, color: color, age: age}))
        (ok true)
    )
)

;; Deactivate pet registration
(define-public (deactivate-pet (pet-id uint))
    (let 
        (
            (pet (unwrap! (get-pet pet-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (map-set pets pet-id (merge pet {is-active: false}))
        (ok true)
    )
)

;; Renew license
(define-public (renew-license (license-id uint) (validity-months uint) (fee uint))
    (let 
        (
            (license (unwrap! (get-license license-id) ERR-NOT-FOUND))
            (pet (unwrap! (get-pet (get pet-id license)) ERR-NOT-FOUND))
        )
        (asserts! (is-eq (get owner pet) tx-sender) ERR-UNAUTHORIZED)
        (map-set licenses license-id 
            (merge license 
                {
                    expiry-date: (+ stacks-block-height (* validity-months u4320)),
                    fee-paid: (+ (get fee-paid license) fee),
                    is-valid: true
                }
            )
        )
        (ok true)
    )
)

;; Administrative functions (contract owner only)

;; Revoke license (admin only)
(define-public (revoke-license (license-id uint))
    (let 
        (
            (license (unwrap! (get-license license-id) ERR-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (map-set licenses license-id (merge license {is-valid: false}))
        (ok true)
    )
)

;; Get contract statistics
(define-read-only (get-stats)
    {
        total-pets: (- (var-get next-pet-id) u1),
        total-licenses: (- (var-get next-license-id) u1),
        total-reports: (- (var-get next-report-id) u1),
        total-adoptions: (- (var-get next-adoption-id) u1)
    }
)


;; title: pet-registry
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

