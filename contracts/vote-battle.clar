;; vote-battle.clar
;; Voting contract for Stacks mainnet (Clarity 4)
;; 20 fixed "A vs B" polls. Each vote = 1 transaction = +1 point.
;;
;; Poll List:
;; 1:  BMW vs Mercedes
;; 2:  iPhone vs Android
;; 3:  PlayStation vs Xbox
;; 4:  Coca-Cola vs Pepsi
;; 5:  Marvel vs DC
;; 6:  McDonald's vs Burger King
;; 7:  Nike vs Adidas
;; 8:  Cat vs Dog
;; 9:  Summer vs Winter
;; 10: Coffee vs Tea
;; 11: Netflix vs YouTube
;; 12: Bitcoin vs Ethereum
;; 13: Morning vs Night
;; 14: Book vs Movie
;; 15: Pizza vs Sushi
;; 16: Rock vs Hip-Hop
;; 17: Twitter/X vs Telegram
;; 18: PC vs Console
;; 19: Gym vs Calisthenics
;; 20: React vs Vue
;;
;; option u1 = Option A (first), option u2 = Option B (second)

;; ---- Constants ----

(define-constant MAX-POLL-ID u20)
(define-constant ERR-INVALID-POLL (err u100))
(define-constant ERR-INVALID-OPTION (err u101))

;; ---- Data Maps ----

;; Vote count per poll per option
(define-map poll-votes
  { poll-id: uint, option: uint }
  { count: uint }
)

;; Total votes cast by an address (for leaderboard)
(define-map voter-stats
  { voter: principal }
  { total-votes: uint }
)

;; Votes cast by an address in a specific poll
(define-map voter-poll-stats
  { voter: principal, poll-id: uint }
  { votes: uint }
)

;; ---- Private Functions ----

(define-private (is-valid-poll (poll-id uint))
  (and (>= poll-id u1) (<= poll-id MAX-POLL-ID))
)

(define-private (is-valid-option (option uint))
  (or (is-eq option u1) (is-eq option u2))
)

;; ---- Public Functions ----

(define-public (vote (poll-id uint) (option uint))
  (begin
    (asserts! (is-valid-poll poll-id) ERR-INVALID-POLL)
    (asserts! (is-valid-option option) ERR-INVALID-OPTION)

    ;; Increment poll vote count
    (let
      (
        (current-votes (default-to { count: u0 } (map-get? poll-votes { poll-id: poll-id, option: option })))
      )
      (map-set poll-votes
        { poll-id: poll-id, option: option }
        { count: (+ (get count current-votes) u1) }
      )
    )

    ;; Increment voter total stats
    (let
      (
        (current-stats (default-to { total-votes: u0 } (map-get? voter-stats { voter: tx-sender })))
      )
      (map-set voter-stats
        { voter: tx-sender }
        { total-votes: (+ (get total-votes current-stats) u1) }
      )
    )

    ;; Increment voter poll stats
    (let
      (
        (current-poll-stats (default-to { votes: u0 } (map-get? voter-poll-stats { voter: tx-sender, poll-id: poll-id })))
      )
      (map-set voter-poll-stats
        { voter: tx-sender, poll-id: poll-id }
        { votes: (+ (get votes current-poll-stats) u1) }
      )
    )

    (ok true)
  )
)

;; ---- Read-Only Functions ----

;; Get votes for a specific option in a poll
(define-read-only (get-poll-votes (poll-id uint) (option uint))
  (default-to u0
    (get count (map-get? poll-votes { poll-id: poll-id, option: option }))
  )
)

;; Get both option counts for a poll
(define-read-only (get-poll-results (poll-id uint))
  {
    option-a: (get-poll-votes poll-id u1),
    option-b: (get-poll-votes poll-id u2)
  }
)

;; Get total votes cast by an address
(define-read-only (get-voter-stats (voter principal))
  (default-to u0
    (get total-votes (map-get? voter-stats { voter: voter }))
  )
)

;; Get votes cast by an address in a specific poll
(define-read-only (get-voter-poll-stats (voter principal) (poll-id uint))
  (default-to u0
    (get votes (map-get? voter-poll-stats { voter: voter, poll-id: poll-id }))
  )
)

;; Get max poll ID (for frontend iteration)
(define-read-only (get-max-poll-id)
  MAX-POLL-ID
)
