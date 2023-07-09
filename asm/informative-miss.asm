hirom

; Informative Miss (FF3 US)
; Bropedio (October 20, 2019)
;
; Originally written for Brave New World, this patch adds
; additional "Fail" and "Null" messages when certain
; attacks fail to hit.
;
; "Null" displays when a target's immunity blocks any of
; the following: Status ailments, instant death/fractional, Suplex
;
; "Fail" displays when a target evades an attack or added
; status via something other than Evade or MBlock (Stamina or Stunner)
;
; The "Miss if Status Unchanged" flag will still remove targets
; from "hit targets", but will never show the "Miss" message.
; Instead, it will show "Null" if status immunity prevents the
; status change, or will show no message if the status was already
; set/clear.
 
; #############################################
; IMPORTANT: Version offset must be changed
; from "0" to "3" for FF3 1.1. This variable
; adjusts the offset in bank C1.

!version = 0

; #############################################
; Variables

!C2Space    = $C26700
!C2SpaceEnd = $C26800

!miss = $3A5A ; miss targets
!msgs = $3F52 ; 2 bytes before "null" bytes (for offset)
!null = $3F54 ; immune targets
!fail = $3F56 ; failed targets
!bck1 = $EA   ; backup $11AA statuses
!bck2 = $EC   ; backup $11AA statuses

; #############################################
; Clear "Null" and "Fail" bytes with "Miss"

org $C23292 : JSR ClearMiss
org $C23488 : JSR ClearMiss

; #############################################
; Set "Null" messages
; It may be cleaner to remove the "null" message
; from X-Kill, since it will show everytime you
; hit a death-immune target with "Fight", but I've
; left it in because it's consistent with the behavior
; in other death-immune situations

org $C22252 : JSR DeathNull  : NOP #2 ; Hit determination
org $C23891 : JSR DeathNull  : NOP #2 ; X-Kill/Slice
org $C23C6E : JSR SuplexNull : NOP #2 ; Suplex immunity

; #############################################
; Set "Fail" messages

org $C223B9 : JSR StamFail : NOP #2 ; Stamina evade
org $C23DA4 : JSR StunFail          ; Stunner evade

; #############################################
; Extend "Miss if status miss" routine
; with "Null" message handling

org $C2445C       ; 35 bytes (from 35)
StatusMiss:
  JSR StatusHelp  ; handle first two status bytes
  NOP             ; align for compatibility with "potent venom" junction patch
  BNE .exit       ; exit if at least one status hits
  LDA $FE         ; status to set (3-4)
  ORA $F6         ; status to clear (3-4)
  TSB $E8         ; combine with all previous statuses
  AND $3330,Y     ; are any vulnerable?
  BNE .exit       ; exit if at least one status hits
  LDA $E8         ; get bytes 1-2 
  BEQ .miss       ; if no statuses to set/clear, check "miss" bit
  JSR SetNull     ; set "Null" miss flag
.miss
  LDA $11A7       ; spell data flags
  LSR             ; "Spell Misses if Status Unchanged" in carry
  BCC .exit       ; exit if attack hits despite status miss
  INC $3A48       ; flag target to be missed
.exit
  PLP             ; restore flags
  RTS
warnpc $C24480

; #############################################
; Route EvilToot through existing status routine

org $C23BCB
EvilToot:         ; 52 bytes (from 57)
  JSR BackUpAttack
  JSR SetOneStatus
  JMP RestoreAttack
SetOneStatus:
  LDA !bck1       ; attack statuses 1-2
  JSR $520E       ; number of statuses in X
  STX $EE         ; store count so far
  LDA !bck2       ; attack statuses 3-4
  JSR $5210       ; continue adding to count
  SEP #$20        ; 8-bit A
  TXA             ; total status count in A
  JSR $4B65       ; select random status index
  CMP $EE         ; which status bytes selected
  REP #$20        ; 16-bit A
  PHP             ; store carry (indicates which bytes)
  LDA !bck1       ; attack statuses 1-2
  BCC .from1or2   ; use these if carry clear
  LDA !bck2       ; else load attack statuses 3-4
.from1or2
  JSR $522A       ; select random status from bytes
  PLP             ; restore carry (indicates which bytes)
  BCC .set1or2    ; set in bytes 1-2 if carry clear
  TSB $11AC       ; else set in bytes 3-4
  RTS
.set1or2
  TSB $11AA       ; set as only attack status
  RTS
warnpc $C23C05
padbyte $FF
pad $C23C04

; #############################################
; Rewrite Mind Blast to leverage status handling

org $C23BB0
MindBlast:
  JSR BackUpAttack
  LDA $3018,Y     ; target's unique bit
  LDX #$06        ; index to mind blast target mask
.loop
  BIT $3A5C,X     ; check mind blast target
  BEQ .next       ; get next if no match
  PHA
  PHX
  JSR SetOneStatus
  PLX
  PLA
.next
  DEX #2          ; point to next target
  BPL .loop       ; loop for all 4 targets
  JMP RestoreAttack
warnpc $C23BCC

; #############################################
; Queue Miss/Fail/Null Animations

org $C2636F : LDY $F0 ; keep running message count
org $C2634D : JSR MissType ; set type flags for regular misses

; Add miss handling that checks and removes the flags

org $C26348
GetMissFlags:
  JSR MissType      ; get "fail"/"null" flags, set carry
  BCC $06           ; branch if no "miss" message
  ORA #$4000        ; add default "miss" flag

; Add second full loop through dmg message routine

org $C26387
MessageFinish:
  TDC               ; clear A/B
  DEC               ; get $FFFF for "empty"
  LDX #$12          ; prepare loop through entities
.loop
  STA $33E4,X       ; set "no healing" for entity
  STA $33D0,X       ; set "no damage" for entity
  DEX #2            ; get next entity index
  BPL .loop         ; loop through all 10 entities
  JMP FinishMiss    ; continue with new null/fail loop
warnpc $C26399

; #############################################
; New Code (C2)

org !C2Space

ClearMiss:
  STZ !miss
  STZ !fail
  STZ !null
  RTS

StatusHelp:        ; (10 bytes)
  LDA $FC          ; status to set (1-2)
  ORA $F4          ; status to clear (1-2)
  STA $E8          ; store for now
  AND $331C,Y      ; are any vulnerable?
  RTS

BackUpAttack:      ; (22 bytes)
  REP #$20         ; 16-bit A
  JSR $44FF        ; zero statuses to set/clear
  LDA $11AA        ; attack statuses 1-2
  STA !bck1        ; save backup
  LDA $11AC        ; attack statuses 3-4
  STA !bck2        ; save backup
  STZ $11AA        ; clear statuses 1-2
  STZ $11AC        ; clear statuses 3-4
  RTS

RestoreAttack:     ; (14 bytes)
  JSR $4406        ; re-run status setting routine
  LDA !bck1        ; get backup statuses
  STA $11AA        ; restore statuses 1-2
  LDA !bck2        ; get backup statuses
  STA $11AC        ; restore statuses 3-4
  RTS 

DeathNull:
  LDA $3AA1,Y     ; check immune to instant death bit
  BRA BitSet
SuplexNull:
  LDA $3C80,Y     ; check fractional dmg immunity bit
BitSet:
  BIT #$04        ; immune to instant death (or fractional)
  BEQ SetEnd      ; if not immune, exit
SetNull:
  PHP             ; save M, Z flags
  REP #$20        ; 16-bit A
  LDA $3018,Y     ; get unique bit for target
  TSB !null       ; set null miss bit
  PLP             ; restore 8-bit A, no zero flag
SetEnd:
  RTS

StunFail:
  STA $3DE8,Y       ; displaced vanilla code
  BRA SetFailBit
StamFail:
  LDA $3B40,Y       ; stamina
  CMP $EE           ; compare to random(128)
  BCC FailExit      ; exit if hits
SetFailBit:
  PHP               ; store flags
  REP #$20          ; 16-bit A
  LDA $3018,Y       ; unique bitmask
  TSB !fail         ; set "Fail" for target
  PLP               ; restore flags
FailExit:
  RTS

MissType:
  PHX
  CLC               ; default to no "miss" text
  LDX #$04          ; point to "fail" bytes first
.loop
  BIT !msgs,X       ; is this miss message flagged
  BNE .done         ; exit with X offset if so
  DEX #2            ; point to next lowest message
  BNE .loop
.done
  TRB !miss         ; test and reset miss flag
  BNE .set_msg      ; if set, convert X to message value
  DEX               ; else check if "null" or "fail" was set
  BMI .exit         ; if not, exit
  TSB !miss         ; else, show fail/null on next loop
.exit
  PLX
  RTS 
.set_msg
  TRB !fail         ; clear "fail" bit
  TRB !null         ; clear "null" bit
  SEC               ; indicate "miss" text
  TXA               ; get message flag in A
  XBA               ; move to hi byte
  PLX
  RTS

FinishMiss:
  LDA !miss         ; any misses left
  BEQ .continue     ; exit if not
  STZ $F2           ; duplicated from C262F3, maybe unnecessary
; For some reason this originally jumped to C262F3, splitting
; a hook in patch_junction_prehit_trigger.
  JMP $62F5         ; else, loop again to handle null/fail
.continue
  PLY
  PLX
  RTS
warnpc !C2SpaceEnd

; Miss Messages #################################################
; Loads in new "miss" message tile data depending on the
; value passed on the high byte of missed targets' damage
; Requires no additional bytes, primarily due to an optimization
; of tile moving to use MVN
;
; The high byte for missed targets is "hm---ii-", where
; the index to the tile offset pointer is stored in ii. In other
; words, the ii value is the message id x 2.

; Address variables
!lastrts = "$C1A4B2-!version"
!cascdmg = "$C1A531-!version"
!cascfin = "$C1A586-!version"
!multdmg = "$C1A655-!version"
!multend = "$C1A6B8-!version"
!delayln = "$C1A4D5-!version"
!cascade = "$C1A50D-!version"
!multipl = "$C1A627-!version"
!targets = "$C1A749-!version"

; Labels for branching
org !lastrts
LastRts:
org !cascdmg
CascDmg:
org !cascfin
CascFin:           ; this branch is moved earlier for MVN cleanup
org !multdmg
MultiDmg:
org !multend
MultiEnd:          ; this branch is moved earlier for MVN cleanup

; Optional extended delay before showing "null/fail" second message
; org !delayln
;   LDA #$20       ; (from 08) wait longer to show second (status) message

org !cascade       ; 36 bytes replaced
Cascade:
  BEQ LastRts      ; use previous RTS (saves one byte)
  STA $1E          ; save dmg/flag byte for later
  ASL              ; move miss flag to bit 7
  BPL CascDmg      ; if no miss, skip to dmg display
  LDY #$60D3       ; set destination (in $7E) 
  JSR PrepMove     ; set X to source offset, A to #bytes to move, 16-bit A
  MVN $7E,$7F      ; move bytes (bank will not change, already 7E)
  BRA CascFin      ; includes TDC,SEP#20

PrepMove:
  LDA #$08
  STA $14          ; set x_pos for miss tiles
  LDA #$06
  AND $1E          ; isolate bits 1-2 to get tile index (0, 2, or 4)
  TAX
  REP #$20         ; 16-bit A
  LDA MissOff,X    ; load tile data offset for miss message
  JMP Prep2

warnpc !cascdmg+1
padbyte $FF
pad !cascdmg       ; 1 unused byte

org !multipl       ; 46 bytes replaced
Multiple:
  ASL              ; move "miss" flag to bit 7
  BPL MultiDmg     ; if no miss, skip to dmg display
  PHB              ; save current bank
  PHY              ; store dmg byte index
  LDA $20          ; get target's index
  ASL              ; carry will be clear after this (adc below)
  TAX              ; make it an index to data word
  LDA #$20         ; start at offset of 2nd tile (out of 4)
  REP #$20         ; 16-bit A
  ADC !targets,X   ; add buffer offset for this target
  TAY              ; put destination offset in Y
  TDC              ; clear B
  SEP #$20         ; 8-bit A
  JSR PrepMove     ; set X to tile source, A to #bytes, 16-bit A
  MVN $7F,$7F      ; move bytes (and change data bank)
  BRA MultiEnd     ; finish up (includes TDC,SEP#20,PLY,PLB)

Prep2:
  TAX              ; move offset to X
  LDA #$003F       ; will move 64 bytes (two tiles)
  RTS

MissOff:
  dw $BC00         ; miss tiles address in $7F
  dw $C180         ; null tiles address in $7F
  dw $C140         ; fail tiles address in $7F

warnpc !multdmg+1
padbyte $FF
pad !multdmg    ; 10 unused bytes

; Compressed tile data
; Current miss tiles input starts at D2E000
; 097C is offset to instruction to fill 00s

org $D2E000
  db $8c,$0b,$de,$dd,$1f,$0c,$04,$0c,$0c,$e0,$0f,$01,$01,$f0,$de,$27,$f4,$07,$e8,$4f,$03,$b8,$0f,$08,$3c,$24,$ff,$70,$50,$60,$20,$60,$20,$70,$10,$ff,$7f,$4f,$3f,$30,$0f,$0f,$3c,$3c,$f7,$70,$70,$60,$34,$00,$70,$70,$7f,$7f,$ef,$3f,$3f,$80,$80,$1a,$08,$02,$02,$04,$7f,$00,$38,$20,$f0,$90,$c0,$40,$3e,$30,$7f,$04,$38,$38,$f0,$f0,$c0,$c0,$c2,$ff,$bc,$7f,$f8,$a1,$08,$04,$04,$0a,$0a,$9a,$c8,$00,$f7,$00,$10,$10,$ba,$b0,$07,$00,$0f,$00,$ed,$1a,$e4,$00,$07,$00,$fd,$07,$07,$07,$0f,$ff,$0f,$1f,$1f,$3f,$3f,$1f,$1f,$0f,$f7,$0f,$07,$07,$dc,$20,$c0,$00,$e0,$00,$e7,$f0,$00,$60,$04,$01,$02,$09,$c0,$e0,$e0,$9f,$f0,$f0,$f8,$f8,$f0,$1a,$01,$db,$28,$03,$d6,$f9,$0f,$00,$02,$da,$30,$03,$36,$01,$01,$01,$51,$03,$26,$09,$00,$21,$4f,$08,$0e,$2d,$31,$c0,$56,$01,$6f,$80,$80,$0f,$0f,$90,$98,$07,$07,$f4,$18,$fc,$fc,$30,$84,$61,$e0,$e0,$f8,$f8,$fc,$fc,$0f,$f8,$f8,$e0,$e0,$84,$f8,$88,$e8,$52,$08,$d4,$b9,$ff,$00,$00,$80,$80,$0c,$0c,$1e,$12,$03,$1e,$16,$f6,$1f,$fe,$21,$14,$02,$08,$2a,$1f,$fa,$41,$fa,$5e,$63,$0a,$11,$08,$11,$11,$5c,$4a,$3f,$78,$02,$93,$1d,$1d,$f5,$49,$d8,$57,$0c,$9a,$09,$5c,$08,$60,$af,$60,$00,$04,$00,$b9,$08,$60,$90,$1a,$60,$11,$60,$b6,$18,$a0,$02,$ba,$2a,$20,$d0,$30,$be,$1a,$c3,$3a,$f0,$c9,$60,$fa,$7f,$d2,$02,$dd,$52,$f0,$f0,$38,$38,$e4,$dc,$4a,$7d,$69,$07,$2e,$03,$22,$4b,$84,$00,$08,$fd,$00,$7d,$4a,$ce,$ce,$8c,$8c,$1f,$1f,$68,$7c,$52,$60,$fb,$82,$e3,$1f,$1c,$00,$00,$07,$38,$09,$28,$9e,$13,$f5,$30,$a8,$2b,$f8,$0c,$03,$00,$9b,$02,$4e,$18,$06,$96,$19,$f0,$f0,$9a,$0a,$ca,$1b,$02,$5a,$e0,$13,$15,$42,$2f,$06,$02,$06,$06,$96,$4b,$06,$10,$04,$24,$4b,$4b,$18,$18,$d2,$0a,$40,$26,$04,$a3,$03,$0c,$1e,$64,$cf,$0f,$e0,$20,$02,$40,$04,$1a,$0c,$08,$08,$03,$30,$30,$12,$09,$40,$5c,$80,$fb,$7f,$fc,$0e,$0c,$96,$dc,$fe,$c3,$cc,$05,$07,$10,$1f,$20,$3f,$20,$7f,$35,$10,$1f,$08,$0e,$02,$03,$74,$19,$f1,$7f,$f4,$04,$b2,$1b,$dc,$0c,$40,$c0,$10,$f0,$7f,$08,$58,$04,$fc,$08,$b8,$10,$1d,$01,$7b,$c0,$c0,$16,$09,$fc,$fc,$fe,$fe,$d2,$0b,$e4,$18,$2b,$2a,$09,$04,$a4,$03,$28,$3b,$03,$03,$06,$89,$06,$5c,$29,$89,$1a,$1c,$3f,$03,$0a,$2a,$dc,$0b,$1e,$07,$1e,$9c,$9c,$5e,$f9,$80,$f9,$a1,$fd,$c3,$fd,$c2,$d9,$1f,$10,$10,$02,$02,$08,$4c,$2d,$01,$76,$21,$fe,$84,$43,$fe,$56,$04,$19,$5a,$5e,$78,$6a,$dc,$1b,$8e,$72,$20,$81,$00,$b5,$1c,$39,$04,$fa,$1a,$b4,$1c,$38,$0c,$b9,$26,$30,$04,$b9,$2e,$b9,$1a,$30,$c1,$3e,$b9,$3a,$2d,$41,$e0,$5e,$da,$06,$c6,$01,$63,$78,$78,$dc,$7a,$14,$40,$5d,$61,$10,$00,$fd,$3b,$2b,$41,$00,$00,$18,$18,$3f,$3f,$23,$3b,$3b,$46,$fe,$75,$ff,$98,$43,$1f,$e9,$08,$25,$01,$82,$ab,$ab,$f8,$c5,$03,$44,$29,$cf,$83,$02,$ce,$3a,$05,$09,$b3,$0f,$0b,$b4,$3c,$3a,$0d,$0f,$0f,$04,$38,$01,$67,$01,$08,$08,$32,$1c,$1a,$08,$0d,$0d,$1e,$68,$33,$b0,$b0,$f0,$29,$30,$08,$80,$80,$3e,$68,$80,$ff,$c0,$7f,$f8,$ee,$42,$db,$16,$a2,$38,$da,$8e,$58,$76,$0f,$20,$ff,$3f,$40,$7f,$40,$6a,$20,$3f,$10,$13,$1f,$04,$f1,$00,$f2,$0c,$ff,$f4,$00,$f6,$1c,$86,$0e,$ff,$20,$e0,$08,$f8,$04,$ac,$00,$fc,$bf,$04,$5c,$08,$f8,$80,$80,$94,$1d,$fe,$56,$16,$11,$fc,$fc,$dc,$08,$1e,$29,$05,$08,$3d,$07,$f9,$3c,$9b,$37,$f6,$0b,$18,$18,$3e,$3e,$3c,$e4,$2a,$29,$44,$01,$70,$9e,$06,$b7,$3e,$00,$00,$78,$07,$78,$70,$70,$bc,$a8,$74,$fd,$96,$fd,$b7,$f9,$ea,$16,$05,$01,$ee,$5e,$03,$ee,$01,$bc,$f9,$13,$fa,$35,$fa,$57,$6a,$8f,$11,$08,$11,$11,$6c,$ce,$43,$25,$8f,$6e,$04,$66,$c5,$46,$02,$30,$f1,$07,$b8,$38,$02,$02,$d0,$06,$4e,$e9,$2f,$20,$00,$50,$54,$18,$a8,$2e,$50,$c9,$12,$16,$de,$ee,$a0,$e0,$00,$5f,$f8,$0e,$03,$12,$cf,$2d,$6b,$f5,$ee,$9e,$02,$0e,$33,$43,$ff,$ff,$ee,$ee,$07,$0f,$0f,$0e,$42,$4b,$60,$fb,$82,$f7,$b6,$07,$a8,$07,$c2,$aa,$af,$00,$d5,$0f,$c8,$07,$ca,$af,$e4,$ba,$03,$03,$c6,$f6,$0f,$01,$01,$ae,$1a,$b6,$1a,$00,$5c,$03,$02,$4f,$0f,$09,$1c,$04,$4a,$01,$4c,$11,$01,$df,$01,$bf,$0f,$0f,$1c,$1c,$20,$20,$26,$2c,$fc,$ff,$0c,$fe,$f2,$0e,$08,$06,$04,$06,$7f,$04,$0e,$0a,$3c,$24,$f0,$10,$14,$09,$f7,$0e,$0e,$06,$54,$04,$0e,$0e,$3c,$3c,$61,$f0,$0d,$05,$5f,$fc,$81,$fc,$a3,$1c,$40,$4c,$9c,$5c,$f1,$4c,$ab,$64,$4c,$0f,$be,$7c,$05,$07,$10,$1f,$ff,$20,$3f,$20,$35,$10,$1f,$08,$0e,$c3,$02,$03,$74,$19,$f5,$04,$f9,$10,$2e,$1f,$40,$c0,$ff,$10,$f0,$08,$58,$04,$fc,$08,$b8,$3d,$10,$5d,$04,$c0,$c0,$f0,$f0,$12,$19,$d2,$0b,$d5,$7c,$25,$11,$21,$4b,$01,$f8,$aa,$03,$03,$00,$ff,$7e,$7e,$7c,$7c,$18,$18,$31,$31,$bf,$61,$61,$fc,$fc,$f9,$f9,$ec,$2b,$e0,$ca,$25,$05,$80,$41,$03,$c4,$07,$37,$d8,$0b,$8f,$8f,$c3,$ee,$ee,$5e,$f9,$80,$f9,$a1,$fd,$c3,$bd,$07,$04,$4b,$07,$05,$ec,$4b,$07,$ee,$05,$e2,$4d,$80,$fe,$05,$0e,$94,$02,$04,$0c,$0c,$f8,$55,$14,$06,$bc,$fd,$39,$fe,$02,$d4,$50,$19,$5a,$5e,$78,$6a,$dc,$5b,$96,$32,$c3,$1a,$0c,$00,$00,$a1,$1e,$d2,$1a,$0c,$08,$b0,$1e,$c3,$56,$f5,$4e,$c7,$a9,$ed,$63,$3c,$d4,$06,$13,$53,$f0,$f0,$78,$78,$12,$cb,$ee,$6e,$f7,$08,$00,$1c,$31,$57,$cc,$cc,$1e,$1e,$11,$1c,$40,$5f,$60,$ff,$83,$f3,$00,$b7,$0b,$aa,$1b,$af,$93,$f1,$00,$55,$0d,$88,$22,$d0,$6b,$e0,$00,$f0,$40,$ff,$d8,$20,$6c,$10,$37,$08,$1a,$05,$fd,$0d,$2f,$0f,$e0,$20,$f0,$50,$f8,$28,$ff,$7c,$14,$3f,$0a,$1f,$04,$0f,$06,$ff,$00,$19,$06,$33,$1e,$47,$3c,$ae,$bf,$50,$54,$88,$8c,$18,$b8,$b4,$06,$02,$ff,$1f,$16,$3f,$1c,$7f,$00,$fe,$68,$5f,$fc,$f8,$fc,$c0,$f8,$de,$0f,$f1,$e3,$17,$df,$36,$08,$1b,$04,$0d,$53,$08,$e0,$20,$bd,$f1,$f4,$17,$3e,$0a,$1f,$05,$fd,$07,$ed,$ff,$06,$9b,$ee,$e6,$1c,$74,$08,$fa,$df,$34,$ba,$64,$6e,$c4,$b5,$06,$ef,$ce,$ff,$ff,$0c,$fe,$20,$7c,$40,$fe,$84,$f3,$fe,$04,$5d,$a5,$73,$38,$0e,$00,$1b,$06,$ff,$17,$0e,$1d,$0e,$0e,$04,$1b,$0e,$ff,$33,$1e,$e3,$3e,$00,$0e,$0e,$1f,$7c,$90,$08,$7d,$00,$1f,$00,$3f,$00,$ff,$44,$19,$ff,$d8,$30,$b8,$70,$ef,$70,$f1,$0f,$5b,$d0,$7f,$43,$19,$70,$f8,$b4,$00,$ff,$9c,$00,$fc,$9d,$10,$2a,$17,$00,$fd,$02,$fb,$fe,$07,$d9,$fe,$25,$3f,$ba,$10,$ff,$03,$29,$01,$77,$10,$ff,$6d,$1c,$76,$0e,$7a,$06,$3e,$02,$fe,$ad,$00,$03,$03,$3f,$2d,$7f,$22,$7f,$ff,$31,$7f,$39,$7f,$1d,$3f,$0f,$7f,$ff,$f0,$00,$98,$c0,$3c,$00,$e6,$00,$ff,$e2,$10,$73,$08,$ff,$40,$fb,$04,$ff,$00,$f0,$20,$f8,$e8,$fc,$dc,$fe,$ff,$0c,$fe,$96,$ff,$9a,$ff,$60,$ff,$ff,$ff,$00,$3b,$00,$0f,$00,$07,$00,$ff,$73,$00,$bb,$60,$af,$60,$d7,$30,$7f,$38,$ff,$06,$3f,$06,$0f,$02,$24,$01,$ff,$10,$fb,$18,$ff,$4c,$ff,$ed,$40,$f7,$9d,$40,$dd,$41,$01,$d7,$40,$ff,$10,$7f,$bf,$60,$f7,$00,$9a,$ff,$ba,$4f,$01,$77,$b2,$ff,$aa,$db,$00,$84,$ff,$0c,$bd,$10,$fe,$2a,$05,$01,$0f,$02,$0f,$06,$df,$07,$f3,$bf,$59,$d2,$10,$2a,$0d,$0f,$06,$0f,$07,$ff,$df,$19,$ff,$c0,$00,$a0,$40,$60,$ff,$c0,$d0,$e0,$f0,$a0,$e8,$b0,$58,$ff,$e0,$38,$c0,$00,$c0,$40,$e0,$80,$7e,$f1,$07,$c0,$f0,$d0,$f8,$a0,$f8,$1c,$08,$fc,$9f,$f9,$c1,$c9,$0c,$03,$0b,$02,$0e,$04,$df,$09,$04,$0f,$04,$0e,$9e,$12,$04,$0f,$d7,$05,$0f,$03,$f1,$01,$02,$e6,$21,$40,$00,$57,$60,$80,$b0,$23,$20,$1e,$f8,$01,$80,$8f,$01,$fd,$a0,$f3,$1f,$3e,$08,$1e,$00,$0e,$06,$ff,$01,$0d,$03,$1b,$06,$36,$0c,$6c,$ff,$18,$c8,$20,$f0,$40,$e0,$00,$02,$ff,$07,$04,$0f,$08,$1f,$10,$3e,$20,$ff,$7c,$50,$f8,$20,$f0,$00,$e0,$c0,$f1,$80,$00,$5a,$8e,$09,$12,$4a,$3f,$10,$1f,$0f,$2d,$18,$b6,$0f,$01,$02,$bc,$1f,$3f,$98,$00,$a3,$07,$fc,$23,$01,$a8,$17,$c7,$fe,$26,$fc,$d6,$fc,$ff,$ae,$fc,$6c,$f8,$dc,$f8,$d8,$70,$ad,$70,$4c,$03,$00,$fe,$90,$12,$fc,$96,$02,$f8,$ff,$00,$70,$d4,$7f,$4b,$3f,$6d,$3f,$ff,$66,$3f,$67,$3f,$33,$1f,$39,$1f,$d5,$1f,$8d,$0a,$7f,$b0,$22,$3f,$6e,$0a,$fe,$fc,$ff,$3e,$fc,$c4,$f8,$7c,$f8,$98,$f0,$e9,$f0,$88,$1e,$93,$3a,$f0,$c9,$0f,$ff,$00,$8f,$af,$00,$f7,$00,$fb,$b4,$12,$af,$dd,$02,$76,$ff,$ff,$79,$ff,$7d,$ff,$3d,$ff,$1d,$ff,$7f,$01,$7f,$7c,$ff,$7e,$ff,$75,$ff,$a2,$db,$46,$1f,$86,$1b,$82,$1f,$ff,$82,$af,$82,$d5,$a0,$f5,$20,$08,$bf,$ff,$30,$ff,$70,$ff,$74,$13,$03,$58,$fb,$ff,$0a,$19,$03,$ee,$11,$fa,$05,$7c,$57,$02,$3e,$02,$73,$2a,$62,$11,$03,$31,$f9,$00,$f5,$07,$26,$03,$03,$2a,$03,$bf,$90,$3d,$00,$ff,$3f,$04,$7f,$18,$ff,$10,$df,$10,$ff,$2e,$30,$fc,$00,$4a,$ff,$d2,$ff,$ff,$c2,$ff,$c6,$ff,$ce,$ff,$2e,$ff,$fd,$cc,$d1,$02,$dd,$2e,$6f,$11,$37,$0b,$ff,$1b,$04,$35,$0a,$5e,$2d,$4f,$36,$ff,$3f,$00,$4e,$ff,$26,$7f,$10,$3f,$7f,$08,$1f,$0c,$3f,$2a,$7f,$37,$b7,$02,$7f,$f0,$20,$50,$e0,$a0,$c0,$e0,$7f,$01,$6f,$c0,$00,$70,$a0,$9d,$05,$f0,$a0,$2a,$0a,$c5,$e0,$92,$03,$c0,$3a,$0a,$bc,$f9,$bf,$eb,$03,$04,$ff,$0d,$0b,$07,$08,$5f,$50,$67,$38,$ff,$40,$3f,$60,$3f,$07,$07,$0f,$0f,$1f,$1f,$1f,$5f,$5f,$ff,$f6,$23,$86,$0e,$36,$04,$df,$fc,$fe,$02,$bc,$7c,$87,$03,$e0,$80,$37,$80,$c0,$c0,$16,$0d,$ff,$ff,$18,$0d,$5c,$3c,$df,$10,$00,$38,$00,$7c,$1f,$2c,$10,$38,$cf,$38,$7c,$7c,$fe,$38,$14,$3a,$2f,$36,$00,$e7,$63,$00,$41,$e7,$02,$3e,$14,$08,$1c,$1c,$cb,$3e,$3e,$f4,$0c,$7f,$4c,$14,$26,$0c,$6c,$00,$47,$c6,$00,$82,$d0,$02,$2e,$8c,$3e,$2c,$3e,$7f,$2c,$7c,$53,$64,$a1,$1c,$01,$01,$02,$03,$05,$b7,$06,$f2,$a0,$3c,$03,$ed,$04,$ea,$1d,$1a,$1c,$6e,$71,$cf,$a1,$c0,$e7,$1f,$fd,$14,$73,$15,$7f,$7f,$fe,$f8,$1b,$07,$07,$fa,$fc,$1e,$e1,$1c,$ff,$03,$f0,$f0,$e6,$1e,$00,$00,$30,$b7,$f0,$07,$07,$d8,$1c,$f0,$f0,$7c,$0c,$f0,$ff,$f0,$ff,$ff,$68,$18,$80,$80,$60,$7e,$9d,$33,$ff,$ff,$f8,$f8,$80,$80,$9c,$3d,$ff,$03,$00,$05,$01,$0a,$03,$0d,$05,$ff,$0b,$03,$1d,$0d,$11,$0b,$15,$0f,$c7,$00,$03,$02,$ee,$03,$78,$01,$f0,$03,$0c,$1f,$fd,$0d,$ac,$02,$c0,$00,$f0,$c0,$38,$f0,$1f,$c4,$f8,$24,$f8,$a4,$47,$05,$1b,$05,$40,$0d,$3f,$f8,$30,$fc,$d0,$fc,$50,$57,$05,$be,$fb,$e8,$7f,$fd,$a1,$fd,$c3,$bd,$30,$9b,$04,$5f,$5f,$0f,$78,$cc,$1c,$0b,$15,$ef,$0d,$5f,$5f,$1f,$1f,$f0,$0d,$47,$3f,$3f,$60,$d9,$07,$da,$1f,$08,$2f,$e0,$10,$06,$88,$10,$0c,$16,$0c,$04,$0e,$7c,$63,$04,$13,$3f,$78,$1c,$7c,$57,$7c,$38,$38,$12,$1f,$7f,$49,$04,$63,$45,$04,$f1,$1c,$85,$04,$3a,$16,$4e,$16,$3e,$3e,$1c,$1c,$f1,$08,$48,$1e,$1f,$fe,$41,$d6,$0b,$0c,$14,$18,$ff,$2e,$31,$30,$20,$5c,$63,$60,$40,$2f,$b8,$c7,$c1,$80,$f0,$0b,$3f,$b2,$06,$90,$0e,$fe,$f8,$0d,$9c,$7c,$3c,$03,$38,$f8,$73,$ff,$0f,$70,$f0,$cc,$3c,$e0,$e0,$88,$ed,$78,$16,$0e,$f8,$f8,$f4,$0c,$fc,$fc,$e0,$1b,$e0,$f8,$4d,$05,$c0,$c0,$d0,$5d,$e0,$6e,$ff,$de,$ff,$17,$0f,$15,$0d,$1f,$0f,$0b,$03,$1f,$0f,$07,$0a,$03,$06,$2e,$07,$21,$07,$2f,$07,$f5,$06,$ef,$01,$07,$28,$13,$24,$f8,$44,$f8,$7f,$88,$f0,$10,$e0,$20,$c0,$40,$04,$16,$ff,$d0,$fc,$b0,$fc,$60,$f8,$c0,$f0,$81,$80,$97,$03,$c9,$27,$60,$ff,$82,$ff,$a4,$ff,$c6,$b7,$44,$3b,$00,$6d,$5f,$06,$55,$00,$45,$e7,$07,$de,$07,$77,$44,$ef,$ef,$f6,$2b,$ef,$ef,$45,$ea,$0f,$4a,$65,$04,$48,$01,$00,$24,$05,$00,$fe,$17,$6c,$18,$0c,$ba,$6e,$1e,$6c,$0a,$00,$3c,$00,$66,$21,$20,$3c,$de,$0d,$00,$3c,$3c,$7e,$7e,$f2,$2f,$7e,$7e,$95,$3c,$2a,$00,$18,$61,$06,$18,$43,$10,$2a,$08,$18,$3f,$18,$3c,$3c,$7c,$7c,$3c,$54,$10,$3a,$18,$2a,$20,$18,$0c,$47,$00,$7e,$2b,$40,$7e,$76,$00,$74,$08,$6e,$5e,$18,$0c,$00,$06,$27,$50,$7e,$7e,$b8,$0e,$ba,$5a,$18,$0c,$85,$06,$2c,$00,$4c,$69,$00,$0c,$fe,$db,$09,$0c,$1e,$1e,$3e,$3e,$7e,$7e,$ee,$14,$0c,$7e,$7e,$0c,$aa,$00,$7e,$00,$60,$20,$e3,$07,$86,$28,$90,$08,$18,$08,$96,$38,$3c,$c1,$10,$26,$48,$54,$12,$1c,$d8,$28,$7e,$e7,$00,$06,$65,$10,$18,$cb,$20,$13,$ff,$ff,$92,$0e,$56,$08,$18,$0a,$01,$80,$08,$60,$18,$c8,$8a,$48,$f6,$38,$24,$19,$3e,$03,$01,$2a,$48,$7f,$7f,$83,$3f,$3f,$3a,$19,$5f,$f9,$81,$f9,$a3,$f9,$c5,$a9,$3c,$5f,$00,$42,$3c,$99,$66,$e2,$19,$42,$5c,$81,$ff,$18,$00,$24,$18,$44,$38,$24,$18,$f8,$04,$0a,$ea,$89,$de,$29,$72,$0c,$66,$18,$81,$bc,$7c,$08,$0f,$92,$72,$0c,$79,$06,$e8,$99,$0c,$ff,$00,$12,$0c,$22,$1c,$52,$2c,$b2,$cf,$4c,$81,$7e,$72,$bc,$08,$6e,$62,$7e,$00,$3f,$81,$7e,$9e,$60,$82,$7c,$46,$aa,$3e,$0a,$f1,$9c,$83,$02,$e6,$a9,$7e,$0a,$99,$66,$79,$06,$83,$32,$0c,$06,$0a,$0a,$09,$30,$8a,$20,$1a,$0a,$ca,$41,$07,$3e,$39,$06,$ea,$8a,$1d,$fb,$3f,$fb,$61,$fb,$83,$fb,$d8,$a5,$fb,$c8,$f9,$ea,$a9,$22,$1c,$04,$fa,$4e,$30,$de,$28,$ca,$4e,$30,$9e,$60,$e8,$9b,$30,$00,$ff,$48,$30,$44,$38,$4a,$34,$4d,$32,$df,$81,$7e,$4e,$30,$30,$ad,$92,$79,$06,$93,$41,$3e,$46,$ac,$3e,$0c,$39,$83,$04,$a6,$da,$9e,$e7,$60,$4c,$30,$c8,$fa,$0a,$cc,$82,$7c,$9c,$5d,$60,$0a,$9b,$78,$00,$43,$1f,$15,$44,$25,$05,$ba,$1e,$05,$78,$f2,$3f,$e7,$e7,$43,$2a,$05,$08,$3f,$00,$28,$00,$88,$00,$a8,$45,$15,$3e,$05,$af,$08,$3c,$3c,$fc,$52,$35,$a8,$4a,$05,$44,$ff,$00,$64,$00,$75,$00,$5d,$00,$4d,$50,$5f,$05,$ec,$0f,$f4,$18,$34,$1d,$44,$6a,$05,$0a,$7f,$05,$35,$2a,$83,$15,$ea,$6b,$05,$0a,$0a,$b0,$0e,$72,$2d,$01,$ea,$8a,$0d,$a0,$fd,$c2,$fd,$e4,$fd,$06,$fe,$28,$fe,$4a,$fe,$00,$6c,$fe,$8e,$fe,$b0,$fe,$d2,$fe,$f4,$fe,$16,$ff,$38,$ff,$5a,$ff,$00,$7c,$ff,$9e,$ff,$c0,$d7
