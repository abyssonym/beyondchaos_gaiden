hirom
; header

table ff6_bank_c3.tbl,rtl
incsrc ff6_bank_c3_defs.asm

org $C3FFFF

org $C30247
         dw #C39621      ; Update entry 36 in C301DB jump table

org $C30285
         dw #C39887      ; Update entry 55 in C301DB jump table

org $C30289
         dw #C3990F      ; Update entry 57 in C301DB jump table

org $C31BB8
; 35: Initialize Equip menu
C31BB8: JMP C31BB8_jump

org $C31BBD
; Initialize variables for Equip menu
C31BBD:  JSR $352F       ; Reset/Stop stuff
         JSR $6A08       ; Set Win1 bounds
         ; LDA #$06        ; Main cursor: On
         ; TSB $46         ; Set menu flag
         LDA #$10        ; Reset/Stop desc
         TSB $45         ; Set menu flag
         STZ $4A         ; List scroll: 0
         STZ $49         ; Top BG1 WR row: 1
         JSR C31BBD_extension

org $C31BD7
C31BD7:  JSR C39032      ; Draw menu; status
         LDA #$01        ; C3/1D7E
         STA $26         ; Next: Fade-in
         LDA #$36        ; C3/9621
         STA $27         ; Queue: Option list
         JMP $3541       ; BRT:1 + NMI

warnpc $C31C32

org $C31BE8
; Update JSR target (L+R switch)
         JSR $964F       ; Update menu colours (Equip)

org $C31BF6
; Update JSR target (L+R switch)
         JSR $9656       ; Update menu colours (Remove)

org $C31C01
; Update JSR and JMP targets
C31C01:  JSR C31BBD      ; Reset variables
         JSR C39032      ; Draw menu; status
         JMP $808A       ; Switch windows

org $C31C26
; Update JSR and branch target
C31C26:  JSR C31BBD      ; Init variables
         JSR $96A8       ; Remove gear
         LDA #$02        ; Menu: Equip
         STA $25         ; Set submenu
         BRA C31BD7      ; Draw menu, etc.

org $C32E72
; Update menu pointer
         dw $1EEB

org $C3372D
; Update text pointer
         dw #review

org $C38E64
; Cursor positions for Equip menu options
C38E64:  dw $0840        ; EQP
         dw $0870        ; OPT
         dw $08A0        ; RM
         dw $08C8        ; EMPTY

warnpc $C38E6D

org $C38E75
; Update LDY pointer
         LDY #C38E80

org $C38E7B
; Navigation data for Equip menu slot selection
C38E7B:  db $81          ; Wraps all ways
         db $00          ; Initial column
         db $00          ; Initial row
         db $02          ; 2 column
         db $03          ; 3 rows

warnpc $C38E88

org $C38FB4
         NOP #12          ; Blanking out a routine that handles allowing/forbidding "Empty", which we no longer use

warnpc $C38FC1

;org $C3FFFF

org $C39032
; Draw Equip menu, create portrait, update status via gear
C39032:  JSR $9093       ; Do boxes; face
         JSR $911B       ; Draw info; status
         JSR C3A6AB
         NOP #10
         JSR $904E       ; Draw top options
         JMP $0E6E       ; Upload BG3 A+B

org $C39093
; Draw elements shared by Equip and Relic menus, create portrait
C39093:  JSR $9110       ; Load actor stats
         REP #$20        ; 16-bit A
         LDA #$0100      ; BG1 H-Shift: 256
         STA $7E9BD0     ; Hide gear list
         SEP #$20        ; 8-bit A
         LDA #$01        ; 64x32 at $0000
         STA $2107       ; Set BG1 map loc
         LDA #$42        ; 32x64 at $4000
         STA $2109       ; Set BG3 map loc
         NOP #3
         JSR $6A28       ; Clear BG2 map A
         JSR $6A2D       ; Clear BG2 map B
         LDY #C3947F     ; C3/947F
         JSR $0341       ; Draw stats box A
         LDY #C39487     ; C3/9487
         JSR $0341       ; Draw option box
         LDY #C3948F     ; C3/9487
         JSR $0341       ; Draw option box
         LDY #C39497     ; C3/9487
         JSR $0341       ; Draw option box
         NOP #12
         JSR $0E52       ; Upload windows
         JSR $6A15       ; Clear BG1 map A
         JSR $6A19       ; Clear BG1 map B
         JSR $0E28       ; Upload BG1 A+B
         JSR $0E36       ; Upload BG1 C...
         JSR $6A3C       ; Clear BG3 map A
         JSR $6A41       ; Clear BG3 map B
         JSR $93E5       ; Draw actor name
         ; JSR $61B2       ; Create portrait
         NOP #3
         LDA #$2C        ; Palette 3
         STA $29         ; Color: Blue
         LDX #$A34D      ; Text ptrs loc
         LDY #$001C      ; Strings: 14
         JSR $69BA       ; Draw Vigor, etc.
         NOP #4
         LDX #$A369      ; Text ptrs loc
         LDY #$0008      ; Strings: 4
         JSR $69BA       ; Draw Speed, etc.
         JMP $0E6E       ; Upload BG3 A+B

org $C3911B
C3911B:  JMP C3911B_extension

org $C393E5
; Draw actor name in Equip or Relic menu
C393E5:  JSR $93F2      ; Actor's address
         LDA #$2C        ; Palette 0
         STA $29         ; Color: Blue
         LDY #$788D      ; Text position

warnpc $C3946D

org $C3947F
; Window layout for Equip and Relic menus
C3947F:  dw $5B4B,$0D1C  ; 30x15 at $5B4B (Stats w/o title)
C39487:  dw $584B,$0A1C  ; 30x04 at $588B (Options)
C3948F:  dw $584B,$011C  ; Menu
C39497:  dw $584B,$0106  ; Name

org $C3960C
; Switch to layout with options in Equip or Relic menu
C3960C:  RTS

org $C3964F
        JMP C3964F_extension

org $C39656
        JMP C39656_extension

org $C3966C
; Jump table for the above
C3966C:  dw $9674       ; EQUIP
         dw do_optimum  ; OPTIMUM
         dw $968E       ; REMOVE
         dw $969F       ; EMPTY

org $C39674
; Update JSR targets
C39674:  NOP #3
         JSR $964F       ; Update menu colours (Equip)

org $C39688
; remove old optimum code to display text
C39688:  NOP #3

org $C3968E
; Update JSR target
C3968E:  NOP #3
         JSR $9656       ; Update menu colours (Remove)

org $C396A2
; Update JSR target
         JSR C3911B      ; Redo text, status

org $C396A8
; Remove character's equipment
C396A8: LDX #$0005       ; Loop index
        BRA rminit
_396A8: LDX #$0003       ; only remove non-relics when using optimum
rminit: PHX
        JSR $93F2
        PLX
rmloop:
        LDA $001F,Y      ; SRAM equipment location
        JSR $9D5E
        LDA #$FF
        STA $001F,Y
        INY
        DEX
        BPL rmloop
        RTS

warnpc $C396D2

org $C396E9
; Minor optimisation -- called at start of /96F0 subroutine
         NOP #3

org $C396F0
; Update JSR target
C396F0:  JSR $9110        ; Get gear FX
         JSR _396A8

org $C39887
;org $C3FFFF
; 55: Handle selection of gear slot to fill
; some duplicated code at C398CF
C39887:  JSR load_description
         LDA $08         ; No-autofire keys
         BIT #$80        ; Pushing A?
         BEQ C398B4      ; Branch if not
         JSR $0EB2       ; Sound: Click
; diverges from 398cf here?
         JSR C39887_section_a
         NOP
         LDA #$57        ; C3/990F
         STA $26         ; Next: Item list
         JSR C39B59      ; Build item list
         JSR $A150       ; Sort it by power
         JSR $9AEB       ; Cursor & Scrollbar
         LDA #$55        ; Return here if..
         STA $27         ; ..list is empty
; section B
         JSR C39887_section_b
         JSR $9233       ; Draw stat preview
         JSR $1368       ; Refresh screen
         JMP $9CAC       ; Draw item list
; section C?
         ;JMP $1368       ; Refresh screen
; Fork: Handle B
C398B4:  LDA $09         ; No-autofire keys
         BIT #$80        ; Pushing B?
         BEQ C398C8      ; Branch if not
C398B4_pushing_b:
         JSR $0EA9       ; Sound: Cursor
         JSR $8E50       ; Load navig data
         JSR C398B4_extension
         LDA #$36        ; C3/9621
         STA $26         ; Next: Option list
         RTS

warnpc $C398c9

org $C398C8
; Fork: Handle L and R, prepare for menu reset
C398C8:  LDA #$7E        ; C3/1BE5
         STA $E0         ; Set init command
         JMP $2022       ; Handle L and R

org $C398CF
; 56: Handle manual gear removal
C398CF:  JSR load_description
         LDA $08         ; No-autofire keys
         BIT #$80        ; Pushing A?
         BEQ C398F4      ; Branch if not
         JSR $0EB2       ; Sound: Click
         JSR $93F2       ; Actor's address
         REP #$21        ; 16-bit A; C-
         TYA             ; Move it to A
         SEP #$20        ; 8-bit A
         ADC $4B         ; Add cursor slot
         TAY             ; Index sum
         LDA $001F,Y     ; Item in slot
         JSR $9D5E       ; Put in stock
         LDA #$FF        ; Empty item
         STA $001F,Y     ; Clear gear slot
         JSR C3911B      ; Redo text, status

; Fork: Handle B
C398F4:  LDA $09         ; No-autofire keys
         BIT #$80        ; Pushing B?
         BEQ C39908      ; Branch if not
         JSR $0EA9       ; Sound: Cursor
         JSR $8E50       ; Load navig data
         JSR $8E59       ; Relocate cursor
         LDA #$36        ; C3/9621
         STA $26         ; Next: Option list
         RTS

; Fork: Handle L and R, prepare for menu reset
C39908:  LDA #$7F        ; C3/1BF3
         STA $E0         ; Set init command
         JMP $2022       ; Handle L and R

org $C3990F
; LOTS of duplicated at C3A097 and C3990F*
; 57: Handle gear browsing
C3990F:
         JSR C3990F_description
         JSR $9233       ; Draw stat preview
; Fork: Handle A
         LDA $08         ; No-autofire keys
         BIT #$80        ; Pushing A?
         BEQ C39944
         JMP C3990F_not_a

org $C39920
C39920:
; equip_fail_buzzer
; duplicated from C39920
         JSR $0EB2       ; Sound: Click
         LDA $001F,Y     ; Item to unequip
         CMP #$FF        ; None?
         BEQ C3992D      ; Branch if so
         JSR $9D5E       ; Put in stock
C3992D:  TDC             ; Clear A
         LDA $4B         ; Gear list slot
         TAX             ; Index it
         LDA $7E9D8A,X   ; Inventory slot
         TAX             ; Index it
         LDA $1869,X     ; Item in slot
         STA $001F,Y     ; Equip on actor
         JSR $9D97       ; Adjust stock
         JSR C3911B      ; Redo text, status
         BRA C39944_exit ; Exit gear list
; Fork: Handle B
C39944:  LDA $09         ; No-autofire keys
         BIT #$80        ; Pushing B?
         BEQ C39944_rts  ; Exit if not
         JSR C39944_close
C39944_exit:
         JMP C3A0E5
C39944_rts:
         RTS
         NOP

org $C3996E
; Fork: Invalid selection
C3996E:  JSR $0EC0       ; Play buzzer
         JSR $305D       ; Pixelate screen
         RTS

warnpc $C3F43A

; org $C39A5D
; From genji_menu_fix.asm
; JSR Wpn_Index

; org $C39A90		; Right hand
; JMP DW_Chk_RH

; org $C39ABC		; Left hand
; JMP DW_Chk_LH

org $C39B59
; Compile compatible gear for actor's body part
C39B59:  JSR $9C2A       ; Init list
         JSR $9C41       ; Define compat
         LDA #$20        ; Palette 0
         STA $29         ; Color: User's
         LDA $4B         ; Body part
         CMP #$02        ; Head?
         BCC C39B72
         BEQ C39BB2
         CMP #$04
         BCC C39BEE      ; Handle torso
         JMP C3A051      ; Handle relics

warnpc $C39B73

org $C39B72
;; ORIGINAL FOR BRANCHING
; Fork: Weapons and shields
C39B72:

org $C39BB2
; Fork: Helmet list
C39BB2:

org $C39BEE
; Fork: Armor list
C39BEE:

;; END ORIGINAL

org $C3A051
; Compile compatible relics
C3A051:  JSR $9C2A      ; Init list
         JSR $9C41      ; Define compat
         LDA #$20        ; Palette 0
         STA $29         ; Color: User's
         LDX $00         ; Clear X...
         TXY             ; Item slot: 1
C3A05E:  TDC             ; Clear A
         LDA $1869,Y     ; Item in slot
         CMP #$FF        ; None?
         BEQ C3A088      ; Skip if so
         JSR $8321      ; Compute index
         LDX $2134       ; Load it
         LDA $D85000,X   ; Properties
         AND #$07        ; Get class
         CMP #$05        ; Relic?
         BNE C3A088      ; Skip if not
         REP #$20        ; 16-bit A
         LDA $D85001,X   ; Compatibility
         BIT $E7         ; Actor can use?
         BEQ C3A088      ; Skip if not
         SEP #$20        ; 8-bit A
         TYA             ; Item slot
         STA $2180       ; Add to list
         INC $E0         ; List size +1
C3A088:  SEP #$20        ; 8-bit A
         INY             ; Item slot +1
         CPY #$0100      ; Done all 256?
         BNE C3A05E      ; Loop if not
         LDA $E0         ; List size
         STA $7E9D89     ; Save to list
         RTS

org $C3A0E5
; duplicated from C3A0E5
C3A0E5:  LDA #$10        ; Description: Off
         TSB $45         ; Set menu flag
; mostly duplicated from C3994D
         JSR $9C87       ; Clear stat preview
         REP #$20        ; 16-bit A
         LDA #$0100      ; BG1 H-Shift: 256
         STA $7E9BD0     ; Hide gear list
         SEP #$20        ; 8-bit A
         LDA #$C1        ; Top cursor: Off
         TRB $46         ; Scrollbar: Off
         JSR $8E6C       ; Load navig data
         LDA $5E
         STA $4E
         JSR C3A0E5_cursor
         LDA #$55        ; C3/9884
         STA $26         ; Next: Body parts
         RTS

org $C3A1C3
;print "C3A1C3 is at: ",pc
; Load item description for equipped relic (unused)
C3A1C3:  JSR $8308      ; Set desc ptrs
         JSR $93F2      ; Define Y (Character SRAM block)
         REP #$20       ; 16-bit A
         TYA            ; Character in A
         ADC $4B        ; Add slot index
         TAY            ; And return to Y
         SEP #$20       ; 8-bit A
         TDC
         LDA $001F,Y
C3A1D5:  JMP $5738      ; Load description

org $C3A2A6
; Text pointers for Equip menu
C3A2A6:  dw C3A31A       ; EQUIP
         dw C3A322       ; OPTIMUM
         dw C3A32C       ; RMOVE
         dw C3A334       ; EMPTY
C3A2AE:  dw C3A2BA       ; R-hand
         dw C3A2C3       ; L-hand
         dw C3A2CC       ; Head
         dw C3A2D3       ; Body
         dw C3A2DA       ; Relic 1
         dw C3A2E2       ; Relic 2

; Positioned text for options in Equip menu
C3A31A:  dw $789D : db "EQP",$00
C3A322:  dw $78A9 : db "OPT",$00
C3A32C:  dw $78B5 : db "RM",$00
C3A334:  dw $78BF : db "EMP",$00

org $C3A6AB
; Build description tilemap for Relic menu
C3A6AB:  LDX #$7849      ; Base: 7E/7849
         STX $EB         ; Set map ptr LBs
         LDA #$7E        ; Bank: 7E
         STA $ED         ; Set ptr HB
         LDY #$013C      ; Ends at 30,7
         STY $E7         ; Set row's limit
         LDY #$0104      ; Starts at 3,7
         LDX #$3500      ; Tile 256, pal 5
         STX $E0         ; Priority enabled
         JSR $A783      ; Do line 1, row 1
         LDY #$017C      ; Ends at 30,8
         STY $E7         ; Set row's limit
         LDY #$0144      ; Starts at 3,8
         LDX #$3501      ; Tile 257, pal 5
         STX $E0         ; Priority enabled
         JSR $A783      ; Do line 1, row 2
         LDY #$01BC      ; Ends at 30,9
         STY $E7         ; Set row's limit
         LDY #$0184      ; Starts at 3,9
         LDX #$3538      ; Tile 312, pal 5
         STX $E0         ; Priority enabled
         JSR $A783      ; Do line 2, row 1
         LDY #$01FC      ; Ends at 30,10
         STY $E7         ; Set row's limit
         LDY #$01C4      ; Starts at 3,10
         LDX #$3539      ; Tile 313, pal 5
         STX $E0         ; Priority enabled
         JMP $A783      ; Do line 2, row 2

;org $C3F43B
;         JSR $9110

; org $C3FBD0
; FREE SPACE

warnpc $C40000

; USE FREE SPACE

org $C3F200
C31BB8_jump:
         JSR C31BBD      ; Init variables
         JMP C31BD7

C31BBD_extension:
         JSR $1B99       ; Queue desc anim
         JSR $94B6       ; Set to shift text
         RTS

; Draw actor info in Equip menu, update status based on gear
C3911B_extension:
         LDA #$20        ; Palette 0
         STA $29         ; Color: User's
redo_genji_merit:
         JSR $913E       ; Do stats; status
         BRA do_genji    ; Handle no genji glove flag
done_genji:
         BRA do_merit    ; Handle no merit award flag
done_merit:
         LDA #$05
done_merit_loop:
         PHA
         JSR draw_generic_equipped
         PLA
         DEC
         BPL done_merit_loop
         RTS
do_genji:
         LDA #$10
         BIT $11D8
         BNE do_genji_exit
         LDY #$0001
do_genji_loop:
         LDA $11C6,Y
         CMP #$FF
         BEQ do_genji_exit
         JSR $8321       ; Compute index
         LDX $2134       ; Load it
         LDA $D85000,X   ; Properties
         AND #$07        ; Get class
         CMP #$03        ; Shield?
         BEQ do_genji_exit  ; Exit if so
         DEY
         BPL do_genji_loop
         JSR $93F2       ; Define Y
         INY
         JSR genji_merit_remove_equipped_item
         BRA redo_genji_merit
do_genji_exit:
         BRA done_genji
do_merit:
         LDA #$20
         BIT $11D8
         BNE do_merit_exit
         JSR $9C41
         LDY #$0005
do_merit_loop:
         LDA $11C6,Y
         CMP #$FF
         BEQ do_merit_loop_skip
         JSR $8321       ; Compute index
         LDX $2134       ; Load it
         REP #$20
         LDA $D85001,X   ; Compatibility
         BIT $E7         ; Actor can use?
         SEP #$20
         BEQ do_merit_violation
do_merit_loop_skip:
         DEY
         BPL do_merit_loop
do_merit_exit:
         BRA done_merit
do_merit_violation:
         PHY
         JSR $93F2       ; Define Y
         REP #$20
         TYA
         CLC
         ADC $01,S
         PLY
         TAY
         SEP #$20
         JSR genji_merit_remove_equipped_item
         BRL redo_genji_merit
genji_merit_remove_equipped_item:
         LDA $001F,Y     ; SRAM equipment location
         JSR $9D5E
         LDA #$FF
         STA $001F,Y
         RTS

; Cursor positions for Equip menu slot selection
C38E80:  dw $3800        ; R-Hand
         dw $3878        ; L-Hand
         dw $4400        ; Head
         dw $4478        ; Body
         dw $5000        ; Relic 1
         dw $5078        ; Relic 2

draw_generic_equipped:
         REP #$20
         AND #$00FF
         PHA
         ASL
         TAX
         LDA C3A2AE,X
         TAX
         PHA
         LDA $C30000,X
         TAX
         JSR $946D       ; Set Y, coords
         REP #$20
         TYA
         ADC $03,S
         TAY
         SEP #$20
         LDA #$20        ; Palette 0
         STA $29         ; Color: User's
         LDA $001F,Y     ; Armor
         PLY
         PLX
         CMP #$FF        ; Is it empty?
         BEQ drEmpty     ; If so, branch and draw empty slot
         JMP $9479       ; Draw its name
drEmpty: LDA #$24        ; Palette 1
         STA $29         ; Color: Gray
         JMP $02F9

; 36: Handle Equip menu options
; some duplicated code at C3A097
C39621:  LDA #$10        ; Description: Off
         TSB $45         ; Set menu flag
         ; JSR $1368       ; Refresh screen
         JSR $9E14       ; Queue BG3 upload
         JSR $904E       ; Draw options
         JSR $8E56       ; Handle D-Pad
         LDA $08         ; No-autofire keys
         BIT #$80        ; Pushing A?
         BEQ C39635      ; Branch if not
         JSR $0EB2       ; Sound: Click
         JMP $9664       ; Handle selection

; Fork: Handle B
C39635:  LDA $09         ; No-autofire keys
         BIT #$80        ; Pushing B?
         BEQ C39648      ; Branch if not
         JSR $0EA9       ; Sound: Cursor
         JSR $9110       ; Update field FX
         LDA #$04        ; C3/1A8A
         STA $27         ; Queue main menu
         STZ $26         ; Next: Fade-out
         RTS

; Fork: Handle L and R, prepare for menu reset
C39648:  LDA #$35        ; C3/1BB8
         STA $E0         ; Set init command
         JMP $2022       ; Handle L and R

do_optimum:
         JSR $9685
         JSR C3911B
         RTS

load_description:
         LDA $09
         BIT #$40        ; Pushing Y?
         BEQ .nodsc      ; Branch if not
         LDA #$10
         TRB $45
.nodsc   JSR $9E14       ; Queue text upload
         JSR $8E72       ; Handle D-Pad
         JSR C3A1C3      ; Load description for equipped gear
         RTS

C3990F_description:
         LDA #$10        ; Description: On
         TRB $45         ; Set menu flag
         JSR $9E14
         JSR $9AD3       ; Handle navigation
         JSR $A1D8
         RTS

C3990F_not_a:
; diverges from C3A097 here
         JSR $9A42       ; On a gray item?
         BCS C3990F_success ; Fail if so
         JMP C3996E
C3990F_success:
         JMP C39920

C39944_close:
         JSR $0EA9       ; Sound: Cursor
         LDA $F0
         STA $11D8
         RTS

C3A0E5_cursor:
         LDA $5D
         STA $4D         ; cursor column?
         JSR $8E75       ; Relocate cursor
         JSR $1368       ; Refresh screen
         RTS

C39887_section_a:
         LDA $4B         ; Cursor position
         STA $5F         ; Set body slot
         LDA $4E         ; Cursor row
         STA $5E         ; Set cursor row
         LDA $4D         ; Get cursor column
         STA $5D         ; Save cursor column
         LDA $11D8       ; Get gear effects
         STA $F0         ; And save
         RTS

C39887_section_b:
         LDA #$10        ; Description: Off
         TSB $45         ; Set menu flag
         JSR $6A15       ; Blank item list
         JSR $9E23       ; Queue BG3 upload
         RTS

C398B4_extension:
         JSR $8E59       ; Relocate cursor
         LDA #$10        ; Description: Off
         TSB $45         ; Set menu flag
         JSR $1368       ; Refresh screen
         RTS

; Replacement positioned text for main menu
review:  dw $7AB9 : db "Review",$00

; Highlight "Equip", gray out "Remove" and "Empty"
C3964F_extension:
         JSR gray_options
         LDY #C3A31A     ; Text pointer
         JMP $02F9

; Highlight "Remove", gray out "Equip" and "Empty"
C39656_extension:
         JSR gray_options
         LDY #C3A32C     ; Text pointer
         JMP $02F9

gray_options:
         LDA #$24
         STA $29
         LDY #C3A31A
         JSR $02F9
         LDY #C3A334
         JSR $02F9
         LDY #C3A322
         JSR $02F9
         LDY #C3A32C
         JSR $02F9
         JSR $808A
         RTS

; Positioned text for Equip and Relic menus
C3A2BA:  dw $7A8D : db " R-hand      ",$00
C3A2C3:  dw $7AAB : db " L-hand      ",$00
C3A2CC:  dw $7B0D : db " Head        ",$00
C3A2D3:  dw $7B2B : db " Body        ",$00
C3A2DA:  dw $7B8D : db " Relic       ",$00
C3A2E2:  dw $7BAB : db " Relic       ",$00

