exhirom
; header

table ff6_bank_c3.tbl,rtl
incsrc ff6_bank_c3_defs.asm

!blt = bcc
!bge = bcs

!dummy_rts = $c300dc
!dummy_rtl = $c300bf

!PORTRAIT_X = $00ca
!PORTRAIT_Y = $000b

macro remote(address)
    phk
    per $0006
    pea.w !dummy_rtl-1
    jml $c3<address>
endmacro

org $C30247
         dw C39621      ; Update entry 36 in C301DB jump table

org $C30285
         dw C39884      ; Update entry 55 in C301DB jump table

org $C30289
         dw C3990F      ; Update entry 57 in C301DB jump table

org $C31BBD
; Initialize variables for Equip menu
C31BBD:  JSR $352F       ; Reset/Stop stuff
         JSR $6A08       ; Set Win1 bounds
         JSL C31BBD_extension
         STZ $4A         ; List scroll: 0
         STZ $49         ; Top BG1 WR row: 1

assert pc() <= $C31BCB

org $C31BE8
; Update JSR target (L+R switch)
         NOP #3

org $C31BF6
; Update JSR target (L+R switch)
         NOP #3

org $C31C01
; Update JSR and JMP targets
C31C01:  JSR C31BBD      ; Reset variables
         JSR C39032      ; Draw menu; status
         JMP $808A       ; Switch windows

assert pc() <= $C31C0A

org $C32E72
; Update menu pointer
         dw $1EEB

org $C3372D
; Update text pointer
         dw review

org $C361BD
         dw !PORTRAIT_X

org $C361CE
         dw !PORTRAIT_Y

org $C38E64
; Cursor positions for Equip menu options
C38E64:  dw $2010        ; EQP
         dw $2038        ; OPT
         dw $2060        ; RM
         dw $2080        ; EMPTY

assert pc() <= $C38E6D

org $C38E72
         ;JSR $072d      ; Standard navigation, can wrap both sides
         ;JSR $81c7      ; "Line feed" wrap like in esper menu
         JSR custom_wrap_navigation_hook
; Update LDY pointer
         LDY #C38E80

assert pc() <= $C38E78

org $C38E7B
; Navigation data for Equip menu slot selection
C38E7B:  db $00          ; Wraps all ways
         db $00          ; Initial column
         db $00          ; Initial row
         db $02          ; 2 column
         db $03          ; 3 rows

assert pc() <= $C38E80

org $C38E80     ; Moved to free space because of added relics
padbyte $ff : pad $C38E88
assert pc() <= $C38E88

org $C38FB4
         NOP #12         ; Blanking out a routine that handles
                         ; allowing/forbidding "Empty", which we no longer use
                         ; This code DOES get executed in "Review",
                         ; so we use NOPs

assert pc() <= $C38FC0

org $C39032
; Draw Equip menu, create portrait, update status via gear
C39032:  JSR $9093       ; Do boxes; face
         JSR $911B       ; Draw info; status
         JSR C3A6AB
         NOP #4
         LDA $26
         CMP #$7E
         !bge C39032_skip_options
         JSR $904E       ; Draw top options
C39032_skip_options:
         JMP $0E6E       ; Upload BG3 A+B

padbyte $ff : pad $C3904E
assert pc() <= $C3904E

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
         JSR $0341       ; Draw bottom-level stats box
         LDY #C39483     ; C3/9487
         JSR $0341       ; Draw mid-level equipment box
         NOP #3
         JSR $6A3C       ; Clear BG3 map A
         JSR $6A41       ; Clear BG3 map B
         LDA $26
         CMP #$7E
         !bge C39093_skip_draw_name
         LDY #C39487     ; C3/9487
         JSR $0341       ; Draw top-level menu box
         JSR $93E5       ; Draw actor name
         ;LDY #C3948F     ; C3/9497
         ;JSR $0341       ; Draw name box
C39093_skip_draw_name:
         JSR $0E52       ; Upload windows
         JSR $6A15       ; Clear BG1 map A
         JSR $6A19       ; Clear BG1 map B
         JSR $0E28       ; Upload BG1 A+B
         JSR $0E36       ; Upload BG1 C...
         NOP #9
         JSR $61B2       ; Create portrait
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

assert pc() <= $C39110

org $C3911B
; Draw actor info in Equip menu, update status based on gear
C3911B:  JSL C3911B_extension
         JMP $0E6E       ; Upload BG3 A+B

padbyte $ff : pad $C39131
assert pc() <= $C39131

org $C393E5
; Draw actor name in Equip or Relic menu
C393E5:  JSR $93F2       ; Actor's address
         LDA #$2C        ; Palette 0
         STA $29         ; Color: Blue
         LDY #$7911      ; Text position
         JMP $34CF

assert pc() <= $C393F2

org $C3947F
; Window layout for Equip and Relic menus
C3947F:  dw $5B4B,$0D1C  ; 30x14 at $5B4B (Bottom-level stats box)
C39483:  dw $588B,$091C  ; 30x09 at $588B (Mid-level equipment box)
C39487:  dw $58CD,$0213  ; Top-level menu box
;C3948F:  dw $5A4B,$0106  ; Name

padbyte $ff : pad $C39497
assert pc() <= $C39497

org $C3960C
; Switch to layout with options in Equip or Relic menu
C3960C:  RTS

padbyte $ff : pad $C39620
assert pc() <= $C39620

org $C39621
; 36: Handle Equip menu options
; some duplicated code at C3A097
C39621:  JSR $9E14       ; Queue BG3 upload
         NOP #3

assert pc() <= $C39627

org $C3966C
; Jump table for the above
C3966C:  dw $9674       ; EQUIP
         dw C39685      ; OPTIMUM
         dw $968E       ; REMOVE
         dw $969F       ; EMPTY

assert pc() <= $C39674

org $C39674
; Leaving top menu to enter equip menu (formerly used to change colors)
; Update JSR targets
C39674:  JSL clear_option_box
         NOP #2

assert pc() <= $C3967A

org $C39685
C39685:  JSR $96F0
         STZ $4D
         JMP C3911B

padbyte $ff : pad $C3968E
assert pc() <= $C3968E

org $C3968E
; Leaving top menu to enter remove menu (formerly used to change colors)
; Update JSR target
C3968E:  JSL clear_option_box
         NOP #2

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

padbyte $ff : pad $C396D2
assert pc() <= $C396D2

org $C396E9
; Minor optimisation -- called at start of /96F0 subroutine
         NOP #3

org $C396F0
; Update JSR target
C396F0:  JSR $9110        ; Get gear FX
         JSR _396A8

org $C39884
; 55: Handle selection of gear slot to fill
; some duplicated code at C398CF
C39884:  JSL load_description
         NOP #2
         LDA $08         ; No-autofire keys
         BIT #$80        ; Pushing A?
         BEQ C398B4      ; Branch if not
         JSR $0EB2       ; Sound: Click
; diverges from 398cf here?
         JSL C39884_section_a
         LDA #$57        ; C3/990F
         STA $26         ; Next: Item list
         JSR C39B59      ; Build item list
         JSR $A150       ; Sort it by power
         JSR $9AEB       ; Cursor & Scrollbar
         LDA #$55        ; Return here if..
         STA $27         ; ..list is empty
; section B
         JSL C39884_section_b
         NOP #2
         JSR $1368       ; Refresh screen
         JMP $9CAC       ; Draw item list

assert pc() == $C398B4

; Fork: Handle B (return to top menu from mid-level menu)
C398B4:  LDA $09         ; No-autofire keys
         BIT #$80        ; Pushing B?
         BEQ C398B4_XY   ; Branch if not
C398B4_pushing_b:
         JSL restore_option_box
         RTS

; Fork: Handle X and Y
C398B4_XY:
         JSL equip_menu_xy
         BCC C398C8
         RTS

padbyte $ff : pad $C398C8
assert pc() <= $C398C8

org $C398C8
; Fork: Handle L and R, prepare for menu reset
C398C8:  LDA #$7E        ; C3/1BE5
         STA $E0         ; Set init command
         JMP $2022       ; Handle L and R

assert pc() <= $C398CF

org $C398CF
; 56: Handle manual gear removal (Remove menu)
C398CF:  JSL C398CF_extension
         NOP
         BIT #$80
         BEQ C398F4

assert pc() <= $C398D8

org $C398F4
; Fork: Handle B
C398F4:  LDA $09         ; No-autofire keys
         BIT #$80        ; Pushing B?
         BEQ C39908      ; Branch if not
         JSL restore_option_box
         RTS

assert pc() <= $C39908
padbyte $ff : pad $C39908

; Fork: Handle L and R, prepare for menu reset
C39908:  LDA #$7F        ; C3/1BF3
         STA $E0         ; Set init command
         JMP $2022       ; Handle L and R

assert pc() <= $C3990F

org $C3990F
; LOTS of duplicated at C3A097 and C3990F*
; 57: Handle gear browsing
C3990F:
         JSL C3990F_description
         JSR $9233       ; Draw stat preview
; Fork: Handle A
         LDA $08         ; No-autofire keys
         BIT #$80        ; Pushing A?
         BEQ C39944
         JML C3990F_not_a

padbyte $ff : pad $C39920
assert pc() <= $C39920

org $C39920
; equip_fail_buzzer
C39920:
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
         
padbyte $ff : pad $C39944
assert pc() <= $C39944
org $C39944
; Fork: Handle B
C39944:  LDA $09         ; No-autofire keys
         BIT #$80        ; Pushing B?
         BEQ C39944_rts  ; Exit if not
         JSL C39944_close
C39944_exit:
         JMP C3A0E5
C39944_rts:
         RTS

padbyte $ff : pad $C3996e
assert pc() <= $C3996e

org $C39B59
; Compile compatible gear for actor's body part
C39B59:  JSR $9C2A       ; Init list
         JSR $9C41       ; Define compat
         LDA #$20        ; Palette 0
         STA $29         ; Color: User's
         LDA $4B         ; Body part
         CMP #$02        ; Head?
         BCC C39B72      ; Fork: Weapons and shields
         BEQ C39BB2      ; Fork: Helmet list
         CMP #$04
         BCC C39BEE      ; Fork: Armor list
         JMP $A051       ; Fork: Relics

assert pc() <= $C39B72

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

org $C3A0E5
; duplicated from C3A0E5
C3A0E5:  JSL backout
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
         JSL C3A0E5_cursor
         RTS

padbyte $ff : pad $C3A10A
assert pc() <= $C3A10A

org $C3A1C3
; Load item description for equipped gear
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

padbyte $ff : pad $C3A1D8
assert pc() <= $C3A1D8

org $C3A2A6
; Text pointers for Equip menu
C3A2A6:  dw C3A31A       ; EQUIP
         dw C3A322       ; OPTIMUM
         dw C3A32C       ; RMOVE
         dw C3A334       ; EMPTY

assert pc() <= $C3A2AE

org $C3A2AE
C3A2AE:  dw C3A2BA       ; R-hand
         dw C3A2C3       ; L-hand
         dw C3A2CC       ; Head
         dw C3A2D3       ; Body
         dw C3A2DA       ; Relic 1
         dw C3A2E2       ; Relic 2

assert pc() <= $C3A2BA

org $C3A2BA
; Positioned text for Equip and Relic menus
C3A2BA:  dw $7A8D : db " R-hand",$00
C3A2C3:  dw $7AAB : db " L-hand",$00
C3A2CC:  dw $7B0D : db " Head",$00
C3A2D3:  dw $7B2B : db " Body",$00
C3A2DA:  dw $7B8D : db " Relic",$00
C3A2E2:  dw $7BAB : db " Relic",$00
clear_slot:         db "             ",$00

padbyte $ff : pad $C3A308
assert pc() <= $C3A308

org $C3A31A
; Positioned text for options in Equip menu
C3A31A:  dw $7991 : db "EQP",$00
C3A322:  dw $799B : db "OPT",$00
C3A32C:  dw $79A5 : db "RM",$00
C3A334:  dw $79AD : db "EMP",$00

padbyte $ff : pad $C3A33C
assert pc() <= $C3A33C

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
         JSR $A783       ; Do line 1, row 1
         LDY #$017C      ; Ends at 30,8
         STY $E7         ; Set row's limit
         LDY #$0144      ; Starts at 3,8
         LDX #$3501      ; Tile 257, pal 5
         STX $E0         ; Priority enabled
         JSR $A783       ; Do line 1, row 2
         LDY #$01BC      ; Ends at 30,9
         STY $E7         ; Set row's limit
         LDY #$0184      ; Starts at 3,9
         LDX #$3538      ; Tile 312, pal 5
         STX $E0         ; Priority enabled
         JSR $A783       ; Do line 2, row 1
         LDY #$01FC      ; Ends at 30,10
         STY $E7         ; Set row's limit
         LDY #$01C4      ; Starts at 3,10
         LDX #$3539      ; Tile 313, pal 5
         STX $E0         ; Priority enabled
         JMP $A783       ; Do line 2, row 2

assert pc() <= $C3A6F4

;; BANK C3 FREE SPACE C393FC-C3946D
org $C393FC              ; Writing over unused code

; Replacement positioned text for main menu
review:  dw $7AB9 : db "Review",$00

; Cursor positions for Equip menu slot selection
C38E80:  dw $3800        ; R-Hand
         dw $3878        ; L-Hand
         dw $4400        ; Head
         dw $4478        ; Body
         dw $5000        ; Relic 1
         dw $5078        ; Relic 2

custom_wrap_navigation_hook:
        JML custom_wrap_navigation

padbyte $ff : pad $C3946D
assert pc() <= $C3946D

; EX FREE SPACE
org $57e800

wait_for_interrupt:
        LDA $26
        CMP #$7E
        !bge skip_wait
        CMP #$36
        !blt skip_wait
        WAI
skip_wait:
        RTS

skip align $10
; Called from C3911B, C39674, C3968E
write_arbitrary:
         STY $E7
         LDA #$C3
         STA $E9
         REP #$20
         LDA [$E7]
write_arbitrary_with_location:
         STA $EB
         STX $E7
         LDX $00
         TXY
         %remote(030C)
         RTS

skip align $10
; Draw actor info in Equip menu, update status based on gear
; Includes complex handling for Genji Glove and Merit Award
C3911B_extension:
         LDA #$20        ; Palette 0
         STA $29         ; Color: User's
redo_genji_merit:
         %remote(913E)   ; Do stats; status
do_genji:                ; Handle no genji glove flag
         LDA #$10
         BIT $11D8
         BNE done_genji
         LDY #$0001
do_genji_loop:
         LDA $11C6,Y
         CMP #$FF
         BEQ done_genji
         %remote(8321)   ; Compute index
         LDX $2134       ; Load it
         LDA $D85000,X   ; Properties
         AND #$07        ; Get class
         CMP #$03        ; Shield?
         BEQ done_genji  ; Exit if so
         DEY
         BPL do_genji_loop
         %remote(93F2)   ; Define Y
         INY
         JSR genji_merit_remove_equipped_item
         BRA redo_genji_merit

done_genji:
do_merit:                ; Handle no merit award flag
         LDA #$20
         BIT $11D8
         BNE do_merit_exit
         %remote(9C41)
         LDY #$0005
do_merit_loop:
         LDA $11C6,Y
         CMP #$FF
         BEQ do_merit_loop_skip
         %remote(8321)   ; Compute index
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
         %remote(93F2)   ; Define Y
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
         %remote(9D5E)
         LDA #$FF
         STA $001F,Y
         RTS

done_merit:
         LDA #$05
done_merit_loop:
         PHA
         ; Formerly JSR draw_generic_equipped
draw_generic_equipped:
         REP #$20
         AND #$00FF
         PHA
         ASL
         TAX
         LDA $C3A2AE,X
         TAX
         PHA
         LDA $C30000,X
         TAX
         %remote(946D)   ; Set Y, coords
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
         %remote(9479)   ; Draw its name
drEmpty: LDA #$24        ; Palette 1
         STA $29         ; Color: Gray
         ; Formerly JSR clear_and_write
clear_and_write:
         PHY
         LDX #clear_slot
         JSR write_arbitrary
         PLY
         %remote(02F9)
         PLA
         DEC
         BPL done_merit_loop
C3911B_extension_exit:
         JSR wait_for_interrupt
         RTL

skip align $10
C31BBD_extension:
         LDA #$10        ; Reset/Stop desc
         TSB $45         ; Set menu flag
         %remote(1B99)   ; Queue desc anim
         RTL

skip align $10
; Called from C38E72
custom_wrap_navigation:
        LDA $0B
        BIT #$02
        BNE custom_wrap_left
        BIT #$01
        BEQ custom_wrap_wrapup
custom_wrap_right:
        LDA $4D
        BEQ custom_wrap_wrapup
        INC $4E
        LDA $54
        CMP $4E
        BNE custom_wrap_wrapup
        STZ $4E
        BRA custom_wrap_wrapup
custom_wrap_left:
        LDA $4D
        BNE custom_wrap_wrapup
        DEC $4E
        BPL custom_wrap_wrapup
        LDA $54
        DEC
        STA $4E
custom_wrap_wrapup:
        %remote(072d)
        JML !dummy_rts

skip align $10
; clear_option_box is called from C39674 and C3968E
clear_option_box:
         LDY #C3947F     ; C3/947F
         %remote(0341)   ; Draw stats box A
         LDY #C39483     ; C3/9487
         %remote(0341)   ; Draw option box

         LDA $26
         CMP #$7E
         !bge clear_option_box_skip

         LDX #$7849      ; Base: 7E/7849
         STX $EB         ; Set map ptr LBs
         LDA #$7E        ; Bank: 7E
         STA $ED         ; Set ptr HB

         LDY #$017C      ; Ends at 30,8
         STY $E7         ; Set row's limit
         LDY #$0144      ; Starts at 3,8
         LDX #$3501      ; Tile 257, pal 5
         STX $E0         ; Priority enabled
         %remote(A783)   ; Do line 1, row 2

         REP #$20
         LDA #$7911
         LDX #clear_slot
         JSR write_arbitrary_with_location

         JSR wait_for_interrupt
clear_option_box_skip:
         %remote(0E52)   ; Upload windows
         %remote(0E6E)   ; Upload BG3 A+B
         %remote(A1C3)   ; Load description for equipped gear
         RTL

skip align $10
; Called from C39884, C398B4
load_description:
        LDA #$10
        TRB $45
        %remote(9E14)   ; Queue text upload
        %remote(8E72)   ; Handle D-Pad
        %remote(A1C3)   ; Load description for equipped gear
        RTL

skip align $10
;C39884 is called whenever navigating the mid-level equipment menu
C39884_section_a:
         LDA $4B         ; Cursor position
         STA $5F         ; Set body slot
         LDA $4E         ; Cursor row
         STA $5E         ; Set cursor row
         LDA $4D         ; Get cursor column
         STA $5D         ; Save cursor column
         LDA $11D8       ; Get gear effects
         STA $F0         ; And save
         RTL

C39884_section_b:
         LDA #$10        ; Description: Off
         TSB $45         ; Set menu flag
         %remote(6A15)   ; Blank item list
         %remote(9E23)   ; Queue BG3 upload
         %remote(9233)   ; Draw stat preview
         RTL

skip align $10
C398CF_extension:
        JSL load_description
        LDA $08         ; No-autofire keys
        RTL

skip align $10
; C3990F is called when browsing equippable items
C3990F_description:
         LDA #$10        ; Description: On
         TRB $45         ; Set menu flag
         %remote(9e14)   ; Queue text upload

         %remote(9ad3)   ; Handle navigation
         %remote(a1d8)
         RTL

C3990F_not_a:
; diverges from C3A097 here
         %remote(9a42)   ; On a gray item?
         BCS C3990F_success ; Fail if so
         JML $C3996E
C3990F_success:
         JML C39920

skip align $10
; called from C398B4, C398F4
restore_option_box:
         %remote(0EA9)   ; Sound: Cursor
         %remote(8E50)   ; Load navig data

         LDA #$10        ; Description: Off
         TSB $45         ; Set menu flag

         %remote(a796)   ; Clear description in RAM
         %remote(a991)   ; Set up description copy to VRAM

         LDY #C39483     ; C3/9487
         %remote(0341)   ; Draw option box

         LDY #C3947F     ; C3/947F
         %remote(0341)   ; Draw stats box A

         LDY #C39487     ; C3/9487
         %remote(0341)   ; Draw option box

         %remote(93E5)   ; Draw actor name
         %remote(904E)   ; Draw top options

         JSR wait_for_interrupt
         %remote(14ac)   ; Force copy to VRAM
         %remote(0E52)   ; Upload windows
         %remote(9E23)   ; Queue BG3 upload

         %remote(8E59)   ; Relocate cursor
         LDA #$36        ; C3/9621
         STA $26         ; Next: Option list
         RTL

skip align $10
; called from C398B4
equip_menu_xy:
         BIT #$40        ; Pushing Y?
         BEQ equip_menu_xy_not_y
         %remote(98D8)   ; Remove gear
         BRA equip_menu_xy_exit_cleanup
equip_menu_xy_not_y:
         LDA $08
         BIT #$40        ; Pushing X?
         BEQ equip_menu_xy_exit_nothing
         LDA $4D         ; Preserve cursor position
         PHA
         %remote(9685)   ; Optimum
         PLA
         STA $4D
equip_menu_xy_exit_cleanup:
         JSL backout
         SEC
         RTL
equip_menu_xy_exit_nothing:
         CLC
         RTL

skip align $10
; This part of C39944 is only called when canceling equippable items browser
C39944_close:
         %remote(0EA9)   ; Sound: Cursor
         LDA $F0
         STA $11D8
         RTL

skip align $10
; called from C3A0E5
; C3A0E5 is called when leaving equippable items, by cancel or otherwise
backout:
        LDA #$01
        TSB $45
        LDA #$00
        STA $7e3649
        STA $7e9ec9
        RTL

C3A0E5_cursor:
        LDA $5D
        STA $4D         ; cursor column?
        %remote(8E72)   ; Relocate cursor
        %remote(1368)   ; Refresh screen
        LDA #$55        ; C3/9884
        STA $26         ; Next: Body parts
        RTL
