exhirom
; header

table ff6_bank_c3.tbl,rtl
incsrc ff6_bank_c3_defs.asm

!blt = bcc
!bge = bcs

!dummy_rts = $c300dc
!dummy_rtl = $c300bf

!portrait_x = $00ca
!portrait_y = $000b

macro remote(address)
    phk
    per $0006
    pea.w !dummy_rtl-1
    jml $c3<address>
endmacro

org $c30247
    dw c39621           ; Update entry 36 in C301DB jump table

org $c30285
    dw c39884           ; Update entry 55 in C301DB jump table

org $c30289
    dw c3990f           ; Update entry 57 in C301DB jump table

org $c31bbd
; Initialize variables for Equip menu
c31bbd:
    jsr $352f           ; Reset/Stop stuff
    jsr $6a08           ; Set Win1 bounds
    jsl c31bbd_extension
    stz $4a             ; List scroll: 0
    stz $49             ; Top BG1 WR row: 1

assert pc() <= $c31bcb

org $c31be8
; Update JSR target (L+R switch)
    nop #3

org $c31bf6
; Update JSR target (L+R switch)
    nop #3

org $c31c01
; Update JSR and JMP targets
c31c01:
    jsr c31bbd          ; Reset variables
    jsr c39032          ; Draw menu; status
    jmp $808a           ; Switch windows

assert pc() <= $c31c0a

org $c32e72
; Update menu pointer
    dw $1eeb

org $c3372d
; Update text pointer
    dw review

org $c361bd
    dw !portrait_x

org $c361ce
    dw !portrait_y

org $c38e64
; Cursor positions for Equip menu options
c38e64:
    dw $2010            ; EQP
    dw $2038            ; OPT
    dw $2060            ; RM
    dw $2080            ; EMPTY

assert pc() <= $c38e6d

org $c38e72
    jsr $072d      ; Standard navigation, can wrap both sides
    ;jsr $81c7      ; "Line feed" wrap like in esper menu
    ;jsr custom_wrap_navigation_hook
; Update LDY pointer
    ldy #c38e80

assert pc() <= $c38e78

org $c38e7b
; Navigation data for Equip menu slot selection
c38e7b:
    db $00              ; Wraps all ways
    db $00              ; Initial column
    db $00              ; Initial row
    db $02              ; 2 column
    db $03              ; 3 rows

assert pc() <= $c38e80

org $c38e80             ; Moved to free space because of added relics
padbyte $ff : pad $c38e88
assert pc() <= $c38e88

org $c38fb4
    nop #12             ; Blanking out a routine that handles
; allowing/forbidding "Empty", which we no longer use
; This code DOES get executed in "Review",
; so we use NOPs

assert pc() <= $c38fc0

org $c39032
; Draw Equip menu, create portrait, update status via gear
c39032:
    jsr $9093           ; Do boxes; face
    jsr $911b           ; Draw info; status
    jsr c3a6ab
    nop #4
    lda $26
    cmp #$7e
    !bge c39032_skip_options
    jsr $904e           ; Draw top options
c39032_skip_options:
    jmp $0e6e           ; Upload BG3 A+B

padbyte $ff : pad $c3904e
assert pc() <= $c3904e

org $c39093
; Draw elements shared by Equip and Relic menus, create portrait
c39093:
    jsr $9110           ; Load actor stats
    rep #$20            ; 16-bit A
    lda #$0100          ; BG1 H-Shift: 256
    sta $7e9bd0         ; Hide gear list
    sep #$20            ; 8-bit A
    lda #$01            ; 64x32 at $0000
    sta $2107           ; Set BG1 map loc
    lda #$42            ; 32x64 at $4000
    sta $2109           ; Set BG3 map loc
    nop #3
    jsr $6a28           ; Clear BG2 map A
    jsr $6a2d           ; Clear BG2 map B
    ldy #c3947f         ; C3/947F
    jsr $0341           ; Draw bottom-level stats box
    ldy #c39483         ; C3/9487
    jsr $0341           ; Draw mid-level equipment box
    nop #3
    jsr $6a3c           ; Clear BG3 map A
    jsr $6a41           ; Clear BG3 map B
    lda $26
    cmp #$7e
    !bge c39093_skip_draw_name
    ldy #c39487         ; C3/9487
    jsr $0341           ; Draw top-level menu box
    jsr $93e5           ; Draw actor name
c39093_skip_draw_name:
    jsr $0e52           ; Upload windows
    jsr $6a15           ; Clear BG1 map A
    jsr $6a19           ; Clear BG1 map B
    jsr $0e28           ; Upload BG1 A+B
    jsr $0e36           ; Upload BG1 C...
    nop #9
    jsr $61b2           ; Create portrait
    lda #$2c            ; Palette 3
    sta $29             ; Color: Blue
    ldx #$a34d          ; Text ptrs loc
    ldy #$001c          ; Strings: 14
    jsr $69ba           ; Draw Vigor, etc.
    nop #4
    ldx #$a369          ; Text ptrs loc
    ldy #$0008          ; Strings: 4
    jsr $69ba           ; Draw Speed, etc.
    jmp $0e6e           ; Upload BG3 A+B

assert pc() <= $c39110

org $c3911b
; Draw actor info in Equip menu, update status based on gear
c3911b:
    jsl c3911b_extension
    jmp $0e6e           ; Upload BG3 A+B

padbyte $ff : pad $c39131
assert pc() <= $c39131

org $c393e5
; Draw actor name in Equip or Relic menu
c393e5:
    jsr $93f2           ; Actor's address
    lda #$2c            ; Palette 0
    sta $29             ; Color: Blue
    ldy #$7911          ; Text position
    jmp $34cf

assert pc() <= $c393f2

org $c3947f
; Window layout for Equip and Relic menus
c3947f:
    dw $5b4b,$0d1c      ; 30x14 at $5B4B (Bottom-level stats box)
c39483:
    dw $588b,$091c      ; 30x09 at $588B (Mid-level equipment box)
c39487:
    dw $58cd,$0213      ; Top-level menu box

padbyte $ff : pad $c39497
assert pc() <= $c39497

org $c3960c
; Switch to layout with options in Equip or Relic menu
c3960c:
    rts

padbyte $ff : pad $c39620
assert pc() <= $c39620

org $c39621
; 36: Handle Equip menu options
; some duplicated code at C3A097
c39621:
    jsr $9e14           ; Queue BG3 upload
    nop #3

assert pc() <= $c39627

org $c3966c
; Jump table for the above
c3966c:
    dw $9674            ; EQUIP
    dw c39685           ; OPTIMUM
    dw $968e            ; REMOVE
    dw $969f            ; EMPTY

assert pc() <= $c39674

org $c39674
; Leaving top menu to enter equip menu (formerly used to change colors)
; Update JSR targets
c39674:
    jsl clear_option_box
    nop #2

assert pc() <= $c3967a

org $c39685
c39685:
    jsr $96f0
    stz $4d
    jmp c3911b

padbyte $ff : pad $c3968e
assert pc() <= $c3968e

org $c3968e
; Leaving top menu to enter remove menu (formerly used to change colors)
; Update JSR target
c3968e:
    jsl clear_option_box
    nop #2

org $c396a2
; Update JSR target
    jsr c3911b          ; Redo text, status

org $c396a8
; Remove character's equipment
c396a8:
    ldx #$0005          ; Loop index
    bra rminit
_396a8:
    ldx #$0003          ; only remove non-relics when using optimum
rminit:
    phx
    jsr $93f2
    plx
rmloop:
    lda $001f,y         ; SRAM equipment location
    jsr $9d5e
    lda #$ff
    sta $001f,y
    iny
    dex
    bpl rmloop
    rts

padbyte $ff : pad $c396d2
assert pc() <= $c396d2

org $c396e9
; Minor optimisation -- called at start of /96F0 subroutine
    nop #3

org $c396f0
; Update JSR target
c396f0:
    jsr $9110           ; Get gear FX
    jsr _396a8

org $c39884
; 55: Handle selection of gear slot to fill
; some duplicated code at C398CF
c39884:
    jsl load_description
    nop #2
    lda $08             ; No-autofire keys
    bit #$80            ; Pushing A?
    beq c398b4          ; Branch if not
    jsr $0eb2           ; Sound: Click
; diverges from 398cf here?
    jsl c39884_section_a
    lda #$57            ; C3/990F
    sta $26             ; Next: Item list
    jsr c39b59          ; Build item list
    jsr $a150           ; Sort it by power
    jsr $9aeb           ; Cursor & Scrollbar
    lda #$55            ; Return here if..
    sta $27             ; ..list is empty
; section B
    jsl c39884_section_b
    nop #2
    jsr $1368           ; Refresh screen
    jmp $9cac           ; Draw item list

assert pc() == $c398b4

; Fork: Handle B (return to top menu from mid-level menu)
c398b4:
    lda $09             ; No-autofire keys
    bit #$80            ; Pushing B?
    beq c398b4_xy       ; Branch if not
c398b4_pushing_b:
    jsl restore_option_box
    rts

; Fork: Handle X and Y
c398b4_xy:
    jsl equip_menu_xy
    bcc c398c8
    rts

padbyte $ff : pad $c398c8
assert pc() <= $c398c8

org $c398c8
; Fork: Handle L and R, prepare for menu reset
c398c8:
    lda #$7e            ; C3/1BE5
    sta $e0             ; Set init command
    jmp $2022           ; Handle L and R

assert pc() <= $c398cf

org $c398cf
; 56: Handle manual gear removal (Remove menu)
c398cf:
    jsl c398cf_extension
    nop
    bit #$80
    beq c398f4

assert pc() <= $c398d8

org $c398f4
; Fork: Handle B
c398f4:
    lda $09             ; No-autofire keys
    bit #$80            ; Pushing B?
    beq c39908          ; Branch if not
    jsl restore_option_box
    rts

assert pc() <= $c39908
padbyte $ff : pad $c39908

; Fork: Handle L and R, prepare for menu reset
c39908:
    lda #$7f            ; C3/1BF3
    sta $e0             ; Set init command
    jmp $2022           ; Handle L and R

assert pc() <= $c3990f

org $c3990f
; LOTS of duplicated at C3A097 and C3990F*
; 57: Handle gear browsing
c3990f:
    jsl c3990f_description
    jsr $9233           ; Draw stat preview
; Fork: Handle A
    lda $08             ; No-autofire keys
    bit #$80            ; Pushing A?
    beq c39944
    jml c3990f_not_a

padbyte $ff : pad $c39920
assert pc() <= $c39920

org $c39920
; equip_fail_buzzer
c39920:
; duplicated from C39920
    jsr $0eb2           ; Sound: Click
    lda $001f,y         ; Item to unequip
    cmp #$ff            ; None?
    beq c3992d          ; Branch if so
    jsr $9d5e           ; Put in stock
c3992d:
    tdc                 ; Clear A
    lda $4b             ; Gear list slot
    tax                 ; Index it
    lda $7e9d8a,x       ; Inventory slot
    tax                 ; Index it
    lda $1869,x         ; Item in slot
    sta $001f,y         ; Equip on actor
    jsr $9d97           ; Adjust stock
    jsr c3911b          ; Redo text, status
    bra c39944_exit     ; Exit gear list

padbyte $ff : pad $c39944
assert pc() <= $c39944
org $c39944
; Fork: Handle B
c39944:
    lda $09             ; No-autofire keys
    bit #$80            ; Pushing B?
    beq c39944_rts      ; Exit if not
    jsl c39944_close
c39944_exit:
    jmp c3a0e5
c39944_rts:
    rts

padbyte $ff : pad $c3996e
assert pc() <= $c3996e

org $c39b59
; Compile compatible gear for actor's body part
c39b59:
    jsr $9c2a           ; Init list
    jsr $9c41           ; Define compat
    lda #$20            ; Palette 0
    sta $29             ; Color: User's
    lda $4b             ; Body part
    cmp #$02            ; Head?
    bcc c39b72          ; Fork: Weapons and shields
    beq c39bb2          ; Fork: Helmet list
    cmp #$04
    bcc c39bee          ; Fork: Armor list
    jmp $a051           ; Fork: Relics

assert pc() <= $c39b72

org $c39b72
; ; ORIGINAL FOR BRANCHING
; Fork: Weapons and shields
c39b72:

org $c39bb2
; Fork: Helmet list
c39bb2:

org $c39bee
; Fork: Armor list
c39bee:

; ; END ORIGINAL

org $c3a0e5
; duplicated from C3A0E5
c3a0e5:
    jsl backout
; mostly duplicated from C3994D
    jsr $9c87           ; Clear stat preview
    rep #$20            ; 16-bit A
    lda #$0100          ; BG1 H-Shift: 256
    sta $7e9bd0         ; Hide gear list
    sep #$20            ; 8-bit A
    lda #$c1            ; Top cursor: Off
    trb $46             ; Scrollbar: Off
    jsr $8e6c           ; Load navig data
    lda $5e
    sta $4e
    jsl c3a0e5_cursor
    rts

padbyte $ff : pad $c3a10a
assert pc() <= $c3a10a

org $c3a1c3
; Load item description for equipped gear
c3a1c3:
    jsr $8308           ; Set desc ptrs
    jsr $93f2           ; Define Y (Character SRAM block)
    rep #$20            ; 16-bit A
    tya                 ; Character in A
    adc $4b             ; Add slot index
    tay                 ; And return to Y
    sep #$20            ; 8-bit A
    tdc
    lda $001f,y
c3a1d5:
    jmp $5738           ; Load description

padbyte $ff : pad $c3a1d8
assert pc() <= $c3a1d8

org $c3a2a6
; Text pointers for Equip menu
c3a2a6:
    dw c3a31a           ; EQUIP
    dw c3a322           ; OPTIMUM
    dw c3a32c           ; RMOVE
    dw c3a334           ; EMPTY

assert pc() <= $c3a2ae

org $c3a2ae
c3a2ae:
    dw c3a2ba           ; R-hand
    dw c3a2c3           ; L-hand
    dw c3a2cc           ; Head
    dw c3a2d3           ; Body
    dw c3a2da           ; Relic 1
    dw c3a2e2           ; Relic 2

assert pc() <= $c3a2ba

org $c3a2ba
; Positioned text for Equip and Relic menus
c3a2ba:
    dw $7a8d : db " R-hand",$00
c3a2c3:
    dw $7aab : db " L-hand",$00
c3a2cc:
    dw $7b0d : db " Head",$00
c3a2d3:
    dw $7b2b : db " Body",$00
c3a2da:
    dw $7b8d : db " Relic",$00
c3a2e2:
    dw $7bab : db " Relic",$00
clear_slot:
    db "             ",$00

padbyte $ff : pad $c3a308
assert pc() <= $c3a308

org $c3a31a
; Positioned text for options in Equip menu
c3a31a:
    dw $7991 : db "EQP",$00
c3a322:
    dw $799b : db "OPT",$00
c3a32c:
    dw $79a5 : db "RM",$00
c3a334:
    dw $79ad : db "EMP",$00

padbyte $ff : pad $c3a33c
assert pc() <= $c3a33c

org $c3a6ab
; Build description tilemap for Relic menu
c3a6ab:
    ldx #$7849          ; Base: 7E/7849
    stx $eb             ; Set map ptr LBs
    lda #$7e            ; Bank: 7E
    sta $ed             ; Set ptr HB
    ldy #$013c          ; Ends at 30,7
    sty $e7             ; Set row's limit
    ldy #$0104          ; Starts at 3,7
    ldx #$3500          ; Tile 256, pal 5
    stx $e0             ; Priority enabled
    jsr $a783           ; Do line 1, row 1
    ldy #$017c          ; Ends at 30,8
    sty $e7             ; Set row's limit
    ldy #$0144          ; Starts at 3,8
    ldx #$3501          ; Tile 257, pal 5
    stx $e0             ; Priority enabled
    jsr $a783           ; Do line 1, row 2
    ldy #$01bc          ; Ends at 30,9
    sty $e7             ; Set row's limit
    ldy #$0184          ; Starts at 3,9
    ldx #$3538          ; Tile 312, pal 5
    stx $e0             ; Priority enabled
    jsr $a783           ; Do line 2, row 1
    ldy #$01fc          ; Ends at 30,10
    sty $e7             ; Set row's limit
    ldy #$01c4          ; Starts at 3,10
    ldx #$3539          ; Tile 313, pal 5
    stx $e0             ; Priority enabled
    jmp $a783           ; Do line 2, row 2

assert pc() <= $c3a6f4

; ; BANK C3 FREE SPACE C393FC-C3946D
org $c393fc             ; Writing over unused code

; Replacement positioned text for main menu
review:
    dw $7ab9 : db "Review",$00

; Cursor positions for Equip menu slot selection
c38e80:
    dw $3800            ; R-Hand
    dw $3878            ; L-Hand
    dw $4400            ; Head
    dw $4478            ; Body
    dw $5000            ; Relic 1
    dw $5078            ; Relic 2

custom_wrap_navigation_hook:
    jml custom_wrap_navigation

padbyte $ff : pad $c3946d
assert pc() <= $c3946d

; EX FREE SPACE
org $57e800

wait_for_interrupt:
    lda $26
    cmp #$7e
    !bge skip_wait
    cmp #$36
    !blt skip_wait
    wai
skip_wait:
    rts

skip align $10
; Called from C3911B, C39674, C3968E
write_arbitrary:
    sty $e7
    lda #$c3
    sta $e9
    rep #$20
    lda [$e7]
write_arbitrary_with_location:
    sta $eb
    stx $e7
    ldx $00
    txy
    %remote(030c)
    rts

skip align $10
; Draw actor info in Equip menu, update status based on gear
; Includes complex handling for Genji Glove and Merit Award
c3911b_extension:
    lda #$20            ; Palette 0
    sta $29             ; Color: User's
redo_genji_merit:
    %remote(913e)       ; Do stats; status
do_genji:               ; Handle no genji glove flag
    lda #$10
    bit $11d8
    bne done_genji
    ldy #$0001
do_genji_loop:
    lda $11c6,y
    cmp #$ff
    beq done_genji
    %remote(8321)       ; Compute index
    ldx $2134           ; Load it
    lda $d85000,x       ; Properties
    and #$07            ; Get class
    cmp #$03            ; Shield?
    beq done_genji      ; Exit if so
    dey
    bpl do_genji_loop
    %remote(93f2)       ; Define Y
    iny
    jsr genji_merit_remove_equipped_item
    bra redo_genji_merit

done_genji:
do_merit:               ; Handle no merit award flag
    lda #$20
    bit $11d8
    bne do_merit_exit
    %remote(9c41)
    ldy #$0005
do_merit_loop:
    lda $11c6,y
    cmp #$ff
    beq do_merit_loop_skip
    %remote(8321)       ; Compute index
    ldx $2134           ; Load it
    rep #$20
    lda $d85001,x       ; Compatibility
    bit $e7             ; Actor can use?
    sep #$20
    beq do_merit_violation
do_merit_loop_skip:
    dey
    bpl do_merit_loop
do_merit_exit:
    bra done_merit
do_merit_violation:
    phy
    %remote(93f2)       ; Define Y
    rep #$20
    tya
    clc
    adc $01,s
    ply
    tay
    sep #$20
    jsr genji_merit_remove_equipped_item
    brl redo_genji_merit

genji_merit_remove_equipped_item:
    lda $001f,y         ; SRAM equipment location
    %remote(9d5e)
    lda #$ff
    sta $001f,y
    rts

done_merit:
    lda #$05
done_merit_loop:
    pha
; Formerly JSR draw_generic_equipped
draw_generic_equipped:
    rep #$20
    and #$00ff
    pha
    asl
    tax
    lda $c3a2ae,x
    tax
    pha
    lda $c30000,x
    tax
    %remote(946d)       ; Set Y, coords
    rep #$20
    tya
    adc $03,s
    tay
    sep #$20
    lda #$20            ; Palette 0
    sta $29             ; Color: User's
    lda $001f,y         ; Armor
    ply
    plx
    cmp #$ff            ; Is it empty?
    beq drempty         ; If so, branch and draw empty slot
    %remote(9479)       ; Draw its name
drempty:
    lda #$24            ; Palette 1
    sta $29             ; Color: Gray
; Formerly JSR clear_and_write
clear_and_write:
    phy
    ldx #clear_slot
    jsr write_arbitrary
    ply
    %remote(02f9)
    pla
    dec
    bpl done_merit_loop
c3911b_extension_exit:
    jsr wait_for_interrupt
    rtl

skip align $10
c31bbd_extension:
    lda #$10            ; Reset/Stop desc
    tsb $45             ; Set menu flag
    %remote(1b99)       ; Queue desc anim
    rtl

skip align $10
; Called from C38E72
custom_wrap_navigation:
    lda $0b
    bit #$02
    bne custom_wrap_left
    bit #$01
    beq custom_wrap_wrapup
custom_wrap_right:
    lda $4d
    beq custom_wrap_wrapup
    inc $4e
    lda $54
    cmp $4e
    bne custom_wrap_wrapup
    stz $4e
    bra custom_wrap_wrapup
custom_wrap_left:
    lda $4d
    bne custom_wrap_wrapup
    dec $4e
    bpl custom_wrap_wrapup
    lda $54
    dec
    sta $4e
custom_wrap_wrapup:
    %remote(072d)
    jml !dummy_rts

skip align $10
; clear_option_box is called from C39674 and C3968E
clear_option_box:
    ldy #c3947f         ; C3/947F
    %remote(0341)       ; Draw stats box A
    ldy #c39483         ; C3/9487
    %remote(0341)       ; Draw option box

    lda $26
    cmp #$7e
    !bge clear_option_box_skip

    ldx #$7849          ; Base: 7E/7849
    stx $eb             ; Set map ptr LBs
    lda #$7e            ; Bank: 7E
    sta $ed             ; Set ptr HB

    ldy #$017c          ; Ends at 30,8
    sty $e7             ; Set row's limit
    ldy #$0144          ; Starts at 3,8
    ldx #$3501          ; Tile 257, pal 5
    stx $e0             ; Priority enabled
    %remote(a783)       ; Do line 1, row 2

    rep #$20
    lda #$7911
    ldx #clear_slot
    jsr write_arbitrary_with_location

    jsr wait_for_interrupt
clear_option_box_skip:
    %remote(0e52)       ; Upload windows
    %remote(0e6e)       ; Upload BG3 A+B
    %remote(a1c3)       ; Load description for equipped gear
    rtl

skip align $10
; Called from C39884, C398B4
load_description:
    lda #$10
    trb $45
    %remote(9e14)       ; Queue text upload
    %remote(8e72)       ; Handle D-Pad
    %remote(a1c3)       ; Load description for equipped gear
    rtl

skip align $10
; C39884 is called whenever navigating the mid-level equipment menu
c39884_section_a:
    lda $4b             ; Cursor position
    sta $5f             ; Set body slot
    lda $4e             ; Cursor row
    sta $5e             ; Set cursor row
    lda $4d             ; Get cursor column
    sta $5d             ; Save cursor column
    lda $11d8           ; Get gear effects
    sta $f0             ; And save
    rtl

c39884_section_b:
    lda #$10            ; Description: Off
    tsb $45             ; Set menu flag
    %remote(6a15)       ; Blank item list
    %remote(9e23)       ; Queue BG3 upload
    %remote(9233)       ; Draw stat preview
    rtl

skip align $10
c398cf_extension:
    jsl load_description
    lda $08             ; No-autofire keys
    rtl

skip align $10
; C3990F is called when browsing equippable items
c3990f_description:
    lda #$10            ; Description: On
    trb $45             ; Set menu flag
    %remote(9e14)       ; Queue text upload

    %remote(9ad3)       ; Handle navigation
    %remote(a1d8)
    rtl

c3990f_not_a:
; diverges from C3A097 here
    %remote(9a42)       ; On a gray item?
    bcs c3990f_success  ; Fail if so
    jml $c3996e
c3990f_success:
    jml c39920

skip align $10
; called from C398B4, C398F4
restore_option_box:
    %remote(0ea9)       ; Sound: Cursor
    %remote(8e50)       ; Load navig data

    lda #$10            ; Description: Off
    tsb $45             ; Set menu flag

    %remote(a796)       ; Clear description in RAM
    %remote(a991)       ; Set up description copy to VRAM

    ldy #c39483         ; C3/9487
    %remote(0341)       ; Draw option box

    ldy #c3947f         ; C3/947F
    %remote(0341)       ; Draw stats box A

    ldy #c39487         ; C3/9487
    %remote(0341)       ; Draw option box

    %remote(93e5)       ; Draw actor name
    %remote(904e)       ; Draw top options

    jsr wait_for_interrupt
    %remote(14ac)       ; Force copy to VRAM
    %remote(0e52)       ; Upload windows
    %remote(9e23)       ; Queue BG3 upload

    %remote(8e59)       ; Relocate cursor
    lda #$36            ; C3/9621
    sta $26             ; Next: Option list
    rtl

skip align $10
; called from C398B4
equip_menu_xy:
    bit #$40            ; Pushing Y?
    beq equip_menu_xy_not_y
    %remote(98d8)       ; Remove gear
    bra equip_menu_xy_exit_cleanup
equip_menu_xy_not_y:
    lda $08
    bit #$40            ; Pushing X?
    beq equip_menu_xy_exit_nothing
    lda $4d             ; Preserve cursor position
    pha
    %remote(9685)       ; Optimum
    pla
    sta $4d
equip_menu_xy_exit_cleanup:
    jsl backout
    sec
    rtl
equip_menu_xy_exit_nothing:
    clc
    rtl

skip align $10
; This part of C39944 is only called when canceling equippable items browser
c39944_close:
    %remote(0ea9)       ; Sound: Cursor
    lda $f0
    sta $11d8
    rtl

skip align $10
; called from C3A0E5
; C3A0E5 is called when leaving equippable items, by cancel or otherwise
backout:
    lda #$01
    tsb $45
    lda #$00
    sta $7e3649
    sta $7e9ec9
    rtl

c3a0e5_cursor:
    lda $5d
    sta $4d             ; cursor column?
    %remote(8e72)       ; Relocate cursor
    %remote(1368)       ; Refresh screen
    lda #$55            ; C3/9884
    sta $26             ; Next: Body parts
    rtl
