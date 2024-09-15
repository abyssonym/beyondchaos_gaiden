exhirom
incsrc ff6_snes_menu_a.include

!blt = bcc
!bge = bcs

!dummy_rts = $c300dc
!dummy_rtl = $c300bf

!new_item_type = $06
!new_toolbar_index = $02
!new_menu_index = $18
!max_item_index = $fe
!item_name_length = 13
!sram_data_start = $316700

macro remote(address)
    phk
    per $0006
    pea.w !dummy_rtl-1
    jml $c3<address>
endmacro

; HOOKS

!interact_dispatcher_launch_address = $c301c4
!interact_dispatcher_return_address = $c301cd
org !interact_dispatcher_launch_address
    jml interact_dispatcher_hook

!initialize_dispatcher_launch_address = $c3261e
org !initialize_dispatcher_launch_address
    jml initialize_dispatcher_hook

!scroll_dispatcher_launch_address = $c382dd
org !scroll_dispatcher_launch_address
    jml scroll_dispatcher_hook

!toolbar_rare_text = $c38d2f
org !toolbar_rare_text
    db "PART",$00

; FREE SPACE

!bank_start = $570000
org $57d800

; DATA

skip align $10

item_name_pointers:
    for j = 0..$10
        for i = 0..$10
            dw test_names+(!i*12)
        endfor
    endfor

db $00,$00
skip align $10

item_order:
    for i = 0..!max_item_index+1
        db !i
    endfor

db $00,$00
skip align $10

; HOOK HANDLERS

interact_dispatcher_hook:
    rep #$20
    asl
    tax
    sep #$20
    cpx.w #!new_menu_index*2
    beq new_interact
    jml !interact_dispatcher_launch_address+4
new_interact:
    jsr interact_inventory
    jml !interact_dispatcher_return_address

initialize_dispatcher_hook:
    tdc
    lda $4b
    asl
    tax
    cpx.w #!new_toolbar_index*2
    beq new_initialize
    jml !initialize_dispatcher_launch_address+4
new_initialize:
    jsr initialize_inventory
    jml !dummy_rts

scroll_dispatcher_hook:
    tdc
    lda $2a
    asl
    tax
    cpx.w #!new_item_type*2
    beq new_scroll_type
    jml !scroll_dispatcher_launch_address+4
new_scroll_type:
    lda $0b
    bit #$0a
    beq new_scroll_not_scrolling_up
    jsr seek_previous
    lda $e5
    sta $4a
new_scroll_not_scrolling_up:
    jsr repaint_screen
    jml !dummy_rts

; MAIN INITIALIZE CODE

initialize_inventory:
    %remote(7d1c)   ; initialize cursor properties (main item window)
    %remote(7d25)   ; set cursor position
repaint_screen:
    %remote(6a15)   ; clear item window
    lda $2a
    cmp #!new_item_type
    beq no_reset_scroll_position
    stz $4a
no_reset_scroll_position:
    %remote(83f7)   ; initialize scroll position from $49,$4a to $e5,$e6
    jsr seek_next
    lda $e5
    sta $4a
    ldy #$0000
initialize_loop:
    phy
    jsr seek_next
    tdc
    lda $e5
    cmp #!max_item_index+1
    !bge initialize_max_index_reached
    tax
    tya
    clc
    adc $49
    and #$0f
    tay
    jsr draw_row_y
    inc $e5
    ply
    iny
    cpy #$000b
    bne initialize_loop
initialize_resume:
    lda #!new_menu_index
    sta $26         ; set menu state
    lda #!new_item_type
    sta $2a
    rts
initialize_max_index_reached:
    ply
    bra initialize_resume

seek_next:
    php
    sep #$20
    tdc
    lda $e5
seek_next_loop:
    tax
    jsr get_scroll_quantity
    bne seek_next_found
    txa
    cmp #!max_item_index
    !bge seek_next_fail
    inc
    beq seek_next_fail
    bra seek_next_loop
seek_next_found:
    txa
    sta $e5
seek_next_fail:
    plp
    rts

seek_previous:
    php
    sep #$20
    tdc
    lda $e5
seek_previous_loop:
    tax
    jsr get_scroll_quantity
    bne seek_previous_found
    txa
    cmp #$00
    beq seek_previous_fail
    dec
    bra seek_previous_loop
seek_previous_found:
    txa
    sta $e5
seek_previous_fail:
    plp
    rts

get_scroll_quantity:
    phx
    sep #$20
    lda.l item_order,x
    tax
    jsr get_item_quantity
    cmp #$00
    beq get_scroll_quantity_zero
    plx
    rep #$02
    rts
get_scroll_quantity_zero:
    plx
    sep #$02
    rts

get_item_quantity:
    cpx.w #!max_item_index+1
    !bge get_item_quantity_no_item
    lda.l !sram_data_start,x
    rts
get_item_quantity_no_item:
    lda #$00
    rts

; X - Scroll index (precalculated)
; Y - Row
draw_row_y:
    php
    sep #$20
    cpx.w #!max_item_index+1
    !bge draw_row_blank
    lda.l item_order,x
    tax
    jsr get_item_quantity
    beq draw_row_blank
    jsr draw_item_x_at_row_y_quantity_a
draw_row_y_exit:
draw_row_blank:
    plp
    rts

draw_item_x_at_row_y_quantity_a:
    php
    phy

    bit #$80
    beq draw_row_set_highlight
    and #$7f
    pha
    beq draw_row_set_grey
    lda #$20
    bra draw_row_set_row_color
draw_row_set_grey:
    lda #$28
    bra draw_row_set_row_color
draw_row_set_highlight:
    pha
    lda #$24
draw_row_set_row_color:
    sta $29

    tya
    asl
    inc
    phx
    ldx #$0004
    %remote(809f)   ; calculate coordinates from A and X
    plx
    rep #$20
    sta $7e9e89

    txa
    jsr load_item_a_name
    jsr draw_loaded_text

    sep #$20
    tdc
    pla
    jsr draw_quantity

    ply
    plp
    rts

load_item_a_name:
    php
    rep #$30

    ldx #$9e8b
    stx $2181
    
    asl
    tax
    lda.l item_name_pointers,x
    tax

    ldy.w #!item_name_length+1

    sep #$20
    jsr wait_status

load_item_loop:
    lda.l !bank_start,x
    beq load_item_early_terminate
    sta $2180
    inx
    dey
    bne load_item_loop
load_item_early_terminate:
    lda #$ff
load_item_early_terminate_loop:
    sta $2180
    dey
    bne load_item_early_terminate_loop
    lda #$c1
    sta $2180
    stz $2180

    plp
    rts

wait_status:
    lda $4212
    and #$40
    beq wait_status
    rts

draw_loaded_text:
    php
    sep #$20
    %remote(7fd9)
    plp
    rts

draw_quantity:
    %remote(04e0)
    rep #$20
    lda $7e9e89
    clc
    adc #$001e         ; 15 columns over
    tax
    sep #$20
    %remote(04b6)
    rts

; MAIN INTERACT CODE

interact_inventory:
    lda $09
    bit #$40
    beq interact_no_highlight

    jsr get_cursor_item
    cpx #$ffff
    beq interact_no_highlight

    eor #$80
    beq interact_no_highlight
    sta.l !sram_data_start,x

    pha
    tdc
    lda $49
    clc
    adc $4e
    and #$0f
    tay
    pla
    jsr draw_item_x_at_row_y_quantity_a

interact_no_highlight:
    lda $09
    bit #$80
    beq no_exit_inventory
    %remote(1f1f)   ; exit, without saving cursor memory
    bra interact_exit

no_exit_inventory:
    lda $0a
    bit #$10
    bne pagedown
    bit #$20
    bne pageup

    lda $4e
    bne interact_not_scrolling_up
    lda $0b
    bit #$0a
    beq interact_not_scrolling_up
    jsr check_can_scroll_up
    beq interact_exit

interact_not_scrolling_up:
    lda $4e
    cmp #$09
    !blt interact_not_scrolling_down
    lda $0b
    bit #$05
    beq interact_not_scrolling_down

    inc $4e
    jsr get_cursor_item
    dec $4e
    cpx #$ffff
    beq interact_exit

interact_not_scrolling_down:
    lda #!new_item_type
    sta $2a
    %remote(7d22)

interact_exit:
    rts

pagedown:
    lda $4a
    pha
    clc
    adc #$0a
    bcc pagedown_no_overflow
    lda #$ff
pagedown_no_overflow:
    cmp #!max_item_index-9
    !blt pagedown_no_overmax
    lda #!max_item_index-9
pagedown_no_overmax:
    sta $4a
    lda $4e
    pha
    lda #$09
    sta $4e
    jsr get_cursor_item
    pla
    sta $4e
    cpx #$ffff
    bne pagedown_exit
    lda $4a
    dec
    bmi pagedown_exit
    bra pagedown_no_overmax
pagedown_exit:
    pla
    cmp $4a
    bne page_exit
    lda #$09
    sta $4e
    bra page_exit_no_repaint

pageup:
    jsr check_can_scroll_up
    bne pageup_can_scroll_up
    lda #$00
    sta $4e
    bra page_exit_no_repaint
pageup_can_scroll_up:
    lda $4a
    sec
    sbc #$0a
    bcs pageup_no_overflow
    lda #$00
pageup_no_overflow:
    sta $4a

page_exit:
    %remote(0eb2)
page_exit_no_sound:
    jsr repaint_screen
page_exit_no_repaint:
    rts

get_cursor_item:
    tdc
    lda $4e
    tay
    lda $4a
    tax
    dex
get_cursor_loop:
    inx
    cpx.w #!max_item_index+1
    !bge get_cursor_fail
    jsr get_scroll_quantity
    beq get_cursor_loop
    dey
    bpl get_cursor_loop
    pha
    lda.l item_order,x
    tax
    pla
    rts
get_cursor_fail:
    ldx #$ffff
    rts

check_can_scroll_up:
    tdc
    lda $4a
    tax
check_can_scroll_loop:
    dex
    bmi check_can_scroll_fail
    jsr get_scroll_quantity
    beq check_can_scroll_loop
check_can_scroll_success:
    rts
check_can_scroll_fail:
    sep #$02
    rts

org $57c000
test_names:
    for i = 0..$10
        db "Test Item ",$80+!i,$00
    endfor
