exhirom
table ff6_snes_menu_a.tbl,rtl

; BANK 57
org $570000
bank_start:
org $57e000

shop_pointer_table:
for i = 0..86
    dw test_data
endfor

db $00,$00
skip align $10

get_shop_data_pointer:
        pha
        tdc
        lda $0201
        rep #$20
        asl
        tax
        lda.l shop_pointer_table,x
        tax
        sep #$20
        pla
        rts

get_material_data_pointer:
        pha
        phy
        phb
        jsr get_shop_data_pointer
        tdc
        lda $4e
        beq get_material_data_pointer_exit
        tay
        phk
        plb
get_material_data_pointer_loop:
        lda $0000,x
        inc
        beq get_material_data_pointer_subdone
        inx
        inx
        inx
        bra get_material_data_pointer_loop
get_material_data_pointer_subdone:
        inx
        dey
        bne get_material_data_pointer_loop
get_material_data_pointer_exit:
        plb
        ply
        pla
        rts

pre_craft_menu_root:
        lda #$01
        sta $28
        stz $2a
        rtl

craft_menu_root:
        lda $28
        cmp $2a
        beq skip_display_materials
        jsr display_all_craft_materials
        lda $28
        sta $2a
skip_display_materials:
        pea $bbc0
        jsr remote_call
        pea $bb53
        jsr remote_call
        rtl

display_all_craft_materials:
        pea $c2f7
        jsr remote_call
        jsr get_material_data_pointer
        ldy #$0000
display_all_loop:
        lda.l bank_start,x
        cmp #$ff
        beq display_all_terminate
        inx
        inx
        inx
        iny
        bra display_all_loop
display_all_terminate:
        cpy #$0000
        beq display_no_materials
        cpy #$0004
        bcs display_only_materials
        ldy #str_materials
        jsr draw_text
        ldy #$0001
        bra display_materials
display_no_materials:
        ldy #str_no_materials_required1
        jsr draw_text
        ldy #str_no_materials_required2
        jsr draw_text
        rts
display_only_materials:
        ldy #$0000
display_materials:
        jsr get_material_data_pointer
        lda #$00
        xba
display_materials_loop:
        lda.l bank_start,x
        cmp #$ff
        beq display_materials_terminate
        phx
        phy
        pha
        lda.l bank_start+2,x
        tax
        pla
        jsr display_craft_material
        ply
        plx
        inx
        inx
        inx
        iny
        bra display_materials_loop
display_materials_terminate:
        rts

multiply_by_quantity:
        pha
multiply_by_not_hblank:
        lda $4212
        and #$40
        beq multiply_by_not_hblank
        pla
        sta $211b
        stz $211b
        lda $28
        sta $211c
        sta $211c
        lda $2134
        rts

; A: item index, X: quantity, Y: row
display_craft_material:
        php
        pha
        phx

        phy
        pea $c068        ; load item name
        jsr remote_call
        ply

        rep #$20
        tya
        asl a            ; row index * 0x80
        asl a
        asl a
        asl a
        asl a
        asl a
        asl a
        clc
        adc #$7a0d       ; offset of first row
        sta $7e9e89

        plx
        txa
        sep #$20
        jsr multiply_by_quantity
        tax
        phx
        lda $03,s
        jsr display_material_appropriate_color

        pea $7fd9        ; draw item name
        jsr remote_call

        plx
        txa
        cmp #$64
        bcs display_material_too_many
        pea $04e0        ; prepare digits
        jsr remote_call
        lda $7e9e89
        clc
        adc #$1e         ; 15 columns over
        tax
        pea $04b6        ; draw digits at location in X
        jsr remote_call

        pla
        plp
        rts
display_material_too_many:
        jsr grey_text_color

        rep #$20
        lda $7e9e89
        clc
        adc #$001e         ; 15 columns over
        tax
        sep #$20
        lda #$ca           ; "+" symbol
        sta $f8
        sta $f9
        pea $04b6
        jsr remote_call

        pla
        plp
        rts

display_material_appropriate_color:
        jsr check_inventory
        bcc appropriate_color_sufficient
appropriate_color_insufficient:
        jmp grey_text_color
appropriate_color_sufficient:
        jmp user_text_color

; item index in A, quantity in X
check_inventory:
        pha
        phx
        phy
        ldy #$0000
check_inventory_loop:
        cmp $1869,y
        beq check_inventory_found_item
        iny
        cpy #$0100
        bcc check_inventory_loop
        bra check_inventory_insufficient
check_inventory_found_item:
        txa
        cmp $1969,y
        bcc check_inventory_sufficient
        beq check_inventory_sufficient
check_inventory_insufficient:
        ply
        plx
        pla
        sec
        rts
check_inventory_sufficient:
        ply
        plx
        pla
        clc
        rts

; pointer to materials data in X
check_all_materials:
        lda.l bank_start,x
        cmp #$ff
        beq check_all_materials_success
        pha
        lda.l bank_start+2,x
        jsr multiply_by_quantity
        tay
        pla
        phx
        tyx
        ply
        jsr check_inventory
        bcs check_all_materials_failure
        tyx
        inx
        inx
        inx
        bra check_all_materials
check_all_materials_success:
        clc
        rts
check_all_materials_failure:
        sec
        rts

check_increase_without_materials:
        lda $28
        cmp $6a
        beq check_increase_cannot_increase
        jsr get_material_data_pointer
        jsr check_all_materials
        bcs check_increase_cannot_increase
check_increase_can_increase:
        rep #$02
        rtl
check_increase_cannot_increase:
        sep #$02
        rtl

; item index in A, quantity in X
deduct_item:
        ldy #$0000
deduct_item_loop:
        cmp $1869,y
        beq deduct_found_item
        iny
        bra deduct_item_loop
deduct_found_item:
        phx
        lda $1969,y
        sec
        sbc $01,s
        sta $1969,y
        plx
        rts

craft_buy:
        jsr get_material_data_pointer
        jsr check_all_materials
        bcs craft_buy_failure
craft_buy_success:
        ; deduct materials from inventory
        jsr get_material_data_pointer
craft_buy_deduct_loop:
        lda.l bank_start,x
        cmp #$ff
        beq craft_buy_deduct_terminate
        pha
        lda.l bank_start+2,x
        jsr multiply_by_quantity
        phx
        tax
        lda $03,s
        jsr deduct_item
        plx
        pla
        inx
        inx
        inx
        bra craft_buy_deduct_loop
craft_buy_deduct_terminate:
        pea $0ece
        jsr remote_call
        pea $b5b7
        jsr remote_call
        bra craft_buy_exit
craft_buy_failure:
        pea $0ec0
        jsr remote_call
craft_buy_exit:
        rtl

user_text_color:
        lda #$20        ; palette 0
        bra store_tcolor
grey_text_color:
        lda #$24        ; grey text
store_tcolor:
        sta $29         ; color: user's
        rts

draw_text:
        sty $e7
        phk
        pla
        pea $02fd
        jsr remote_call
        rts

str_materials:
    dw $7a0d : db "Materials:",$00
str_no_materials_required1:
    dw $7a8f : db "Materials",$00
str_no_materials_required2:
    dw $7b13 : db "not required.",$00

remote_call:
        phk
        per remote_call_return-1
        pea.w c3_rtl-1
        pea $c3ff
        php
        php
        rep #$20
        pha
        lda $0e,s
        dec
        sta $04,s
        pla
        plp
        rtl
remote_call_return:
        php
        rep #$20
        pha
        lda $04,s
        sta $06,s
        lda $03,s
        sta $05,s
        pla
        plp
        plp
        plp
        rts

db $00,$00
skip align $10

test_data:
test_data1:
    ;db $6d,$01,$6e,$02,$6f,$01,$70,$03,$71,$04,$ff
    ;db $6d,$01,$6e,$02,$6f,$01,$ff
    db $6d,$00,$01,$6e,$00,$02,$6f,$00,$01,$ff
    db $ff
    db $6f,$00,$02,$6b,$00,$01,$ff
test_data2:
    db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

; BANK C3

org $c300bf
c3_rtl:
        rtl

org $c300dc
c3_rts:
        rts

org $c3b51a
        jsl check_increase_without_materials

org $c3b550
        ldx #$000a
tencrement_loop:
        phx
        jsr $b517
        plx
        dex
        bne tencrement_loop
        rts

org $c3b5b0
        jsl craft_buy
        rts

org $c3bacc
        jsl pre_craft_menu_root
        jmp $bbae

org $c3bbae
        jsl craft_menu_root
        jmp $bbd7

; Move buy/sell item to top window
org $c3bab1
dw $7921

; Move buy/sell quantity to top window
org $c3bbd2
dw $793f

; Cursor to top window (buy)
org $c3b854
dw $0050
org $c3b859
dw $0010

; Cursor to top window (sell)
org $c3b656
dw $0050
org $c3b65b
dw $0010

; "GP" in left hand window
org $c3c33f
db $00

; Total Price
org $c3bbdb
dw $7ab3

; "Owned: XX"
org $c3c342
dw $7b33 : db "Owned:",$00

org $c3bc8d
dw $7b41

; "Equip: XX"
org $c3c34b
dw $7bb3 : db "Equip:",$00

org $c3bfbd
dw $7bc1

; Original "How many?" text string
org $c3c398
db $00
org $c3c3b3
db $00

org $c3c3bd
dw $7921 : db "  Bye!"
