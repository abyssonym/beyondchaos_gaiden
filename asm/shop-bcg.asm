hirom
table ff6_snes_menu_a.tbl,rtl

org $c388ce
ldx #$7B65
org $c388da
ldx #$7BE5
org $c388e6
ldx #$7C65
org $c388f2
ldx #$7CE5

org $C38D69
dw ele_resist
dw ele_absorb
dw ele_nullify
dw ele_weakness

org $c3baba
nop #3

org $c38746
;Fork: Draw offensive properties
C38746:    JSR $879C      ; Draw Bat.Pwr
        jsr gear_stats_evasion_attack_elements
        jsr load_item_index
        and #$80
        jsr check_text_color
        ldy #$8e30
        jsr process_text
        jsr load_item_index
        and #$40
        jsr check_text_color
        ldy #$8e38
        jsr process_text
        jsr load_item_index
        and #$02
        jsr check_text_color
        ldy #$8e26
        jsr process_text
        rts
padbyte $FF : pad $C38795
        
org $c3b4bd
        jsr handle_shop_stats
        rts
check_nmi:
        ;jsr $1368        ; NMI
        jsr $7fd9        ; draw name (from BAC9)
        rts
padbyte $FF : pad $c3b4e6

org $c3b8c4
        jsr clear_screen_bg2
        nop #6
        
org $C3B95a
        jsr clear_screen_bg3
C3B963:    JSR $BFD3      ; Draw title
C3B969:    LDY #$C338     ; Text pointer
C3B96C:    JSR $02F9      ; Draw "GP"
C3B96F:    JSR $C2F2      ; Color: User's
C3B972:    LDY $1860      ; Gold LBs
C3B975:    STY $F1        ; Memorize it
C3B977:    LDA $1862      ; Gold HB
C3B97A:    STA $F3        ; Memorize it
C3B97C:    JSR $0582      ; Turn into text
C3B97F:    LDX #$7A33     ; Text position
C3B982:    JSR $04AC      ; Draw GP total
C3B985:    RTS

org $c3b989
        ldy #help_text
        
org $c3bac9
        jsr check_nmi
        
org $c3bfd3
;draw shop title, define shop index
C3BFD3:    JSR $C2F7      ; Color: Blue
C3BFD6:    LDA $4212      ; PPU status
C3BFD9:    AND #$40       ; H-Blank?
C3BFDB:    BEQ C3BFD6      ; Loop if not
C3BFDD:    LDA $0201      ; Shop number
C3BFE0:    STA $211B      ; Set matrix A LB
C3BFE3:    STZ $211B      ; Clear HB
C3BFE6:    LDA #$09       ; Shop data size
C3BFE8:    STA $211C      ; Set matrix B
C3BFEB:    STA $211C      ; ...
C3BFEE:    LDX $2134      ; Index product
C3BFF1:    STX $67        ; Set shop index
C3BFF3:    LDA $C47AC0,X  ; Shop flags
C3BFF7:    AND #$07       ; Title number
C3BFF9:    ASL A          ; Double it
C3BFFA:    TAX            ; Index it
C3BFFB:    REP #$20       ; 16-bit A
C3BFFD:    LDA $C3C00C,X  ; Text pointer
C3C001:    STA $E7        ; Set src LBs
C3C003:    SEP #$20       ; 8-bit A
C3C005:    LDA #$C3       ; Bank: C3
C3C007:    STA $E9        ; Set src HB
C3C009:    JMP $02FF      ; Draw title

org $c3c037
        db $2f,$06,$00
org $c3c2e1
        jsr buy_menu_exp
org $c3f480
gear_stats_evasion_attack_elements:
C38749:    JSR $87EB      ; Draw evasions
        jsr $c2f7
C38750:    LDY #$8E1D     ; Text pointer
C38753:    JSR $02F9      ; Draw "Attack"
C38756:    jmp $88A0      ; Draw elements
        
check_text_color:
        beq grey_text_color
user_text_color:
        lda #$20        ; palette 0
        bra store_tcolor
grey_text_color:
        lda #$24        ; grey text
store_tcolor:
        sta $29            ; color: user's
        rts
        
load_item_index:
        ldx $2134        ; item index
        lda $d85013,x    ; byte to look at
        rts
        
process_text:
        sty $e7
        jsr $8795
        rts
        
clear_screen_bg2:
        jsr $6a28        ; clear bg2 map a
        jsr $6a2d        ; clear bg2 map b
        jsr $6a32        ; clear bg2 map c
        jsr $6a37        ; clear bg2 map d
        ldy #shop_gear_window
        jsr $0341        ; draw shop gear window
        ldy #shop_gear_window_actors
        jsr $0341        ; draw actors' window
        ldy #shop_gear_weapon_name
        jsr $0341
        ldy #shop_gear_description
        jsr $0341
clear_screen_bg3:
        jsr $6a3c        ; clear bg3 map a
        jsr $6a41        ; clear bg3 map b
        jsr $6a46        ; clear bg3 map c
        jsr $6a4b        ; clear bg3 map d
        rts
        
draw_title_dupe:
        ; jsr $02ff        ; draw title
handle_shop_stats:
handle_buy_item_list:
        lda #$10        ; reset/stop desc
        tsb $45            ; set menu flags
        lda $0D
        bit #$40        ; holding Y?
        bne shop_handle_y ; branch if not
        jsr $0f39        ; queue text upload
        ;jsr $1368
        jsr $b8a6        ; handle d-pad
        jsr check_stats
        jsr $bc84        ; draw quantity owned
        jsr $bca8        ; draw quantity worn
        bra shop_handle_b
;Handle hold Y
shop_handle_y:
        jsr $b8a6        ; handle d-pad
        jsr check_stats
        jsr $0f4d        ; queue text upload 2
        rep #$20        ; 16-bit A
        lda #$0100        ; BG2 scroll position
        sta $3b            ; BG2 Y position
        sta $3d            ; BG3 X position
        sep #$20        ; 8-bit A
        lda #$04        ; bit 2
        trb $45            ; set bit in menu flags A
        jsr gear_desc
not_press_y_this_frame:
        rts
;Fork: Handle B
shop_handle_b:
        stz $3c
        stz $3e
        lda #$04
        tsb $45
C3B4CD:    LDA $09        ; No-autofire keys
C3B4CF:    BIT #$80       ; Pushing B?
C3B4D1:    BEQ shop_handle_a     ; Branch if not
C3B4D3:    JSR $0EA9      ; Sound: Cursor
C3B4D6:    JMP $B760      ; Exit submenu

;Fork: Handle A
shop_handle_a:
C3B4D9:    LDA $08        ; No-autofire keys
C3B4DB:    BIT #$80       ; Pushing A?
        beq not_pushing_a
C3B4DF:    JSR $B82F      ; Set buy limit
C3B4E2:    JSR $B7E6      ; Test GP, stock
not_pushing_a:
        rts

buy_menu_exp:
        ;jsr $1368
        jsr $c2f7        ; color: blue
        ldy #text_vigor    ; text: vigor
        jsr $02f9        ; draw text
        ldy #text_magic ; text: magic
        jsr $02f9        ; draw text
        ldy #text_stamina ; text: stamina
        jsr $02f9        ; draw text
        ldy #text_speed ; text: speed
        jsr $02f9        ; draw text
        ldy #text_def    ; text: defense
        jsr $02f9        ; draw text
        ldy #text_mdef    ; text: magic defense
        jsr $02f9        ; draw text
        ldy #text_eva    ; text: evasion
        jsr $02f9        ; draw text
        ldy #text_meva    ; text: magic evasion
        jsr $02f9        ; draw text
        ldy #text_bpow  ; text: bat.pow.
        jsr $02f9        ; draw text
        jsr $bfc2        ; get item
        rts
        
check_stats:
        ;jsr $1368        ; trigger NMI
        pha
        phx
        phy
        php
        jsr $c2f2
        jsr $bfc2        ; get item
C38699:    JSR $8321      ; Compute index
C3869C:    LDX $2134      ; Load it
C3869F:    TDC            ; Terminator
C386A0:    STA $7E9E8D    ; Set mod B3
C386A4:    STA $7E9E8E    ; Set mod B4
C386A8:    REP #$20       ; 16-bit A
C386AA:    LDA #$8223     ; Tilemap ptr
C386AD:    STA $7E9E89    ; Set position
C386B1:    SEP #$20       ; 8-bit A
C386B3:    TDC            ; Clear A
C386B4:    LDA $D85010,X  ; Stat mods LB
C386B8:    PHA            ; Memorize them
C386B9:    AND #$0F       ; Vigor index
C386BB:    ASL A          ; Double it
C386BC:    JSR $8836      ; Draw modifier
C386BF:    REP #$20       ; 16-bit A
C386C1:    LDA #$82a3     ; Tilemap ptr
C386C4:    STA $7E9E89    ; Set position
C386C8:    SEP #$20       ; 8-bit A
C386CA:    TDC            ; Clear A
C386CB:    PLA            ; Stat mods LB
C386CC:    AND #$F0       ; Speed index
C386CE:    LSR A          ; Put in b3-b6
C386CF:    LSR A          ; Put in b2-b5
C386D0:    LSR A          ; Put in b1-b4
C386D1:    JSR $8836      ; Draw modifier
C386D4:    REP #$20       ; 16-bit A
C386D6:    LDA #$8241     ; Tilemap ptr
C386D9:    STA $7E9E89    ; Set position
C386DD:    SEP #$20       ; 8-bit A
C386DF:    LDX $2134      ; Item index
C386E2:    TDC            ; Clear A
C386E3:    LDA $D85011,X  ; Stats mods HB
C386E7:    PHA            ; Memorize them
C386E8:    AND #$0F       ; Stamina index
C386EA:    ASL A          ; Double it
C386EB:    JSR $8836      ; Draw modifier
C386EE:    REP #$20       ; 16-bit A
C386F0:    LDA #$82c1     ; Tilemap ptr
C386F3:    STA $7E9E89    ; Set position
C386F7:    SEP #$20       ; 8-bit A
C386F9:    TDC            ; Clear A
C386FA:    PLA            ; Stat mods HB
C386FB:    AND #$F0       ; Mag.Pwr index
C386FD:    LSR A          ; Put in b3-b6
C386FE:    LSR A          ; Put in b2-b5
C386FF:    LSR A          ; Put in b1-b4
C38700:    JSR $8836      ; Draw modifier
        
;draw defensive properties
C38703:    LDX $2134      ; Item index
C38706:    LDA $D85000,X  ; Properties
C3870A:    AND #$07       ; Get class
        beq not_weapon    ; branch if tool
C3870C:    CMP #$01       ; Weapon?
C3870E:    BEQ is_weapon    ; Branch if so
        cmp #$06        ; item?
        beq not_weapon    ; branch if so
C38710:    LDA $D85014,X  ; Defense
C38714:    JSR $04E0      ; Turn into text
C38717:    LDX #$83a1     ; Text position
C3871A:    JSR $04C0      ; Draw 3 digits
C3871D:    LDX $2134      ; Item index
C38720:    LDA $D85015,X  ; Mag.Def
C38724:    JSR $04E0      ; Turn into text
C38727:    LDX #$83bf     ; Text position
C3872A:    JSR $04C0      ; Draw 3 digits
        ldy #bat_pow_dash
        jsr $02f9
is_weapon:
        tdc
        ldx $2134        ; item index
        lda $d85000,x    ; properties
        and #$07
        cmp #$01
        bne not_weapon
C3868E:    LDA #$20       ; Palette 0
C38690:    STA $29        ; Color: User's
C387AB:    CMP #$51       ; Dice?
C387AD:    BEQ hide_bpow      ; Hide if so
        ldx $2134
C387B3:    LDA $D85014,X  ; Bat.Pwr
C387B7:    JSR $04E0      ; Turn into text
C387BA:    LDX #$813f        ; Text position
C387BD:    JSR $04C0      ; Draw 3 digits
        ldy #def_dash
        jsr $02f9
        ldy #mdef_dash
        jsr $02f9
        bra not_weapon

hide_bpow:
        LDY #wpn_unknown     ; Text pointer
C387C3:    JSR $02F9      ; Draw "???"
        
not_weapon:
        jsr all_dashes
C387EB:    REP #$20       ; 16-bit A
C387ED:    LDA #$8321     ; Tilemap ptr
C387F0:    STA $7E9E89    ; Set position
C387F4:    SEP #$20       ; 8-bit A
C387F6:    LDX $2134      ; Item index
C387F9:    TDC            ; Clear A
C387FA:    LDA $D8501A,X  ; Evasion mods
C387FE:    PHA            ; Memorize them
C387FF:    AND #$0F       ; Evade index
C38801:    ASL A          ; x2
C38802:    ASL A          ; x4
C38803:    JSR $881A      ; Draw modifier
C38806:    REP #$20       ; 16-bit A
C38808:    LDA #$833f     ; Tilemap ptr
C3880B:    STA $7E9E89    ; Set position
C3880F:    SEP #$20       ; 8-bit A
C38811:    LDX $2134      ; Item index
C38814:    TDC            ; Clear A
C38815:    PLA            ; Evasion mods
C38816:    AND #$F0       ; MBlock index
C38818:    LSR A          ;
C38819:    LSR A          ;
C3881A:    TAX            ; Index it
C3881B:    LDA $C38854,X  ; Sign
C3881F:    STA $7E9E8B    ; Add to string
C38823:    LDA $C38855,X  ; Tens digit
C38827:    STA $7E9E8C    ; Add to string
C3882B:    LDA $C38856,X  ; Ones digit
C3882F:    STA $7E9E8D    ; Add to string
C38833:    JSR $8847      ; Draw modifier

; name and cleanup
        REP #$20
        lda #$810d        ; tilemap ptr
        sta $7e9e89
        SEP #$20        
        jsr $bfc2        ; get item
        jsr $c068        ; load item name
        jsr $7fd9
        plp
        ply
        plx
        pla
        jsr $bc92
        rts

all_dashes:
        ldx $2134
        lda $d85000,x
        and #$07
        cmp #$06
        bne skip_all_dashes
        ldy #bat_pow_dash
        jsr $02f9
        ldy #def_dash
        jsr $02f9
        ldy #mdef_dash
        jsr $02f9
skip_all_dashes:
        rts
gear_desc:
        lda $02
        cmp $4B
        bne gear_desc_end
        jsr gear_desc2    ; build description tilemap for shop menu
        jsr $b4e6        ; set description flags
        jsr $b4ef        ; load item description for buy menu
gear_desc_end:
        lda $4b
        sta $02
        rts
        
gear_desc2:
C3A6F4:    LDX #$7849     ; Base: 7E/7849
C3A6F7:    STX $EB        ; Set map ptr LBs
C3A6F9:    LDA #$7E       ; Bank: 7E
C3A6FB:    STA $ED        ; Set ptr HB
C3A6FD:    LDY #$0CBC     ; Ends at 30,19
C3A700:    STY $E7        ; Set row's limit
C3A702:    LDY #$0C84     ; Starts at 3,19
C3A705:    LDX #$3500     ; Tile 256, pal 5
C3A708:    STX $E0        ; Priority enabled
C3A70A:    JSR $A783      ; Do line 1, row 1
C3A70D:    LDY #$0CFC     ; Ends at 30,20
C3A710:    STY $E7        ; Set row's limit
C3A712:    LDY #$0CC4     ; Starts at 3,20
C3A715:    LDX #$3501     ; Tile 257, pal 5
C3A718:    STX $E0        ; Priority enabled
C3A71A:    JSR $A783      ; Do line 1, row 2
C3A71D:    LDY #$0D3C     ; Ends at 30,21
C3A720:    STY $E7        ; Set row's limit
C3A722:    LDY #$0D04     ; Starts at 3,21
C3A725:    LDX #$3538     ; Tile 312, pal 5
C3A728:    STX $E0        ; Priority enabled
C3A72A:    JSR $A783      ; Do line 2, row 1
C3A72D:    LDY #$0D7C     ; Ends at 30,22
C3A730:    STY $E7        ; Set row's limit
C3A732:    LDY #$0D44     ; Starts at 3,22
C3A735:    LDX #$3539     ; Tile 313, pal 5
C3A738:    STX $E0        ; Priority enabled
C3A73A:    JMP $A783      ; Do line 2, row 2

help_text:
        dw $791f : db "Hold Y for details.",$00
text_vigor:
        dw $820d : db "Vigor",$00 ; +12 = $821f
text_magic:
        dw $828d : db "Speed",$00 ; +12 = $829f
text_stamina:
        dw $822b : db "Stamina",$00 ; +12 = $823d
text_speed:
        dw $82ab : db "Magic",$00 ; +12 = $82bd
text_def:
        dw $838d : db "Defense",$00 ; +12 = $819f
text_mdef:
        dw $83ab : db "M.Def.",$00 ; +12 = $81bd
text_eva:
        dw $830d : db "Evade",$00 ; +12 = $831f
text_meva:
        dw $832b : db "M.Evade",$00 ; +12 = $833f
text_bpow:
        dw $812b : db "B.Power",$00 ; + 12 = $813d
wpn_unknown:
        dw $813f : db "???",$00
bat_pow_dash:
        dw $813f : db "---",$00
def_dash:
        dw $83a1 : db "---",$00
mdef_dash:
        dw $83bf : db "---",$00
ele_resist:
        dw $7B8D : db "Resist:",$00
ele_absorb:
        dw $7C0D : db "Absorb:",$00
ele_nullify:
        dw $7C8D : db "Nullify:",$00
ele_weakness:
        dw $7D0D : db "Weakness:",$00

;Window layout data
shop_gear_window:
dw $718B : db $1C,$06
shop_gear_window_actors:
dw $750B : db $1C,$06
shop_gear_weapon_name:
dw $708B : db $1C,$02
shop_gear_description:
dw $738B : db $1c,$04
