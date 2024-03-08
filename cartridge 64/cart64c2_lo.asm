;--------------------------------------------------
; Cartridge 64
; ACME Assembler
;--------------------------------------------------
!to "cart64c2_lo.bin", plain

;--------------------------------------------------
; Custom locations
;--------------------------------------------------
select_bank     = $d0
char_pointer    = $d1
zpd2            = $d2
zpd3            = $d3
dest            = $d4
;               = $d5
text_ptr        = $d6
;               = $d7

;--------------------------------------------------
; Constants
;--------------------------------------------------
block_start     = $8400         ; copy start address
block_end       = $fc00         ; copy end address
destination1    = $1001-2       ; destination - two loading bytes
destination2    = $87ff         ; destination1+30*1024 ; +30 kb = $87ff

;--------------------------------------------------
; System locations
;--------------------------------------------------
CURBNK          = $fb
PKYBUF          = $0567
LNGJMP          = $05f0
FETPTR          = $be
FETARG          = $05f2
FETXRG          = $05f3
FETSRG          = $05f4
TAPBUF          = $0332         ; input buffer
RAMROM          = $0728         ; RAM/ROM read in Monitor

;--------------------------------------------------
; Kernal routines
;--------------------------------------------------
PRIMM           = $ff4f         ; print immediatelly
FETCHL          = $fcf7         ; long fetch
GETIN           = $ffe4         ; getin
CHROUT          = $ffd2         ; chrout or bsout
RAMTAS          = $ff87         ; RAM test
IOINIT          = $ff84         ; initialize I/O devices
CINT	        = $ff81         ;
RESTOR  	= $ff8a         ;
LONG            = $fcfa         ; jump long

;--------------------------------------------------
; Commodore cartridge definition
;--------------------------------------------------
*=$8000
cold_jmp
 jmp cold_start
warm_jmp
 jmp warm_start
!by 10                          ; autostart=1
!pet "cbm"                      ; mandatory CBM signature
!pet "20240306"                 ; ROM identification (optional)
!by 0                           ; EOD (optional)

message_01
!by $93                         ; SCNCLR
!by $0e, $12                    ; lowercse, reverson
!pet "Cartridge 64"
!by $92, $0d, $0d               ; reverse off, crlf, crlf
!pet "This program tests a combined C2/C3"
!by $0d
!pet "cartridge (32+32 kb)."
!by $0d, 0

message_02
!by $0d
!pet "Press <Enter> to copy the payload "
!by 0

message_03
!pet " cartridge 64 on key f2"
!by $0d, 0

message_04
!by $0d
!pet "Press <Enter> to exit to BASIC "
!by 0

message_05
!by $93, $8e
!by 0

key:
!pet "sys758"                   ; free at $02f6 then jmp 1545
key_end

;--------------------------------------------------
; cold_start
;--------------------------------------------------
cold_start      lda CURBNK	; Get current ROM bank (to provide runnability in any slots)
	        and #$03	; Use our ROM in low bank only and KERNAL in high
	        tax		
	        stx CURBNK
	        sta $fdd0,x     
	        ldx #<message_03	; Print message below Basic header
                ldy #>message_03
                jsr print_text_classic  ; print message
                jsr def_key             ; register F1 key (simply hardwired)
                jsr def_sys             ; register sys routine (simply hardwired)
                rts

;--------------------------------------------------
; warm_start
;--------------------------------------------------
warm_start      ldx #<message_01        ; intro message
                ldy #>message_01                  
                jsr print_text
                ldx #<message_02        ; copy message
                ldy #>message_02                  
                jsr print_text   
                jsr read_text

                lda #$0a                ; c2/c2 cartridge
                sta select_bank
                lda #<destination1      ; $1001-2
                sta dest
                lda #>destination1
                sta dest+1
                jsr transfer_block

                lda #$0f                ; c3/c3 cartridge
                sta select_bank
                lda #<destination2      ; 
                sta dest
                lda #>destination2
                sta dest+1
                jsr transfer_block
                
                lda #$ff                ; fix the first two bytes
                sta $0fff
                lda #$00
                sta $1000

                ldx #<message_04        ; launch message
                ldy #>message_04                  
                jsr print_text   
                jsr read_text

                ldx #<message_05        ; switch to upper case
                ldy #>message_05                  
                jsr print_text

                lda #<warm_jmp            
                sta LNGJMP 
                lda #>warm_jmp         
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; to to BASIC or launch payload

;--------------------------------------------------
; def_key
;--------------------------------------------------
def_key         ldy #key_end-key-1
-               lda key,y
                sta PKYBUF+7,y          ; replace F2 key "dload"
                dey
                bpl -
                rts

;--------------------------------------------------
; def_sys
;--------------------------------------------------
def_sys         ldy #sys_end-sys-1
-               lda sys,y
                sta 1545,y              ; 1545 ($0609)
                dey
                bpl -
                lda #$4c                ; jmp 1545 ($0609)
                sta 758
                lda #$09
                sta 758+1
                lda #$06
                sta 758+2
                rts

;--------------------------------------------------
; sys (sys routine to reside in low ram)
;--------------------------------------------------
sys             ldx #$02                ; c2/c2
	        sta $fdd0,x
                stx CURBNK
                jmp warm_jmp            ; initiate warm_start
sys_end

;--------------------------------------------------
; rom_chrout
;--------------------------------------------------
rom_chrout      stx zpd2                ; store x
                sty zpd3                ; store y
                sta FETARG              ; acc to arguments
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<CHROUT              
                sta LNGJMP 
                lda #>CHROUT               
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr CHROUT via long
                ldx zpd2                ; retrive x
                ldy zpd3                ; retrive y
                rts

;--------------------------------------------------
; rom_getin
;--------------------------------------------------
rom_getin       stx zpd2                ; store x
                sty zpd3                ; store y
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<GETIN              
                sta LNGJMP  
                lda #>GETIN             
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr GETIN via long
                ldx zpd2                ; retrive x
                ldy zpd3                ; retrive y
                lda FETARG              ; retrive acc
                rts

;--------------------------------------------------
; print_text_classic, xy=pointer, 0
;--------------------------------------------------
print_text_classic
                stx text_ptr              ; set pointer
                sty text_ptr+1
                ldy #0          	; we're pointing to 1st byte of string
-               lda (text_ptr),y  	; loop to output string
                beq +
                jsr CHROUT              ; do a long call later              
                iny
                bne -
                inc text_ptr+1
                bne -                   ; this will never be 0
+               rts

;--------------------------------------------------
; print_text, xy=pointer, 0 
;--------------------------------------------------
print_text      stx text_ptr              ; set pointer
                sty text_ptr+1
                ldy #0          	; we're pointing to 1st byte of string
-               lda (text_ptr),y  	; loop to output string
                beq +
                jsr rom_chrout          ; do a long call later
                iny
                bne -
                inc text_ptr+1
                bne -                   ; this will never be 0
+               rts

;--------------------------------------------------
; read_text to TAPBUF, x= length
;--------------------------------------------------
read_text       ldx #$00
	        stx char_pointer        ; input TAPBUF char_pointer
read_char       jsr rom_getin               
                beq read_char           ; queue is empty
	        ldx char_pointer
	        cmp #$0d                ; carrige return
	        beq read_end      
	        cmp #$14                ; delete
	        beq read_del
	        cpx #$14                ; maximum 20 characters read?
	        beq read_char           ; don't write and wait
	        jsr rom_chrout	        ; KERNAL: BSOUT (output to channel)
                sta TAPBUF,x            ; write to TAPBUF
	        inc char_pointer        ; 
	        jmp read_char	        ;
read_del	ldx char_pointer
                cpx #$00                ; position 0?
	        beq read_char           ; do nothing and read
	        jsr rom_chrout	        ; KERNAL: BSOUT (output to channel)
	        dec char_pointer
	        jmp read_char           ;
read_end	jsr rom_chrout	        ; KERNAL: BSOUT (output to channel) - last character
                ldx char_pointer
	        rts

;--------------------------------------------------
; transfer_block
;--------------------------------------------------
transfer_block  
                lda #<block_start
                sta FETPTR
                lda #>block_start
                sta FETPTR+1
                ldy #$00        ; index
-               ldx select_bank ; select bank to copy from
                lda CURBNK      ; current bank
                sei
                jsr FETCHL      ; KERNAL fetch long
                cli
                sta (dest),y    ; write to new location
                iny
                bne -
                inc dest+1
                lda dest+1
                cmp #$fb        ; hard stop
                beq +
                inc FETPTR+1
                lda FETPTR+1
                cmp #>block_end
                bne -
+               rts

!fi $8400-*, $00

*=$8400

!bin "payload01",,0   

; fill to 16k boundary
!fi $c000-*, $00


