;--------------------------------------------------
; Cartridge Challenge
; ACME Assembler
;--------------------------------------------------

!to "cartchal_lo.bin", plain

;--------------------------------------------------
; Custom locations
;--------------------------------------------------
char_pointer    = $d1

;--------------------------------------------------
; Constants
;--------------------------------------------------



;--------------------------------------------------
; System locations
;--------------------------------------------------
IMPARM          = $bc
CURBNK          = $fb
PKYBUF          = $0567
LNGJMP          = $05f0
FETPTR          = $be
FETARG          = $05f2
FETXRG          = $05f3
FETSRG          = $05f4
TAPBUF          = $0332         ; input buffer
CINV            = $0314         ; custom interrupt vector

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

*=$8000
 jmp cold_start
 jmp warm_start
!by 10                           ; autostart=1
!pet "cbm"                      ; CBM signature
!pet "20240224"                 ; ROM identification
!by 0

intro_message:

!by $93                         ; SCNCLR
!by $0e, $12                    ; lowercse, reverson
!pet "Cartridge Challenge"
!by $92, $0d, $0d               ; reverse off, crlf, crlf
!pet "I want to do a proof of concept and call"
;
!pet "Kernal and Basic routines from within a"
!by $0d
!pet "32k cartridge. This will be the first"
!by $0d
!pet "step to create a new 3+1 ROM based on"
!by $0d
!pet "SpeedScript and SpeedCalc that will run "
;
!pet "from ROM and utilize all available RAM."
!by $0d
!by 0

cart_message:
!pet " new 3+1 on key f1"
!by $0d, 0

key:
!pet "sys1525"
key_end

;--------------------------------------------------
; cold_start
;--------------------------------------------------
cold_start:

                lda CURBNK	; Get current ROM bank (to provide runnability in any slots)
	        and #$03	; Use our ROM in low bank only and KERNAL in high
	        tax		
	        stx CURBNK
	        sta $fdd0,x     
	        ldx #<cart_message	; Print message below Basic header
                ldy #>cart_message
                jsr print_text
                jsr def_key
                jsr def_sys
                rts

;--------------------------------------------------
; warm_start
;--------------------------------------------------
warm_start:
                jsr print_intro
                jsr read_textl
                jmp *

;--------------------------------------------------
; def_key
;--------------------------------------------------
def_key:
                ldy #key_end-key-1
-               lda key,y
                sta PKYBUF,y
                dey
                bpl -
                rts

;--------------------------------------------------
; def_sys
;--------------------------------------------------
def_sys:
                ldy #sys_end-sys-1
-               lda sys,y
                sta 1525,y
                dey
                bpl -
                rts

;--------------------------------------------------
; sys
;--------------------------------------------------
sys:
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<$8003               
                sta LNGJMP  
                lda #>$8003               
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$05                ; 3+1/3+1             
                jmp LONG                ; initiate warm_start
sys_end:

;--------------------------------------------------
; print_intro
;--------------------------------------------------
print_intro:
                ldx #<intro_message
                ldy #>intro_message                  
                jsr print_textl
                rts

;--------------------------------------------------
; print_text, xy=pointer, 0
;--------------------------------------------------
print_text:
                stx IMPARM              ; set pointer
                sty IMPARM+1
                ldy #0          	; we're pointing to 1st byte of string
-   
                lda (IMPARM),y  	; loop to output string
                beq +
                jsr CHROUT              ; do a long call later              
                iny
                bne -
                inc IMPARM+1
                bne -                   ; this will never be 0
+               rts

;--------------------------------------------------
; print_textl, xy=pointer, 0
;--------------------------------------------------
print_textl:
                stx IMPARM              ; set pointer
                sty IMPARM+1
                ldy #0          	; we're pointing to 1st byte of string
-   
                lda (IMPARM),y  	; loop to output string
                beq +

                ;jsr CHROUT              ; do a long call later
                sta FETARG              ; store acc
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<CHROUT              
                sta LNGJMP  
                lda #>CHROUT               
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr CHROUT via long

                iny
                bne -
                inc IMPARM+1
                bne -                   ; this will never be 0
+               rts

;--------------------------------------------------
; read_textl to TAPBUF, x= length
;--------------------------------------------------
read_textl:	
                ldx #$00
	        stx char_pointer       ; input TAPBUF char_pointer

read_char:      ; jsr GETIN
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<GETIN              
                sta LNGJMP  
                lda #>GETIN             
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr GETIN via long
                
                beq read_char           ; queue is empty
	        ldx char_pointer
	        cmp #$0d                ; carrige return
	        beq read_end      
	        cmp #$14                ; delete
	        beq read_del
	        cpx #$14                ; maximum 20 characters read?
	        beq read_char           ; don't write and wait

	        ;jsr CHROUT	        ; KERNAL: BSOUT (output to channel)
                sta FETARG              ; store acc
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<CHROUT              
                sta LNGJMP  
                lda #>CHROUT               
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr CHROUT via long

                sta TAPBUF,x            ; write to TAPBUF
	        inc char_pointer        ; 
	        jmp read_char	        ;

read_del:	cpx #$00                ; position 0?
	        beq read_char           ; do nothing and read

	        ;jsr CHROUT	        ; KERNAL: BSOUT (output to channel)
                sta FETARG              ; store acc
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<CHROUT              
                sta LNGJMP  
                lda #>CHROUT               
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr CHROUT via long

	        dec char_pointer
	        jmp read_char           ;

read_end:	;jsr CHROUT	        ; KERNAL: BSOUT (output to channel) - last character
                sta FETARG              ; store acc
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<CHROUT              
                sta LNGJMP  
                lda #>CHROUT               
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr CHROUT via long

                ldx char_pointer
	        rts

; fill to 16k boundary
!fi $c000-*, $00















