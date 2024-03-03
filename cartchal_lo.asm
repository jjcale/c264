;--------------------------------------------------
; Cartridge Challenge
; ACME Assembler
;--------------------------------------------------
!to "cartchal_lo.bin", plain

;--------------------------------------------------
; Custom locations
;--------------------------------------------------
zpd0            = $d0
char_pointer    = $d1
zpd2            = $d2
zpd3            = $d3

;--------------------------------------------------
; Constants
;--------------------------------------------------
; tbd

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

;--------------------------------------------------
; BASIC locations
;--------------------------------------------------
                        ; graphic zp storage
graphm	= $84		; current graphic mode
colsel	= $85		; current color selected
mc1     = $86		; multicolor1
fg	= $87		; foreground color
scxmax	= $88		; maximum # of columns
scymax	= $89		; maximum # of rows

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
; Basic routines
;--------------------------------------------------
SCNCLR          = $c567

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
!pet "20240224"                 ; ROM identification
!by 0

message_00
!by $0d
!pet "This is a proof of concept to call"
!by $0d
!pet "Kernal and Basic routines from within"
!by $0d
!pet "a 32k cartridge. This will be the first"
!by $0d
!pet "step to create a new 3+1 ROM based on"
!by $0d
!pet "SpeedScript and SpeedCalc that will run"
!by $0d
!pet "from ROM and utilize all available RAM."
!by $0d, $0d
!by 0

message_01
!pet "Input test #1: "
!by 0

message_02
!pet "Input test #2: "
!by 0

message_03
!pet " new 3+1 on key f1"
!by $0d, 0

message_04
!by $0d
!pet "Press <Enter> to exit to BASIC"
!by 0

message_05
!by $0d
!pet "Press <Enter> to execute SCNCLR"
!by 0

message_06
!by $93                         ; SCNCLR
!by $0e, $12                    ; lowercse, reverson
!pet "Cartridge Challenge"
!by $92, $0d, $0d               ; reverse off, crlf, crlf
!pet "This program runs in 3+1 ROM (low/high)."
!by $0d, 0

key:
!pet "sys1525"
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
                                        ; ROM charset in hi memory (see cartchal.hi.asm)
                rts

;--------------------------------------------------
; warm_start
;--------------------------------------------------
warm_start      ldx #<message_06        ; run message
                ldy #>message_06                  
                jsr print_text
                ldx #<message_00        ; intro message
                ldy #>message_00                  
                jsr print_text
                ldx #<message_01        ; input file
                ldy #>message_01                
                jsr print_text         
                jsr read_text
                ldx #<message_02        ; output file
                ldy #>message_02                
                jsr print_text         
                jsr read_text
                ldx #<message_05        ; execute SCNCLR?
                ldy #>message_05                
                jsr print_text         
                jsr read_text
                jsr basic_scnclr
                ldx #<message_04        ; exit to BASIC?
                ldy #>message_04                
                jsr print_text         
                jsr read_text
                lda #<warm_jmp              
                sta LNGJMP 
                lda #>warm_jmp           
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr CHROUT via long

;--------------------------------------------------
; def_key
;--------------------------------------------------
def_key         ldy #key_end-key-1
-               lda key,y
                sta PKYBUF,y
                dey
                bpl -
                rts

;--------------------------------------------------
; def_sys
;--------------------------------------------------
def_sys         ldy #sys_end-sys-1
-               lda sys,y
                sta 1525,y
                dey
                bpl -
                rts

;--------------------------------------------------
; sys (sys routine to reside in low ram)
;--------------------------------------------------
sys             ldx #$01                ; 3+1/3+1 bank
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
; basic_scnclr
;--------------------------------------------------
basic_scnclr    lda #$00
                sta graphm
                lda #$36
                sta colsel
                lda #$10
                sta mc1
                lda #$00
                sta fg
                lda #$00
                sta scxmax
                lda #$00
                sta scymax
                lda #$00                ; status register=0             
                sta FETSRG
                lda #<SCNCLR             
                sta LNGJMP  
                lda #>SCNCLR            
                sta LNGJMP+1                                
                lda CURBNK               
                ldx #$00                ; bank in Basic/Kernal             
                jsr LONG                ; jsr GETIN via long
                rts

;--------------------------------------------------
; print_text_classic, xy=pointer, 0
;--------------------------------------------------
print_text_classic
                stx IMPARM              ; set pointer
                sty IMPARM+1
                ldy #0          	; we're pointing to 1st byte of string
-               lda (IMPARM),y  	; loop to output string
                beq +
                jsr CHROUT              ; do a long call later              
                iny
                bne -
                inc IMPARM+1
                bne -                   ; this will never be 0
+               rts

;--------------------------------------------------
; print_text, xy=pointer, 0 
;--------------------------------------------------
print_text      stx IMPARM              ; set pointer
                sty IMPARM+1
                ldy #0          	; we're pointing to 1st byte of string
-               lda (IMPARM),y  	; loop to output string
                beq +
                jsr rom_chrout          ; do a long call later
                iny
                bne -
                inc IMPARM+1
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

; fill to 16k boundary
!fi $c000-*, $00















