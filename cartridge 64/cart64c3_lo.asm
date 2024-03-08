;--------------------------------------------------
; Cartridge 64
; ACME Assembler
;--------------------------------------------------
!to "cart64c3_lo.bin", plain

;--------------------------------------------------
; Commodore cartridge definition
;--------------------------------------------------
*=$8000
                jmp cold_start
                jmp warm_start
!by 10                           ; ignore=0, autostart=1
!pet "cbm"                      ; mandatory CBM signature
!pet "20240307"                 ; ROM identification (optional)
!by 0                           ; EOM (optional)

cold_start
warm_start
                rts             ; do nothing

!fi $8400-*, $00

*=$8400

!bin "payload03",,0   

; fill to 16k boundary
!fi $c000-*, $00















