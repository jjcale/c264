!to "cartchal_hi.bin", plain

*=$c000
; fill to until char rom
!fi $d000-*, $00

!bin "charrom.prg",,2                

; fill to 16k boundary
!fi $10000-*, $00