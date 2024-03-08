!to "cart64c2_hi.bin", plain

*=$c000

!bin "payload02",,0                

; fill to 16k boundary
!fi $10000-*, $00
