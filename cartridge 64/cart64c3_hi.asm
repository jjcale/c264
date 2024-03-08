!to "cart64c3_hi.bin", plain

*=$c000

!bin "payload04",,0                

; fill to 16k boundary
!fi $10000-*, $00
