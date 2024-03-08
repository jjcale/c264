SHELL=/bin/bash
all:
	acme cart64c2_hi.asm
	acme cart64c2_lo.asm
	acme cart64c3_hi.asm
	acme cart64c3_lo.asm   
run:
	xplus4 -functionlo cartchal_lo.bin -functionhi cartchal_hi.bin
	split -b 15360 --hex-suffixes=00 mercenary.prg payload
clean:
	rm *.bin
    
    
