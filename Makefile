SHELL=/bin/bash
all:
	acme cartchal_hi.asm
	acme cartchal_lo.asm   
run:
	xplus4 -functionlo cartchal_lo.bin -functionhi cartchal_hi.bin
clean:
	rm *.bin
    
    
