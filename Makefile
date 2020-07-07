all:
	dasm *.asm -f3 -v0 -ocart.bin

run:
	/Applications/Stella.app/Contents/MacOS/Stella cart.bin
