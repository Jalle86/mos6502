	*=$0002
	LDX #0
Label1:	TXA
	STA $0400,X
	LDA #1
	STA $D800,X
Label2:	INX
	BNE Label1
	RTS
.END