;Lalala comments

.MEMORYMAP							;Define the memory map of the system
SLOTSIZE 			$020000			;Define a ROM of $000000-$01FFFF
SLOT 0				$000000			;Start of slot 0 (only slot)
DEFAULTSLOT			0				;Select this slot
.ENDME

.ROMBANKMAP							;Define the ROM map of the cartridge
BANKSTOTAL 			1				;Single bank
BANKSIZE			$020000			;Define a bank of $000000-$01FFFF
BANKS 				1				;1 bank of size $020000
.ENDRO

.BANK 0 SLOT 0						;Start the cartridge bank 0 in Mem slot 0

.EQU z_HL			$20				;Zero page variables for our 24 bit addresses
.EQU z_L			$20
.EQU z_H			$21
.EQU z_HLU			$22

.EQU CursorX		$40				;X position of next character draw
.EQU CursorY		$41				;Y position of next character draw

.ORGA				$8000			;Start of ROM

MyProg:
	SEI								;Stop interrupts
	CLC								;Clear carry flag
	XCE								;16 bit mode (65816)

	;--MX---- 						M=Accumulator X=XY
	REP #%00010000					;16 bit XY ; Met REP setten we alleen éénen in P-Register
	.INDEX 16						;XY 16 bit

;SET UP THE SCREEN
	;aaaabbbb - aaa=base addr for BG2, bbb=base addr for BG1
	LDA #%00010001
	STA $210B						;BG1 & BG2 VRAM location register [BG12NBA]

	;xxxxxxss - xxx=addr, ss=SC size, 00=32x32, 01=64x32, 10=32x64, 11=64x64
	STZ $2107						;BG1SC - BG1 Tilemap VRAM location (%00000000)

	;x000bbbb - x=screen disable (1=disable), bbbb=brightness (15=max)
	LDA #%10001111					;INIDISP - Screen display register
	STA $2100

	;i000abcd - i=0 inc on $2118/2139 i=1 $2119/213A abcd=move zie
	STZ $2115						;VMAIN - Video port control
									;(Inc on write to $2118)

	;abcdeff - abcd=tile sizes e=pri fff=mode def
	LDA #%00001001
	STA $2105						;BGMODE - Screen mode register

;SET UP PALLETTE					Color 0
	STZ $2121						;CGADD - Palette selection
	STZ $2122						;CGDATA - Color data register gggrrrrr
	STZ $2122						;GGDATA = Color data register ?bbbbbgg

	LDA #3							;Color 3
	STA $2121						;CGADD - Palette selection
	LDA #%11111111
	STA $2122						;CGDATA - Color data register gggrrrrr
	LDA #%00000111
	STA $2122						;GGDATA = Color data register ?bbbbbgg

;TRANSFER PATTERNS (1 BPP FONT)
	LDA #<BitmapFont
	STA z_L							;L byte if address (24 bit UHL)
	LDA #>BitmapFont
	STA z_H							;H byte of address (24 bit UHL)
	LDA #BitmapFont>>16
	STA z_HLU						;U byte of address (24 bit UHL)

	LDX #$1000						;Vram Dest = $1000 (Patterns)
	STX $2116						;2116/7 - Video port address (VMADDL/H)

	LDY #0
fontchar_loop:
	LDX #8							;8 lines - Bitplanes 0/1
fontchar_NextWord:
	LDA [z_HL],y					;We write 1 word/2 bytes to each VRAM address
	STA $2119						;1st byte H
	STA $2118						;2nd byte L (and Inc VRAM address)
	INY
	DEX
	BNE fontchar_NextWord			;BNE = Branch if not equal

	LDX #8							;8 lines - Bitplanes 2/3
fontchar_NextZero:
	STZ $2119						;1st byte H
	STZ $2118						;2nd byte L (and Inc VRAM address)
	DEX								;DEcrement X index register
	BNE fontchar_NextZero

	CPY #27*8						;All characters done? (ComPare Y with memory)
	BNE fontchar_loop

;Set Scroll positions
	STZ $210D						;BG1HOFS BG1 horizontal scroll
	STZ $210D						;BG1HOFS
	LDA #-1
	STA $210E						;BG1VOFS BG1 vertical scroll
	STZ $210E						;BG1VOFS

	STZ $2116						;MemL - Video port address
	STZ $2117						;MemH - Video port address

;Clear Tilemap
	LDX #$400						;Size of tilemap
ClearTilemap:
	STZ $2119						;1st byte H
	STZ $2118						;2nd byte L (and Inc VRAM address)
	DEX								;DEcrement X index register
	BNE ClearTilemap				;BNE = Branch if not equal

;Turn on screen
		; ---S4321 					;S=Sprites, 4-1=enable Bgx
	LDA #%00000001					;Turn on BG1
	STA $212C						;Main screen designation [TM]

	;	  x000bbbb					;x=screen disable (1=disable) bbbb=brightness
	LDA #%00001111					;Screen ON
	STA $2100						;INIDISP - Screen display register

;Start out test program,
	STZ CursorX						;Zero Text Drawing Position
	STZ CursorY

	LDA #<txtHelloWorld
	STA z_L							;L byte of address (24 bit UHL)
	LDA #>txtHelloWorld
	STA z_H							;H byte of address (24 bit UHL)
	LDA #txtHelloWorld>>16
	STA z_HLU						;U byte of address (24 bit UHL)

	JSR PrintString					;Show z_HL string to screen. JSR=Jump to SubRoutine

infloop:
	JMP infloop						;Halt program

txtHelloWorld:						;255 Terminated string
	.DB "HALLO WOLLIE", 255			;Let op, WorlD

NewLine:							;Move down a line
	STZ CursorX						;Zero X pos
	INC CursorY						;Move down one line
	RTS								;Return

PrintString:
	LDY #0							;Char offset
PrintString_again:
	LDA [z_HL],y					;Load a character from z_HL + Y
	CMP	#255						;Compare Accumulator with memory (clears overflow flg)
	BEQ PrintString_Done			;If it's 255, we're done! (Branch if R)
	JSR PrintChar					;Print the character
	INY
	JMP PrintString_again
PrintString_Done:
	RTS

PrintChar:							;Print ASCII Character A
	AND #%11011111					;We have no lowercase
	BEQ PrintCharSpace				;Char 32 (0) = Space
	SEC								;SEC = Set carry flag
	SBC #64							;First char after space is A (64)
PrintCharSpace:
	PHA								;Push Accumulator (STACK)
		LDX z_HL					;Backup z_HL
		PHX							;Push X (STACK)
			LDA CursorY
			STA z_H

			LDA #0
			CLC						;CLC = Clear carry flag
			ROR z_H					;Y pos * 32. ROR shifts right; Input (c) -> high bit; low bit -> C
			ROR
			ROR z_H
			ROR
			ROR z_H
			ROR
			ADC CursorX				;Add X pos
			STA z_L
WaitVblank:
			LDA $4212				;HVBJOY - Status
				 ;xy00000a			;x=vblank state y=hblank state a=joypad ready
			AND #%10000000
			BEQ WaitVblank			;Wait until we get nonzero, this means we're in VBLANK

			LDX z_HL
			STX $2116				;MemHL - Video port address [VMADDL/VMADDH]

			STZ $2119				;DataH - Video port data
		PLX							;Pull X
		STX z_HL					;Restpre z_HL
	PLA								;Pull Accumulator
	STA $2118						;DataL - Video port data

	inc CursorX						;Move cursor across
	RTS								;Return

BitmapFont:
	.DB $00,$00,$00,$00,$00,$00,$00,$00		;Space
	.DB $18,$3C,$66,$66,$7E,$66,$24,$00 	;A
	.DB $3C,$66,$66,$7C,$66,$66,$3C,$00		;B
	.DB $38,$7C,$C0,$C0,$C0,$7C,$38,$00		;C
	.DB $3C,$64,$66,$66,$66,$64,$38,$00		;D
	.DB $3C,$7E,$60,$78,$60,$7E,$3C,$00		;E
	.DB $38,$7C,$60,$78,$60,$60,$20,$00		;F
	.DB $3C,$66,$C0,$C0,$CC,$66,$3C,$00		;G
	.DB $24,$66,$66,$7E,$66,$66,$24,$00		;H
	.DB $10,$18,$18,$18,$18,$18,$08,$00		;I
	.DB $08,$0C,$0C,$0C,$4C,$FC,$78,$00		;J
	.DB $24,$66,$6C,$78,$6C,$66,$24,$00		;K
	.DB $20,$60,$60,$60,$60,$7E,$3E,$00		;L
	.DB $44,$EE,$FE,$D6,$D6,$D6,$44,$00		;M
	.DB $44,$E6,$F6,$DE,$CE,$C6,$44,$00		;N
	.DB $38,$6C,$C6,$C6,$C6,$6C,$38,$00		;O
	.DB $38,$6C,$64,$7C,$60,$60,$20,$00		;P
	.DB $38,$6C,$C6,$C6,$CA,$74,$3A,$00		;Q
	.DB $3C,$66,$66,$7C,$6C,$66,$26,$00		;R
	.DB $3C,$7E,$60,$3C,$06,$7E,$3C,$00		;S
	.DB $3C,$7E,$18,$18,$18,$18,$08,$00		;T
	.DB $24,$66,$66,$66,$66,$66,$3C,$00		;U
	.DB $24,$66,$66,$66,$66,$3C,$18,$00		;V
	.DB $44,$C6,$D6,$D6,$FE,$EE,$44,$00		;W
	.DB $C6,$6C,$38,$38,$6C,$C6,$44,$00		;X
	.DB $24,$66,$66,$3C,$18,$18,$08,$00		;Y
	.DB $7C,$FC,$0C,$18,$30,$7E,$7C,$00		;Z

.ORG $FFFA							;Cartridge footer
	.DW $0000						;NMI (VBlank)
	.DW	$8000						;RESET VECTOR
	.DW $0000						;IRQ/BRK VECTOR
