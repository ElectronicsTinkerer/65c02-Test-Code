;Prot'er - Prototyping Computer Main Kernel
;
;Modification of the WOZ Monitor
;  *Now has:
;  *? - Opens Help Menu
;  *T/t - toggles IRQ interrupt enable bit - displays message indicating if it is set
;  *All commands are now not case-sensitive
;
;
;
;ET           - Started: November 15, 2017
;               Last Update: August 21, 2018
;
;
;
;Only works on 65c02 variant
;




	processor 65c02

;+--------------------------------+
;|           Registers            |
;+--------------------------------+

ACIA_DATA	equ $c000
ACIA_STAT	equ ACIA_DATA + 1
ACIA_COM	equ ACIA_DATA + 2
ACIA_CTRL	equ ACIA_DATA + 3

;+--------------------------------+
;|           Variables            |
;+--------------------------------+

;-----WOZ Monitor vars-----
wozb	equ $F0

mode	equ wozb	;Stores what mode we are in
low	equ wozb+1	;Input value for hex parsing
high	equ wozb+2
xaml	equ wozb+3	;Last opened location
xamh	equ wozb+4
stl	equ wozb+5	;Store address
sth	equ wozb+6
strptr	equ wozb+7	;String pointer for echostr routine (Low, High byte)
	;   wozb+8 is used as the high byte of 'strptr'
ysav	equ wozb+9	;Temporary storage for Y 
debug	equ wozb+10	;Location where A,X are stored for the BRK routine
	;   wozb+11
bytetmp	equ wozb+12	;Stores the byte in use by prbytebin 

in	equ $0200	;Input buffer base location (Uses entire page)

;+--------------------------------+
;|           Constants            |
;+--------------------------------+

cr	equ $0d		;Carriage return


;+--------------------------------+
;|       ACIA echo routine        |
;+--------------------------------+
;
;This just echoes what is sent to it so the user can check the cable and ACIA connections
	org $e000

chkac:
	lda ACIA_STAT	;Check if there is new data
	and #$08
	beq chkac	;If not, check again
	lda ACIA_DATA	;Get byte
	cmp #$1b	;If an esc is received, reset the computer
	beq chkacdone	
	sta ACIA_DATA	;Send it
	jmp chkac	;Check for more data
chkacdone:
	jmp ($fffc)	;Reset vector


;+--------------------------------+
;|       BRK check routine        |
;+--------------------------------+

	org $e100

brklp:
	brk
	.byte $42
	adc #$01
	dex
	iny
	nop
	pha
	jmp brklp



;+--------------------------------+
;|       Audio out routine        |
;+--------------------------------+

	org $e200

audlp:
	lda ACIA_STAT	;Check if there is new data
	and #$08
	beq audlp	;If not, check again
	lda ACIA_DATA	;Get byte
	sta $2000	;Store in audio out register
	bra audlp	;Loop



;+------------------------------------------------------------+
;|         Start of Code for the Enhanced Woz Monitor         |
;+------------------------------------------------------------+
	org $f000
begin:
	cld
	sei

	ldx #$ff	;Initialize stack pointer
	txs

begindelay:		;Delays the code start to allow all devices to fully reset 
	dex		;This appears necessary to get the ACIA working
	bne begindelay	;Loop, clears X

	lda #$00	;Initialize ACIA with (115200) BAUD, no parity, no handshaking, 8 bits
	sta ACIA_CTRL
	lda #$cb	; 
	sta ACIA_COM

	ldy #$ff	;Causes auto-escape upon reset
notcr:	
	cmp #$7f	;Backspace?
	beq backspace	;Yes!
	cmp #$1b	;Escape?
	beq getline	;Yes!
	iny		;Go to next text address
	bne nextchar	;If 256 characters, auto escape

getline:
	lda #cr		;Go to next line
	jsr echo

	lda #"@"	;Output prompt character
	jsr echo
	
	ldy #$01	;Clear text pointer
backspace:
	dey		;Backup text pointer
	cpy #$ff
	beq getline	;If end of line, restart line

nextchar:	
	lda ACIA_STAT	;Check if a byte has been received
	and #$08
	beq nextchar
	lda ACIA_DATA	;Get the character and store it in the text buffer
	sta in,y
	jsr echo	;Spit it out
	cmp #cr
	bne notcr
	jmp parse

;These are to extend the range of the local branches from the parse routine.
helpmenujmp:
	jmp helpmenu	;Goto the help menu routine
inttogljmp:
	jmp inttogl	;Goto the interrupt toggle routine


;+-----------------------------------------+
;|         Line entered, parse it          |
;+-----------------------------------------+
parse:
	ldy #$ff
	lda #$00
	tax	
setmode:
	asl
setstor:
	asl
	sta mode
	
blskip:
	iny		;Move to next text buffer location
nextitem:
	lda in,y	;Get next character
	
	cmp #cr		;Carriage Return?
	beq getline	;Yes, wait for next line of commands
	cmp #"."	;Ignore everything below the ascii value of "."
	bcc blskip	;If it is <'.' skip it
	beq setmode	;Set mode to examination 
	cmp #":"	;Store mode?
	beq setstor	;Yes!
	cmp #"?"	;Help Menu
	beq helpmenujmp	;Go to help menu
	and #$5f	;Mask off bits 7 and 5 so case is irrelevant
	cmp #"R"	;Run?
	beq run		;Yes
	cmp #"T"	;Toggle interrupt flag?
	beq inttogljmp	;Yes, do it
	stx low		;Clear the input value	
	stx high	;Clear the input value
	sty ysav	;Save Y for comparison later


;Let's see if it is a hex value
nexthex:
	lda in,y	;Get character
	eor #$30	;Map the character to the digits 0-9
	cmp #$0a	;If this is a decimal digit,
	bcc dig		; then branch
	and #$5f	;Mask off bits 7 and 5 so case is irrelevant
	adc #$a8	;Map to letters 'A'..'F'
	cmp #$fa	;Is it hex?
	bcc nothex
		
dig:
	asl		;Moves hex value into the highest nibble of A
	asl
	asl
	asl

	ldx #$04	;Initialize cycle counter
hexshift:		;Shifts the hex value that has been input into the 
			; hex parsing start location variable
	asl		;Move the MSB to the carry bit
	rol low		;Move the carry into the low bit of pointer
	rol high	;Move any carry (MSB) from the low pointer to the high pointer
	dex		;Have we done four shifts? (One hex value, 4 bits)
	bne hexshift	;No, we have not, loop
	iny		;Advance the text index
	bne nexthex	;Always taken
	
nothex:
	cpy ysav	;Was at least one hex digit given?
	beq getlinejmp	;No! Ignore the entire line and start from scratch!
	
	bit mode	;Test what mode we are in
	bvc notstor	;Not in store mode! Go do examination

;+------------------------------------+
;|             Store Mode             |
;+------------------------------------+
	
	lda low		;Get the LSD of the hex data
	sta (stl,x)	;Store A at the current "store index" (x = 0 from hexshift)
	inc stl		;Increment store index
	bne nextitem	;No carry!
	inc sth		;Add the carry to the 'store index' high byte
tonextitem:
	jmp nextitem	;Get next item in line

;This extends the range of branches to the getline routine
getlinejmp:
	jmp getline	;Jump to getline

	
;+-------------------------------+
;|       Run User Program        |
;+-------------------------------+

run:			
	jmp (xaml)	;Goto user program


;+------------------------------------+
;|          Examination Mode          |
;+------------------------------------+
	
notstor:
	bmi xamnext	;B7 = 0 for XAM, B7 = 1 for Block XAM

;right now, we are in the XAM mode
	ldx #$02	;Copy 02 bytes
setadr:
	lda low-1,x	;Copy address data to
	sta stl-1,x	; the 'store index'
	sta xaml-1,x	; and the 'XAM index''
	dex		;Go to next byte to copy
	bne setadr	;Loop unless both bytes have been copied

;This prints the address to the screen, then falls through to print memory bytes

nxtprnt:
	bne prdata	;If not equal, there is no address to print (not a new line)
	lda #cr		;Carriage return (New line)
	jsr echo
	lda xamh	;Spit out the high byte of the address
	jsr prbyte
	lda xaml	;Spit out the low byte of the address
	jsr prbyte
	lda #":"	;Print line delimiter, aka the colon
	jsr echo

prdata:
	lda #" "	;Print a space
	jsr echo	
	lda (xaml,x)	;Get data from address (x = 0)
	jsr prbyte	;Output it in hex format
xamnext:
	stx mode	;Clears mode (x = 0)
	lda xaml	;See if there is more to print
	cmp low
	lda xamh
	sbc high
	bcs tonextitem	;Not less!, There is more data to output

	inc xaml	;Increment 'examine index'
	bne mod8chk	;If did not roll over, check if the line is too long
	inc xamh	;If did roll over, increment high byte of the 'examine index'

mod8chk:
	lda xaml	;If address ends in $7, start a new line when branching back
	and #$07	;Sets line length (In bytes)
	bpl nxtprnt	;Always taken


;-------------------------------------------------------------------------
;  Subroutine to print a byte in BIN (Clobbers A)
;-------------------------------------------------------------------------

prbytebin:
	phx		;Save X

	sta bytetmp	;Save A
	lda #"%"	;Indicate the number is BIN
	jsr echo	;Output "%"

	ldx #$08	;Number of bits to be printed
prbtbnlp:
	lda #$00	;Clear A
	rol bytetmp	;Next bit into Carry
	adc #"0"	;Make the number an ASCII 0 or 1	
	jsr echo	;Output it
	dex
	bne prbtbnlp	;Loop until all bits are cycled through
	plx		;Restore X
	rts		;Done


;-------------------------------------------------------------------------
;  Subroutine to print a byte in A in hex form (destructive)
;-------------------------------------------------------------------------

prbyte:
	pha	    
	lsr
	lsr
	lsr	    
	lsr
	jsr prhex
	pla	    

; Fall through to print hex routine

;-------------------------------------------------------------------------
;  Subroutine to print a hexadecimal digit
;-------------------------------------------------------------------------

prhex:
	and     #$0f    ;Mask LSD for hex print
	ora     #"0"    ;Add "0"
	cmp     #"9"+1  ;Is it a decimal digit?
	bcc     echo	;Yes! output it
	adc     #$06	;Add offset for letter A-F


;+--------------------------------+
;|       Echo Byte Routine        |
;+--------------------------------+
; Registers preserved, except status
echo: 	
	phx		;Save x (3)
	ldx #$0d	;Get count value (2)
echoloop:
	dex 		;Increment delay counter (2)
	bne echoloop	;loop (3)
	sta ACIA_DATA	;Output data (4)
	plx		;Restore x (4)
donstr:
	rts		;Return (6)

;+--------------------------------+
;|      Echo String routine       |
;+--------------------------------+

;This clobbers A
;Zero-terminated strings only!

echostr:
	lda (strptr) 	;Get the first byte of the string at the strptr position
	beq donstr	; | If the value pulled is $00, we are done
	jsr echo	; \ Spit the char out if not $00
	inc strptr	;Increment string character pointer
	bne echostr	;Loop again unless it happens to carry over the next page
	inc strptr+1	;If it does, increment the page part of the pointer
	bra echostr	;Loop whether or not the page pointer rolls over


;+----------------------------------------+
;|       IRQ Enable Toggle Routine        |
;+----------------------------------------+

inttogl:
	;Print the first part of the message, then flip the IRQ enable bit
	;By keeping this as a separate string, it saves 13 bytes of space
	lda #<irqtxt	;Get lowest byte of address for text string
	sta strptr
	lda #>irqtxt	;Get highest byte of the address for text string
	sta strptr+1	
	jsr echostr

	tsx		;Get current stack position
	php		;Save status to stack
	lda $0100,x	;Get status register values
	eor #$04	;Flip status of interrupt enable bit
	sta $0100,x	;Store the value at the stack position (Status)
	plp		;Restore status values, with an inverted bit 2 -
			; IRQ disable [1=disabled]
	and #$04	;Mask off extra bits
	beq irqset	;IRQ is enabled
			;If IRQ is disabled, fall through and display disabled message
	lda #<irqdtxt	;Get lowest byte of address for text string
	sta strptr
	lda #>irqdtxt	;Get highest byte of the address for text string
	sta strptr+1	
	jsr echostr
	beq retblskip	;Done, always taken (see echostr routine)
irqset:
	lda #<irqetxt	;Get lowest byte of address for text string
	sta strptr
	lda #>irqetxt	;Get highest byte of the address for text string
	sta strptr+1	
	jsr echostr
retblskip:
	ldx #$00	;Clear x for when returning to the parsing code
	jmp blskip	;Done, check for next character


;+------------------------------------+
;|       Display User Help Menu       |
;+------------------------------------+

helpmenu:
	lda #<helptxt	;Get lowest byte of address for text string
	sta strptr
	lda #>helptxt	;Get highest byte of the address for text string
	sta strptr+1	
	jsr echostr	;Print Help Menu
	jmp blskip	;Return to finish parsing line

;+--------------------------+
;|        Text Bank         |
;+--------------------------+
helptxt:
	.byte $0d
	.byte "+-------------------Command List-------------------+",$0d
	.byte "| ? - Displays Help Menu",$0d
	.byte "| T/t - Toggle IRQ Enable",$0d
	.byte "| R/r - Run program (can BRK back to MONITOR)",$0d
	.byte "| hhll: ## - Store values at address hhll",$0d
	.byte "| hhll.hhll - View locations hhll through hhll",$0d
	.byte $0d
	.byte "| *Input buffer is only 255 chars!",$0d
	.byte "| *NMI is indirect jump to $0300",$0d
	.byte "| *IRQ is indirect jump to $0302",$0d
	.byte "| *BRK is jump to debugger",$0d
	.byte "|  ~ESC will reset the computer",$0d
	.byte "|  ~Any other key to return execution to BRK address",$0d
	.byte $00
irqtxt:
	.byte $0d
	.byte "IRQ interrupts are now ",$00
irqetxt:
	.byte "enabled",$00
irqdtxt:
	.byte "disabled",$00





;+--------------------------+
;|         Vectors          |
;+--------------------------+

	org $ff55

breakaddr:
	.byte "BRK@PC:",$00
	.byte ", A:",$00
	.byte ", X:",$00
	.byte ", Y:",$00
	.byte ", S:",$00
	.byte ", SP:",$00

nmib: 
	jmp ($0300)	;Indirect jump to $0300


irqb:	;Check if the interrupt was caused by hardware or software

	sta debug	;Save A and X (3 cycles)
	stx debug+1	;(3 cycles)
	tsx		;Get current stack position
	inx		;Increment the stack because it is post-incremented
	lda $0100,x	;Get status register values
	and #$10	;Mask off all bits except BRK
	bne break	;If the interrupt was caused by BRK, goto to report routine
irqjmp:			; |
	jmp ($0302)	; \ else indirect jump to $0302

break:
	lda #<breakaddr	;Get lowest byte of address for text string
	sta strptr
	lda #>breakaddr	;Get highest byte of the address for text string
	sta strptr+1	
	jsr echostr	;Print "BRK encountered at"
	
	inx
	lda $0100,x	;Get the low byte of the return address
	sec		;Clear the borrow flag to allow for proper subtraction
	sbc #$02	;Return the address pushed onto the stack to the original address of the instruction
	pha		;Save modified PCL
	inx
	lda $0100,x	;Get the PCH of the BRK
	sbc #$00	;Finish the subtraction
	jsr prbyte	;Print the high byte
	pla		;Restore modified PCL
	jsr prbyte	;Print the low byte

	inc strptr	;Move pointer to " A: " text
	jsr echostr	;Print
	lda debug	;Restore A 
	jsr prbyte

	inc strptr	;Move pointer to " X: " text
	jsr echostr	;Print
	lda debug+1	;Restore X 
	jsr prbyte

	inc strptr	;Move pointer to " Y: " text
	jsr echostr	;Print
	tya		;Transfer Y to A
	jsr prbyte
	
	inc strptr	;Move pointer to " S: " text
	jsr echostr	;Print
	dex
	dex
	lda $0100,x	;Get status 
	jsr prbytebin

	inc strptr	;Move pointer to " SP: " text
	jsr echostr	;Print
	txa		;X is stack pointer because of irqb routine
	adc #$03	;Set stack to where interrupt occurred
	jsr prbyte

	lda #cr		;Carriage Return
	jsr echo

waitforkey:
	lda ACIA_STAT	;Check if a byte has been received
	and #$08
	beq waitforkey	;No, check again
	lda ACIA_DATA	;Get key pressed
	cmp #$1b	;If an esc is received, reset the computer
	beq brkreset	; |
	lda debug	; \ Else, control is back to program that caused the BRK
	ldx debug+1
	rti		;Return to instruction after BRK opcode

brkreset:
	jmp ($FFFC)	;Jump to reset vector



	;Start of vectors for processor

	.word nmib	;NMIB
	.word begin	;RESET
	.word irqb	;IRQ/BRK
