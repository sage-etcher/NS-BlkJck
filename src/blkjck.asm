
;struct pixel { 
;  u16 offset, 
;  u8  data, 
;  u8  mask 
;}
pixeloffset	equ	00000h	;uptrdiff pixel.offset
pixeldata       equ	00002h	;byte     pixel.data
pixelmask       equ	00003h	;mask     pixel.mask
pixelsize	equ	00004h	;sizeof (struct pixel)

dram	equ	08000h		;start address of dram
gx	equ	00100h		;move > 1 cell in dram (8px)
gy	equ	00001h		;move v 1 cell in dram (1px)


tpa	equ	00100h
	org	tpa
start:	
	call	loaddram	;load dram
	call	resetscroll	;reset dram scroll
	call	drawwelcome	;ret draw the welcome screen
				;define tsalt from ui

	;lhld	tsalt		;HL=tsalt
	;call	genprand	;define trandarr from tsalt

	;call	newdeck		;define deck
				;define decktop=deck
	;call	getrand		;HL=randnum
	;call	shuffledeck	;shuffle the deck

	;call	gclear		;clear the screen
	;call	drawdeck	;HL=card ptr
	;call	gdrawcard	;display card to screen

exit:
	call	loadmainram	;restore ram (cpm)
	rst	0		;warm-boot
halt:	hlt
	jmp	halt


;procedure drawwelcome (void): tsalt
;draws welcome page and defines tsalt
;side effects: (gdraw) HL DE cursor tsalt
;/*{{{*/
drawwelcome:

	lxi	H,dram		;HL=dram addr
	shld	cursor		;place cursor at start of drom (topleft)

	lxi	H,gmwelcome	;HL=welcome page pixels
	lxi	D,gmwelcomelen	;DE=welcome page len
	call	gdraw		;draw pixels to screen

				;TODO: vvvv
				;prompt for user input
				;validate the input
				;convert input str to u16
				;store the salt

				;DEBUG SOLUTION
	lxi	h,0000h		;HL=0000h (debug salt)
	shld	tsalt		;store the salt

	ret
;/*}}}*/


;procedure loaddram (void): ram[08000H..0C000H]
;loads dram into vram pages 2+3 ram[08000h..0FFFFh]
;side effects: A ram[08000H..0FFFFH] out[0A2h] out[0A3h]
;/*{{{*/
loaddram:
	mvi	a,1000$0000B	;load dram page 0
	out	0A2H		;into vram page 2 (08000h-0BFFFh)
	mvi	a,1000$0001B	;load dram page 1
	out	0A3H		;load vram page 3 (0C000h-0FFFFh)
	ret
;/*}}}*/


;procedure loadmainram (void): ram[08000H..0C000H]
;loads main ram page 2+3 into vram pages 2+3 ram[08000h..0FFFFh]
;side effects: A ram[08000H..0FFFFH] out[0A2h] out[0A3h]
;/*{{{*/
loadmainram:
	mvi	a,0000$0010B	;load main page 2
	out	0A2H		;into vram page 2 (08000h-0C000h)
	mvi	a,0000$0011B	;load main page 3
	out	0A3H		;into vram page 3 (0C000h-0FFFFh)
	ret
;/*}}}*/


;procedure resetscroll (void): out[0??h]
;sets the scroll register to 0, so we can draw things easier
;side effects: A out[0??h]
;/*{{{*/
resetscroll:
	mvi	a,0		;A=scroll value
	out	0??h		;output to scroll register
	ret
;/*}}}*/


;procedure gdraw (DE=arraylen, HL=*pixelarr): void
;takes an array of pixels, and draws them to dram, relative to cursor
;side effects: AF BC DE HL dram[*]
;/*{{{*/
gdraw$arrptr:	DS	2	;*pixel
gdraw$i		DS	2	;iterator u16
gdraw:	;HL=pixel array
	;DE=array len
	shld	gdraw$arrptr	;store pixelarray
	xchg			;HL=array len
	shld	gdraw$i		;store arraylen
gdrawlc:	;loop check
	lhld	gdraw$i
	xra	A		;A=0
	cmp	H		;if (H == 0 && L == 0) break;
	jnz	gdrawl
	cmp	L
	jnz	gdrawl
	;continue after loop finished
	ret
gdrawl:		;loop
	lhld	gdraw$arrptr	;HL=*struct pixel
	mov	e,m		;DE=pixel->offset
	inx	h
	mov	d,m
	inx	h		;HL=&pixel.data
	mov	b,m		;B=pixel->data
	inx	h		;HL=&pixel.mask
	mov	c,m		;C=pixel->mask
	lhld	cursor		;HL=cursor position
	dad	d		;HL=cursor+offset
	mov	a,m		;A=old display data 
	ana	c		;A &= pixel->mask
	ora	b		;A |= pixel->data
	mov	m,a		;write the data into dram

	lhld	gdraw$arrptr	;HL=*struct pixel
	lxi	d,pixelsize	;DE=sizeof (struct pixel)
	dad	d		;HL=*struct pixel++
	shld	gdraw$arrptr	;store the ptr for next iteration
	lhld	gdraw$i		;HL=iter
	dcx	h		;HL--
	shld	gdraw$i		;iter=HL
	jmp	gdrawlc		;check if loop is finished
;/*}}}*/


;procedure gdrawrect (DE=offset_a, HL=offset_b): dram[*]
;draws a rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*] 
;/*{{{*/
offsetx		equ	00000h
offsety		equ	00001h
offsetsize	equ	00002h

gdr$init:	DS	offsetsize
gdr$last:	DS	offsetsize
gdr$i:		DS	offsetsize

gdrawrect:
	shld	gdr$end		;store end
	xchg			;store start
	shld	gdr$start

	
	
	;fall through
x
xchk:
	lxi	h,gdr$i+offsetx	;HL=&i.x
	mov	b,m		;B=i.x
	lda	gdr$last+offsetx;A=last.x
	cmp	b		;if (i.x > last.x) no loop
	jnc	xloop
	;xloop over
	ret

xloop:
	call	y


	lxi	h,gdr$i+offsetx	;HL=&i.x
	mov	a,m		;A=i.x
	inr	a		;A++
	mov	m,a		;i.x=A

	jmp	xchk

y
ychk:
yloop:
	jmp	ychk
		
	



	ret
;/*}}}*/	


;byte *cursor;
cursor		ds	2	;current position in dram

;struct pixel gmwelcome[]
gmwelcomelen	equ	2
gmwelcome:	;pixel.offset		pixel.data   pixel.mask
	dw	(gy*2)+(gx*2) !db	1010$1010B,  0000$0000B
	dw	(gy*3)+(gx*2) !db	0101$0101B,  0000$0000B
	dw	(gy*2)+(gx*2) !db	1111$0000b,  0000$1111B
	dw	(gy*3)+(gx*2) !db	1111$0000b,  0000$0000B

; 1 1 1 1 1 0 1 0
; 1 1 1 1 0 0 0 0


;uint16 tsalt;
tsalt:		DS	2	;salt for random number generation

;byte *genprand;
genprand:	DS	2	;psudo random number ptrarray

;byte *memory;
memory:	equ	$


; vim: ts=8 sts=8 sw=8 noet fdm=marker
