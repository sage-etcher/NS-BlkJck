

;struct offset {
;  u8 x,
;  u8 y,
;}
offsetx		equ	00001h	;x_offset offset.x
offsety		equ	00000h	;y_offset offset.y
offsetsize	equ	00002h	;sizeof (struct offset)

;struct pixel { 
;  u16 offset, 
;  u8  data, 
;  u8  mask 
;}
pixeloffset	equ	00000h	;offset   pixel.offset
pixeldata       equ	00002h	;byte     pixel.data
pixelmask       equ	00003h	;mask     pixel.mask
pixelsize	equ	00004h	;sizeof (struct pixel)

;in/out register and const values
ctrlreg		equ	0F0h	;output 
statreg1	equ	0E0h	;input
statreg2	equ	0D0h	;input

scrollreg	equ	090h	;output

vrampage0	equ	0A0h	;output 00000h-03FFFh 
vrampage1	equ	0A1h	;output 04000h-07FFFh
vrampage2	equ	0A2h	;output 08000h-0BFFFh
vrampage3	equ	0A3h	;output 0C000h-0FFFFh

mainpage0	equ	0000$0000b	;program+bdos
mainpage1	equ	0000$0001b	;program
mainpage2	equ	0000$0010b	;program
mainpage3	equ	0000$0011b	;ccp, bdos, cpm

drampage0	equ	1000$0000b	;16kb (first 4/5 vertical lines)
drampage1	equ	1000$0001b	;4kb repeats to fill

prompage0	equ	1000$0100b	;2kb repeats to fill


dram	equ	08000h		;start address of dram (in page 2)
gx	equ	00100h		;move > 1 cell in dram (8px)
gy	equ	00001h		;move v 1 cell in dram (1px)


;program start
tpa	equ	00100h
	org	tpa
start:	
	;call	loaddram	;load dram
	;call	resetscroll	;reset dram scroll
	;call	drawwelcome	;ret draw the welcome screen
				;define tsalt from ui
	
	lxi	h,dram		;hl=dram
	lxi	d,01010h	;x=16 y=16
	dad	d		;hl=dram[16][16]
	shld	cursor		;set cursor

	lxi	d,00000h	;x1=0  y1=0
	lxi	h,00A0Ah	;x2=10 y2=10
	mvi	b,1010$1010b	;fill byte
	call	grectf		;draw filled rectange

	lxi	h,cursor+offsetx;get cursor.x
	mov	a,m		;in a
	adi	010h		;move it right 16 cells	
	mov	m,a		;saves it


	lxi	d,00000h	;x1=0  y1=0
	lxi	h,00A0Ah	;x2=10 y2=10
	call	grect		;draw rectange

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
	lxi	h,00000h	;HL=0000h (debug salt)
	shld	tsalt		;store the salt

	ret
;/*}}}*/


;procedure loaddram (void): ram[08000H..0C000H]
;loads dram into vram pages 2+3 ram[08000h..0FFFFh]
;side effects: A ram[08000H..0FFFFH] out[vrampage2] out[vrampage3]
;/*{{{*/
loaddram:
	mvi	a,drampage0	;load dram page 0
	out	vrampage2	;into vram page 2 (08000h-0BFFFh)
	mvi	a,drampage1	;load dram page 1
	out	vrampage3	;load vram page 3 (0C000h-0FFFFh)
	ret
;/*}}}*/


;procedure loadmainram (void): ram[08000H..0C000H]
;loads main ram page 2+3 into vram pages 2+3 ram[08000h..0FFFFh]
;side effects: A ram[08000H..0FFFFH] out[vrampage2] out[vrampage3]
;/*{{{*/
loadmainram:
	mvi	a,mainpage2	;load main page 2
	out	vrampage2	;into vram page 2 (08000h-0C000h)
	mvi	a,mainpage3	;load main page 3
	out	vrampage3	;into vram page 3 (0C000h-0FFFFh)
	ret
;/*}}}*


;procedure resetscroll (void): out[0??h]
;sets the scroll register to 0, so we can draw things easier
;side effects: A out[scrollreg]
;/*{{{*/
resetscroll:
	mvi	a,0		;A=scroll value
	out	scrollreg	;output to the start scan register
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
	mov	e,m		;DE=pixel->offset (little endian)
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


;procedure grectf (A=fillbyte, DE=offset_a, HL=offset_b): void
;draws a filled rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*]
;/*{{{*/
grectf$fill:	DS	1
grectf$init:	DS	offsetsize
grectf$last:	DS	offsetsize
grectf$i:	DS	offsetsize

grectf:
	sta	grectf$fill	;store fill byte
	shld	grectf$last	;store last
	xchg			;store start
	shld	grectf$init
	shld	grectf$i	;i=start	
	
	;fall through
grectf$xchk:
	lxi	h,grectf$i+offsetx	;HL=&i.x
	mov	b,m			;B=i.x
	lda	grectf$last+offsetx	;A=last.x
	cmp	b			;while (i.x < last.x) xloop
	jnc	grectf$xloop
	;xloop over
	ret				;return to caller (exit proc)

grectf$xloop:
	lda	grectf$init+offsety	;i.y = init.y
	sta	grectf$i+offsety
	;fall through
grectf$ychk:
	lxi	h,grectf$i+offsety	;HL=&i.y
	lxi	b,m			;B=i.y
	lda	grectf$last+offsety	;A=last.y
	cmp	b			;while (i.y < last.y) yloop
	jnc	grectf$yloop			
	;yloop over	
	lxi	h,grectf$i+offsetx	;i.x++
	inr	m

	jmp	grectf$xchk		;draw next line
grectf$yloop:
	lhld	grectf$i		;HL=i
	lxi	d,cursor		;DE=cursor
	dad	d			;HL=cursor+i

	sta	grectf$fill		;A=fillbyte	
	mov	m,a			;draw fill byte to dram

	lxi	h,grectf$i+offsety	;i.y++
	inr	m

	jmp	grectf$ychk
;/*}}}*/	


;procedure grect (DE=offset_a, HL=offset_b): void
;draw a rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*]
;/*{{{*/
grect$init:	DS	offsetsize
grect$last:	DS	offsetsize
grect$i:	DS	offsetsize

grect$btop:	DB	1111$1111b
grect$bbottom:	DB	1111$1111b
grect$bleft:	DB	1000$0000b
grect$bright:	DB	0000$0001b

grect:
	shld	grect$last	;store last
	xchg			;stort init
	shld	grect$init

	;fall throught
grect$lftrght:			;draw left and right
	lhld	grect$init	;hl=init
	shld	grect$i		;i=hl

	lda	grect$bleft	;a=leftvalue
	mov	b,a		;b=leftvalue
	lda	grect$bright	;a=rightvalue
	mov	c,a		;c=rightvalue

	;fall through
grect$lrchk:
	lhld	grect$i			;HL=i
	lda	grect$last+offsety	;A=last.y
	cmp	l			;while (i.y <= last.y) tbloop
	jnc	grect$tbloop
	;lrloop over
	jmp	grect$topbtm		;draw top and bottom

grect$lrloop:
	lxi	d,cursor		;de=cursor
	dad	d			;hl=cursor + i

	mov	m,b			;draw the left border cell

	lda	grect$last+offsetx	;get address of the right cell
	add	d			;get the x address in ref on cursor (de)
	mov	h,a			;a=right x address

	mov	m,c			;draw the right border cell

	lxi	h,grect$i+offsety	;i.x++
	inr	m

	jmp	grect$lrchk		;loop

grect$topbtm:			;draw top and bottom
	lhld	grect$init	;hl=init
	shld	grect$i		;i=hl

	lda	grect$btop	;a=topvalue
	mov	b,a		;b=topvalue
	lda	grect$bbottom	;a=bottomvalue
	mov	c,a		;c=bottomvalue

	;fall through
grect$tbchk:
	lhld	grect$i			;HL=i
	lda	grect$last+offsetx	;A=last.x
	cmp	h			;while (i.x <= last.x) tbloop
	jnc	grect$tbloop
	;tbloop over
	ret				;return to caller (exit procedure)

grect$tbloop:
	lxi	d,cursor		;de=cursor
	dad	d			;hl=cursor + i

	mov	m,b			;draw the top border cell

	lda	grect$last+offsety	;get address of the bottom cell
	add	e			;get the y address in ref on cursor (de)
	mov	l,a			;a=bottom y address

	mov	m,c			;draw the bottom border cell

	lxi	h,grect$i+offsetx	;i.x++
	inr	m

	jmp	grect$tbchk		;loop
;/*}}}*/


;byte *cursor;
cursor		ds	2	;current position in dram

;struct pixel gmwelcome[]
gmwelcomelen	equ	2
gmwelcome:     ;y   x	pixel.data   pixel.mask
	db	2,  2,	1010$1010B,  0000$0000B
	db	2,  3,	0101$0101B,  0000$0000B
	db	2,  2,	1111$0000b,  0000$1111B
	db	2,  3,	1111$0000b,  0000$0000B

; 1 1 1 1 1 0 1 0
; 1 1 1 1 0 0 0 0


;uint16 tsalt;
tsalt:		DS	2	;salt for random number generation

;byte *genprand;
genprand:	DS	2	;psudo random number ptrarray

;byte *memory;
memory:		equ	$


; vim: ts=8 sts=8 sw=8 noet fdm=marker
