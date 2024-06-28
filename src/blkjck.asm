

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

					;yes the numbers are backwards :/
mainpage0	equ	0000$0011b	;program+bdos
mainpage1	equ	0000$0010b	;program
mainpage2	equ	0000$0001b	;program
mainpage3	equ	0000$0000b	;ccp, bdos, cpm

drampage0	equ	1000$0000b	;16kb (first 4/5 vertical lines)
drampage1	equ	1000$0001b	;4kb repeats to fill

prompage0	equ	1000$0100b	;2kb repeats to fill


dram	equ	08000h		;start address of dram (in page 2)
gx	equ	00100h		;move > 1 cell in dram (8px)
gy	equ	00001h		;move v 1 cell in dram (1px)


;program start
tpa	equ	00100h
	org	tpa
init:
	lxi	h,0		;HL=SP
	dad	sp
	shld	cpmstack	;cpmstack=SP
	lxi	sp,progstack	;SP=program stack
start:	
	call	loaddram	;load dram
	call	resetscroll	;reset dram scroll

	lxi	h,dram		;cursor = (0,0)
	shld	cursor		;set cursor

	xra	a		;A=0  fill byte
	lxi	d,00000h	;DE=0000  start offset
	lxi     h,050FFh	;HL=50FF  end offset (full screen)
	call	grectf		;clear the screen

	call	drawwelcome	;ret draw the welcome screen
				;define tsalt from ui

	;done. clean up
	call	loadmainram	;restore ram (cpm)
	
	mvi	c,001h
	call	0005h
exit:
	lhld	cpmstack	;HL=cpmstack
	SPHL			;SP=cpmstack
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
gdraw$loop:
	lhld	gdraw$i
	xra	A		;A=0
	cmp	H		;if (H == 0 && L == 0) break;
	jnz	gdraw$block
	cmp	L
	jz	gdraw$done
	;fall through
gdraw$block:
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

	jmp	gdraw$loop	;loop

gdraw$done:
	ret
;/*}}}*/


;procedure grectf (A=fillbyte, DE=offset_a, HL=offset_b): void
;draws a filled rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*]
;/*{{{*/
grectf$data:	DS	1
grectf$start:	DS	offsetsize
grectf$end:	DS	offsetsize
grectf$i:	DS	offsetsize

grectf:	;A=data  DE=point1  HL=point2
	sta	grectf$data	;store data
	shld	grectf$end	;store end
	xchg			;store start
	shld	grectf$start

;grectf$xinit:
	lda	grectf$start+offsetx	;i.x = start.x
	sta	grectf$i+offsetx

	;fallthrough
grectf$xloop:
	lda	grectf$i+offsetx	;B=i.x
	mov	b,a
	lda	grectf$end+offsetx	;A=end.x
	cmp	b			;if (i.x >= end.x) break
	jc	grectf$xdone
	jz	grectf$xdone

;grectf$yinit:
	lda	grectf$start+offsety	;i.y = start.y
	sta	grectf$i+offsety

	;fallthrough
grectf$yloop:
	lda	grectf$i+offsety	;B=i.y
	mov	b,a
	lda	grectf$end+offsety	;A=end.y
	cmp	b			;if (i.y >= end.y) break
	jc	grectf$ydone
	jz	grectf$ydone

	lda	grectf$data		;A=data
	lhld	cursor			;HL=cursor
	xchg				;DE=cursor
	lhld	grectf$i		;HL=i
	dad	d			;HL=cursor + i
	mov	m,a			;*(HL)=data

	lxi	h,grectf$i+offsety	;i.y++
	inr	m

	jmp	grectf$yloop		;loop yloop

grectf$ydone:
	lxi	h,grectf$i+offsetx	;i.x++
	inr	m

	jmp	grectf$xloop		;loop xloop

grectf$xdone:
	ret				;return to caller
;/*}}}*/	


;procedure grect (DE=offset_a, HL=offset_b): void
;draw a rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*]
;/*{{{*/
grect$start:	DS	offsetsize
grect$end:	DS	offsetsize
grect$i:	DS	offsetsize

grect$top:	DB	1111$1111b
grect$bottom:	DB	1111$1111b
grect$left:	DB	1000$0000b
grect$right:	DB	0000$0001b

grect:
	shld	grect$end	;store end
	xchg			;stort start
	shld	grect$start

;grect$yinit:	;draw left and right using yloop
	lhld	grect$start		;i=start
	shld	grect$i
	
grect$yloop:	
	lda	grect$i+offsety		;B=i.y
	mov	b,a
	lda	grect$end+offsety	;A=end.y
	cmp	b			;if (i.y >= end.y) break
	jc	grect$ydone
	jz	grect$ydone

	lhld	cursor			;DE=cursor
	xchg
	lhld	grect$i			;HL=i
	dad	d			;HL=cursor+i
	lda	grect$left		;A=left byte
	mov	m,a			;*(HL)=left byte

	lda	grect$i+offsety		;HL.y = i.y
	mov	l,a
	lda	grect$end+offsetx	;HL.x = end.x-1 (exclusive)
	dcr	a
	mov	h,a
	dad	d			;HL=cursor+right offset
	lda	grect$right		;A=right byte
	mov	m,a			;*(HL)=right byte
	
	lxi	h,grect$i+offsety	;i.y++
	inr	m

	jmp	grect$yloop		;loop yloop

grect$ydone:
	;fall through
;grect$xinit:	;draw top and bottom using xloop
	lhld	grect$start	;i=start
	shld	grect$i

	;fallthrough
grect$xloop:
	lda	grect$i+offsetx		;B=i.x
	mov	b,a
	lda	grect$end+offsetx	;A=end.x
	cmp	b			;if (i.x >= end.x) break
	jc	grect$xdone
	jz	grect$xdone

	lhld	cursor			;DE=cursor
	xchg
	lhld	grect$i			;HL=i
	dad	d			;HL=cursor+i
	lda	grect$top		;A=top byte
	mov	m,a			;*(HL)=top byte

	lda	grect$i+offsetx		;HL.x = i.x
	mov	h,a
	lda	grect$end+offsety	;HL.y = end.y-1 (exclusive)
	dcr	a
	mov	l,a
	dad	d			;HL=cursor+bottom offset
	lda	grect$bottom		;A=bottom byte
	mov	m,a			;*(HL)=bottom byte
	
	lxi	h,grect$i+offsetx	;i.x++
	inr	m

	jmp	grect$xloop		;loop xloop

grect$xdone:
	ret
;/*}}}*/


;byte *cursor;
cursor		ds	2	;current position in dram

;struct pixel gmwelcome[]
gmwelcomelen	equ	4
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

cpmstack:	DS	2

stacksize	equ	1024
stackstart:	DS	stacksize
progstack:	equ	$

memory:		equ	$


; vim: ts=8 sts=8 sw=8 noet fdm=marker
