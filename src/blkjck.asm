

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

;enum cardface {
;  FACE_TWO   = 0x00   
;  FACE_THREE = 0x01   
;  FACE_FOUR  = 0x02   
;  FACE_FIVE  = 0x03   
;  FACE_SIX   = 0x04   
;  FACE_SEVEN = 0x05   
;  FACE_EIGHT = 0x06   
;  FACE_NINE  = 0x07   
;  FACE_JACK  = 0x08   
;  FACE_QUEEN = 0x09   
;  FACE_KING  = 0x0A   
;  FACE_ACE   = 0x0B   
;}
FACE_TWO	EQU	000H   
FACE_THREE	EQU	001H   
FACE_FOUR 	EQU	002H   
FACE_FIVE 	EQU	003H   
FACE_SIX  	EQU	004H   
FACE_SEVEN	EQU	005H   
FACE_EIGHT	EQU	006H   
FACE_NINE 	EQU	007H   
FACE_JACK 	EQU	008H   
FACE_QUEEN	EQU	009H   
FACE_KING 	EQU	00AH   
FACE_ACE  	EQU	00BH   
FACE_COUNT	EQU	00CH

;enum cardtype {
;  TYPE_SPADE   = 0x00
;  TYPE_HEART   = 0x01
;  TYPE_CLOVER  = 0x02
;  TYPE_DIAMOND = 0x03
;}
TYPE_SPADE	EQU	000H
TYPE_HEART	EQU	001H
TYPE_CLOVER	EQU	002H
TYPE_DIAMOND	EQU	003H
TYPE_COUNT	EQU	004H

;struct card {
;  u8 fipped
;  u8 face
;  u8 sign
;  <u8 padding>		makes multiplying faster '<<2'
;}
cardflipped	equ	00000h
cardface	equ	00001h
cardsign	equ	00002h
cardsize	equ	1+1+1+1

;struct hand {
;  u8  count
;  u8  hardtotal
;  u8  softtotal
;  card *m[maxhandcards]
;}
handcount	equ	00000h
handsoft	equ	00001h
handhard	equ	00002h
handm		equ	00003h
maxhandcards	equ	21	;21 aces is max
handsize	equ	(2*maxhandcards)+1+1+1

;in/out register and const values
;/*{{{*/
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
;/*}}}*/

maxwidth	equ	80
maxheight	equ	240

cardwidth	equ	12
cardheight	equ	82

deckx	equ	3
decky	equ	12

;program start
tpa	equ	00100h
	org	tpa
init:
	lxi	h,0		;HL=SP
	dad	sp
	shld	cpmstack	;cpmstack=SP
	lxi	sp,progstack	;SP=program stack
	call	loaddram	;load dram
	;call	kbmi$enable	;enable keyboard maskable interupts
	call	resetscroll	;reset dram scroll
start:	
	call	testpage	;draw a test page

exit:
	call	loadmainram	;load mainram
	lhld	cpmstack	;HL=cpmstack
	SPHL			;SP=cpmstack
	rst	0		;warm-boot
halt:	hlt
	jmp	halt



testpage:
	lxi	h,dram		;cursor = (0,0)
	shld	cursor		;set cursor
	call	clearscr	;clear the screen
	call	drawborder	;draw the table decorations

	lxi	h,dram		;HL=dram
	mvi	d,deckx		;DE=(deckx,decky)
	mvi	e,decky
	dad	d		;HL=dram + deckoffset
	shld	cursor		;set cursor
	call	drawcardb	;draw card base
	call	drawback	;draw the card's background

testloop:
	call	readkey		;read a key
	cpi	'Q'		;if (key == 'Q') break
	jnz	testloop

	ret


;procedure initdeck (void): deck
;initialize the deck (unshuffled)
;side effects:
;/*{{{*/
ideck$facei:	DS	1
ideck$typei:	DS	1
ideck$decki:	DS	1

initdeck:
	lxi	h,0
	shld	deck$index

;deckloop$init:
	mvi	a,numofdecks
	sta	ideck$decki
deckloop$loop:
	lda	ideck$decki
	cpi	0
	jz	deckloop$done

;faceloop$init:
	mvi	a,FACE_COUNT-1
	sta	ideck$facei
faceloop$loop:
	lda	ideck$facei
	cpi	0
	jz	faceloop$done
;typeloop$init:
	mvi	a,TYPE_COUNT-1
	sta	ideck$typei
typeloop$loop:
	lda	ideck$typei
	cpi	0
	jz	typeloop$done

	;HL=deck[deck$index]
	lhld	deck$index		;HL=index
	mov	a,h			;A=highorder byte
	ani	0011$1111B		;mask off top 2 bits
	rlc !rlc			;multiply by 4
	mov	h,a			;temporarily store it
	mov	a,l			;A=loworder byte
	rlc !rlc			;multiply by 4 (kinda)
	mov	l,a			;store temp in L
	ani	0000$0011B		;masc off all but bottom 2 bits
	ora	H			;combine with highorder
	mov	h,a			;new highorder done
	mov	a,l			;A=temp loworder
	ani	1111$1100B		;mask off lower 2 trash bits
	mov	l,a			;store L

	lxi	d,deck			;DE=&deck
	dad	d			;HL=deck + (index * sizeof (card))

	push	h			;protect HL
	lxi	d,cardtype		;HL=&deck[index].type
	dad	d
	lda	ideck$typei		;A=type
	mov	m,a			;deck[index].type = type
	pop	h			;restore HL

	push	h			;protect HL
	lxi	d,cardface		;HL=&deck[index].face
	dad	d
	lda	ideck$facei		;A=type
	mov	m,a			;deck[index].type = type
	pop	h			;restore HL

	push	h			;protect HL
	lxi	d,cardtype		;HL=&deck[index].type
	dad	d
	lda	ideck$typei		;A=type
	mov	m,a			;deck[index].type = type
	pop	h			;restore HL



	




	lxi	h,ideck$typei
	dcr	m
	jmp	typeloop$loop
typeloop$done:
	lxi	h,ideck$facei
	dcr	m
	jmp	faceloop$loop
faceloop$done:
	lxi	h,ideck$decki
	dcr	m
	jmp	deckloop$loop
deckloop$done:

	ret

	


	
;/*}}}*/


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


;procedure command (C=command): statreg1 statreg2
;wait for the command acknowledgement flag to toggled
;side effects: A ctrlreg statreg1 statreg2
;/*{{{*/
command:
	push	b		;store bc

	in	statreg2	;get pre status
	ani	1000$0000B	;mask away trash
	mov	b,a		;store prestatus in B

	mov	a,c		;a=command	
	out	ctrlreg		;run command

command$ack:
	in	statreg2	;get post status
	ani	1000$0000B	;mask away trash
	cmp	B		;while (ackflag == preflag) loop
	jz	command$ack

command$done:
	pop	b		;restore bc
	ret			;else return
;/*}}}*/


;procedure kbmi$enable (void): kbmi
;enable keyboard maskable interupts flag
;side effects
;/*{{{*/
kbmi$enable:
kbmi$toggle:
	mvi	c,0001$0011B	;complement keyboard mi flag command
	call	command		;run command
	in	statreg1	;get result
	ani	0000$0001B	;mask away trash
	cpi	0		;if (kbmi_flag == false) toggle
	jz	kbmi$toggle
	ret			;else the flag is true, return
;/*}}}*/


;procedure readkey (void): A=keycode
;wait for keyboard input, return the first key
;side effects: hwkeybuf A
;/*{{{*/
readkey:
	;wait keycode
	in	statreg2	;get keyboard status
	ani	0100$0000B	;mask away trash
	cpi	0		;while (keyboard_data_flag == 0) loop
	jz	readkey

	;read keycode
	mvi	c,0001$0001B	;get low nibble command
	call	command		;run command
	in	statreg2	;get result
	ani	0000$1111B	;mask away trash
	mov	b,a		;store low nib in B

	mvi	c,0001$0010B	;get high nibble command
	call	command		;run command
	in	statreg2	;get result
	ani	0000$1111B	;mask away trash
	rlc !rlc !rlc !rlc	;bit shift high nib into position
	
	ora	b		;combine the high and low nibbles

	ret			;return to caller (A=key)
;/*}}}*/


;procedure clearscr (void): dram
;clears the screen
;side effects: DE HL B (grectf) dram
;/*{{{*/
clearscr:
	lxi	d,0		;start point
	mvi	h,maxwidth	;end point
	mvi	l,maxheight
	mvi	b,0000$0000B	;mask
	mvi	a,0000$0000B	;data
	call	grectf		;clear full screen
	ret
;/*}}}*/


;procedure drawborder (void): dram
;draw the board decorations
;side effects: dram A DE HL (grect)
;/*{{{*/
field$left	equ	0011$0000B
field$right	equ	0000$1100B
field$mleft	equ	0011$1111B
field$mright	equ	1111$1100B

drawborder:
	mvi	a,field$left
	sta	border$left
	mvi	a,field$mleft
	sta	border$mleft
	mvi	a,field$right
	sta	border$right
	mvi	a,field$mright
	sta	border$mright

	mvi	d,0		;offset_a = (0,1)
	mvi	e,1
	mvi	h,maxwidth	;offset_b = (maxwidth,maxheight-1)
	mvi	l,maxheight-1
	call	grect

	ret
;/*}}}*/


;procedure drawcardb (void): dram
;draws the card base
;side effects: dram
;/*{{{*/
cardba$left	equ	1100$0000B	;left  card border A
cardba$right	equ	0000$0011B	;right card border A
cardba$mask	equ	1111$1111B	;left/right card mask A

cardbb$left	equ	0000$1000B	;left  card border B
cardbb$right	equ	0001$0000B	;right card border B
cardbb$mleft	equ	1100$1111B	;left  card mask B
cardbb$mright	equ	1111$0011B	;right card mask B

	;this procedure is verry lazily done, and is infentient but it works
	;should be optimized and "unstupided" later, for now it will suffice
drawcardb:
	lxi	d,0		;start offset
	mvi	h,cardwidth	;stop offset
	mvi	l,cardheight	
	xra	A		;data = 0000$0000B
	mov	b,a		;mask = 0000$0000B
	call	grectf

	mvi	a,cardba$left
	sta	border$left
	mvi	a,cardba$right
	sta	border$right
	mvi	a,cardba$mask
	sta	border$mleft
	sta	border$mright

	lxi	d,0		;offset_a = (0,0)
	mvi	h,cardwidth	;offset_b = (cardwidth,cardheight)
	mvi	l,cardheight
	call	grect		;draw outer border
	
	mvi	a,cardbb$left	;left
	sta	border$left
	mvi	a,cardbb$mleft	;left mask
	sta	border$mleft
	mvi	a,cardbb$right	;right
	sta	border$right
	mvi	a,cardbb$mright	;right mask
	sta	border$mright
	
	mvi	d,0		;offset_a = (0,1)
	mvi	e,2
	mvi	h,cardwidth	;offset_b = (cardwidth, cardheight-2)
	mvi	l,cardheight-2
	call	grect

	ret
;/*}}}*/


;procedure curleft (A=value): cursor
;move the cursor to the left
;side effects:
;/*{{{*/
curright:
	lhld	cursor
	add	h
	mov	h,a
	shld	cursor
	ret
curleft:
	lhld	cursor
	mov	b,a
	mov	a,h
	sub	b
	mov	h,a
	shld	cursor
	ret
curdown:
	lhld	cursor
	add	l
	mov	l,a
	shld	cursor
	ret
curup:
	lhld	cursor
	mov	b,a
	mov	a,l
	sub	b
	mov	l,a
	shld	cursor
	ret
;/*}}}*/


;procedure drawback (void): dram
;draws the back of a card
;side effects: dram
;/*{{{*/
back$offset	equ	0208h
back$data	equ	1010$1010B
back$mask	equ	0000$0000B

back$i:			DS	1
back$j:			DS	1
back$k:			DS	1
back$l:			DS	1
drawback:
	lhld	cursor
	push	H

	mvi	a,1
	call	curright
	mvi	a,5
	call	curdown

back$initb:
	lhld	cursor
	push	h

	mvi	a,5
	sta	back$j

back$loopb:
	lda	back$j
	cpi	0
	jz	back$doneb

	;fall through
back$inita:
	lhld	cursor
	push	h

	mvi	a,3
	sta	back$i
	
	;fall through
back$loopa:
	lda	back$i	
	cpi	0
	jz	back$donea

	lxi	d,0		;offset_a
	lxi	h,back$offset	;offset_b
	mvi	a,back$data	;data byte
	mvi	b,back$mask	;data mask
	call	grectf		;draw the thing

	mvi	a,4
	call	curright

	lxi	h,back$i
	dcr	m

	jmp	back$loopa

back$donea: 
	pop	h
	shld	cursor
	mvi	a,16
	call	curdown

	lxi	h,back$j
	dcr	m

	jmp	back$loopb

back$doneb:
	pop	H
	shld	cursor

back$initc:
	mvi	a,2
	call	curright
	mvi	a,8
	call	curdown

	lhld	cursor
	push	h

	mvi	a,4
	sta	back$k

back$loopc:
	lda	back$k
	cpi	0
	jz	back$donec

back$initd:
	lhld	cursor
	push	h

	mvi	a,2
	sta	back$l

back$loopd:
	lda	back$l
	cpi	0
	jz	back$doned

	lxi	d,0		;offset_a
	lxi	h,back$offset	;offset_b
	mvi	a,back$data	;data byte
	mvi	b,back$mask	;data mask
	call	grectf		;draw the thing

	mvi	a,4
	call	curright

	lxi	h,back$l
	dcr	m

	jmp	back$loopd

back$doned:
	pop	h
	shld	cursor
	mvi	a,16
	call	curdown

	lxi	h,back$k
	dcr	m

	jmp	back$loopc

back$donec:
	pop	H
	shld	cursor

	pop	H
	shld	cursor
	
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


;procedure grectf (A=fillbyte, B=bitmask, DE=offset_a, HL=offset_b): void
;draws a filled rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*]
;/*{{{*/
grectf$data:	DS	1
grectf$mask:	DS	1
grectf$start:	DS	offsetsize
grectf$end:	DS	offsetsize
grectf$i:	DS	offsetsize

grectf:	;A=data  B=bitmask  DE=point1  HL=point2
	sta	grectf$data	;store data
	mov	a,b		;store mask
	sta	grectf$mask
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

	lhld	cursor			;HL=cursor
	xchg				;DE=cursor
	lhld	grectf$i		;HL=i
	dad	d			;HL=cursor + i
	mov	b,m			;B=old byte
	lda	grectf$mask		;A=(old & maks) | data
	ana	b
	mov	b,a
	lda	grectf$data
	ora	b
	mov	m,a			;*(HL)=new byte

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

grect:
	shld	grect$end	;store end
	xchg			;stort start
	shld	grect$start

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
	mov	a,m			;A=old byte
	lda	border$mtop		;A=(oldbyte & tmask) | topbyte
	ana	b
	mov	b,a
	lda	border$top
	ora	b
	mov	m,a			;*(HL)=top byte

	lda	grect$i+offsetx		;HL.x = i.x
	mov	h,a
	lda	grect$end+offsety	;HL.y = end.y-1 (exclusive)
	dcr	a
	mov	l,a
	dad	d			;HL=cursor+bottom offset
	mov	b,m			;A=oldbyte
	lda	border$mbottom		;A=(oldbyte & bmask) | bottombyte
	ana	b
	mov	b,a
	lda	border$bottom
	ora	b
	mov	m,a			;*(HL)=bottom byte
	
	lxi	h,grect$i+offsetx	;i.x++
	inr	m

	jmp	grect$xloop		;loop xloop

grect$xdone:
;grect$yinit:	;draw left and right using yloop
	lhld	grect$start		;i=start
	shld	grect$i
	
	;fall through
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
	mov	b,m			;A=old byte
	lda	border$mleft		;A=(oldbyte & lmask) | leftbyte
	ana	b			
	mov	b,a
	lda	border$left
	ora	b
	mov	m,a			;*(HL)=new left byte

	lda	grect$i+offsety		;HL.y = i.y
	mov	l,a
	lda	grect$end+offsetx	;HL.x = end.x-1 (exclusive)
	dcr	a
	mov	h,a
	dad	d			;HL=cursor+right offset
	mov	b,m			;A=old byte
	lda	border$mright		;A=(oldbyte & rmask) | rightbyte
	ana	b
	mov	b,a
	lda	border$right
	ora	b
	mov	m,a			;*(HL)=right byte
	
	lxi	h,grect$i+offsety	;i.y++
	inr	m

	jmp	grect$yloop		;loop yloop

grect$ydone:
	ret
;/*}}}*/


;procedure gline (A=length, B=fillbyte, C=bitmask, DE=offsetelem, HL=offset): dram
;draw a line
;side effects: dram
;/*{{{*/
gline$data:	DB	1
gline$mask:	DB	1
gline$offset:	DS	offsetsize
gline$offelem:	DS	offsetsize
gline$length:	DB	1

gline:	
	SHLD	gline$offset	;store start offset
	xchg			;store elem (x or y)
	shld	gline$offelem
	STA	gline$length	;store line length
	mov	A,B		;store fill byte
	STA	gline$data
	mov	A,B		;store bit mask 
	sta	gline$mask

gline$loop:
	lda	gline$length	;while (not (len == 0)) loop
	cpi	0
	jz	gline$done

	lhld	cursor		;HL=cursor+offset
	xchg
	lhld	gline$offset
	dad	d

	mov	b,m		;*HL = (old data & mask) | data
	lda	gline$mask
	ana	b
	mov	b,a
	lda	gline$data
	ora	b
	mov	m,a

	lxi	d,gline$offset	;DE=&offset
	lhld	gline$offelem	;HL=offsetof(offset.elem)
	dad	d		;HL=&offset.elem
	inr	m		;increment the element

	lxi	h,gline$length	;decrement loop counter
	dcr	m

	jmp	gline$loop	;loop

gline$done:
	ret
;/*}}}*/
	

border$top:	DB	1111$1111b
border$bottom:	DB	1111$1111b
border$left:	DB	1000$0000b
border$right:	DB	0000$0001b

border$mtop:	DB	1111$1111b
border$mbottom:	DB	1111$1111b
border$mleft:	DB	1111$1111b
border$mright:	DB	1111$1111b

;byte *cursor;
cursor		ds	2	;current position in dram

;struct pixel gmwelcome[]
gmwelcomelen	equ	4
gmwelcome:     ;y   x	pixel.data   pixel.mask
	db	2,  2,	1010$1010B,  0000$0000B
	db	2,  3,	0101$0101B,  0000$0000B
	db	2,  2,	1111$0000b,  0000$1111B
	db	2,  3,	1111$0000b,  0000$0000B

tsalt:		DS	2	;salt for random number generation
genprand:	DS	2	;psudo random number ptrarray

numofdecks	equ	6	;number of decks
singledecklen	equ	52	;number of cards in 1 deck
decklen		equ	(singledecklen * numofdecks)
deck:		DS	decklen * cardsize
deck$index:	DW	0 


cpmstack:	DS	2
stacksize	equ	1024
stackstart:	DS	stacksize
progstack:	equ	$

memory:		equ	$


; vim: ts=8 sts=8 sw=8 noet fdm=marker
