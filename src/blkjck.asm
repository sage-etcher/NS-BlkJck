

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
;  face$two   = 0x00   
;  face$three = 0x01   
;  face$four  = 0x02   
;  face$five  = 0x03   
;  face$six   = 0x04   
;  face$seven = 0x05   
;  face$eight = 0x06   
;  face$nine  = 0x07   
;  face$jack  = 0x08   
;  face$queen = 0x09   
;  face$king  = 0x0a   
;  face$ace   = 0x0b   
;}
face$two	equ	000h   
face$three	equ	001h   
face$four 	equ	002h   
face$five 	equ	003h   
face$six  	equ	004h   
face$seven	equ	005h   
face$eight	equ	006h   
face$nine 	equ	007h   
face$jack 	equ	008h   
face$queen	equ	009h   
face$king 	equ	00ah   
face$ace  	equ	00bh   
face$count	equ	00ch

;enum cardtype {
;  type$spade   = 0x00
;  type$heart   = 0x01
;  type$clover  = 0x02
;  type$diamond = 0x03
;}
type$spade	equ	000h
type$heart	equ	001h
type$clover	equ	002h
type$diamond	equ	003h
type$count	equ	004h

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
ctrlreg		equ	0f0h	;output 
statreg1	equ	0e0h	;input
statreg2	equ	0d0h	;input

scrollreg	equ	090h	;output

vrampage0	equ	0a0h	;output 00000h-03fffh 
vrampage1	equ	0a1h	;output 04000h-07fffh
vrampage2	equ	0a2h	;output 08000h-0bfffh
vrampage3	equ	0a3h	;output 0c000h-0ffffh

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
cardheight	equ	74

deckoffset	equ	00308h
presentoffset	equ	02280h


;program start
tpa	equ	00100h
	org	tpa
init:
	lxi	h,0		;hl=sp
	dad	sp
	shld	cpmstack	;cpmstack=sp
	lxi	sp,progstack	;sp=program stack
	call	loaddram	;load dram
	call	resetscroll	;reset dram scroll
start:	
	call	initdeck	;create unshuffled deck
	call	testpage	;draw a test page


exit:
	call	clearscr	;clear screen
	call	loadmainram	;load mainram
	lhld	cpmstack	;hl=cpmstack
	sphl			;sp=cpmstack
	rst	0		;warm-boot
halt:	hlt
	jmp	halt



testpage:
	call	clearscr	;clear the screen
	call	drawborder	;draw the table decorations

	lxi	h,dram+deckoffset
	shld	cursor		;set cursor
	call	drawcardb	;draw card base
	call	drawback	;draw the card's background

testloop:
	call	readkey		;read a key
	cpi	'q'		;if (key == 'q') break
	jz	testdone

	lxi	h,dram+presentoffset
	shld	cursor
	call	drawcardb	;draw card base
	call	getdeckindex
	call	drawcarddata

	xra	a
	lhld	deck$index
	cmp	h
	jnz	testskip
	cmp	l
	jnz	testskip
	jmp	testdone	
testskip:
	dcx	h
	shld	deck$index

	jmp	testloop

testdone:
	ret


savecursor:	DS	2
drawcarddata:
	push	h
	lhld	cursor
	shld	savecursor
	mvi	a,1
	call	curright
	mvi	a,6
	call	curdown
	pop	h
	call	drawsign
	
	push	h
	lhld	savecursor
	shld	cursor
	mvi	a,cardwidth-3
	call	curright
	mvi	a,cardheight-14
	call	curdown
	pop	h
	call	drawsign

	ret

drawsign:
	push	h
	lxi	d,cardsign
	dad	d
	mov	l,m
	mvi	h,0
	dad	h
	lxi	d,gmtypearr
	dad	d
	mov	e,m
	inx	h
	mov	d,m
	xchg
	lxi	d,gmtypesize
	call	gdraw
	pop	h
	ret


;procedure initdeck (void): deck
;initialize the deck (unshuffled)
;side effects:
;/*{{{*/
ideck$facei:	ds	1
ideck$typei:	ds	1
ideck$decki:	ds	1

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
	mvi	a,face$count-1
	sta	ideck$facei
faceloop$loop:
;typeloop$init:
	mvi	a,type$count-1
	sta	ideck$typei
typeloop$loop:
	call	getdeckindex		;HL=&deck[index]

	push	h
	lxi	d,cardflipped
	dad	d
	mvi	a,0			;A = false
	mov	m,a			;deck_iter->flipped = false
	pop	h

	push	h
	lxi	d,cardface
	dad	d
	lda	ideck$facei		;A=face
	mov	m,a			;deck_iter->face = face
	pop	h

	push	h
	lxi	d,cardsign
	dad	d
	lda	ideck$typei		;A=type
	mov	m,a			;deck_iter->type = type
	pop	h

	lhld	deck$index
	inx	h
	shld	deck$index

	lxi	h,ideck$typei
	mov	a,m
	cpi	0
	jz	typeloop$done
	dcr	m
	jmp	typeloop$loop
typeloop$done:
	lxi	h,ideck$facei
	mov	a,m
	cpi	0
	jz	faceloop$done
	dcr	m
	jmp	faceloop$loop
faceloop$done:
	lxi	h,ideck$decki
	dcr	m
	jmp	deckloop$loop
deckloop$done:
	lhld	deck$index
	dcx	h
	shld	deck$index
	ret
;/*}}}*/


;procedure getdeckindex (deck$index): HL=&deckiter
;get the current index of the deck
;side effects: 
;/*{{{*/
getdeckindex:
	;hl=deck[deck$index]
	lhld	deck$index		;hl=index
	mov	a,h			;a=highorder byte
	ani	0011$1111b		;mask off top 2 bits
	rlc !rlc			;multiply by 4
	mov	h,a			;temporarily store it
	mov	a,l			;a=loworder byte
	rlc !rlc			;multiply by 4 (kinda)
	mov	l,a			;store temp in l
	ani	0000$0011b		;masc off all but bottom 2 bits
	ora	h			;combine with highorder
	mov	h,a			;new highorder done
	mov	a,l			;a=temp loworder
	ani	1111$1100b		;mask off lower 2 trash bits
	mov	l,a			;store l
	lxi	d,deck			;de=&deck
	dad	d			;hl=deck + (index * sizeof (card))
	ret
;/*}}}*/


;procedure drawwelcome (void): tsalt
;draws welcome page and defines tsalt
;side effects: (gdraw) hl de cursor tsalt
;/*{{{*/
drawwelcome:

	lxi	h,dram		;hl=dram addr
	shld	cursor		;place cursor at start of drom (topleft)

	lxi	h,gmwelcome	;hl=welcome page pixels
	lxi	d,gmwelcomelen	;de=welcome page len
	call	gdraw		;draw pixels to screen

				;todo: vvvv
				;prompt for user input
				;validate the input
				;convert input str to u16
				;store the salt

				;debug solution
	lxi	h,00000h	;hl=0000h (debug salt)
	shld	tsalt		;store the salt

	ret
;/*}}}*/


;procedure command (c=command): statreg1 statreg2
;wait for the command acknowledgement flag to toggled
;side effects: a ctrlreg statreg1 statreg2
;/*{{{*/
command:
	push	b		;store bc

	in	statreg2	;get pre status
	ani	1000$0000b	;mask away trash
	mov	b,a		;store prestatus in b

	mov	a,c		;a=command	
	out	ctrlreg		;run command

command$ack:
	in	statreg2	;get post status
	ani	1000$0000b	;mask away trash
	cmp	b		;while (ackflag == preflag) loop
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
	mvi	c,0000$0011b	;complement keyboard mi flag command
	call	command		;run command
	in	statreg1	;get result
	ani	0000$0001b	;mask away trash
	cpi	0		;if (kbmi_flag == false) toggle
	jz	kbmi$toggle
	ret			;else the flag is true, return
;/*}}}*/


;procedure readkey (void): a=keycode
;wait for keyboard input, return the first key
;side effects: hwkeybuf a
;/*{{{*/
readkey:
	;wait keycode
	call	kbmi$enable	;make sure maskable interupts are enabled
readkey$loop:
	in	statreg2	;get keyboard status
	ani	0100$0000b	;mask away trash
	cpi	0		;while (keyboard_data_flag == 0) loop
	jz	readkey$loop

	;read keycode
	mvi	c,0000$0001b	;get low nibble command
	call	command		;run command
	in	statreg2	;get result
	ani	0000$1111b	;mask away trash
	mov	b,a		;store low nib in b

	mvi	c,0000$0010b	;get high nibble command
	call	command		;run command
	in	statreg2	;get result
	ani	0000$1111b	;mask away trash
	rlc !rlc !rlc !rlc	;bit shift high nib into position
	
	ora	b		;combine the high and low nibbles

	ret			;return to caller (a=key)
;/*}}}*/


;procedure clearscr (void): dram
;clears the screen
;side effects: de hl b (grectf) dram
;/*{{{*/
clearscr:
	lxi	h,dram
	shld	cursor
	lxi	d,0		;start point
	mvi	h,maxwidth	;end point
	mvi	l,maxheight
	mvi	b,0000$0000b	;mask
	mvi	a,0000$0000b	;data
	call	grectf		;clear full screen
	ret
;/*}}}*/


;procedure drawborder (void): dram
;draw the board decorations
;side effects: dram a de hl (grect)
;/*{{{*/
field$left	equ	0011$0000b
field$right	equ	0000$1100b
field$mleft	equ	0011$1111b
field$mright	equ	1111$1100b

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
cardba$left	equ	1100$0000b	;left  card border a
cardba$right	equ	0000$0011b	;right card border a
cardba$mask	equ	1111$1111b	;left/right card mask a

cardbb$left	equ	0000$1000b	;left  card border b
cardbb$right	equ	0001$0000b	;right card border b
cardbb$mleft	equ	1100$1111b	;left  card mask b
cardbb$mright	equ	1111$0011b	;right card mask b

	;this procedure is verry lazily done, and is infentient but it works
	;should be optimized and "unstupided" later, for now it will suffice
drawcardb:
	lxi	d,0		;start offset
	mvi	h,cardwidth	;stop offset
	mvi	l,cardheight	
	xra	a		;data = 0000$0000b
	mov	b,a		;mask = 0000$0000b
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


;procedure curleft (a=value): cursor
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
back$data	equ	1010$1010b
back$mask	equ	0000$0000b

back$i:			ds	1
back$j:			ds	1
back$k:			ds	1
back$l:			ds	1
drawback:
	lhld	cursor
	push	h

	mvi	a,1
	call	curright
	mvi	a,5
	call	curdown

back$initb:
	lhld	cursor
	push	h

	mvi	a,4
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
	pop	h
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
	pop	h
	shld	cursor

	pop	h
	shld	cursor
	
	ret
	
	
;/*}}}*/


;procedure loaddram (void): ram[08000h..0c000h]
;loads dram into vram pages 2+3 ram[08000h..0ffffh]
;side effects: a ram[08000h..0ffffh] out[vrampage2] out[vrampage3]
;/*{{{*/
loaddram:
	mvi	a,drampage0	;load dram page 0
	out	vrampage2	;into vram page 2 (08000h-0bfffh)
	mvi	a,drampage1	;load dram page 1
	out	vrampage3	;load vram page 3 (0c000h-0ffffh)
	ret
;/*}}}*/


;procedure loadmainram (void): ram[08000h..0c000h]
;loads main ram page 2+3 into vram pages 2+3 ram[08000h..0ffffh]
;side effects: a ram[08000h..0ffffh] out[vrampage2] out[vrampage3]
;/*{{{*/
loadmainram:
	mvi	a,mainpage2	;load main page 2
	out	vrampage2	;into vram page 2 (08000h-0c000h)
	mvi	a,mainpage3	;load main page 3
	out	vrampage3	;into vram page 3 (0c000h-0ffffh)
	ret
;/*}}}*


;procedure resetscroll (void): out[0??h]
;sets the scroll register to 0, so we can draw things easier
;side effects: a out[scrollreg]
;/*{{{*/
resetscroll:
	mvi	a,0		;a=scroll value
	out	scrollreg	;output to the start scan register
	ret
;/*}}}*/


;procedure gdraw (de=arraylen, hl=*pixelarr): void
;takes an array of pixels, and draws them to dram, relative to cursor
;side effects: af bc de hl dram[*]
;/*{{{*/
gdraw$arrptr:	ds	2	;*pixel
gdraw$i		ds	2	;iterator u16
gdraw:	;hl=pixel array
	;de=array len
	shld	gdraw$arrptr	;store pixelarray
	xchg			;hl=array len
	shld	gdraw$i		;store arraylen
gdraw$loop:
	lhld	gdraw$i
	xra	a		;a=0
	cmp	h		;if (h == 0 && l == 0) break;
	jnz	gdraw$block
	cmp	l
	jz	gdraw$done
	;fall through
gdraw$block:
	lhld	gdraw$arrptr	;hl=*struct pixel
	mov	e,m		;de=pixel->offset (little endian)
	inx	h
	mov	d,m
	inx	h		;hl=&pixel.data
	mov	b,m		;b=pixel->data
	inx	h		;hl=&pixel.mask
	mov	c,m		;c=pixel->mask
	lhld	cursor		;hl=cursor position
	dad	d		;hl=cursor+offset
	mov	a,m		;a=old display data 
	ana	c		;a &= pixel->mask
	ora	b		;a |= pixel->data
	mov	m,a		;write the data into dram

	lhld	gdraw$arrptr	;hl=*struct pixel
	lxi	d,pixelsize	;de=sizeof (struct pixel)
	dad	d		;hl=*struct pixel++
	shld	gdraw$arrptr	;store the ptr for next iteration
	lhld	gdraw$i		;hl=iter
	dcx	h		;hl--
	shld	gdraw$i		;iter=hl

	jmp	gdraw$loop	;loop

gdraw$done:
	ret
;/*}}}*/


;procedure grectf (a=fillbyte, b=bitmask, de=offset_a, hl=offset_b): void
;draws a filled rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*]
;/*{{{*/
grectf$data:	ds	1
grectf$mask:	ds	1
grectf$start:	ds	offsetsize
grectf$end:	ds	offsetsize
grectf$i:	ds	offsetsize

grectf:	;a=data  b=bitmask  de=point1  hl=point2
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
	lda	grectf$i+offsetx	;b=i.x
	mov	b,a
	lda	grectf$end+offsetx	;a=end.x
	cmp	b			;if (i.x >= end.x) break
	jc	grectf$xdone
	jz	grectf$xdone

;grectf$yinit:
	lda	grectf$start+offsety	;i.y = start.y
	sta	grectf$i+offsety

	;fallthrough
grectf$yloop:
	lda	grectf$i+offsety	;b=i.y
	mov	b,a
	lda	grectf$end+offsety	;a=end.y
	cmp	b			;if (i.y >= end.y) break
	jc	grectf$ydone
	jz	grectf$ydone

	lhld	cursor			;hl=cursor
	xchg				;de=cursor
	lhld	grectf$i		;hl=i
	dad	d			;hl=cursor + i
	mov	b,m			;b=old byte
	lda	grectf$mask		;a=(old & maks) | data
	ana	b
	mov	b,a
	lda	grectf$data
	ora	b
	mov	m,a			;*(hl)=new byte

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


;procedure grect (de=offset_a, hl=offset_b): void
;draw a rectange from offset_a (top left) to offset_b (bottom right)
;side effects: dram[*]
;/*{{{*/
grect$start:	ds	offsetsize
grect$end:	ds	offsetsize
grect$i:	ds	offsetsize

grect:
	shld	grect$end	;store end
	xchg			;stort start
	shld	grect$start

;grect$xinit:	;draw top and bottom using xloop
	lhld	grect$start	;i=start
	shld	grect$i

	;fallthrough
grect$xloop:
	lda	grect$i+offsetx		;b=i.x
	mov	b,a
	lda	grect$end+offsetx	;a=end.x
	cmp	b			;if (i.x >= end.x) break
	jc	grect$xdone
	jz	grect$xdone

	lhld	cursor			;de=cursor
	xchg
	lhld	grect$i			;hl=i
	dad	d			;hl=cursor+i
	mov	a,m			;a=old byte
	lda	border$mtop		;a=(oldbyte & tmask) | topbyte
	ana	b
	mov	b,a
	lda	border$top
	ora	b
	mov	m,a			;*(hl)=top byte

	lda	grect$i+offsetx		;hl.x = i.x
	mov	h,a
	lda	grect$end+offsety	;hl.y = end.y-1 (exclusive)
	dcr	a
	mov	l,a
	dad	d			;hl=cursor+bottom offset
	mov	b,m			;a=oldbyte
	lda	border$mbottom		;a=(oldbyte & bmask) | bottombyte
	ana	b
	mov	b,a
	lda	border$bottom
	ora	b
	mov	m,a			;*(hl)=bottom byte
	
	lxi	h,grect$i+offsetx	;i.x++
	inr	m

	jmp	grect$xloop		;loop xloop

grect$xdone:
;grect$yinit:	;draw left and right using yloop
	lhld	grect$start		;i=start
	shld	grect$i
	
	;fall through
grect$yloop:	
	lda	grect$i+offsety		;b=i.y
	mov	b,a
	lda	grect$end+offsety	;a=end.y
	cmp	b			;if (i.y >= end.y) break
	jc	grect$ydone
	jz	grect$ydone

	lhld	cursor			;de=cursor
	xchg
	lhld	grect$i			;hl=i
	dad	d			;hl=cursor+i
	mov	b,m			;a=old byte
	lda	border$mleft		;a=(oldbyte & lmask) | leftbyte
	ana	b			
	mov	b,a
	lda	border$left
	ora	b
	mov	m,a			;*(hl)=new left byte

	lda	grect$i+offsety		;hl.y = i.y
	mov	l,a
	lda	grect$end+offsetx	;hl.x = end.x-1 (exclusive)
	dcr	a
	mov	h,a
	dad	d			;hl=cursor+right offset
	mov	b,m			;a=old byte
	lda	border$mright		;a=(oldbyte & rmask) | rightbyte
	ana	b
	mov	b,a
	lda	border$right
	ora	b
	mov	m,a			;*(hl)=right byte
	
	lxi	h,grect$i+offsety	;i.y++
	inr	m

	jmp	grect$yloop		;loop yloop

grect$ydone:
	ret
;/*}}}*/


;procedure gline (a=length, b=fillbyte, c=bitmask, de=offsetelem, hl=offset): dram
;draw a line
;side effects: dram
;/*{{{*/
gline$data:	db	1
gline$mask:	db	1
gline$offset:	ds	offsetsize
gline$offelem:	ds	offsetsize
gline$length:	db	1

gline:	
	shld	gline$offset	;store start offset
	xchg			;store elem (x or y)
	shld	gline$offelem
	sta	gline$length	;store line length
	mov	a,b		;store fill byte
	sta	gline$data
	mov	a,b		;store bit mask 
	sta	gline$mask

gline$loop:
	lda	gline$length	;while (not (len == 0)) loop
	cpi	0
	jz	gline$done

	lhld	cursor		;hl=cursor+offset
	xchg
	lhld	gline$offset
	dad	d

	mov	b,m		;*hl = (old data & mask) | data
	lda	gline$mask
	ana	b
	mov	b,a
	lda	gline$data
	ora	b
	mov	m,a

	lxi	d,gline$offset	;de=&offset
	lhld	gline$offelem	;hl=offsetof(offset.elem)
	dad	d		;hl=&offset.elem
	inr	m		;increment the element

	lxi	h,gline$length	;decrement loop counter
	dcr	m

	jmp	gline$loop	;loop

gline$done:
	ret
;/*}}}*/
	

border$top:	db	1111$1111b
border$bottom:	db	1111$1111b
border$left:	db	1000$0000b
border$right:	db	0000$0001b

border$mtop:	db	1111$1111b
border$mbottom:	db	1111$1111b
border$mleft:	db	1111$1111b
border$mright:	db	1111$1111b

;byte *cursor;
cursor		ds	2	;current position in dram

;struct pixel gmwelcome[]
gmwelcomelen	equ	4
gmwelcome:     ;y   x	pixel.data   pixel.mask
	db	2,  2,	1010$1010b,  0000$0000b
	db	2,  3,	0101$0101b,  0000$0000b
	db	2,  2,	1111$0000b,  0000$1111b
	db	2,  3,	1111$0000b,  0000$0000b

;/*{{{*/
gmheart:
	db	0,  0,  0011$1110B,  0000$0000b
	db	0,  1,  0111$1100B,  0000$0000b
	db	1,  0,  0111$1111B,  0000$0000b 
	db	1,  1,  1111$1110B,  0000$0000b
	db	2,  0,  1111$1111B,  0000$0000b 
	db	2,  1,  1111$1111B,  0000$0000b
	db	3,  0,  1111$1111B,  0000$0000b 
	db	3,  1,  1111$1111B,  0000$0000b
	db	4,  0,  0111$1111B,  0000$0000b 
	db	4,  1,  1111$1110B,  0000$0000b
	db	5,  0,  0011$1111B,  0000$0000b 
	db	5,  1,  1111$1100B,  0000$0000b
	db	6,  0,  0000$1111B,  0000$0000b 
	db	6,  1,  1111$0000B,  0000$0000b
	db	7,  0,  0000$0011B,  0000$0000b 
	db	7,  1,  1100$0000B,  0000$0000b

gmdiamond:
	db	0,  0,  0000$0001b,  0000$0000b
	db	0,  1,  1000$0000b,  0000$0000b
	db	1,  0,  0000$0111b,  0000$0000b
	db	1,  1,  1110$0000b,  0000$0000b
	db	2,  0,  0001$1111b,  0000$0000b
	db	2,  1,  1111$1000b,  0000$0000b
	db	3,  0,  0011$1111b,  0000$0000b
	db	3,  1,  1111$1100b,  0000$0000b
	db	4,  0,  0011$1111b,  0000$0000b
	db	4,  1,  1111$1100b,  0000$0000b
	db	5,  0,  0001$1111b,  0000$0000b
	db	5,  1,  1111$1000b,  0000$0000b
	db	6,  0,  0000$0111b,  0000$0000b
	db	6,  1,  1110$0000b,  0000$0000b
	db	7,  0,  0000$0001b,  0000$0000b
	db	7,  1,  1000$0000b,  0000$0000b

gmspade:
	db	0,  0,  0000$0001b,  0000$0000b
	db	0,  1,  1000$0000b,  0000$0000b
	db	1,  0,  0000$1111b,  0000$0000b
	db	1,  1,  1111$0000b,  0000$0000b
	db	2,  0,  0011$1111b,  0000$0000b
	db	2,  1,  1111$1100b,  0000$0000b
	db	3,  0,  0111$1111b,  0000$0000b
	db	3,  1,  1111$1110b,  0000$0000b
	db	4,  0,  1111$1111b,  0000$0000b
	db	4,  1,  1111$1111b,  0000$0000b
	db	5,  0,  0111$1111b,  0000$0000b
	db	5,  1,  1111$1110b,  0000$0000b
	db	6,  0,  0011$1001b,  0000$0000b
	db	6,  1,  1001$1100b,  0000$0000b
	db	7,  0,  0000$0011b,  0000$0000b
	db	7,  1,  1100$0000b,  0000$0000b

gmclover:
	db	0,  0,  0000$0011b,  0000$0000b
	db	0,  1,  1100$0000b,  0000$0000b
	db	1,  0,  0000$0111b,  0000$0000b
	db	1,  1,  1110$0000b,  0000$0000b
	db	2,  0,  0000$0111b,  0000$0000b
	db	2,  1,  1110$0000b,  0000$0000b
	db	3,  0,  0111$1011b,  0000$0000b
	db	3,  1,  1101$1110b,  0000$0000b
	db	4,  0,  1111$1111b,  0000$0000b
	db	4,  1,  1111$1111b,  0000$0000b
	db	5,  0,  1111$1101b,  0000$0000b
	db	5,  1,  1011$1111b,  0000$0000b
	db	6,  0,  0111$1001b,  0000$0000b
	db	6,  1,  1001$1110b,  0000$0000b
	db	7,  0,  0000$0011b,  0000$0000b
	db	7,  1,  1100$0000b,  0000$0000b
;/*}}}*/

gmtypesize	equ	16
gmtypearr:
	dw	gmspade
	dw	gmheart
	dw	gmclover
	dw	gmdiamond


tsalt:		ds	2	;salt for random number generation
genprand:	ds	2	;psudo random number ptrarray

numofdecks	equ	6	;number of decks
singledecklen	equ	52	;number of cards in 1 deck
decklen		equ	(singledecklen * numofdecks)
deck:		ds	decklen * cardsize
deck$index:	dw	0 


cpmstack:	ds	2
stacksize	equ	1024
stackstart:	ds	stacksize
progstack:	equ	$

memory:		equ	$


; vim: ts=8 sts=8 sw=8 noet fdm=marker
