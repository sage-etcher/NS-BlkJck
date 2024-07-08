

;data types (structures and enums)
;/*{{{*/

true	equ	0FFH
false	equ	000H

u8size		equ	1
u16size		equ	2
ptrsize		equ	u16size
charsize	equ	u8size

structbase	equ	00000h
enumbase	equ	0
enumiter	equ	1

;struct offset {
;  u8 y,
;  u8 x,
;}
offsety		equ	structbase
offsetx		equ	u8size + offsety
offsetsize	equ	u8size + offsetx

;struct texture { 
;  u8	width,
;  u8   height,
;  u8   data[]
;}
texturewidth	equ	structbase
textureheight	equ	u8size + texturewidth
texturedata	equ	u8size + textureheight
;the texturesize is determinate by it's width*height

;enum handstate {
;  hs$normal    = 0000 0001
;  hs$bust      = 0000 0010
;  hs$blackjack = 0000 0100
;}
hs$normal	equ	0000$0001b
hs$bust		equ	0000$0010b
hs$blackjack	equ	0000$0100b

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
face$two	equ	enumbase
face$three	equ	enumiter + face$two
face$four 	equ	enumiter + face$three
face$five 	equ	enumiter + face$four
face$six  	equ	enumiter + face$five
face$seven	equ	enumiter + face$six
face$eight	equ	enumiter + face$seven
face$nine 	equ	enumiter + face$eight
face$ten	equ	enumiter + face$nine
face$jack 	equ	enumiter + face$ten
face$queen	equ	enumiter + face$jack
face$king 	equ	enumiter + face$queen
face$ace  	equ	enumiter + face$king
face$max	equ	face$ace

;enum cardsign {
;  sign$spade   = 0x00
;  sign$heart   = 0x01
;  sign$clover  = 0x02
;  sign$diamond = 0x03
;}
sign$spade	equ	enumbase
sign$heart	equ	enumiter + sign$spade
sign$clover	equ	enumiter + sign$heart
sign$diamond	equ	enumiter + sign$clover
sign$max	equ	sign$diamond

;struct card {
;  u8 face
;  u8 sign
;  <pad to 16bits/2bytes>	;makes multiplication easier
;}
cardface	equ	structbase
cardsign	equ	cardface    + u8size
cardsize	equ	2
;if cardsize is too small, asm.com will complain (V error code)
cardsizechk	equ	cardsize - (cardsign + u8size)

;struct hand {
;  u8  count
;  u8  softtotal
;  u8  hardtotal
;  hs  state
;  card *m[maxhandcards]
;}
maxhandcards	equ	21	;21 aces is max
handcount	equ	structbase
handsoft	equ	handcount + u8size
handhard	equ	handsoft  + u8size
handstate	equ	handhard  + u8size
handm		equ	handstate + u8size
handsize	equ	handm     + (ptrsize*maxhandcards)


;typedef struct hand dealer;
dealercount	equ	handcount
dealersoft	equ	handsoft
dealerhard	equ	handhard
dealerstate	equ	handstate
dealerm		equ	handm
dealersize	equ	handsize


;struct player {
;  char  name[namelen]
;  u16   score
;  u16   bet
;  u8    handcount
;  hand *handiter
;  hand  handarr[handlen]
;}
pnamelen	equ	32
phandlen	equ	5
playername	equ	structbase
playerscore	equ	playername    + (charsize*pnamelen)
playerbet	equ	playerscore   + u16size
playerhcount	equ	playerbet     + u16size
playerhandi	equ	playerhcount  + u8size
playerhandarr	equ	playerhandi   + ptrsize
playersize	equ	playerhandarr + (handsize*phandlen)



;/*}}}*/

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

LF		equ	0A0H
CR		equ	0D0H
				;visibly if were 1:1
maxwidth	equ	80	;640
maxheight	equ	240	;480

cardwidth	equ	12	;96
cardheight	equ	74	;148

deckoffset	equ	00308h	;(24,16)

dealerhandoff	equ	01508h	;x values must match for dealer and
dealermsgoff	equ	01718h	;offset for printing dealer messages

playerhandoff	equ	01560h	;player for x overflow to work
playermsgoff	equ	01770h	;offset for printing player messages

gamemsgoff	equ	02054h	;game message offset

presentoffset	equ	02280h	;(34,128)

nextcardx	equ	6
nextcardy	equ	0

doublecardx	equ	nextcardx/4
doublecardy	equ	256-12

temp$a		set	((dram + playerhandoff) AND 0FF00H) SHR 8
temp$b		set	(temp$a + (nextcardx/2)) XOR (temp$a)
overflowcardx	equ	temp$b
overflowcardy	equ	30

;trap, if the x value of dealer and player offsets differ, the overflow
;calculation will fail. the following will throw an assembler error, if
;they differ
TRAP	SET	(((dealerhandoff XOR playerhandoff) AND 0FF00H) SHR 8)
IF TRAP
error_player_and_dealer_x_offsets_differ:
ENDIF


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
	call	drawtestpage	;draw a test page

exit:
	call	clearscr	;clear screen
	call	loadmainram	;load mainram
	lhld	cpmstack	;hl=cpmstack
	sphl			;sp=cpmstack
	rst	0		;warm-boot
halt:	hlt
	jmp	halt

;main procedures
;/*{{{*/


;procedure getcardvalue (HL=cardptr): A=hardvalue B=softvalue
;return the value associated with a card
;side effects: idk i stopped keeping track
;/*{{{*/
getcardvalue:
	push	h	;preserve cardptr

	lxi	d,cardface	;A=cardptr->face
	dad	d
	mov	a,m

	cpi	face$ace	! jz	cardvalue$ace
	
	cpi	face$king	! jz	cardvalue$face
	cpi	face$queen	! jz	cardvalue$face
	cpi	face$jack	! jz	cardvalue$face

	cpi	face$ten	! jz	cardvalue$number
	cpi	face$nine	! jz	cardvalue$number
	cpi	face$eight	! jz	cardvalue$number
	cpi	face$seven	! jz	cardvalue$number
	cpi	face$six	! jz	cardvalue$number
	cpi	face$five	! jz	cardvalue$number
	cpi	face$four	! jz	cardvalue$number
	cpi	face$three	! jz	cardvalue$number
	cpi	face$two	! jz	cardvalue$number

	jmp	cardvalue$unknown
cardvalue$number:
	adi	2-face$two
	mov	b,a
	jmp	cardvalue$exit
cardvalue$ace:
	mvi	b,11
	mvi	a,1
	jmp	cardvalue$exit
cardvalue$face:
	mvi	a,10	
	mov	b,a
	jmp	cardvalue$exit
cardvalue$unknown:
	xra	a
	mov	b,a
	jmp	cardvalue$exit

cardvalue$exit:
	pop	h	;restore cardptr
	ret
;/*}}}*/

;procedure drawwelcome (void): tsalt
;draws welcome page and defines tsalt
;side effects: (gdraw) hl de cursor tsalt
;/*{{{*/
drawwelcome:

	lxi	h,dram		;hl=dram addr
	shld	cursor		;place cursor at start of drom (topleft)

	lxi	h,gmwelcome	;hl=welcome page texture
	call	gdraw		;draw texture to screen

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


;procedure drawtestpage (void): 
;draw page for testing
;side effects: Assume ALL
;calls: initdeck initplayer initdealer newgame
;/*{{{*/
drawtestpage:
	call	initdeck	;initialize the deck (unshuffled)
	call	initplayer	;initialize the player object
	call	initdealer	;initialize the dealer
	call	newgame
	ret
;/*}}}*/


;procedure newgame (void): void
;starts a new game, clearing variables and initiating the loop
;side effects: Assume ALL
;/*{{{*/
newgame:
	;prepare variables
	call	resetdealer
	call	resetplayer

	;prepare the screen
	call	clearscr	;clear the screen
	call	drawborder	;draw the table decorations

	;draw the deck
	lhld	deck$cursor		;set cursor
	shld	cursor
	call	drawcard$facedown	;draw facedown card

	;game begine
	call	dealer$hiddenhit
	call	dealer$hit

	call	player$hit		;player draws 2 cards
	call	player$hit

	lhld	player+playerhandi
	call	check$21
	cpi	true
	jz	player$bj

game$loop:
player$turn:
	call	readkey

	cpi	'q'		;if (key == 'q') break
	jz	game$done
	
	cpi	'h'
	jz	game$hit

	cpi	'd'
	jz	game$double

	cpi	's'
	jz	game$stand
	
	jmp	game$loop	;and wait for next key

game$hit:
	call	player$hit	;player hits

	lhld	player+playerhandi
	call	check$21
	cpi	true
	jz	player$done

	lhld	player+playerhandi
	call	check$bust
	cpi	true
	jz	player$bust

	jmp	game$loop	;and returns to prompt

game$double:
	lhld	player$cursor
	mov	a,h
	adi	doublecardx
	mov	h,a
	mov	a,l
	adi	doublecardy
	mov	l,a
	shld	player$cursor

	call	player$hit	;player hits
	
	lhld	player+playerhandi
	call	check$bust
	cpi	true
	jz	player$bust

	jmp	player$done	;and their turn is over

game$stand:
	;fallthrough
player$done:
	;fall through
dealer$turn:
	lhld	dealer+dealerm		;draw first card faceup
	call	dealer$drawcard
	
	lxi	h,dealer
	call	check$21
	cpi	true
	jz	dealer$bj
	
dealer$loop:
	lda	dealer+dealerhard
	cpi	17
	jnc	dealer$done

	call	dealer$hit		;dealer hits 2 times

	lxi	h,dealer
	call	check$21
	cpi	true
	jz	dealer$done
	
	lxi	h,dealer
	call	check$bust
	cpi	true
	jz	dealer$bust

	jmp	dealer$loop		;check for bust of bj

dealer$done:

game$results:
	;B = player->handi->state
	lhld	player+playerhandi
	lxi	d,handstate
	dad	d
	mov	b,m

	;C = dealer->state
	lxi	h,dealer+dealerstate
	mov	c,m

	;if player and dealer hit blackjack its a tie
	mov	a,b
	ora	c
	cpi	hs$blackjack
	jz	game$tie
	
	;else if only player hit blackjack they winbonus
	mov	a,b
	cpi	hs$blackjack
	jz	game$blackjack

	;else if only dealer hit blackjack player loses
	mov	a,c
	cpi	hs$blackjack
	jz	game$lose

	;else if player bust == player lose
	mov	a,b
	cpi	hs$bust
	jz	game$lose

	;else if dealer bust == player win 
	mov	a,c
	cpi	hs$bust
	jz	game$win

	;else if player has higher total, win
	lhld	player+playerhandi
	call	gethandtotal
	mov	b,a

	push	b
	lxi	h,dealer
	call	gethandtotal
	pop	b

	cmp	b
	jz	game$tie
	jc	game$win

	;else lose
	jmp	game$lose

game$tie:
	lxi	h,dram+gamemsgoff
	shld	cursor
	
	lxi	d,mtie
	call	gprint

	jmp	gameover

game$win:
	lxi	h,dram+gamemsgoff
	shld	cursor
	
	lxi	d,mwin
	call	gprint

	jmp	gameover

game$blackjack:
	lxi	h,dram+gamemsgoff
	shld	cursor
	
	lxi	d,mblackjack
	call	gprint

	jmp	gameover

game$lose:
	lxi	h,dram+gamemsgoff
	shld	cursor
	
	lxi	d,mlose
	call	gprint

	jmp	gameover

gameover:
	call	readkey			;read a key
	jmp	newgame

game$done:
	ret

ret$false:
	mvi	a,false
	ret
ret$true:
	mvi	a,true
	ret

check$21$hand:	ds	ptrsize
check$21:
	shld	check$21$hand	

	lhld	check$21$hand
	lxi	d,handsoft
	dad	d
	mov	a,m
	cpi	21
	jz	ret$true

	lhld	check$21$hand
	lxi	d,handhard
	dad	d
	mov	a,m
	cpi	21
	jz	ret$true

	jmp	ret$false

check$bust:
	lxi	d,handhard
	dad	d
	mov	a,m
	cpi	21
	jnc	ret$true
	jmp	ret$false


mbust:		db	'Bust!',0
mblackjack:	db	'Blackjack!!',0
mwin:		db	'Win!',0
mlose:		db	'Lose',0
mtie:		db	'Tie',0

player$bj:
	lxi	h,dram+playermsgoff
	shld	cursor

	lxi	d,mblackjack
	call	gprint

	lhld	player+playerhandi	;player->handi->state = blackjack
	lxi	d,handstate
	dad	d
	mvi	a,hs$blackjack
	mov	m,a

	jmp	player$done

player$bust:
	lxi	h,dram+playermsgoff
	shld	cursor
	
	lxi	d,mbust
	call	gprint

	lhld	player+playerhandi	;player->handi->state = bust
	lxi	d,handstate
	dad	d
	mvi	a,hs$bust
	mov	m,a

	jmp	player$done


dealer$bj:
	lxi	h,dram+dealermsgoff
	shld	cursor

	lxi	d,mblackjack
	call	gprint

	mvi	a,hs$blackjack
	sta	dealer+dealerstate
	
	jmp	dealer$done

dealer$bust:
	lxi	h,dram+dealermsgoff
	shld	cursor

	lxi	d,mbust
	call	gprint

	mvi	a,hs$bust
	sta	dealer+dealerstate

	jmp	dealer$done


ght$hand:	ds	ptrsize
gethandtotal:
	shld	ght$hand
	lxi	d,handsoft
	dad	d
	mov	a,m
	cpi	21
	rnc
	lhld	ght$hand
	lxi	d,handhard
	dad	d
	mov	a,m
	ret
;/*}}}*/


;procedure player$hit (void): [player]
;trys to draws a card for the player, then displays it
;side effects: AF DE HL [cursor] [player$cursor] [player$cursorreset] [deck]
;/*{{{*/
player$hit:
	lhld	player$cursor		;set cursor position
	shld	cursor

	lhld	player+playerhandi	;DE=player->handiter
	xchg
	call	deckdraw		;HL=*newcard
	call	handaddcard		;add newcard (HL) to hand (DE)
	cpi	false			;if (card failed to add)
	jz	deckudraw		;  undo the deckdraw call + return

	call	drawcard$faceup		;draw card faceup

	call	getcardvalue		;A=card value
	lhld	player+playerhandi	;update hand totals
	call	updatetotals

	lxi	d,player$cursorreset	;DE=&player$cursorreset
	lxi	h,player$cursor		;hl=&player$cursor
	call	movenextcard		;update DE and HL to new positions
	
	ret

;/*}}}*/

updatetotals:
	push	h		;preserve hand
	lxi	d,handhard	;hand->hard += hardvalue
	dad	d
	add	m
	mov	m,a

	pop	h		;restore hand
	push	h		;preserve hand
	lxi	d,handsoft	;hand->soft += softvalue
	dad	d
	mov	a,b
	add	m
	mov	m,a

	pop	h		;restore hand
	ret			;return

;procedure dealer$hiddenhit (void): [dealer]
;trys to draws a card for the dealer, then displays face down
;side effects: AF DE HL [cursor]
;/*{{{*/
dealer$hiddenhit:
	lxi	d,dealer		;dealer draws 1 card
	call	deckdraw
	call	handaddcard
	cpi	false			;if (card failed to add)
	jz	deckudraw		;  undo the deckdraw call + return

	lhld	dealer$cursor		;store dealer$cursor
	push	h
	lhld	dealer$cursorreset	;store dealer$cursorreset
	push	h

	lxi	h,dealer$cursor
	lxi	d,dealer$cursorreset
	call	movenextcard

	lhld	dealer$cursor
	shld	cursor
	call	drawcard$facedown	;draw card faceup

	pop	h			;restore dealer$cursorreset
	shld	dealer$cursorreset
	pop	h			;restore dealer$cursor
	shld	dealer$cursor
	
	ret
;/*}}}*/


;procedure dealer$hit (void): [dealer]
;trys to draws a card for the dealer, then displays it
;side effects: AF DE HL [cursor]
;/*{{{*/
dealer$hit:
	lxi	d,dealer		;dealer draws 1 card
	call	deckdraw
	call	handaddcard
	cpi	false			;if (card failed to add)
	jz	deckudraw		;  undo the deckdraw call + return

dealer$drawcard:
	push	h			;store card temp
	lhld	dealer$cursor		;set cursor position
	shld	cursor
	pop	h			;retore card
	call	drawcard$faceup		;draw card faceup
	
	call	getcardvalue		;A=card value
	lxi	h,dealer		;update hand totals
	call	updatetotals
	
	lxi	d,dealer$cursorreset	;DE=&dealer$cursorreset
	lxi	h,dealer$cursor		;hl=&dealer$cursor
	call	movenextcard		;update DE and HL to new positions
	
	ret
;/*}}}*/


;procedure movenextcard (DE=resetptr, HL=currentptr): void
;moves cursor and cursorreset for next card
;side effects: assume ALL
;/*{{{*/
mnc$cursor:	DS	ptrsize
mnc$reset:	DS	ptrsize
movenextcard:
	shld	mnc$cursor
	xchg
	shld	mnc$reset
	
	lhld	mnc$cursor
	call	derefget
	lxi	d,dram
	mov	a,d
	adi	maxwidth-nextcardx-cardwidth-1
	cmp	h
	jc	movenext$overflow

	xchg			;DE=*mnc$cursor
	mov	a,d		;mnc$cursor->x + nextcardx
	adi	nextcardx
	mov	d,a
	mov	a,e		;mnc$cursor->y + nextcardy
	adi	nextcardy
	mov	e,a

	lhld	mnc$cursor	;*mnc$cursor = newcurosr
	call	derefset

	ret

movenext$overflow:
	lhld	mnc$reset
	call	derefget

	mov	a,h
	xri	overflowcardx	
	mov	h,a

	mov	a,l
	adi	overflowcardy
	mov	l,a
	shld	cursor

	xchg			;DE=newcursor
	lhld	mnc$cursor	;*mnc$cursor = newcursor
	call	derefset

	lhld	mnc$reset	;*mnc$reset = newcursor
	call	derefset

	ret
;/*}}}*/

;/*}}}*/


;helper procs
;/*{{{*/

;procedure handaddcard (DE=handptr, HL=cardptr): A=boolerr
;add a card to a hand, if it cannot be done, return false, otherwise true
;side effects: assume all
;/*{{{*/
hac$phand:	DS	ptrsize
hac$pcard:	DS	ptrsize
handaddcard:
	;DE=hand
	;HL=cardptr
	shld	hac$pcard	;store card ptr
	xchg			;store hand ptr
	shld	hac$phand

	lhld	hac$phand	;HL=handptr->count
	lxi	d,handcount
	dad	d
	mov	A,M		;A=count

	cpi	maxhandcards	;if (handptr->count >= maxhandcards)
	jnc	hac$panic	;  panic, cannot add card, hand full

	push	H		;store &handptr->count

	lhld	hac$phand	;HL=handptr->m[handptr->count]
	lxi	d,handm
	dad	d
	xchg
	mov	l,a		;A is still loaded from precheck
	mvi	h,0
	dad	h
	dad	d

	xchg			;DE=handiter	
	lhld	hac$pcard	;HL=cardptr
	xchg			;HL=handiter  DE=cardptr
	
	mov	m,e		;store loworder byte (little endian)
	inx	h
	mov	m,d		;store highorder byte
	inx	h

	pop	H		;HL=&handptr->count
	inr	m		;handptr->count++

hac$normal:
	mvi	a,true		;return success
hac$exit:
	lhld	hac$phand	;restore hand pointer to DE
	xchg
	lhld	hac$pcard
	ret
hac$panic:
	mvi	a,false		;return failure
	jmp	hac$exit
;/*}}}*/


;procedure deckshuffle ([deck] [deck$top]): [deck] [deck$index]
;TODO: implement shuffle procedure
;side effects: to be defined
;/*{{{*/
deckshuffle:
	;idk howwww
	ret
;/*}}}*/


;procedure initplayer (void): [player]
;initialize the player with default values

;procedure resetplayer (void): [player]
;reset player values to default
;side effects: [player]
;/*{{{*/
initplayer:
	;player.score = 1000
	lxi	h,1000
	shld	player+playerscore

	;player.bet = 0
	lxi	h,0
	shld	player+playerbet

	;player.name = 0
	xra	a
	sta	player+playername

resetplayer:
	lxi	h,player+playerhandarr+(0*handsize)
	shld	player+playerhandi

	lxi	h,dram+playerhandoff
	shld	player$cursor
	shld	player$cursorreset

	;player.handarr[0].count = 0
	;player.handarr[0].soft = 0
	;player.handarr[0].hard = 0
	xra	a
	sta	player+playerhandarr+(0*handsize)+handcount
	sta	player+playerhandarr+(0*handsize)+handsoft
	sta	player+playerhandarr+(0*handsize)+handhard

	;player.handcount = 1
	inr	a
	sta	player+playerhcount

	;player.handarr[0].state = normal
	mvi	a,hs$normal
	sta	player+playerhandarr+(0*handsize)+handstate
	

	ret
;/*}}}*/


;procedure initdealer (void): [dealer]
;procedure resetdealer (void): [dealer]
;initialize the dealer with default values
;side effects: [dealer]
;/*{{{*/
resetdealer:
initdealer:
	lxi	h,dram+dealerhandoff
	shld	dealer$cursor
	shld	dealer$cursorreset

	xra	a
	sta	dealer+dealercount
	sta	dealer+dealersoft
	sta	dealer+dealerhard

	mvi	a,hs$normal
	sta	dealer+dealerstate
	ret

;/*}}}*/


;procedure initdeck (void): [deck] HL=[deck$index]
;initialize the deck (unshuffled)
;side effects: AF DE HL [deck] [deck$index] [deck$cursor] [ideck$facei] \
;              [ideck$signi] [ideck$decki]
;calls: getdeckindex
;/*{{{*/
ideck$facei:	ds	1
ideck$signi:	ds	1
ideck$decki:	ds	1

initdeck:
	lxi	h,0
	shld	deck$index

	lxi	h,dram+deckoffset
	shld	deck$cursor

;deckloop$init:
	mvi	a,numofdecks
	sta	ideck$decki
deckloop$loop:
;signloop$init:
	mvi	a,sign$max
	sta	ideck$signi
signloop$loop:
;faceloop$init:
	mvi	a,face$max
	sta	ideck$facei
faceloop$loop:
	call	getdeckindex		;HL=&deck[index]

	push	h
	lxi	d,cardface
	dad	d
	lda	ideck$facei		;A=face
	mov	m,a			;deck_iter->face = face
	pop	h

	push	h
	lxi	d,cardsign
	dad	d
	lda	ideck$signi		;A=sign
	mov	m,a			;deck_iter->sign = sign
	pop	h

	lhld	deck$index
	inx	h
	shld	deck$index

	lxi	h,ideck$facei
	mov	a,m
	cpi	0
	jz	faceloop$done
	dcr	m
	jmp	faceloop$loop
faceloop$done:
signloop$continue:
	lxi	h,ideck$signi
	mov	a,m
	cpi	0
	jz	signloop$done
	dcr	m
	jmp	signloop$loop
signloop$done:
deckloop$continue:
	lxi	h,ideck$decki
	dcr	m
	mov	a,m
	cpi	0
	jz	deckloop$done
	jmp	deckloop$loop
deckloop$done:
	lhld	deck$index
	dcx	h
	shld	deck$index
	shld	deck$top
	ret
;/*}}}*/


;procedure getdeckindex ([deck], [deck$index]): HL=deck[deck$index]
;get the current index of the deck
;side effects: AF DE
;/*{{{*/
getdeckindex:
	;hl=deck[deck$index]
	lhld	deck$index		;hl=index
	dad	h			;hl=index * 2<sizeof card>
	lxi	d,deck			;de=&deck
	dad	d			;hl=&deck[index]
	ret
;/*}}}*/


;procedure deckdraw ([deck]): HL=card
;return a ptr to the top card on the deck
;side effects: assume all
;/*{{{*/
deckdraw:
	push	d
	call	getdeckindex	;get to card of deck
	push	h		;store the card_ptr

	;reshuffle when deck empty
	xra	a		;A=0
	lhld	deck$index	;HL=index
	cmp	h		;if (index == 0) shuffle
	jnz	deckdraw$continue
	cmp	l
	jnz	deckdraw$continue
	call	deckshuffle
deckdraw$continue:	
	lhld	deck$index	;pop card off deck
	dcx	h
	shld	deck$index

	pop	h
	pop	d
	ret
;/*}}}*/


;procedure deckudraw ([deck$iter]): void
;moved deck$iter back 1 card in the case of a failure
;side effects: assume all
;/*{{{*/
deckudraw:
	lhld	deck$index	;place card back on the deck
	inx	h
	shld	deck$index
	ret
;/*}}}*/


;procedure drawcard$cardfaceup (HL=cardptr [cursor]): <display>
;draw a card to the display in face up position
;side effects: assume all
;/*{{{*/
drawcard$cardptr:	DS	ptrsize
drawcard$faceup:
	shld	drawcard$cardptr
	lhld	cursor		;store the cardpos
	push	H

	lhld	drawcard$cardptr	
	call	drawcardb	;draw border
	call	drawflipcard	;draw card details

	pop	H		;cursor=card position
	shld	cursor
	lhld	drawcard$cardptr
	ret
;/*}}}*/


;procedure drawcard$cardfacedown ([cursor]): <display>
;draw a card to the display in face down position
;side effects: assume all
;/*{{{*/
drawcard$facedown:
	push	H
	lhld	cursor		;store the cardpos
	push	H

	call	drawcardb	;draw border
	call	drawback	;draw back of card

	pop	H
	shld	cursor
	pop	H
	ret
;/*}}}*/


;procedure drawcardb ([cursor]): <display>
;draws the card base
;side effects: AF B DE [dram] [border$left] [border$mleft] [border$right] \
;              [border$mright]
;calls: grectf, grect
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
	push	H
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

	pop	h
	ret
;/*}}}*/


;procedure drawback ([cursor]): <display>
;draws the back of a card
;side effects: AF B DE [dram] [back$i] [back$j] [back$k] [back$l]
;calls: rectf, curright, curdown, curleft, curright
;/*{{{*/
back$offset	equ	0208h
back$data	equ	1010$1010b
back$mask	equ	0000$0000b

back$i:			ds	1
back$j:			ds	1
back$k:			ds	1
back$l:			ds	1
drawback:
	push	h
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
	pop	h
	shld	cursor
	
	pop	h

	ret
;/*}}}*/


;procedure drawsign (HL=cardptr, [gmsignarr], [cursor]): <display>
;draws the card's sign/sign at cursor position
;side effects: AF BC DE [dram]
;calls: gdraw
;/*{{{*/
drawsign:
	push	h
	lxi	d,cardsign
	dad	d
	mov	l,m
	mvi	h,0
	dad	h
	lxi	d,gmsignarr
	dad	d
	mov	e,m
	inx	h
	mov	d,m
	xchg
	call	gdraw
	pop	h
	ret
;/*}}}*/


;procedure drawface (HL=cardptr, [gmcardfont], [cursor]): <display>
;draws the card's face/number at cursor position
;side effects: AF BC DE [dram]
;calls: gdraw
;/*{{{*/
drawface:
	push	h
	lxi	d,cardface
	dad	d
	mov	l,m
	mvi	h,0
	dad	h
	lxi	d,gmcardfont
	dad	d
	mov	e,m
	inx	h
	mov	d,m
	xchg
	call	gdraw
	pop	h
	ret
;/*}}}*/


;procedure drawflipcard (HL=cardptr, [cursor]): <display> 
;draws a flipped card at cursor
;side effects: AF [dram] [deckptr] [cardpos]
;calls: curright, curdown, curleft, drawsign
;/*{{{*/
cardsigntl$x	equ	1
cardsigntl$y	equ	6
cardsignbr$x	equ	cardwidth-2-1
cardsignbr$y	equ	cardheight-8-6
deckptr:	DS	2
cardpos:	DS	2
drawflipcard:
	push	h
	shld	deckptr		;store deckptr
	lhld	cursor		;store card screen position
	shld	cardpos

	lhld	cardpos		;set curosr
	shld	cursor
	mvi	a,001h		;move to relative position
	call	curright
	mvi	a,006h
	call	curdown
	lhld	deckptr		;get card pointer
	call	drawface	;draw top left card face
	
	mvi	a,gmfaceheight+3
	call	curdown
	lhld	deckptr		;get card pointer
	call	drawsign	;draw bottom right card sign

	lhld	cardpos		;restore the cursor
	shld	cursor
	pop	h
	ret

;/*}}}*/


;procedure drawborder ([cursor]): <display>
;draw the board decorations
;side effects: AF DE HL [dram] [border$left] [border$mleft] [border$right] \
;              [border$mright]
;calls: grect
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


;procedure clearscr (void): <display>
;clears the screen
;side effects: AF B DE HL [dram] [cursor] 
;calls: grectf
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


;/*}}}*/


;graphic procs
;/*{{{*/
;procedure curright (a=nbytes): HL=[cursor]
;move the cursor to the right n bytes
;side effects: AF HL [cursor]
;/*{{{*/
curright:
	lhld	cursor
	add	h
	mov	h,a
	shld	cursor
	ret
;/*}}}*/


;procedure curleft (a=nbytes): HL=[cursor]
;move the cursor to the left n bytes
;side effects: AF B HL [cursor]
;/*{{{*/
curleft:
	lhld	cursor
	mov	b,a
	mov	a,h
	sub	b
	mov	h,a
	shld	cursor
	ret
;/*}}}*/


;procedure curdown (a=nbytes): HL=[cursor]
;move the cursor to the down n bytes
;side effects: AF HL [cursor]
;/*{{{*/
curdown:
	lhld	cursor
	add	l
	mov	l,a
	shld	cursor
	ret
;/*}}}*/


;procedure curup (a=nbytes): HL=[cursor]
;move the cursor to the up n bytes
;side effects: AF B HL [cursor]
;/*{{{*/
curup:
	lhld	cursor
	mov	b,a
	mov	a,l
	sub	b
	mov	l,a
	shld	cursor
	ret
;/*}}}*/


;procedure gdraw (hl=texture *s, [cursor]): <display> 
;draws a texture to the screen at cursor
;side effects: unknown
;/*{{{*/
gdraw$self:	ds	ptrsize
gdraw$iter:	ds	ptrsize
gdraw$endcur:	ds	offsetsize
gdraw:	;hl=texture *self_ptr
	shld	gdraw$self

	lhld	cursor		;HL=cursor
	push	h		;preserver cursor

	lhld	gdraw$self	;iter = self_ptr->data
	lxi	d,texturedata
	dad	d
	shld	gdraw$iter

	lhld	gdraw$self	;B = self_ptr->width
	lxi	d,texturewidth
	dad	d
	mov	b,m

	lhld	gdraw$self	;c = self_ptr->height
	lxi	d,textureheight
	dad	d
	mov	c,m

	lhld	cursor		;HL=cursor
	mov	a,h		;H = cursor.x + self_ptr->width
	add	b
	mov	h,a
	mov	a,l		;L = cursor.y + self_ptr->height
	add	c
	mov	l,a
	shld	gdraw$endcur	;end_cursor = HL

	lhld	cursor		;HL=cursor
	mov	c,h		;C=cursor.x	never modified

	;fall through
gdraw$loop:
	lhld	gdraw$endcur	;DE=end_cursor
	xchg
	lhld	cursor		;HL=cursor

	mov	a,d		;if (cursor.x == end_cursor.x) move$nextrow()
	cmp	h
	cz	gdraw$nextrow

	mov	a,e		;if (cursor.y == end_cursor.y) done
	cmp	l
	jz	gdraw$done
	;fall through
gdraw$continue:	
	lhld	gdraw$iter	;A=databyte
	mov	a,m
	lhld	cursor		;HL=cursor
	mov	m,a		;*cursor = databyte

	lhld	gdraw$iter	;iter++
	inx	h
	shld	gdraw$iter

	call	gdraw$nextcol	;iterate to next column

	jmp	gdraw$loop
	
gdraw$nextcol:	
	lxi	h,cursor+offsetx
	inr	m
	ret

gdraw$nextrow:	
	lhld	cursor		;HL = cursor
	inr	l		;cursor.y++
	mov	h,c		;cursor.x = initial_cursor_x
	shld	cursor
	ret

gdraw$done:
	pop	h		;restore cursor
	shld	cursor
	ret
;/*}}}*/


;procedure gputchar (A=char, B=linestart.x): <display> [cursor]
;draws character (A) to screen at cursor
;side effects: yes
;/*{{{*/
gputchar:
	cpi	CR	! jz	gputc$cr
	cpi	LF	! jz	gputc$lf 
	jmp	gputc$graphic

gputc$cr:
	lxi	h,cursor+offsetx
	mov	m,b
	ret

gputc$lf:
	lxi	h,cursor+offsety
	mov	a,m
	adi	gmfont$linefeed
	mov	m,a
	ret
	
gputc$graphic:
	sui	gmfontoffset	;HL=(u16)((char - fontoffset) * 2)
	mvi	h,0
	mov	l,a
	dad	h
	lxi	d,gmfont	;DE=fontptr
	dad	d		;HL=fontptr+texture_offset
	call	derefget
	push	h
	call	gdraw		;print the character

	pop	h
	lxi	d,texturewidth	;B=char_texture->width
	dad	d
	mov	a,m

	lhld	cursor		;HL=cursor
	add	h		;cursor.x += char_texture->width
	mov	h,a
	shld	cursor
	ret
;/*}}}*/


;procedure gprint (de=srcstr, [cursor]): <display>
;prints a message in ascii at cursor
;side effects: idk man, im tired
;/*{{{*/
gprint:
	lxi	h,cursor+offsetx	;restore x addr for newline
	mov	b,m

	xchg			;iter in HL

	;fall through
gprint$loop:
	mov	a,m		;A=*iter
	cpi	0		;if (char == NULL) done
	jz	gprint$done
	inx	h

	push	H		;preserve iter
	push	B		;preserve linestart.x
	call	gputchar	;print the character
	pop	B
	pop	h

	jmp	gprint$loop	;loop

gprint$done:
	ret
;/*}}}*/


;procedure grectf (a=fillbyte, b=bitmask, de=offset_a, hl=offset_b, \
;                  [cursor]): <display>
;draws a filled rectange from offset_a (top left) to offset_b (bottom right)
;side effects: AF B DE HL [dram] [grectf$data] [grectf$mask] [grectf$start] \
;              [grectf$end] [grectf$i]
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


;procedure grect (de=offset_a, hl=offset_b, [cursor]): <display>
;draw a rectange from offset_a (top left) to offset_b (bottom right)
;side effects: AF B DE HL [dram] [grect$start] [grect$end] [grect$i]
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


;procedure gline (a=length, b=fillbyte, c=bitmask, de=offsetelem, hl=offset, \
;                [cursor]): <display>
;draw a line
;side effects: AF B DE HL [dram] [gline$data] [gline$mask] [gline$offset] \
;              [gline$offelem] [gline$length]
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


;/*}}}*/


;system procs
;/*{{{*/

;procedure derefget (HL=ptr): HL=*ptr
;dereferences a ptr in HL into a 16bit value
;side effects: F HL
;/*{{{*/
derefget:
	push	d

	mov	e,m
	inx	h
	mov	d,m
	xchg

	pop	d
	ret
;/*}}}*/


;procedure derefget (DE=value HL=ptr): [ptr]
;dereferences a ptr HL loading a 16bit value into it
;side effects: F
;/*{{{*/
derefset:
	push	h
	push	d
	
	mov	m,e
	inx	h
	mov	m,d

	pop	d
	pop	h
	ret
;/*}}}*/


;procedure command (c=command): (statreg1) (statreg2)
;send a command to the control register, and wait for the acknowledgement bit
;to compliment.
;side effects: AF (ctrlreg) (statreg1) (statreg2)
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


;procedure kbmi$enable (void): void
;enable keyboard maskable interupts flag
;side effects: AF C (ctrlreg) (statreg1) (statreg2)
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


;procedure readkey (void): A=keycode
;await 1 key of user input
;side effects: AF BC (ctrlreg) (statreg1) (statreg2)
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


;procedure resetscroll (void): <display>
;set scroll value to 0 lines.
;side effects: A (scrollreg) <display>
;/*{{{*/
resetscroll:
	mvi	a,0		;a=scroll value
	out	scrollreg	;output to the start scan register
	ret
;/*}}}*/


;procedure loaddram (void): [08000..0ffffh]
;load the display ram pages into memory at 08000 and 0C000.
;side effects: A (vrampage2) (vrampage3) [08000..0ffffh]
;/*{{{*/
loaddram:
	mvi	a,drampage0	;load dram page 0
	out	vrampage2	;into vram page 2 (08000h-0bfffh)
	mvi	a,drampage1	;load dram page 1
	out	vrampage3	;load vram page 3 (0c000h-0ffffh)
	ret
;/*}}}*/


;procedure loadmainram (void): [08000..0ffffh]
;load the upper main ram pages into memory at 08000 and 0C000.
;side effects: A (vrampage2) (vrampage3) [08000..0ffffh]
;/*{{{*/
loadmainram:
	mvi	a,mainpage2	;load main page 2
	out	vrampage2	;into vram page 2 (08000h-0c000h)
	mvi	a,mainpage3	;load main page 3
	out	vrampage3	;into vram page 3 (0c000h-0ffffh)
	ret
;/*}}}*


;/*}}}*/


;variable data section
;deck allocation
deck$cursor	ds	ptrsize
numofdecks	equ	6	;number of decks
singledecklen	equ	52	;number of cards in 1 deck
decklen		equ	(singledecklen * numofdecks)
deck:		ds	decklen * cardsize
deck$index:	dw	0 
deck$top:	dw	0 


;player data section
player$cursor		ds	ptrsize
player$cursorreset	ds	ptrsize
player:			ds	playersize

;dealer data section
dealer$cursor		ds	ptrsize
dealer$cursorreset	ds	ptrsize
dealer:			ds	dealersize


;welcome screen graphics
;/*{{{*/
;struct texture gmwelcome
gmwelcome:
	;	w,  h
	db	2,  2
	;	data
	db	1010$1010b, 0101$0101b
	db	1111$0000b, 1111$0000b

;/*}}}*/

;card sign graphics
;/*{{{*/
;struct texture *gmsignarr[]
gmsignarr:
	dw	gmspade
	dw	gmheart
	dw	gmclover
	dw	gmdiamond

;struct texture gmheart, gmdiamond, gmclover, gmspade
gmheart:
	;	w, h
	db	2, 8
	;	pixel data
	db	0011$1110B, 0111$1100B
	db	0111$1111B, 1111$1110B
	db	1111$1111B, 1111$1111B
	db	1111$1111B, 1111$1111B
	db	0111$1111B, 1111$1110B
	db	0011$1111B, 1111$1100B
	db	0000$1111B, 1111$0000B
	db	0000$0011B, 1100$0000B

gmdiamond:
	db	2, 8
	db	0000$0001b, 1000$0000b
	db	0000$0111b, 1110$0000b
	db	0001$1111b, 1111$1000b
	db	0011$1111b, 1111$1100b
	db	0011$1111b, 1111$1100b
	db	0001$1111b, 1111$1000b
	db	0000$0111b, 1110$0000b
	db	0000$0001b, 1000$0000b

gmspade:
	db	2, 8
	db	0000$0011b, 1100$0000b
	db	0000$1111b, 1111$0000b
	db	0011$1111b, 1111$1100b
	db	0111$1111b, 1111$1110b
	db	1111$1111b, 1111$1111b
	db	0111$1111b, 1111$1110b
	db	0011$1001b, 1001$1100b
	db	0000$0011b, 1100$0000b

gmclover:
	db	2, 8
	db	0000$0011b, 1100$0000b
	db	0000$0111b, 1110$0000b
	db	0000$0111b, 1110$0000b
	db	0111$1011b, 1101$1110b
	db	1111$1111b, 1111$1111b
	db	1111$1101b, 1011$1111b
	db	0111$1001b, 1001$1110b
	db	0000$0011b, 1100$0000b
;/*}}}*/

;card face graphics
;/*{{{*/
;struct texture *gmsignarr[]
gmcardfont:
	dw	gmcardfont$two
	dw	gmcardfont$three
	dw	gmcardfont$four
	dw	gmcardfont$five
	dw	gmcardfont$six
	dw	gmcardfont$seven
	dw	gmcardfont$eight
	dw	gmcardfont$nine
	dw	gmcardfont$ten
	dw	gmcardfont$jack
	dw	gmcardfont$queen
	dw	gmcardfont$king
	dw	gmcardfont$ace

gmfacewidth	equ	2
gmfaceheight	equ	8

;struct texture gmcardfont$two, gmcardfont$three, ...
gmcardfont$two:
	;	w, h
	db	2, 8
	;	pixel data
	db	0000$0111B, 1110$0000B
	db	0000$1000B, 0001$0000B
	db	0001$0000B, 0000$1000B
	db	0000$0000B, 0001$0000B
	db	0000$0000B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0110B, 0000$0000B
	db	0001$1111B, 1111$1000B

gmcardfont$three:
	db	2, 8
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0001$0000B
	db	0000$0000B, 0110$0000B
	db	0000$0001B, 1100$0000B
	db	0000$0000B, 0010$0000B
	db	0000$0000B, 0001$0000B
	db	0001$0000B, 0010$0000B
	db	0000$1111B, 1100$0000B

gmcardfont$four:
	db	2, 8
	db	0000$0000B, 0110$0000B
	db	0000$0000B, 1010$0000B
	db	0000$0001B, 0010$0000B
	db	0000$0010B, 0010$0000B
	db	0000$0100B, 0010$0000B
	db	0000$1111B, 1111$1000B
	db	0000$0000B, 0010$0000B
	db	0000$0000B, 0010$0000B

gmcardfont$five:
	db	2, 8
	db	0001$1111B, 1111$1000B
	db	0001$0000B, 0000$0000B
	db	0001$0000B, 0000$0000B
	db	0000$1111B, 1110$0000B
	db	0000$0000B, 0001$0000B
	db	0000$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0000$1111B, 1111$0000B

gmcardfont$six:
	db	2, 8
	db	0000$1111B, 1111$0000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$0000B
	db	0001$0111B, 1111$0000B
	db	0001$1000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0000$1111B, 1111$0000B

gmcardfont$seven:
	db	2, 8
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0001$0000B
	db	0000$0000B, 0010$0000B
	db	0000$0111B, 1111$1000B
	db	0000$0000B, 1000$0000B
	db	0000$0001B, 0000$0000B
	db	0000$0010B, 0000$0000B
	db	0000$0100B, 0000$0000B

gmcardfont$eight:
	db	2, 8
	db	0000$0111B, 1110$0000B
	db	0000$1000B, 0001$0000B
	db	0000$1000B, 0001$0000B
	db	0000$0111B, 1110$0000B
	db	0000$1000B, 0001$0000B
	db	0001$0000B, 0000$1000B
	db	0000$1000B, 0001$0000B
	db	0000$0111B, 1110$0000B

gmcardfont$nine:
	db	2, 8
	db	0000$1111B, 1111$0000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0001$1000B
	db	0000$1111B, 1110$1000B
	db	0000$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0000$1111B, 1111$0000B

gmcardfont$ten:
	db	2, 8
	db	1100$0000B, 1111$1110B
	db	0010$0001B, 0000$0101B
	db	0010$0001B, 0000$1001B
	db	0010$0001B, 0001$0001B
	db	0010$0001B, 0010$0001B
	db	0010$0001B, 0100$0001B
	db	0010$0001B, 1000$0001B
	db	1111$1000B, 1111$1110B

gmcardfont$ace:
	db	2, 8
	db	0000$0011B, 1100$0000B
	db	0000$0100B, 0010$0000B
	db	0000$1000B, 0001$0000B
	db	0000$1000B, 0001$0000B
	db	0001$1111B, 1111$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B

gmcardfont$king:
	db	2, 8
	db	0001$0000B, 0001$0000B
	db	0001$0000B, 0010$0000B
	db	0001$0000B, 0100$0000B
	db	0001$1001B, 1000$0000B
	db	0001$0110B, 0000$0000B
	db	0001$0001B, 1000$0000B
	db	0001$0000B, 0110$0000B
	db	0001$0000B, 0001$1000B

gmcardfont$queen:
	db	2, 8
	db	0000$0111B, 1110$0000B
	db	0000$1000B, 0001$0000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0100$1000B
	db	0000$1000B, 0011$0000B
	db	0000$0111B, 1111$0000B
	db	0000$0000B, 0000$1000B

gmcardfont$jack:
	db	2, 8
	db	0000$1111B, 1111$1000B
	db	0000$0000B, 0100$0000B
	db	0000$0000B, 0100$0000B
	db	0000$0000B, 0100$0000B
	db	0000$0000B, 0100$0000B
	db	0001$0000B, 0100$0000B
	db	0001$0000B, 0100$0000B
	db	0000$1111B, 1000$0000B
;/*}}}*/

;font graphics
;/*{{{*
;     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
;00  \0 -- -- -- -- -- -- -- -- -- LF -- -- CR -- --
;10  -- TB -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
;20  SP BG  "  #  $  %  &  '  (  )  *  +  ,  -  .  /
;30  0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
;40  @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O
;50  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
;60  `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o
;70  p  q  r  s  t  u  v  w  x  y  z  {  |  } ~  --

;LF  line feed
;CR  cartridge return
;TB  Tab
;Sp  Space
;BG  Bang

;struct pixel *gmsignarr[13]
;deref from offset to skip first 0x20 characters
gmfont$linefeed	equ	18
gmfontoffset	equ	(020h)
gmfont:
	;20-2f
	;SP BG  "  #  $  %  &  '  (  )  *  +  ,  -  .  /
	dw	gmfont$space,	gmfont$bang,	gmfont$dquote,	gmfont$hash
	dw	gmfont$dollar,	gmfont$percent,	gmfont$and,	gmfont$squote
	dw	gmfont$popen,	gmfont$pclose,	gmfont$star,	gmfont$plus
	dw	gmfont$comma,	gmfont$hyphen,	gmfont$period,	gmfont$fslash

	;30-3f
	;0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
	dw	gmfont$zero,	gmfont$one,	gmfont$two,	gmfont$three
	dw	gmfont$four,	gmfont$five,	gmfont$six,	gmfont$seven
	dw	gmfont$eight,	gmfont$nine,	gmfont$colon,	gmfont$semi
	dw	gmfont$less,	gmfont$equal,	gmfont$more,	gmfont$question

	;40-4f
	;@  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O
	dw	gmfont$at,	gmfont$cap$a,	gmfont$cap$b,	gmfont$cap$c
	dw	gmfont$cap$d,	gmfont$cap$e,	gmfont$cap$f,	gmfont$cap$g
	dw	gmfont$cap$h,	gmfont$cap$i,	gmfont$cap$j,	gmfont$cap$k
	dw	gmfont$cap$l,	gmfont$cap$m,	gmfont$cap$n,	gmfont$cap$o

	;50-5f
	;P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
	dw	gmfont$cap$p,	gmfont$cap$q,	gmfont$cap$r,	gmfont$cap$s
	dw	gmfont$cap$t,	gmfont$cap$u,	gmfont$cap$v,	gmfont$cap$w
	dw	gmfont$cap$x,	gmfont$cap$y,	gmfont$cap$z,	gmfont$bopen
	dw	gmfont$fslash,	gmfont$bclose,	gmfont$carat,	gmfont$under

	;60-6f
	;`  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o
	dw	gmfont$tick,	gmfont$low$a,	gmfont$low$b,	gmfont$low$c
	dw	gmfont$low$d,	gmfont$low$e,	gmfont$low$f,	gmfont$low$g
	dw	gmfont$low$h,	gmfont$low$i,	gmfont$low$j,	gmfont$low$k
	dw	gmfont$low$l,	gmfont$low$m,	gmfont$low$n,	gmfont$low$o

	;70-7e 
	;p  q  r  s  t  u  v  w  x  y  z  {  |  } ~  --
	dw	gmfont$low$p,	gmfont$low$q,	gmfont$low$r,	gmfont$low$s
	dw	gmfont$low$t,	gmfont$low$u,	gmfont$low$v,	gmfont$low$w
	dw	gmfont$low$x,	gmfont$low$y,	gmfont$low$z,	gmfont$copen
	dw	gmfont$vline,	gmfont$cclose,	gmfont$squigle


gmfont$unknown:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$0000B, 0000$1000B
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B

gmfont$space:
;		w  h
	db	2, 12
;		data
	dw	0,0,0,0,0,0
	dw	0,0,0,0,0,0

gmfont$bang:
	db	1,12
	db	0000$0000B
	db	0000$0000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0000$0000B
	db	0001$1000B
	db	0000$0000B
	db	0000$0000B
gmfont$dquote:	equ	gmfont$unknown
gmfont$hash:	equ	gmfont$unknown
gmfont$dollar:	equ	gmfont$unknown
gmfont$percent:	equ	gmfont$unknown
gmfont$and:	equ	gmfont$unknown
gmfont$squote:
	db	1,12
	db	0000$0000B
	db	0000$1100B
	db	0000$1100B
	db	0011$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$popen:
	db	1,12
	db	0000$0000B
	db	0000$0110B
	db	0001$1000B
	db	0001$1000B
	db	0110$0000B
	db	0110$0000B
	db	0110$0000B
	db	0110$0000B
	db	0001$1000B
	db	0001$1000B
	db	0000$0110B
	db	0000$0000B
gmfont$pclose:
	db	1,12
	db	0000$0000B
	db	0110$0000B
	db	0001$1000B
	db	0001$1000B
	db	0000$0110B
	db	0000$0110B
	db	0000$0110B
	db	0000$0110B
	db	0001$1000B
	db	0001$1000B
	db	0110$0000B
	db	0000$0000B
gmfont$star:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0001B, 1000$0000B
	db	0001$1001B, 1001$1000B
	db	0000$0111B, 1110$0000B
	db	0001$1001B, 1001$1000B
	db	0000$0001B, 1000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$plus:	equ	gmfont$unknown
gmfont$comma:
	db	1,12
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$1100B
	db	0000$1100B
	db	0011$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$hyphen:	equ	gmfont$unknown
gmfont$period:
	db	1,12
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0001$1000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$fslash:	equ	gmfont$unknown
gmfont$zero:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$one:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0111B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$two:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0000$0000B, 0001$1000B
	db	0000$0000B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0110B, 0000$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$three:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0000B, 0110$0000B
	db	0000$0001B, 1110$0000B
	db	0000$0000B, 0001$1000B
	db	0000$0000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$four:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0110$0000B
	db	0001$1000B, 0110$0000B
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0110$0000B
	db	0000$0001B, 1111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$five:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0001$1000B, 0000$0000B
	db	0001$1000B, 0000$0000B
	db	0001$1111B, 1110$0000B
	db	0000$0000B, 0001$1000B
	db	0000$0000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$six:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0001B, 1110$0000B
	db	0000$0110B, 0000$0000B
	db	0001$1000B, 0000$0000B
	db	0001$1111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$seven:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0000B, 0001$1000B
	db	0000$0000B, 0110$0000B
	db	0000$0000B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$eight:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$nine:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1111$1000B
	db	0000$0000B, 0001$1000B
	db	0000$0000B, 0110$0000B
	db	0000$0111B, 1000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$colon:
	db	1,12
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0001$1000B
	db	0000$0000B
	db	0000$0000B
	db	0001$1000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$semi:
	db	1,12
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
	db	0000$1100B
	db	0000$0000B
	db	0000$0000B
	db	0000$1100B
	db	0000$1100B
	db	0011$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$less:	equ	gmfont$unknown
gmfont$equal:	equ	gmfont$unknown
gmfont$more:	equ	gmfont$unknown
gmfont$question:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$1111B, 0000$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0000B, 0011$0000B
	db	0000$0000B, 0011$0000B
	db	0000$0000B, 0011$0000B
	db	0000$0011B, 1100$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$at:	equ	gmfont$unknown
gmfont$cap$a:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0111$1110B, 0111$1110B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$b:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1110$0000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0001$1000B
	db	0001$1111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$c:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0001B, 1111$1000B
	db	0000$0110B, 0001$1000B
	db	0001$1000B, 0000$0000B
	db	0001$1000B, 0000$0000B
	db	0001$1000B, 0000$0000B
	db	0001$1000B, 0000$0000B
	db	0000$0110B, 0001$1000B
	db	0000$0001B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$d:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0011$1111B, 1100$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0000$1100B
	db	0000$1100B, 0000$1100B
	db	0000$1100B, 0000$1100B
	db	0000$1100B, 0000$1100B
	db	0000$1100B, 0011$0000B
	db	0011$1111B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$e:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0001$1000B
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$f:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0001$1111B, 1000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$g:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0011$0000B, 0000$0000B
	db	0011$0000B, 0000$0000B
	db	0011$0000B, 1111$1100B
	db	0011$0000B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$h:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0111$1110B, 0111$1110B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1111B, 1111$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0111$1110B, 0111$1110B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$i:
	db	1,12
	db	0000$0000B
	db	0111$1110B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0111$1110B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$cap$j:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1111$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0000B, 1100$0000B
	db	0000$1100B, 1100$0000B
	db	0000$1111B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$k:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0011$1111B, 0011$1100B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 1100$0000B
	db	0000$1111B, 0000$0000B
	db	0000$1100B, 1100$0000B
	db	0000$1100B, 1100$0000B
	db	0000$1100B, 0011$0000B
	db	0011$1111B, 0011$1100B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$l:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1000$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0001$1000B
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$m:
	db	3,12
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0001$1111B, 1000$0001B, 1111$1000B
	db	0000$0111B, 1000$0001B, 1110$0000B
	db	0000$0110B, 0110$0110B, 0110$0000B
	db	0000$0110B, 0110$0110B, 0110$0000B
	db	0000$0110B, 0110$0110B, 0110$0000B
	db	0000$0110B, 0001$1000B, 0110$0000B
	db	0000$0110B, 0001$1000B, 0110$0000B
	db	0001$1111B, 1000$0001B, 1111$1000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
gmfont$cap$n:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0111$1000B, 0111$1110B
	db	0001$1110B, 0001$1000B
	db	0001$1110B, 0001$1000B
	db	0001$1001B, 1001$1000B
	db	0001$1001B, 1001$1000B
	db	0001$1000B, 0111$1000B
	db	0001$1000B, 0111$1000B
	db	0111$1110B, 0001$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$o:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0110$0000B, 0000$0110B
	db	0110$0000B, 0000$0110B
	db	0110$0000B, 0000$0110B
	db	0110$0000B, 0000$0110B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$p:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1110$0000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0001$1111B, 1000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$q:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0111B, 1110$0000B
	db	0001$1000B, 0001$1000B
	db	0110$0000B, 0000$0110B
	db	0110$0000B, 0000$0110B
	db	0110$0000B, 0000$0110B
	db	0110$0000B, 0000$0110B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0000B, 0111$1000B
	db	0000$0000B, 0000$0000B
gmfont$cap$r:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0011$1111B, 1100$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1111B, 1100$0000B
	db	0000$1100B, 1100$0000B
	db	0000$1100B, 0011$0000B
	db	0011$1111B, 0011$1100B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$s:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0111B, 1001$1000B
	db	0001$1000B, 0111$1000B
	db	0001$1000B, 0000$0000B
	db	0000$0111B, 1000$0000B
	db	0000$0000B, 0110$0000B
	db	0000$0000B, 0001$1000B
	db	0001$1110B, 0001$1000B
	db	0001$1001B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$t:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0001$1001B, 1001$1000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$u:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0111$1110B, 0111$1110B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$v:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0111$1110B, 0111$1110B
	db	0001$1000B, 0001$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$w:
	db	3,12
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0001$1111B, 1000$0001B, 1111$1000B
	db	0000$0110B, 0000$0000B, 0110$0000B
	db	0000$0110B, 0001$1000B, 0110$0000B
	db	0000$0110B, 0001$1000B, 0110$0000B
	db	0000$0110B, 0110$0110B, 0110$0000B
	db	0000$0110B, 0110$0110B, 0110$0000B
	db	0000$0001B, 1000$0001B, 1000$0000B
	db	0000$0001B, 1000$0001B, 1000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
gmfont$cap$x:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0111$1110B, 0111$1110B
	db	0001$1000B, 0001$1000B
	db	0000$0110B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0110B, 0110$0000B
	db	0001$1000B, 0001$1000B
	db	0111$1110B, 0111$1110B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$y:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0111$1110B, 0111$1110B
	db	0001$1000B, 0001$1000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0111B, 1110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$cap$z:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0001$1111B, 1111$1000B
	db	0001$1000B, 0001$1000B
	db	0000$0000B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0110B, 0000$0000B
	db	0001$1000B, 0001$1000B
	db	0001$1111B, 1111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$bopen:	equ	gmfont$unknown
gmfont$bslash:	equ	gmfont$unknown
gmfont$bclose:	equ	gmfont$unknown
gmfont$carat:	equ	gmfont$unknown
gmfont$under:	equ	gmfont$unknown
gmfont$tick:	equ	gmfont$unknown
gmfont$low$a:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1100$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0000B, 1111$0000B
	db	0000$0011B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 1111$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$b:
	db	2,12
	db	0011$1100B, 0000$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 1111$0000B
	db	0000$1111B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1111B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$c:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$d:
	db	2,12
	db	0000$0000B, 1111$0000B
	db	0000$0000B, 0011$0000B
	db	0000$0000B, 0011$0000B
	db	0000$0011B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 1111$0000B
	db	0000$0011B, 0011$1100B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$e:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1111B, 1100$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$f:
	db	2,12
	db	0000$0000B, 1111$0000B
	db	0000$0011B, 0011$0000B
	db	0000$0011B, 0000$0000B
	db	0000$1111B, 1111$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$1111B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$g:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1100$1100B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 1100$0000B
	db	0000$1100B, 0000$0000B
	db	0000$0011B, 1111$0000B
	db	0000$1100B, 0000$1100B
	db	0000$1111B, 1111$0000B
gmfont$low$h:
	db	2,12
	db	0011$1100B, 0000$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 1111$0000B
	db	0000$1111B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0011$1111B, 0011$1100B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$i:
	db	1,12
	db	0000$0000B
	db	0001$1000B
	db	0000$0000B
	db	0111$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0111$1110B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$low$j:
	db	1,12
	db	0000$0000B
	db	0000$0110B
	db	0000$0000B
	db	0001$1110B
	db	0000$0110B
	db	0000$0110B
	db	0000$0110B
	db	0000$0110B
	db	0000$0110B
	db	0000$0110B
	db	0110$0110B
	db	0111$1000B
gmfont$low$k:
	db	2,12
	db	0001$1110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0000$0000B
	db	0000$0110B, 0111$1000B
	db	0000$0110B, 0110$0000B
	db	0000$0111B, 1000$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0110$0000B
	db	0001$1110B, 0111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$l:
	db	1,12
	db	0111$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0001$1000B
	db	0111$1110B
	db	0000$0000B
	db	0000$0000B
	db	0000$0000B
gmfont$low$m:
	db	3,12
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0001$1110B, 0111$1001B, 1110$0000B
	db	0000$0111B, 1001$1110B, 0110$0000B
	db	0000$0110B, 0001$1000B, 0110$0000B
	db	0000$0110B, 0001$1000B, 0110$0000B
	db	0000$0110B, 0001$1000B, 0110$0000B
	db	0001$1110B, 0111$1100B, 1111$1000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B, 0000$0000B
gmfont$low$n:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0011$1100B, 1111$0000B
	db	0000$1111B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0011$1100B, 1111$1100B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$o:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1100$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$p:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0011$1100B, 1111$0000B
	db	0000$1111B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1111B, 1100$0000B
	db	0000$1100B, 0000$0000B
	db	0000$1100B, 0000$0000B
	db	0011$1111B, 0000$0000B
gmfont$low$q:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$1111B, 0011$1100B
	db	0000$1100B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 1111$0000B
	db	0000$0000B, 0011$0000B
	db	0000$0000B, 0011$0000B
	db	0000$0000B, 1111$1100B
gmfont$low$r:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$1111B, 0011$1100B
	db	0000$0011B, 1100$1100B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$1111B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$s:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0000B, 1100$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1111B, 1100$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$t:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$1111B, 1111$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0000$0000B
	db	0000$0011B, 0011$0000B
	db	0000$0000B, 1111$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$u:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0011$1100B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 1111$0000B
	db	0000$0011B, 0011$1100B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$v:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0011$1100B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1100B, 1100$0000B
	db	0000$1111B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$w:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0111$1110B, 0111$1110B
	db	0001$1000B, 0001$1000B
	db	0001$1001B, 1001$1000B
	db	0001$1001B, 1001$1000B
	db	0000$0110B, 0110$0000B
	db	0000$0110B, 0110$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$x:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0001$1110B, 0111$1000B
	db	0000$0110B, 0110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0110B, 0110$0000B
	db	0001$1110B, 0111$1000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$low$y:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0001$1110B, 0111$1110B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0001$1000B
	db	0000$0110B, 0110$0000B
	db	0000$0001B, 1110$0000B
	db	0000$0001B, 1000$0000B
	db	0000$0001B, 1000$0000B
	db	0001$1001B, 1000$0000B
	db	0001$1110B, 0000$0000B
gmfont$low$z:
	db	2,12
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$1111B, 1111$0000B
	db	0000$1100B, 0011$0000B
	db	0000$0000B, 1100$0000B
	db	0000$0011B, 0000$0000B
	db	0000$1100B, 0011$0000B
	db	0000$1111B, 1111$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
	db	0000$0000B, 0000$0000B
gmfont$copen:	equ	gmfont$unknown
gmfont$vline:	equ	gmfont$unknown
gmfont$cclose:	equ	gmfont$unknown
gmfont$squigle:	equ	gmfont$unknown
;/*}}}*/


;graphics variables
;/*{{{*/
;byte *cursor;
cursor		ds	2	;current position in dram

border$top:	db	1111$1111b
border$bottom:	db	1111$1111b
border$left:	db	1000$0000b
border$right:	db	0000$0001b

border$mtop:	db	1111$1111b
border$mbottom:	db	1111$1111b
border$mleft:	db	1111$1111b
border$mright:	db	1111$1111b
;/*}}}*/

;random numbers
tsalt:		ds	2	;salt for random number generation
genprand:	ds	2	;psudo random number ptrarray

;program stack
cpmstack:	ds	2
stacksize	equ	1024
stackstart:	ds	stacksize
progstack	equ	$

memory		equ	$


; vim: ts=8 sts=8 sw=8 noet fdm=marker
; end of file

