IDEAL
WARN

EXTRN	__parc:FAR, __parnd:FAR, __retclen:FAR, __retnd:FAR


SEGMENT	_prog	DWORD	'CODE'
        ASSUME cs:_prog, ds:nothing, es:nothing

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;
;;      Syntax: string-10 := l_dtot( <number> )
;;
;;      Purpose: Converts an IEEE 8 byte into an IEEE 10 byte real.
;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
PUBLIC	l_dtot
PROC	l_dtot	FAR
	push	bp
	mov	bp, sp

	xor	ax, ax			; Push a 0 TREAL onto the stack.
	push	ax
	push	ax
	push	ax
	push	ax
	push	ax

	inc	ax			; Get the address of our double.
	push	ax
	call	__parnd
	pop	bx

	mov	bx, sp			; Base address of TREAL.

	mov	cx, 10			; Setup return logic for __retclen.
	push	cx
	push	ss
	push	bx

	push	ds			; Save what we mangle...
	push	si
	push	di

	mov	ds, dx			; Return from __parnd is source.
	mov	si, ax

	push	ds			; Test for 0.
	pop	es
	mov	di, si
	xor	ax, ax
	mov	cx, 4
	rep	scasw
	je	@@skip

	push	ss			; Stack is destination.
	pop	es
	mov	di, bx

;;
;;	This next step is kinda kludgy.  The mantissa of an 8-IEEE is shifted 
;;	4 bits from the 10-IEEE.  But the assumed 1.0 of an 8-IEEE isn't in 
;;	the 10-IEEE.  That means that we end up with a 3-bit shift and an 
;;	explicit 1 bit added (except for 0).
;;
	mov	cl, 5
	xor	ax, ax			; LSB will always be 0.  First 3 bits
	stosb				; of next nibble will also be 0.

	REPT	6
	lodsb
	rol	ax, 1
	rol	ax, 1
	rol	ax, 1
	stosb
	shl	ah, cl
	ENDM

	lodsb				; Special processing for last byte to
	rol	ax, 1			; keep the start of the mantissa out
	rol	ax, 1			; of it.
	rol	ax, 1
	rol	ax, 1
	stc
	rcr	al, 1
	stosb

;;
;;	We have the LS-nibble of the exponent in AH now.  Load the rest
;;	(including the sign), change the bias from 1023 (QUAD) to 16383
;;	(TREAL), and store it all...
;;
	lodsb
	mov	bh, al			; Save sign bit.
	dec	cl
	shl	ah, cl
	rol	ax, cl
	and	ah, 07h

	add	ax, 16383d - 1023d

	and	bh, 80h			; Move in sign.
	or	ah, bh
	stosw

@@skip:	pop	di			; Restore mangled registers.
	pop	si
	pop	ds

	call	__retclen		; Make our call and return.
	mov	sp, bp
	pop	bp
	ret
ENDP



;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;
;;      Syntax:  number := l_ttod( <string-10> )
;;
;;      Purpose: Converts an IEEE 10 byte into an IEEE 8 byte real.
;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
ALIGN	DWORD
PUBLIC	l_ttod
PROC	l_ttod	far
	push	bp
	mov	bp, sp

	xor	ax, ax			; Push a 0 QUAD onto the stack.
	push	ax
	push	ax
	push	ax
	push	ax

	mov	bx, sp			; Save our QUAD address.

	push	ds			; Save what we mangle...
	push	si
	push	di
	push	bx

	inc	ax			; Get the address of our double.
	push	ax
	call	__parc
	pop	bx

	pop	bx			; Retrieve Quad address.

	mov	ds, dx			; Return from __parc is source.
	mov	si, ax

	mov	es, dx			; Test for 0.
	mov	di, ax
	xor	ax, ax
	mov	cx, 5
	rep	scasw
	je	@@skip

	mov	ax, [ si + 8 ]		; Set flags for under/over flow.
	and	ah, 7fh
	sub	ax, 16383d - 1023d
	and	ah, 0f8h
	js	@@skip			; Underflow - result is already 0'd.

	push	ss			; Stack is destination.
	pop	es
	mov	di, bx

	jne	@@ovrf			; Overflow.  ES:DI setup for result.


;;
;;	This next step is kinda kludgy.  The mantissa of an 8-IEEE is shifted 
;;	4 bits from the 10-IEEE.  But the assumed 1.0 of an 8-IEEE isn't in 
;;	the 10-IEEE.  That means that we end up with a 3-bit shift and an 
;;	explicit 1 bit added (except for 0).
;;
	mov	cl, 3

	inc	si			; Skip over excess - need to check for
					; rounding in the future.

	lodsw
	mov	dl, ah
	shr	ax, cl
	stosb

	REPT	5
	lodsb
	mov	ah, al
	mov	al, dl
	mov	dl, ah
	shr	ax, cl
	stosb
	ENDM

	shr	dl, cl			; Hold on to low 4 bits of dl.

	lodsw				; Get ahold of mantissa and sign.
	mov	dh, ah			; Save sign.
	and	ah, 7fh

	sub	ax, 16383d - 1023d	; Convert exponent and move into pos.
	inc	cl
	shl	ax, cl

	and	dx, 800fh		; Mask out non- sign/mantissa.

	or	ax, dx			; Or into exponent everyting else.
	stosw

@@skip:	pop	di			; Restore mangled registers.
	pop	si
	pop	ds

	call	__retnd			; Make our call and return.
	mov	sp, bp
	pop	bp
	ret



@@ovrf:	xor	ax, ax			; ES:DI points to return value.
	dec	ax
	REPT	4
	stosw
	ENDM
	jmp	@@skip
ENDP
ENDS	_prog
END
