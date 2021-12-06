INCLUDE Irvine32.inc

.data
	Computer word 0518h,0519h,0520h,0521h,0522h,50 DUP(?)
	CompHead word 0522h
	xDirec1 BYTE 1
	yDirec1 BYTE 0
	SectionOfComp DWORD 5

	Snake word 0810h,0811h,0812h,0813h,0814h,50 DUP(?)
	SnakeHead word 0814h
	xDirec2 BYTE 1
	yDirec2 BYTE 0
	SectionOfSnake DWORD 5
	
	Apple WORD 1560

	Crush BYTE 0 ; Game end if Crush = 1

	xMax BYTE 50h
	yMax BYTE 01ch

	msg1 BYTE "Draw Game",0
	msg2 BYTE "You won",0
	msg3 BYTE "You Lose",0

	posOK Byte 0
	ScreenSpeed WORD 120


.code
main PROC

	call Randomize
	call DrawArea
	call SnakeGame

	;call Clrscr
	mov eax, 15
	call setTextColor
	movzx eax,Crush
	cmp Crush,2
	je Lose
	jl Win
	jg Draw

Win:
	mov edx, offset msg2
	jmp Print
Lose:
	mov edx, offset msg3
	jmp Print
Draw:
	mov edx, offset msg1
	jmp Print
Print:
	call WriteString
 exit
main ENDP


NewApple PROC uses ebx eax
	pushad
TryAgain:
	movzx eax, yMax
	sub eax, 5
	call BetterRandomRange
	
	mov bh, al
	
	movzx eax, xMax
	sub eax, 5
	call BetterRandomRange
	
	mov bl, al

	call AppleCheck

	mov al,posOk
	cmp al,1
	je Okay

	jmp TryAgain
Okay:
	mov Apple, bx
	mov posOK, 0
	call SpeedUp
	popad
ret
NewApple ENDP

SpeedUp Proc
	pushad

	mov ax,ScreenSpeed
	cmp ax,65
	je NoMoreSpeedUp
	sub ax,5
	mov ScreenSpeed,ax
NoMoreSpeedUp:
	popad
ret
SpeedUp ENDP

AppleCheck Proc uses eax ebx
	
	mov ax,bx

	mov esi, offset snake
	mov ecx, SectionOfSnake
	;dec ecx
L1:
	cmp ax, [esi]
	je FindProblem
	add esi,2 ; type snake
loop L1
	
	mov esi, offset computer
	mov ecx, SectionOfComp
	;dec ecx

L2:
	cmp ax, [esi]
	je FindProblem
	add esi,2 ; type computer
loop L2
	mov posOk,1
	jmp ACheckEnd

FindProblem:
	mov posOk,0
ACheckEnd:
ret
AppleCheck ENDP

BetterRandomRange PROC
	
	call RandomRange
	add eax, 2                 ;pixels away from the borderline
	ret 
BetterRandomRange ENDP

EatApple PROC uses eax esi ecx
	mov ax,CompHead
	cmp ax, Apple
	jne FailToEat

	mov eax, SectionOfComp
	cmp eax, 55
	je Food1
	call AddSection
Food1:
	call NewApple
	call DrawApple

	jmp FailToEat

FailToEat:
	ret
EatApple ENDP

EatApple2 PROC uses eax esi ecx
	mov ax, SnakeHead
	cmp ax, Apple
	jne FailToEat

	mov eax, SectionOfSnake
	cmp eax, 55
	je Food1
	call AddSectionSnake
Food1:
	call NewApple
	call DrawApple

	jmp FailToEat

FailToEat:
	ret
EatApple2 ENDP

;======= add at the end======
; value stored in eax
AddSection PROC uses esi eax
	pushad

	mov esi, SectionOfComp

	mov ax,CompHead
	add ah, yDirec1
	add al, xDirec1
	mov CompHead, ax
	add esi,esi
	mov computer[esi],ax
	inc SectionOfComp
	popad

ret
AddSection ENDP

AddSectionSnake PROC uses esi eax
	pushad

	mov esi, SectionOfSnake

	mov ax,SnakeHead
	add ah, yDirec2
	add al, xDirec2
	mov SnakeHead, ax
	add esi,esi
	mov Snake[esi],ax
	inc SectionOfSnake
	popad

ret
AddSectionSnake ENDP

CheckForCrush PROC uses eax esi ecx
	pushad
	mov ax,CompHead

	mov esi, offset computer
	mov ecx, SectionOfComp
	dec ecx
L1:
	cmp ax, [esi]
	jne Nope
	mov Crush,1
Nope:
	add esi,2 ; type computer
loop L1

mov esi, offset snake
	mov ecx, SectionOfSnake
	dec ecx
L2:
	cmp ax, [esi]
	jne Nope2
	mov Crush,1
Nope2:
	add esi,2 ; type snake
loop L2
	
	mov ax,CompHead

CheckLeft:
	cmp ah, 0
	jne CheckTop
	mov Crush,1

CheckTop:
	cmp al, 0
	jne CheckRight
	mov Crush,1

	
CheckRight:
	inc ah
	inc al

	cmp ah, ymax 
	jne CheckBot
	mov Crush,1


CheckBot:

	cmp al, xMax 
	jne EndCheckCrush
	mov Crush,1



EndCheckCrush:
	popad
ret
CheckForCrush ENDP

CheckForCrush2 PROC uses eax esi ecx
	pushad
	mov ax,SnakeHead

	mov esi, offset snake
	mov ecx, SectionOfSnake
	dec ecx
L1:
	cmp ax, [esi]
	jne Nope
	mov Crush,2
Nope:
	add esi,2 ; type snake
loop L1
	
	mov esi, offset computer
	mov ecx, SectionOfComp
	dec ecx

L2:
	cmp ax, [esi]
	jne Nope2
	mov Crush,1
Nope2:
	add esi,2 ; type computer
loop L2

	
	mov ax,SnakeHead

CheckLeft:
	cmp ah, 0
	jne CheckTop
	mov Crush,2

CheckTop:
	cmp al, 0
	jne CheckRight
	mov Crush,2

	
CheckRight:
	inc ah
	inc al

	cmp ah, ymax 
	jne CheckBot
	mov Crush,2


CheckBot:

	cmp al, xMax 
	jne EndCheckCrush
	mov Crush,2


EndCheckCrush:
	popad
ret
CheckForCrush2 ENDP

CheckForCrush3 Proc
	mov ax,CompHead
	cmp ax,SnakeHead
	jne EndCheck
	mov Crush,3
EndCheck:
ret
CheckForCrush3 ENDP
;=================================
SnakeGame PROC uses eax
	
	call DrawApple
	gameloop:
		call DrawComp
		call DrawSnake

		call CheckForCrush
		call CheckForCrush2
		call CheckForCrush3
		cmp Crush,0
		jg ENDGAME

		call EatApple
		call EatApple2

		call NextElement
		
		call UserInput
		call delaytime
	jmp gameloop
ENDGAME:
ret
SnakeGame ENDP
;=================================

NextElement Proc uses eax ebx
	movzx eax, CompHead 
	xchg ah,al
	;div ForDiv ; eax = 0000 1605
cmp ah,1
	jne CheckCol2
cmp al,1
	je MRight

CheckRow26:
cmp al,26
	je MUp
	jmp Nochange
;===========================
CheckCol2:
cmp ah,2
	jne CheckCol3
cmp al,26
	je Nochange

	mov ebx,eax
	and bl,01
	cmp bl,0
	je MDown
	jmp MRight
;===========================
CheckCol3:
cmp ah,78
	jne NoChange
;cmp al,28
	;je MLeft

	mov ebx,eax
	and bl,01
	cmp bl,0
	je MLeft
	jmp MDown
	
;===========================
MLeft:
	mov xDirec1,-1
	mov yDirec1,0
	jmp Nochange
MUp:
	mov xDirec1,0
	mov yDirec1,-1
	jmp Nochange
MRight:
	mov xDirec1,1
	mov yDirec1,0
	jmp Nochange
MDown:
	mov xDirec1,0
	mov yDirec1,1
	jmp Nochange

NoChange:
ret
NextElement ENDP

;======= Movement speed of snake =========
delaytime PROC uses eax
	movzx eax, ScreenSpeed
	call Delay
	ret
delaytime ENDP
;=========================================

DrawApple PROC uses eax edx
	mov dx,Apple
	call GotoXY
	mov eax, 64
	call setTextColor
	mov al,'O'
	call WriteChar
ret
DrawApple ENDP


;=================================
DrawComp PROC uses ecx esi edx
	
	mov esi, offset computer
	
	mov dx,[esi]
	call GotoXY

	mov eax, 0
	call setTextColor

	mov al,' '
	call WriteChar

	call Update

	mov esi, offset computer
	mov ecx, SectionOfComp
	
L1:

	mov dx,[esi]
	add esi, type computer
	call GotoXY
	mov eax, 176
	call setTextColor
	mov al,'*'
	call WriteChar
loop L1
	
	
ret
DrawComp ENDP
;=================================

DrawSnake PROC uses ecx esi edx
	
	mov esi, offset snake
	
	mov dx,[esi]
	call GotoXY

	mov eax, 0
	call setTextColor

	mov al,' '
	call WriteChar

	call Update2

	mov esi, offset snake
	mov ecx, SectionOfSnake
	
L1:

	mov dx,[esi]
	add esi, type snake
	call GotoXY
	mov eax, 112
	call setTextColor
	mov al,'#'
	call WriteChar
loop L1
	
	
ret
DrawSnake ENDP

;=================================
Update PROC uses eax 
	mov esi, offset computer
	mov edi, offset computer

	mov ecx, SectionOfComp
	dec ecx
	add edi,2
L1:
	mov ax,[edi]
	mov [esi],ax
	add esi, type computer
	add edi, type computer
loop L1
	mov ax,[esi]
	add ah, yDirec1
	add al, xDirec1
	
	mov [esi],ax
	mov CompHead,ax
ret
Update ENDP
;=================================

Update2 PROC uses eax 
	mov esi, offset snake
	mov edi, offset snake

	mov ecx, SectionOfSnake
	dec ecx
	add edi,2
L1:
	mov ax,[edi]
	mov [esi],ax
	add esi, type snake
	add edi, type snake
loop L1
	mov ax,[esi]
	add ah, yDirec2
	add al, xDirec2
	
	mov [esi],ax
	mov SnakeHead,ax
ret
Update2 ENDP
;=================================


UserInput Proc
	call ReadKey
	
	;cmp al,'X'
	;je ENDGAME

	cmp al,'A'
	je MoveLeft
	cmp al,'a'
	je MoveLeft

	cmp al,'D'
	je MoveRight
	cmp al,'d'
	je MoveRight
		

	cmp al,'W'
	je MoveUp
	cmp al,'w'
	je MoveUp

	cmp al,'S'
	je MoveDown
	cmp al,'s'
	je MoveDown
	jmp GoDelay

MoveLeft:
		cmp xDirec2,1
		je GoDelay

		mov xDirec2,-1
		mov yDirec2,0
		jmp GoDelay

MoveRight:
		cmp xDirec2,-1
		je GoDelay

		mov xDirec2,1
		mov yDirec2,0
		jmp GoDelay
MoveUp:
		cmp yDirec2,1
		je GoDelay

		mov xDirec2,0
		mov yDirec2, -1
		jmp GoDelay
MoveDown:
		cmp yDirec2,-1
		je GoDelay			

		mov xDirec2,0
		mov yDirec2,1
		jmp GoDelay

GoDelay:		
		;call delaytime
ret
UserInput ENDP


;--------------------------------------------------------------------------
; Name: DrawArea Proc 
;
; Purpose: Draws the Areas
;			calls DrawRows/DrawCols
; Recieves: Nothing
;
; Modifies: Nothing
;
; Output: Area of game
;
; Returns: Nothing
;--------------------------------------------------------------------------
DrawArea proc
	pushad
	mov eax, 240
	call setTextColor

	call DrawRows
	call DrawCols

	popad
	ret
DrawArea endp


;--------------------------------------------------------------------------
; Name: DrawRows
;
; Purpose: Draws the Upper and Lower Row
;
; Recieves: xMax, yMax
;
; Modifies: Nothing
;
; Output: Displays Row Output
;
; Returns: Nothing
;--------------------------------------------------------------------------
DrawRows Proc 
	pushad


	
	mov al, ' '

	; Drawing 2 rows
	mov ecx, 2

	; dh = rows (y)
	; dl = cols (x)

	; set first y 
	mov dh, 0

	OuterLoopRows:
		
		; save ecx
		push ecx

		movzx ecx, xMax
		mov dl, 0

		InnerLoopRows:

			call Gotoxy
			call WriteChar
			inc dl
		
			loop InnerLoopRows


		; set final y
		mov dh, yMax
		dec dh		; off by one: interval is [0, yMax -1] not [0, yMax]

		; restore ecx
		pop ecx
		loop OuterLoopRows

	popad
	ret 
DrawRows endp

;--------------------------------------------------------------------------
; Name: DrawCols
;
; Purpose: Draws the Left and Right Columns
;
; Recieves: xMax, yMax
;
; Modifies: Nothing
;
; Output: Displays Row Output
;
; Returns: Nothing
;--------------------------------------------------------------------------
DrawCols Proc 
	pushad
	
	mov al, ' '

	; Drawing 2 rows
	mov ecx, 2

	; dh = rows (y)
	; dl = cols (x)

	; set first x 
	mov dl, 0

	OuterLoopCols:
		
		; save ecx
		push ecx

		movzx ecx, yMax
		mov dh, 0

		InnerLoopCols:

			call Gotoxy
			call WriteChar
			inc dh
		
			loop InnerLoopCols


		; set final x
		mov dl, xMax
		dec dl	; off by one: interval is [0, yMax -1] not [0, yMax]
		

		; restore ecx
		pop ecx
		loop OuterLoopCols


	popad
	ret 
DrawCols endp


END main
