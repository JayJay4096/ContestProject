; We need Irvine Libraries for this.
INCLUDE Irvine32.inc

BufSize = 5

printMe MACRO msgLabel, msgSize
	pushad
	INVOKE WriteConsole,
      outputHandler,        ; console output handle
      ADDR msgLabel,           ; string pointer
      msgSize,            ; string length
      0,					; Don't care how many bytes I write. Ignoring.
      0                    ; not used
	popad
ENDM

moveCursor MACRO coords
	pushad
	INVOKE SetConsoleCursorPosition,
		outputHandler,
		coords
ENDM
    
timeCheck MACRO
	.code
	push ESI
	push EDI
	call GetMseconds
	MOV currTime, EAX
	MOV ESI, OFFSET currTime 
	MOV EDI, OFFSET endTime
	CMPSD ; compares the doublewords
	pop EDI
	pop ESI
ENDM

;color is fore*16 + back

.data
inputHandler HANDLE 0
outputHandler HANDLE 0

newLine EQU <0dh, 0ah>

testMsg LABEL BYTE
	BYTE "You have one minute.", newLine
	BYTE "Try and get as many correct letters as you can!", newLine
	BYTE "Ready to start? Press any key!", newLine
msgSize DWORD ($-testMsg)

wordMsg LABEL BYTE
	BYTE "Your current word is:",newLine
wmSize DWORD ($-wordMsg)

perfMsg LABEL BYTE
	BYTE "Perfect Word! +2 Bonus Points", newLine
perfSize DWORD ($-perfMsg)

timeMsg1 LABEL BYTE
	BYTE "Time's up! Let's see how you did...", newLine
tms1 DWORD ($-timeMsg1)

endMsg LABEL BYTE
	BYTE newLine,"Thanks for playing!", newLine
	BYTE "Press any key to close the program.", newLine
emSize DWORD ($-endMsg)

pString LABEL BYTE
	BYTE "> ",0
pSize DWORD ($-pString)

bytesWritten DWORD ?
bytesRead DWORD ?

charBuf BYTE ?
charNum DWORD ?
tempAddress DWORD ?

startTime DWORD ?
currTime DWORD ?
endTime DWORD ?

timeFlag BYTE ?
perfectFlag BYTE 0
initPos COORD <0, 0>
perfPos COORD <4, 6>
scorePos COORD <0, 5>
endPos COORD <0, 15>

charactersWritten DWORD 0
correctCharacters DWORD 0
wordCounter DWORD 0
perfectWords DWORD 0
finalScore DWORD 0

buffer LABEL BYTE
	BYTE "tests",0

.code
charPrompt PROC
	pushad
promptLoop:
	call ReadChar
	CMP AL, 'Z'
	JBE upCheck ; Check if uppercase letter
	CMP AL, 'a'
	JB promptLoop ; In the gap between low-a and cap-Z, not a letter.
	CMP AL, 'z'
	JA promptLoop ; After the letters, not a letter.
	JMP checkClear
upCheck:
	CMP AL, 'A'
	JB promptLoop ; Before the letters, not a letter.
checkClear:
	mov charBuf, AL	
	popad
	ret
charPrompt ENDP

scoreCard PROC
	.data
	charString BYTE "Characters, Correct / Typed - ",0
	wordString BYTE "Words, Perfected / Finished - ",0
	scoreString BYTE "YOUR FINAL SCORE IS... ",0
	line BYTE newLine,0
	.code
	pushad
	MOV EDX, OFFSET charString
	CALL WriteString
	MOV EAX, correctCharacters
	CALL WriteDec
	MOV AL, "/"
	CALL WriteChar
	MOV EAX, charactersWritten
	CALL WriteDec
	MOV EDX, OFFSET line
	CALL WriteString

	MOV EDX, OFFSET wordString
	CALL WriteString
	MOV EAX, perfectWords
	SHR EAX, 1
	CALL WriteDec
	MOV AL, "/"
	CALL WriteChar
	MOV EAX, wordCounter
	CALL WriteDec
	MOV EDX, OFFSET line
	CALL WriteString	

	MOV EDX, OFFSET scoreString
	CALL WriteString
	MOV EAX, finalScore
	CALL WriteDec
	popad
	ret
scoreCard ENDP



main PROC
	MOV AX, black*16 + lightGray
	call SetTextColor
	; Setting up handles we care about
	INVOKE GetStdHandle, STD_INPUT_HANDLE
	MOV inputHandler, EAX
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	MOV outputHandler, EAX

	printMe testMsg, msgSize
	printMe pString, pSize
	call ReadChar

	call GetMseconds
	MOV currTime, EAX
	MOV startTime, EAX
	ADD EAX, (10 * 1000)
	MOV endTime, EAX

	call ClrScr
gameLoop:
	jmp preparation
charLoop:
	jmp gameplay
endcharLoop:
	inc charNum
	LOOP charLoop
	call ClrScr
	CMP perfectFlag, 1
	JE perfectWord
backToIt:
	MOV ECX, 2
	ADD wordCounter, 1
	LOOP gameLoop

preparation:
	; TODO figure out how to pick a random word
	printMe wordMsg, wmSize
	mov EDX, OFFSET buffer
	call WriteString
	MOV ECX, 5
	MOV charNum, 0
	MOV perfectFlag, 1
	call Crlf
	printMe pString, pSize
	jmp charLoop

perfectWord:
	ADD perfectWords, 2
	moveCursor perfPos
	MOV EAX, black*16 + yellow
	call SetTextColor
	printMe perfMsg, perfSize
	MOV AX, black*16 + lightGray
	call SetTextColor
	moveCursor initPos
	jmp backToIt

gameplay:
	call CharPrompt
	pushad
	MOV ESI, OFFSET buffer
	ADD ESI, charNum
	MOV BL, [ESI]
	CMP charBuf, BL
	popad

	jne missed
	ADD correctCharacters, 1
	MOV AX, green*16 + white
	jmp writing
missed:
	MOV perfectFlag, 0
	MOV AX, red*16 + white
writing:
	call SetTextColor
	printMe charBuf, 1
	ADD charactersWritten, 1
	MOV AX, black*16 + lightGray
	call SetTextColor
	timeCheck
	jge timeUp
	jmp endCharLoop

timeUp:
	MOV EAX, finalScore
	ADD EAX, correctCharacters
	ADD EAX, perfectWords
	MOV finalScore, EAX
	call ClrScr
	printMe timeMsg1, tms1
	MOV EAX, 2000
	call Delay

	moveCursor scorePos
	call ScoreCard

	moveCursor endPos
	printMe endMsg, emSize
	call CharPrompt
	exit
main ENDP
END main