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

asciiIncrement MACRO target, value
	.code
	pushad
	MOV AX, target
	ADD AX, value
	AAA
	OR AX, 3030h
	MOV target, AX
	popad
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

scoreCard MACRO charCount, goodChars, wordCount, goodWords, score
	.data
	charString1 BYTE "Out of the ",charCount," characters you typed, you got ",goodChars," correct.", newLine
	wordString BYTE "Out of the ",wordCount," words you finished, you did ",goodWords," perfectly.", newLine
	scoreString BYTE "YOUR FINAL SCORE IS... ",score,"!", newLine
	.code
	printMe charString1, LENGTHOF charString1
	printMe wordString, LENGTHOF wordString
	printMe scoreString, LENGTHOF scoreString
ENDM

;color is fore*16 + back

.data
inputHandler HANDLE 0
outputHandler HANDLE 0

fileHandler HANDLE 0
wordFile BYTE "quickdict.txt",0

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

buffer LABEL BYTE
	BYTE "tests",0

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

charactersWritten WORD '0'
correctCharacters WORD '0'
wordCounter WORD '0'
perfectWords WORD '0'
finalScore WORD '0'

.code
charPrompt PROC
	pushad
	call ReadChar
	mov charBuf, AL	
	popad
	ret
charPrompt ENDP



main PROC
	MOV AX, black*16 + lightGray
	call SetTextColor
	; Setting up handles we care about
	INVOKE GetStdHandle, STD_INPUT_HANDLE
	MOV inputHandler, EAX
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	MOV outputHandler, EAX
	; Here's a file containing the words we'll be throwing
	INVOKE CreateFile, 
		ADDR wordFile, ; filename
		GENERIC_READ, ; what I plan to do with it
		DO_NOT_SHARE, ; share it
		NULL, ; security attributes to worry about
		OPEN_EXISTING, ; open or create
		FILE_ATTRIBUTE_NORMAL, ; attributes to expect
		0 ; template file, not using
	MOV fileHandler, EAX

	printMe testMsg, msgSize
	printMe pString, pSize
	call CharPrompt

	call GetMseconds
	MOV currTime, EAX
	MOV startTime, EAX
	ADD EAX, (60 * 1000)
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
	ADD wordCounter, '1'
	AAA
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
	ADD perfectWords, '2'
	AAA
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
	ADD correctCharacters, '1'
	AAA
	MOV AX, green*16 + white
	jmp writing
missed:
	MOV perfectFlag, 0
	MOV AX, red*16 + white
writing:
	call SetTextColor
	printMe charBuf, 1
	ADD charactersWritten, '1'
	AAA
	MOV AX, black*16 + lightGray
	call SetTextColor
	timeCheck
	jge timeUp
	jmp endCharLoop

timeUp:
	asciiIncrement finalScore, correctCharacters
	asciiIncrement finalScore, perfectWords
	call ClrScr
	printMe timeMsg1, tms1
	MOV EAX, 2000
	call Delay

	moveCursor scorePos
	scoreCard WORD PTR charactersWritten, WORD PTR correctCharacters, WORD PTR wordCounter, WORD PTR perfectWords, WORD PTR finalScore

	moveCursor endPos
	printMe endMsg, emSize
	call CharPrompt

	MOV EAX, fileHandler
	call CloseFile
	exit
main ENDP
END main