						.386p
						ASSUME	DS:DATA,CS:CODE
						PUBLIC	TEST_MOV_M_TO_A
						PUBLIC	TEST_MOV_A_TO_M

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



EFLAGS_CF   	   		EQU		00001H
EFLAGS_PF   	  		EQU		00004H
EFLAGS_AF  				EQU		00010H
EFLAGS_ZF       		EQU		00040H
EFLAGS_SF       		EQU		00080H
EFLAGS_TRAP       		EQU		00100H
EFLAGS_IF 				EQU		00200H
EFLAGS_DF  				EQU		00400H
EFLAGS_OF  	 			EQU		00800H
EFLAGS_IOPL       		EQU		03000H
EFLAGS_NF   	  		EQU		04000H
EFLAGS_RF   	  		EQU		10000H
EFLAGS_VF		  		EQU		20000H
EFLAGS_ALIGN_CHECK		EQU		40000H


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATA					SEGMENT
TESTDATA32:
						DD		000001000H,000007007H,000010000H,000100055H,001000000H,010000707H,040000000H,07FFFFFFFH
						DD		080000000H,0800B000BH,0A0000000H,0A00B000BH,0B00B000BH,0D00B000BH,0F0000000H,0FFFFFFFFH
COPYBUF:
						DD		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA					ENDS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


CODE					SEGMENT

TEST_MOV_A_TO_M			PROC
						PUSHAD
						PUSH	ES

						MOV		AL,BYTE PTR [TESTDATA32]
						CMP		AL,BYTE PTR [TESTDATA32]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR [TESTDATA32+4]
						CMP		AL,BYTE PTR [TESTDATA32+4]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR [TESTDATA32+8]
						CMP		AL,BYTE PTR [TESTDATA32+8]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR [TESTDATA32+12]
						CMP		AL,BYTE PTR [TESTDATA32+12]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR [TESTDATA32+16]
						CMP		AL,BYTE PTR [TESTDATA32+16]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR [TESTDATA32+20]
						CMP		AL,BYTE PTR [TESTDATA32+20]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR [TESTDATA32+24]
						CMP		AL,BYTE PTR [TESTDATA32+24]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR [TESTDATA32+28]
						CMP		AL,BYTE PTR [TESTDATA32+28]
						JNE		MOV_A_TO_M_ERR

						MOV		AX,WORD PTR [TESTDATA32]
						CMP		AX,WORD PTR [TESTDATA32]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR [TESTDATA32+4]
						CMP		AX,WORD PTR [TESTDATA32+4]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR [TESTDATA32+8]
						CMP		AX,WORD PTR [TESTDATA32+8]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR [TESTDATA32+12]
						CMP		AX,WORD PTR [TESTDATA32+12]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR [TESTDATA32+16]
						CMP		AX,WORD PTR [TESTDATA32+16]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR [TESTDATA32+20]
						CMP		AX,WORD PTR [TESTDATA32+20]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR [TESTDATA32+24]
						CMP		AX,WORD PTR [TESTDATA32+24]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR [TESTDATA32+28]
						CMP		AX,WORD PTR [TESTDATA32+28]
						JNE		MOV_A_TO_M_ERR

						MOV		EAX,DWORD PTR [TESTDATA32]
						CMP		EAX,DWORD PTR [TESTDATA32]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+4]
						CMP		EAX,DWORD PTR [TESTDATA32+4]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+8]
						CMP		EAX,DWORD PTR [TESTDATA32+8]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+12]
						CMP		EAX,DWORD PTR [TESTDATA32+12]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+16]
						CMP		EAX,DWORD PTR [TESTDATA32+16]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+20]
						CMP		EAX,DWORD PTR [TESTDATA32+20]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+24]
						CMP		EAX,DWORD PTR [TESTDATA32+24]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+28]
						CMP		EAX,DWORD PTR [TESTDATA32+28]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+32]
						CMP		EAX,DWORD PTR [TESTDATA32+32]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+36]
						CMP		EAX,DWORD PTR [TESTDATA32+36]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+40]
						CMP		EAX,DWORD PTR [TESTDATA32+40]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+44]
						CMP		EAX,DWORD PTR [TESTDATA32+44]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+48]
						CMP		EAX,DWORD PTR [TESTDATA32+48]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+52]
						CMP		EAX,DWORD PTR [TESTDATA32+52]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+56]
						CMP		EAX,DWORD PTR [TESTDATA32+56]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR [TESTDATA32+60]
						CMP		EAX,DWORD PTR [TESTDATA32+60]
						JNE		MOV_A_TO_M_ERR


						PUSH	DS
						POP		ES

						MOV		AL,BYTE PTR ES:[TESTDATA32]
						CMP		AL,BYTE PTR ES:[TESTDATA32]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR ES:[TESTDATA32+4]
						CMP		AL,BYTE PTR ES:[TESTDATA32+4]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR ES:[TESTDATA32+8]
						CMP		AL,BYTE PTR ES:[TESTDATA32+8]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR ES:[TESTDATA32+12]
						CMP		AL,BYTE PTR ES:[TESTDATA32+12]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR ES:[TESTDATA32+16]
						CMP		AL,BYTE PTR ES:[TESTDATA32+16]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR ES:[TESTDATA32+20]
						CMP		AL,BYTE PTR ES:[TESTDATA32+20]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR ES:[TESTDATA32+24]
						CMP		AL,BYTE PTR ES:[TESTDATA32+24]
						JNE		MOV_A_TO_M_ERR
						MOV		AL,BYTE PTR ES:[TESTDATA32+28]
						CMP		AL,BYTE PTR ES:[TESTDATA32+28]
						JNE		MOV_A_TO_M_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32]
						CMP		AX,WORD PTR ES:[TESTDATA32]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR ES:[TESTDATA32+4]
						CMP		AX,WORD PTR ES:[TESTDATA32+4]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR ES:[TESTDATA32+8]
						CMP		AX,WORD PTR ES:[TESTDATA32+8]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR ES:[TESTDATA32+12]
						CMP		AX,WORD PTR ES:[TESTDATA32+12]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR ES:[TESTDATA32+16]
						CMP		AX,WORD PTR ES:[TESTDATA32+16]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR ES:[TESTDATA32+20]
						CMP		AX,WORD PTR ES:[TESTDATA32+20]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR ES:[TESTDATA32+24]
						CMP		AX,WORD PTR ES:[TESTDATA32+24]
						JNE		MOV_A_TO_M_ERR
						MOV		AX,WORD PTR ES:[TESTDATA32+28]
						CMP		AX,WORD PTR ES:[TESTDATA32+28]
						JNE		MOV_A_TO_M_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32]
						CMP		EAX,DWORD PTR ES:[TESTDATA32]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+4]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+4]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+8]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+8]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+12]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+12]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+16]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+16]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+20]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+20]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+24]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+24]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+28]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+28]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+32]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+32]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+36]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+36]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+40]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+40]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+44]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+44]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+48]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+48]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+52]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+52]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+56]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+56]
						JNE		MOV_A_TO_M_ERR
						MOV		EAX,DWORD PTR ES:[TESTDATA32+60]
						CMP		EAX,DWORD PTR ES:[TESTDATA32+60]
						JNE		MOV_A_TO_M_ERR


						POP		ES
						POPAD
						XOR		EAX,EAX
						RET

MOV_A_TO_M_ERR:			POP		ES
						POPAD
						MOV		EAX,1
						RET

TEST_MOV_A_TO_M			ENDP


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


TEST_MOV_M_TO_A			PROC
						PUSHAD
						PUSH	ES

						MOV		AL,BYTE PTR [TESTDATA32]
						MOV		BYTE PTR [COPYBUF],AL
						CMP		AL,BYTE PTR [COPYBUF]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR [TESTDATA32+4]
						MOV		BYTE PTR [COPYBUF+4],AL
						CMP		AL,BYTE PTR [COPYBUF+4]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR [TESTDATA32+8]
						MOV		BYTE PTR [COPYBUF+8],AL
						CMP		AL,BYTE PTR [COPYBUF+8]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR [TESTDATA32+12]
						MOV		BYTE PTR [COPYBUF+12],AL
						CMP		AL,BYTE PTR [COPYBUF+12]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR [TESTDATA32+16]
						MOV		BYTE PTR [COPYBUF+16],AL
						CMP		AL,BYTE PTR [COPYBUF+16]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR [TESTDATA32+20]
						MOV		BYTE PTR [COPYBUF+20],AL
						CMP		AL,BYTE PTR [COPYBUF+20]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR [TESTDATA32+24]
						MOV		BYTE PTR [COPYBUF+24],AL
						CMP		AL,BYTE PTR [COPYBUF+24]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR [TESTDATA32+28]
						MOV		BYTE PTR [COPYBUF+28],AL
						CMP		AL,BYTE PTR [COPYBUF+28]
						JNE		MOV_M_TO_A_ERR



						MOV		AX,WORD PTR [TESTDATA32]
						MOV		WORD PTR [COPYBUF],AX
						CMP		AX,WORD PTR [COPYBUF]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR [TESTDATA32+4]
						MOV		WORD PTR [COPYBUF+4],AX
						CMP		AX,WORD PTR [COPYBUF+4]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR [TESTDATA32+8]
						MOV		WORD PTR [COPYBUF+8],AX
						CMP		AX,WORD PTR [COPYBUF+8]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR [TESTDATA32+12]
						MOV		WORD PTR [COPYBUF+12],AX
						CMP		AX,WORD PTR [COPYBUF+12]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR [TESTDATA32+16]
						MOV		WORD PTR [COPYBUF+16],AX
						CMP		AX,WORD PTR [COPYBUF+16]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR [TESTDATA32+20]
						MOV		WORD PTR [COPYBUF+20],AX
						CMP		AX,WORD PTR [COPYBUF+20]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR [TESTDATA32+24]
						MOV		WORD PTR [COPYBUF+24],AX
						CMP		AX,WORD PTR [COPYBUF+24]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR [TESTDATA32+28]
						MOV		WORD PTR [COPYBUF+28],AX
						CMP		AX,WORD PTR [COPYBUF+28]
						JNE		MOV_M_TO_A_ERR


						MOV		EAX,DWORD PTR [TESTDATA32]
						MOV		DWORD PTR [COPYBUF],EAX
						CMP		EAX,DWORD PTR [COPYBUF]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR [TESTDATA32+4]
						MOV		DWORD PTR [COPYBUF+4],EAX
						CMP		EAX,DWORD PTR [COPYBUF+4]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR [TESTDATA32+8]
						MOV		DWORD PTR [COPYBUF+8],EAX
						CMP		EAX,DWORD PTR [COPYBUF+8]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR [TESTDATA32+12]
						MOV		DWORD PTR [COPYBUF+12],EAX
						CMP		EAX,DWORD PTR [COPYBUF+12]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR [TESTDATA32+16]
						MOV		DWORD PTR [COPYBUF+16],EAX
						CMP		EAX,DWORD PTR [COPYBUF+16]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR [TESTDATA32+20]
						MOV		DWORD PTR [COPYBUF+20],EAX
						CMP		EAX,DWORD PTR [COPYBUF+20]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR [TESTDATA32+24]
						MOV		DWORD PTR [COPYBUF+24],EAX
						CMP		EAX,DWORD PTR [COPYBUF+24]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR [TESTDATA32+28]
						MOV		DWORD PTR [COPYBUF+28],EAX
						CMP		EAX,DWORD PTR [COPYBUF+28]
						JNE		MOV_M_TO_A_ERR


						PUSH	DS
						POP		ES



						MOV		AL,BYTE PTR ES:[TESTDATA32]
						MOV		BYTE PTR ES:[COPYBUF],AL
						CMP		AL,BYTE PTR ES:[COPYBUF]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR ES:[TESTDATA32+4]
						MOV		BYTE PTR ES:[COPYBUF+4],AL
						CMP		AL,BYTE PTR ES:[COPYBUF+4]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR ES:[TESTDATA32+8]
						MOV		BYTE PTR ES:[COPYBUF+8],AL
						CMP		AL,BYTE PTR ES:[COPYBUF+8]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR ES:[TESTDATA32+12]
						MOV		BYTE PTR ES:[COPYBUF+12],AL
						CMP		AL,BYTE PTR ES:[COPYBUF+12]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR ES:[TESTDATA32+16]
						MOV		BYTE PTR ES:[COPYBUF+16],AL
						CMP		AL,BYTE PTR ES:[COPYBUF+16]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR ES:[TESTDATA32+20]
						MOV		BYTE PTR ES:[COPYBUF+20],AL
						CMP		AL,BYTE PTR ES:[COPYBUF+20]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR ES:[TESTDATA32+24]
						MOV		BYTE PTR ES:[COPYBUF+24],AL
						CMP		AL,BYTE PTR ES:[COPYBUF+24]
						JNE		MOV_M_TO_A_ERR

						MOV		AL,BYTE PTR ES:[TESTDATA32+28]
						MOV		BYTE PTR ES:[COPYBUF+28],AL
						CMP		AL,BYTE PTR ES:[COPYBUF+28]
						JNE		MOV_M_TO_A_ERR



						MOV		AX,WORD PTR ES:[TESTDATA32]
						MOV		WORD PTR ES:[COPYBUF],AX
						CMP		AX,WORD PTR ES:[COPYBUF]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32+4]
						MOV		WORD PTR ES:[COPYBUF+4],AX
						CMP		AX,WORD PTR ES:[COPYBUF+4]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32+8]
						MOV		WORD PTR ES:[COPYBUF+8],AX
						CMP		AX,WORD PTR ES:[COPYBUF+8]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32+12]
						MOV		WORD PTR ES:[COPYBUF+12],AX
						CMP		AX,WORD PTR ES:[COPYBUF+12]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32+16]
						MOV		WORD PTR ES:[COPYBUF+16],AX
						CMP		AX,WORD PTR ES:[COPYBUF+16]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32+20]
						MOV		WORD PTR ES:[COPYBUF+20],AX
						CMP		AX,WORD PTR ES:[COPYBUF+20]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32+24]
						MOV		WORD PTR ES:[COPYBUF+24],AX
						CMP		AX,WORD PTR ES:[COPYBUF+24]
						JNE		MOV_M_TO_A_ERR

						MOV		AX,WORD PTR ES:[TESTDATA32+28]
						MOV		WORD PTR ES:[COPYBUF+28],AX
						CMP		AX,WORD PTR ES:[COPYBUF+28]
						JNE		MOV_M_TO_A_ERR


						MOV		EAX,DWORD PTR ES:[TESTDATA32]
						MOV		DWORD PTR ES:[COPYBUF],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32+4]
						MOV		DWORD PTR ES:[COPYBUF+4],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF+4]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32+8]
						MOV		DWORD PTR ES:[COPYBUF+8],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF+8]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32+12]
						MOV		DWORD PTR ES:[COPYBUF+12],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF+12]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32+16]
						MOV		DWORD PTR ES:[COPYBUF+16],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF+16]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32+20]
						MOV		DWORD PTR ES:[COPYBUF+20],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF+20]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32+24]
						MOV		DWORD PTR ES:[COPYBUF+24],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF+24]
						JNE		MOV_M_TO_A_ERR

						MOV		EAX,DWORD PTR ES:[TESTDATA32+28]
						MOV		DWORD PTR ES:[COPYBUF+28],EAX
						CMP		EAX,DWORD PTR ES:[COPYBUF+28]
						JNE		MOV_M_TO_A_ERR



						POP		ES
						POPAD
						XOR		EAX,EAX
						RET

MOV_M_TO_A_ERR:			POP		ES
						POPAD
						MOV		EAX,1
						RET

TEST_MOV_M_TO_A			ENDP


CODE					ENDS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

						END
