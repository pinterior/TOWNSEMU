						ASSUME	CS:CODE

						PUBLIC	MOVEAXDS
						PUBLIC	MOVEAXDS7FFF

CODE					SEGMENT

MOVEAXDS				PROC
						MOV		EAX,[ESP+4]
						MOV		AX,DS
						RET
MOVEAXDS				ENDP

MOVEAXDS7FFF			PROC
						MOV		EAX,7FFFFFFFH
						MOV		AX,DS
						RET
MOVEAXDS7FFF			ENDP

CODE					ENDS

						END




