IDENTIFICATION DIVISION.
PROGRAM-ID. programming-past.
AUTHOR. Christopher Lee

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  a						PIC 9(3).
01 	b						PIC 9(3).
01	c						PIC 9(3) VALUE 0.
01	wordlist				PIC X(50) VALUE 'Hello world'. 	*> Change the sentence to be encrypted here
01	encrypted				PIC X(50).
01	decrypted				PIC X(50).
01	solver					PIC X(50).
01	counter					PIC 99 VALUE 0.
01	shift					PIC 99 VALUE 3.					*> Change the shift value for encryption here
01	max-shift-value			PIC 99 VALUE 26.				*> Change the max shift value to solve the encryption here

PROCEDURE DIVISION.
MAINLINE.
    DISPLAY "Original sentence: ", wordlist
    DISPLAY "Shift amount: ", shift
	PERFORM ENCRYPT.
    DISPLAY "Encrypted sentence: ", encrypted
    PERFORM DECRYPT.
    DISPLAY "Decrypted sentence: ", decrypted
	ADD max-shift-value TO counter.
	ADD 1 TO max-shift-value.
	DISPLAY "Solving cipher..."
	PERFORM SOLVE UNTIL c IS EQUAL TO max-shift-value.
	STOP RUN.
ENCRYPT.
	MOVE FUNCTION LOWER-CASE(wordlist) TO encrypted
	PERFORM VARYING b FROM 1 BY 1 UNTIL b > FUNCTION LENGTH(wordlist)
		*> Varying b is basically a for loop starting at 1, incrementing by 1, until b is
		*> greater than the length of wordlist. Length is an intrinsic COBOL function, as
		*> is Lower-Case.
		IF encrypted (b:1) IS NOT ALPHABETIC OR encrypted (b:1) = SPACE
			EXIT PERFORM CYCLE
		END-IF
		MOVE FUNCTION ORD("a") to a
		MOVE FUNCTION CHAR(FUNCTION MOD(FUNCTION ORD(encrypted(b:1)) - a + shift, 26) + a) TO encrypted(b:1)
	END-PERFORM.
DECRYPT.
    MOVE FUNCTION LOWER-CASE(encrypted) TO decrypted
    PERFORM VARYING b FROM 1 BY 1 UNTIL b > FUNCTION LENGTH(encrypted)
		*> Char returns a one-character alphanumeric value in the position at the value specified
        IF decrypted (b:1) IS NOT ALPHABETIC OR decrypted (b:1) = SPACE
            EXIT PERFORM CYCLE
        END-IF
        MOVE FUNCTION ORD("a") to a
		*> Ord("a") returns 97, the ASCII ordered location of lowercase a
        MOVE FUNCTION CHAR(FUNCTION MOD(FUNCTION ORD(decrypted(b:1)) - a - shift, 26) + a) TO decrypted(b:1)
    END-PERFORM.
SOLVE.
	MOVE FUNCTION LOWER-CASE(wordlist) TO solver
	PERFORM VARYING b FROM 1 BY 1 UNTIL b > FUNCTION LENGTH(wordlist)
		*> Mod is the intrinsic Modulo function. Ord returns the ordinal position of a character
		*> in a sequence. Easily done when the argument is the substring of a data item
		IF solver (b:1) IS NOT ALPHABETIC OR solver (b:1) = SPACE
			EXIT PERFORM CYCLE
		END-IF
		MOVE FUNCTION ORD("a") to a
		MOVE FUNCTION CHAR(FUNCTION MOD(FUNCTION ORD(solver(b:1)) - a - c, 26) + a) TO solver(b:1)
	END-PERFORM.
	DISPLAY "Caesar", counter, ": ", solver
	ADD 1 TO c.
	SUBTRACT 1 FROM counter.