      SUBROUTINE GETPAR (NUMPAR,PROMPT,TYPARR,PAR1,PAR2,PAR3,
     *           PAR4,PAR5,PAR6,PAR7,PAR8,PAR9,PAR10,
     *           PAR11,PAR12,PAR13,PAR14,PAR15,PAR16,PAR17,
     *           PAR18,PAR19,PAR20,PAR21,PAR22)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a set of parameters for a command by prompting
C-                         for each one separately.
C-
C-
C-   Inputs : NUMPAR     [I]: Number of parameters to get
C-            PROMPT [C*(*)]: Array of prompt strings for each parameter
C-            TYPARR [C*(*)]: Array of parameter types
C-
C-   Outputs: PAR1-PAR10 [I]: Parameters to be filled.
C-
C-   Controls: PF may be changed inside this routine
C-
C-     NOTE
C-
C-       To INCREASE the number of parameters possible, add PAR11 etc. here
C-       AND change MAXCOM in COMNUM.INC
C-
C-   Created  16-JUN-1992   Lupe Howell
C-              Based on GETPAR by Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR
      CHARACTER*(*) PROMPT(*)
      CHARACTER*1 TYPARR(*)
      INTEGER PAR1
      INTEGER PAR2
      INTEGER PAR3
      INTEGER PAR4
      INTEGER PAR5
      INTEGER PAR6
      INTEGER PAR7
      INTEGER PAR8
      INTEGER PAR9
      INTEGER PAR10
C
      INCLUDE 'fgl.h'
      INCLUDE 'fdevice.h'
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
C
      INTEGER PAR11,PAR12,PAR13,PAR14,PAR15,PAR16,PAR17,PAR18,PAR19
      INTEGER PAR20,PAR21,PAR22
      INTEGER LEN_PROMPT
      INTEGER I, J, NNUM, NCHAR
      INTEGER PAR(10), PTON(10), PTOC(10)
      CHARACTER*80 CPAR(10)
      INTEGER RETURN, ADDR
      INTEGER ADDNPAR(10), ADDCPAR(10), LENCPAR(10)
      INTEGER COMUSE,NUMUSE
      CHARACTER*2 BUFCOM
C----------------------------------------------------------------------
C
C *** Check that the number of parameters is valid
C
      IF(NUMPAR.LT.0) THEN
        NUMUSE=-NUMPAR
      ELSE
        NUMUSE=NUMPAR
        COMUSE=1
      ENDIF
      IF(NUMUSE.GT.MAXCOM) THEN
        WRITE(BUFCOM,98) MAXCOM
   98   FORMAT(I2)
        CALL ABOMEN(COMPACK__MAXPAR,BUFCOM)
      ENDIF
C
C ***  Construct mapping of parameter number to array index for numeric and
C ***  character parameters.  Copy parameters.
C
      NNUM = 0
      NCHAR = 0
      DO 50 I = 1, NUMUSE
        IF(TYPARR(I).EQ.'I' .OR. TYPARR(I).EQ.'R' .OR.
     &    TYPARR(I).EQ.'L' .OR. TYPARR(I).EQ.'H')THEN
          NNUM = NNUM + 1
          PTON(I) = NNUM
          J = I
          ASSIGN 10 TO RETURN
          GO TO 100
   10     CONTINUE
          ADDNPAR(I) = ADDR
          CALL MOVINT(%VAL(ADDR), PAR(NNUM))
        ELSE
          NCHAR = NCHAR + 1
          PTOC(I) = NCHAR
          J = NCHAR + NUMUSE + 2
          ASSIGN 20 TO RETURN
          GO TO 100
   20     CONTINUE
          LENCPAR(I) = ADDR
          J = I
          ASSIGN 30 TO RETURN
          GO TO 100
   30     CONTINUE
          ADDCPAR(I) = ADDR
          CALL MOVSTR(%VAL(ADDR), CPAR(NCHAR), %VAL(LENCPAR(I)))
        ENDIF
   50 CONTINUE
C
C *** Get passed length of PROMPT
C
      J = NUMUSE + 1
      ASSIGN 60 TO RETURN
      GO TO 100
   60 CONTINUE
      LEN_PROMPT = ADDR
C
C *** Interact with user
C
      CALL GETPAR77(NUMPAR, %REF(PROMPT), %REF(TYPARR),
     &  PTON, PTOC, PAR, %REF(CPAR), %VAL(LEN_PROMPT), %VAL(1),
     &  %VAL(LEN(CPAR)))
C
C *** Copy parameters back to calling program
C
      DO 90 I=1,NUMUSE
        IF(TYPARR(I).EQ.'I' .OR. TYPARR(I).EQ.'R' .OR.
     &    TYPARR(I).EQ.'L' .OR. TYPARR(I).EQ.'H')THEN
          CALL MOVINT(PAR(PTON(I)), %VAL(ADDNPAR(I)))
        ELSE
          CALL MOVSTR(%REF(CPAR(PTOC(I))), %VAL(ADDCPAR(I)),
     &      %VAL(LEN(CPAR(PTOC(I)))), %VAL(LENCPAR(I)))
        ENDIF
   90 CONTINUE
      GO TO 999
C
C *** The following code is a "subroutine" to return the address of parameter
C *** J (1-22) in the variable ADDR.
C
  100 CONTINUE
      GO TO (101,102,103,104,105,106,107,108,109,110,
     &       111,112,113,114,115,116,117,118,119,120,
     &       121,122), J
  101 ADDR = %LOC(PAR1)
      GO TO RETURN
  102 ADDR = %LOC(PAR2)
      GO TO RETURN
  103 ADDR = %LOC(PAR3)
      GO TO RETURN
  104 ADDR = %LOC(PAR4)
      GO TO RETURN
  105 ADDR = %LOC(PAR5)
      GO TO RETURN
  106 ADDR = %LOC(PAR6)
      GO TO RETURN
  107 ADDR = %LOC(PAR7)
      GO TO RETURN
  108 ADDR = %LOC(PAR8)
      GO TO RETURN
  109 ADDR = %LOC(PAR9)
      GO TO RETURN
  110 ADDR = %LOC(PAR10)
      GO TO RETURN
  111 ADDR = %LOC(PAR11)
      GO TO RETURN
  112 ADDR = %LOC(PAR12)
      GO TO RETURN
  113 ADDR = %LOC(PAR13)
      GO TO RETURN
  114 ADDR = %LOC(PAR14)
      GO TO RETURN
  115 ADDR = %LOC(PAR15)
      GO TO RETURN
  116 ADDR = %LOC(PAR16)
      GO TO RETURN
  117 ADDR = %LOC(PAR17)
      GO TO RETURN
  118 ADDR = %LOC(PAR18)
      GO TO RETURN
  119 ADDR = %LOC(PAR19)
      GO TO RETURN
  120 ADDR = %LOC(PAR20)
      GO TO RETURN
  121 ADDR = %LOC(PAR21)
      GO TO RETURN
  122 ADDR = %LOC(PAR22)
      GO TO RETURN
  999 RETURN
      END
