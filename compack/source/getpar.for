C&IF VAXVMS
      SUBROUTINE GETPAR (NUMPAR,PROMPT,TYPARR,PAR1,PAR2,PAR3,
     *             PAR4,PAR5,PAR6,PAR7,PAR8,PAR9,PAR10)
C&ELSE
C&      SUBROUTINE GETPAR (NUMPAR,PROMPT,TYPARR,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6,PAR7,PAR8,PAR9,PAR10,
C&     *           PAR11,PAR12,PAR13,PAR14,PAR15,PAR16,PAR17,
C&     *           PAR18,PAR19,PAR20,PAR21,PAR22)
C&ENDIF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a set of parameters for a command by prompting 
C-                         for each one separately.
C-
C-   Inputs :  NUMPAR: Number of parameters to get
C              PROMPT: Array of prompt strings for each parameter
C              TYPARR: Array of parameter types
C-   Outputs:  PAR1-PAR10:  Parameters to be filled.
C-   Controls: PF may be changed inside this routine
C-
C-     NOTE
C-
C-       To INCREASE the number of parameters possible, add PAR11 etc. here
C-       AND change MAXCOM in COMNUM.INC
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated   3-OCT-1991  Herbert Greenlee
C-      Added a machine dependent block to act as an interface to getdis77.
C-   Modified    13-AUG-1992 sss - compile on ibm
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
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
C&IF VAXVMS
      CHARACTER*80 CHARS,TRANUP,OUTSTR,CTEMP
      CHARACTER*2 BUFCOM
      REAL XIN,CONREA
      LOGICAL LIN,GETDEV,PARSET,FIRST
      INTEGER COMUSE,NUMUSE,CONHEX
      INTEGER I,J,TRULEN,IN,CONINT
      EQUIVALENCE (IN,XIN), (IN,LIN)
C&ELSE
C&      INTEGER PAR11,PAR12,PAR13,PAR14,PAR15,PAR16,PAR17,PAR18,PAR19
C&      INTEGER PAR20,PAR21,PAR22
C&      INTEGER LEN_PROMPT
C&      INTEGER I, J, NNUM, NCHAR
C&      INTEGER PAR(10), PTON(10), PTOC(10)
C&      CHARACTER*80 CPAR(10)
C&      INTEGER RETURN, ADDR
C&      INTEGER ADDNPAR(10), ADDCPAR(10), LENCPAR(10)
C&      INTEGER COMUSE,NUMUSE
C&      CHARACTER*2 BUFCOM
C&      INTEGER D0_LOC
C&ENDIF
C----------------------------------------------------------------------
C
C       Check that the number of parameters is valid
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
C&IF VAXVMS
C
C        Loop over parameters
C
      PF=0
      FIRST=.TRUE.
      DO I=1,NUMUSE
   99   CONTINUE
        IF(COMNUM.GE.COMUSE) THEN
          CHARS=COMPRT(COMUSE)
          IF(TYPARR(I).EQ.'A') THEN
            DO J=COMUSE+1,COMNUM
              CHARS=CHARS(1:TRULEN(CHARS))//' '//
     *                   COMPRT(J)(1:TRULEN(COMPRT(J)))
            ENDDO
            COMNUM=0
          ELSE
            COMUSE=COMUSE+1
          ENDIF
          PARSET=.FALSE.         ! Parameter should NOT be written to command file
        ELSE
          IF(FIRST) THEN
            CALL OUTMSG(' ')                      ! PUT IN A BLANK LINE FIRST TIME AROUND
            FIRST=.FALSE.
          ENDIF
          CALL README(CHARS,PF,GETDEV(),PROMPT(I)
     *                    (1:TRULEN(PROMPT(I)))//' ')
          IF(PF.NE.0) THEN
            GOTO 1000
          ENDIF
          PARSET=.TRUE.
        ENDIF
        IF(TYPARR(I).EQ.'R') THEN
          IF(TRULEN(CHARS).GT.0) THEN
            XIN=CONREA(CHARS)
            IF(XIN.EQ.-999999.99) GOTO 99
            GOTO (101,102,103,104,105,106,107,108,109,110), I
  101       PAR1=IN
            GOTO 120
  102       PAR2=IN
            GOTO 120
  103       PAR3=IN
            GOTO 120
  104       PAR4=IN
            GOTO 120
  105       PAR5=IN
            GOTO 120
  106       PAR6=IN
            GOTO 120
  107       PAR7=IN
            GOTO 120
  108       PAR8=IN
            GOTO 120
  109       PAR9=IN
            GOTO 120
  110       PAR10=IN
            GOTO 120
  120       CONTINUE
          ENDIF
          IF((SETUP.OR.LOGUP).AND.PARSET.AND..NOT.ASTFLG) THEN
            WRITE(COMUNI,2) CHARS(1:TRULEN(CHARS)),PROMPT(I)
    2       FORMAT(A,'    !-<',A)
          ENDIF
        ELSEIF(TYPARR(I).EQ.'I'.OR.TYPARR(I).EQ.'H') THEN
          IF(TRULEN(CHARS).GT.0) THEN
            IF(TYPARR(I).EQ.'I') THEN
              IN=CONINT(CHARS)
              IF(IN.EQ.9999999) GOTO 99
            ELSEIF(TYPARR(I).EQ.'H') THEN
              IN=CONHEX(CHARS)
              IF(IN.EQ.-1) GOTO 99
            ENDIF
            GOTO (201,202,203,204,205,206,207,208,209,210), I
  201       PAR1=IN
            GOTO 220
  202       PAR2=IN
            GOTO 220
  203       PAR3=IN
            GOTO 220
  204       PAR4=IN
            GOTO 220
  205       PAR5=IN
            GOTO 220
  206       PAR6=IN
            GOTO 220
  207       PAR7=IN
            GOTO 220
  208       PAR8=IN
            GOTO 220
  209       PAR9=IN
            GOTO 220
  210       PAR10=IN
            GOTO 220
  220       CONTINUE
          ENDIF
          IF((SETUP.OR.LOGUP).AND.PARSET.AND..NOT.ASTFLG) THEN
            WRITE(COMUNI,2) CHARS(1:TRULEN(CHARS)),PROMPT(I)
          ENDIF
        ELSEIF(TYPARR(I).EQ.'L') THEN
          IF((SETUP.OR.LOGUP).AND.PARSET.AND..NOT.ASTFLG) THEN
            WRITE(COMUNI,2) CHARS(1:TRULEN(CHARS)),PROMPT(I)
          ENDIF
          IF(TRULEN(CHARS).GT.0) THEN
            CHARS=TRANUP(CHARS)
            DO 100 J=1,TRULEN(CHARS)
              IF (CHARS(1:1).EQ.' ') CHARS=CHARS(2:)
  100       CONTINUE
C
C       Assume that T for .true. or Y for 'yes' are the only valid .TRUE. codes.
C
            IF(CHARS(1:1).EQ.'T'.OR.CHARS(1:1).EQ.'Y') THEN
              LIN=.TRUE.
            ELSE
              LIN=.FALSE.
            ENDIF
            GOTO (301,302,303,304,305,306,307,308,309,310), I
  301       PAR1=IN
            GOTO 320
  302       PAR2=IN
            GOTO 320
  303       PAR3=IN
            GOTO 320
  304       PAR4=IN
            GOTO 320
  305       PAR5=IN
            GOTO 320
  306       PAR6=IN
            GOTO 320
  307       PAR7=IN
            GOTO 320
  308       PAR8=IN
            GOTO 320
  309       PAR9=IN
            GOTO 320
  310       PAR10=IN
            GOTO 320
  320       CONTINUE
          ENDIF
        ELSEIF(TYPARR(I).EQ.'C'.OR.TYPARR(I).EQ.'A'.OR.
     *       TYPARR(I).EQ.'U') THEN
          IF(TYPARR(I).EQ.'U') THEN   ! ONLY WANT UPPERCASE
            CHARS=TRANUP(CHARS)
          ENDIF
          IF((SETUP.OR.LOGUP).AND.PARSET.AND..NOT.ASTFLG) THEN
            WRITE(COMUNI,2) CHARS(1:TRULEN(CHARS)),PROMPT(I)
          ENDIF
          GOTO (401,402,403,404,405,406,407,408,409,410), I
  401     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR1)
          GOTO 420
  402     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR2)
          GOTO 420
  403     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR3)
          GOTO 420
  404     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR4)
          GOTO 420
  405     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR5)
          GOTO 420
  406     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR6)
          GOTO 420
  407     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR7)
          GOTO 420
  408     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR8)
          GOTO 420
  409     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR9)
          GOTO 420
  410     CALL MOVSTR(CHARS(1:TRULEN(CHARS)),PAR10)
          GOTO 420
  420     CONTINUE
        ENDIF
      ENDDO
 1000 CONTINUE
C&ELSE
C&C-
C&C-  Construct mapping of parameter number to array index for numeric and 
C&C-  character parameters.  Copy parameters.
C&C-
C&      NNUM = 0
C&      NCHAR = 0
C&      DO 50 I = 1, NUMUSE
C&        IF(TYPARR(I).EQ.'I' .OR. TYPARR(I).EQ.'R' .OR. 
C&     &    TYPARR(I).EQ.'L' .OR. TYPARR(I).EQ.'H')THEN
C&          NNUM = NNUM + 1
C&          PTON(I) = NNUM
C&          J = I
C&          ASSIGN 10 TO RETURN
C&          GO TO 100
C& 10       CONTINUE
C&          ADDNPAR(I) = ADDR
C&          CALL MOVINT(%VAL(ADDR), PAR(NNUM))
C&        ELSE
C&          NCHAR = NCHAR + 1
C&          PTOC(I) = NCHAR
C&          J = NCHAR + NUMUSE + 2
C&          ASSIGN 20 TO RETURN
C&          GO TO 100
C& 20       CONTINUE
C&          LENCPAR(I) = ADDR
C&          J = I
C&          ASSIGN 30 TO RETURN
C&          GO TO 100
C& 30       CONTINUE
C&          ADDCPAR(I) = ADDR
C&          CALL MOVSTR(%VAL(ADDR), CPAR(NCHAR), %VAL(LENCPAR(I)))
C&        ENDIF
C& 50   CONTINUE
C&C-
C&C- Get passed length of PROMPT
C&C-
C&      J = NUMUSE + 1
C&      ASSIGN 60 TO RETURN
C&      GO TO 100
C& 60   CONTINUE
C&      LEN_PROMPT = ADDR
C&C-
C&C- Interact with user
C&C-
C&      CALL GETPAR77(NUMPAR, %REF(PROMPT), %REF(TYPARR),
C&     &  PTON, PTOC, PAR, %REF(CPAR), %VAL(LEN_PROMPT), %VAL(1),
C&     &  %VAL(LEN(CPAR(1))))
C&C-
C&C- Copy parameters back to calling program
C&C-
C&      DO 90 I=1,NUMUSE
C&        IF(TYPARR(I).EQ.'I' .OR. TYPARR(I).EQ.'R' .OR.
C&     &    TYPARR(I).EQ.'L' .OR. TYPARR(I).EQ.'H')THEN
C&          CALL MOVINT(PAR(PTON(I)), %VAL(ADDNPAR(I)))
C&        ELSE
C&          CALL MOVSTR(%REF(CPAR(PTOC(I))), %VAL(ADDCPAR(I)), 
C&     &      %VAL(LEN(CPAR(PTOC(I)))), %VAL(LENCPAR(I)))
C&        ENDIF
C& 90   CONTINUE
C&      GO TO 999    
C&C-
C&C- The following code is a "subroutine" to return the address of parameter
C&C- J (1-22) in the variable ADDR.
C& 100  CONTINUE
C&      GO TO (101,102,103,104,105,106,107,108,109,110,
C&     &       111,112,113,114,115,116,117,118,119,120,
C&     &       121,122), J
C& 101  ADDR = D0_LOC(PAR1)
C&      GO TO RETURN
C& 102  ADDR = D0_LOC(PAR2)
C&      GO TO RETURN
C& 103  ADDR = D0_LOC(PAR3)
C&      GO TO RETURN
C& 104  ADDR = D0_LOC(PAR4)
C&      GO TO RETURN
C& 105  ADDR = D0_LOC(PAR5)
C&      GO TO RETURN
C& 106  ADDR = D0_LOC(PAR6)
C&      GO TO RETURN
C& 107  ADDR = D0_LOC(PAR7)
C&      GO TO RETURN
C& 108  ADDR = D0_LOC(PAR8)
C&      GO TO RETURN
C& 109  ADDR = D0_LOC(PAR9)
C&      GO TO RETURN
C& 110  ADDR = D0_LOC(PAR10)
C&      GO TO RETURN
C& 111  ADDR = D0_LOC(PAR11)
C&      GO TO RETURN
C& 112  ADDR = D0_LOC(PAR12)
C&      GO TO RETURN
C& 113  ADDR = D0_LOC(PAR13)
C&      GO TO RETURN
C& 114  ADDR = D0_LOC(PAR14)
C&      GO TO RETURN
C& 115  ADDR = D0_LOC(PAR15)
C&      GO TO RETURN
C& 116  ADDR = D0_LOC(PAR16)
C&      GO TO RETURN
C& 117  ADDR = D0_LOC(PAR17)
C&      GO TO RETURN
C& 118  ADDR = D0_LOC(PAR18)
C&      GO TO RETURN
C& 119  ADDR = D0_LOC(PAR19)
C&      GO TO RETURN
C& 120  ADDR = D0_LOC(PAR20)
C&      GO TO RETURN
C& 121  ADDR = D0_LOC(PAR21)
C&      GO TO RETURN
C& 122  ADDR = D0_LOC(PAR22)
C&      GO TO RETURN
C&ENDIF
 999  RETURN
      END
