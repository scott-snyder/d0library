      SUBROUTINE GETPAR77 (NUMPAR,PROMPT,TYPARR,PTON,PTOC,PAR,CPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a set of parameters for a command by prompting
C-                         for each one separately.  It will pop a window to
C-                         print the message and get the input.
C-
C-                         This is a fortran-77 version of GETPAR.
C-                         Parameters are returned in either PAR or CPAR,
C-                         depending on type.  The calling program must
C-                         supply the mapping from parameter number to
C-                         array index.
C-
C-   Inputs : NUMPAR     [I]: Number of parameters to get
C-            PROMPT [C*(*)]: Array of prompt strings for each parameter
C-            TYPARR [C*(*)]: Array of parameter types
C-            PTON    [I(*)]: Mapping of parameter number to numeric 
C-                            array index.
C-            PTOC    [I(*)]: Mapping of parameter number to character 
C-                            array index.
C-
C-   Outputs: PAR     [I(*)]: Array of numeric parameters to be filled.
C-            CPAR   [C*(*)]:Array of character parameters to be filled.
C-
C-   Controls: PF may be changed inside this routine
C-
C-   Created  16-JUN-1992   Lupe Howell
C-              Based on GETPAR77 by Herbert Greenlee and
C-              old SGI_GETPAR by Mike Shupe
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR
      CHARACTER*(*) PROMPT(*)
      CHARACTER*1 TYPARR(*)
      INTEGER PAR(*)
      CHARACTER*(*) CPAR(*)
      INTEGER PTON(*), PTOC(*)
C
      INCLUDE 'fgl.h'
      INCLUDE 'fdevice.h'
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
C
      CHARACTER*80 CHARS,TRANUP,OUTSTR,CTEMP
      CHARACTER*2 BUFCOM
      REAL XIN,CONREA
      LOGICAL LIN,GETDEV,PARSET,FIRST
      INTEGER COMUSE,NUMUSE,CONHEX,NREAD,LNG
      INTEGER I,J,TRULEN,IN,CONINT,IWOLD,IWTEXT,ISCRX
      EQUIVALENCE (IN,XIN), (IN,LIN)
      REAL CX,CY
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
C *** Set up window location
C
      IWOLD=WINGET()
      ISCRX=GETGDE(GDXPMA)
      CALL PREFPO(ISCRX-700,ISCRX-5,80,5)
C
C *** Open Window
C
      IWTEXT = WINOPE('Input Parameter',15)
      CALL WINTIT('Input Parameter',15)
C
C *** Set Output to Window
C
      CALL WINSET(IWTEXT)
      CALL CLKON
      CALL QDEVIC(KEYBD)
      CALL QDEVIC(RIGHTM)
C
C ***  Reset the input queue
C
      CALL QRESET()
C
C ***  Write string
C
      CX=10.
      CY=60.
      CALL FRONTB(.TRUE.)
C
C *** Loop over parameters
C
      PF=0
      FIRST=.TRUE.
      DO I=1,NUMUSE
        CALL COLOR(0)

C *** ERASE NEW WINDOW TO BLACK
C
        CALL CLEAR
        CALL COLOR(YELLOW)
C
C *** TEXT IN YELLOW
C
        CALL CMOV2(CX,CY)
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
          PARSET=.FALSE.
        ELSE
          IF(FIRST) THEN
            CALL OUTMSG(' ')
            FIRST=.FALSE.
          ENDIF
          LNG = TRULEN(PROMPT(I))+1
          CTEMP = PROMPT(I)(1:TRULEN(PROMPT(I)))//' '
          CALL CHARST(CTEMP,LNG)
          CALL CHARST('  ',2)
C
C *** INPUT IN GREEN
C
   10     CALL COLOR(5)
C
C *** Read string
C
          CALL RDSTRN(CHARS,80,NREAD,YELLOW,BLACK)
          PARSET=.TRUE.
        ENDIF
        IF(TYPARR(I).EQ.'R') THEN
          IF(TRULEN(CHARS).GT.0) THEN
            XIN=CONREA(CHARS)
            IF(XIN.EQ.-999999.99) GOTO 99
            PAR(PTON(I)) = IN
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
            PAR(PTON(I)) = IN
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
C ***  Assume that T for .true. or Y for 'yes' are the only valid .TRUE. codes.
C
            IF(CHARS(1:1).EQ.'T'.OR.CHARS(1:1).EQ.'Y') THEN
              LIN=.TRUE.
            ELSE
              LIN=.FALSE.
            ENDIF
            PAR(PTON(I)) = IN
  320       CONTINUE
          ENDIF
        ELSEIF(TYPARR(I).EQ.'C'.OR.TYPARR(I).EQ.'A'.OR.
     *       TYPARR(I).EQ.'U') THEN
          IF(TYPARR(I).EQ.'U') THEN
            CHARS=TRANUP(CHARS)
          ENDIF
          IF((SETUP.OR.LOGUP).AND.PARSET.AND..NOT.ASTFLG) THEN
            WRITE(COMUNI,2) CHARS(1:TRULEN(CHARS)),PROMPT(I)
          ENDIF
          CPAR(PTOC(I)) = CHARS
  420     CONTINUE
        ENDIF
      ENDDO
C
C ***  Close Window
C
      CALL CLKOFF
C
C ***  Close Text Window
C
      CALL WINCLO(IWTEXT)
      IF(IWOLD.GE.0) CALL WINSET(IWOLD)
 1000 CONTINUE
      RETURN
      END
