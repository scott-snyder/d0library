      SUBROUTINE PXWRITE_PARAM
     &  (LUN,PARAM,LPAR,IVAL,CVAL,LCVAL,REM,LREM,TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out a PIXIE parameter.
C-
C-   Inputs  : LUN      [I]     Output unit number
C-             PARAM    [C*]    Name of parameter
C-             LPAR     [I]     Length of PARAM string
C-             IVAL     [I]     Value (uses equivalences)
C-             CVAL     [C*]    Value (character)
C-             LCVAL    [I]     Length of CVAL
C-             REM      [C*]    Value (character)
C-             LREM     [I]     Length of REM
C-             TYPE     [I]     Value type
C-   Outputs : None
C-
C-   Created  22-AUG-1990   Lupe Howell
C-   Updated  14-SEP-1990   Harrison B. Prosper  
C-      Took code from WRITE_RCP 
C-   Updated  11-JUL-1991   Lupe Howell  The format for NAME was adjusted so it
C-   can include %ACTION(s) and %TITLE
C-   Updated  26-NOV-1991   Lupe Howell  Maximum size for the remark adjusted 
C-   Updated  27-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN
      CHARACTER*(*) PARAM
      INTEGER LPAR
      INTEGER IVAL
      CHARACTER*(*) CVAL
      INTEGER LCVAL
      CHARACTER*(*) REM
      INTEGER LREM
      INTEGER TYPE
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      CHARACTER*42 PARANAME,CNUMBER,REMARK
      CHARACTER*80 LINE
      CHARACTER*81 TEMP
C
      LOGICAL FIRSTFMT
C
      INTEGER IIVAL
      REAL    RRVAL
      LOGICAL LLVAL
      EQUIVALENCE(IIVAL,RRVAL,LLVAL)
C
      INTEGER LENGTH, I, J,NUM,LR
      LOGICAL SMALL
C
      CHARACTER*1 QUOTE
      PARAMETER( QUOTE = '''' )
C----------------------------------------------------------------------
      PARANAME = PARAM(1:LPAR)
      REMARK   = REM(1:LREM)
      IIVAL    = IVAL
C
C ****  Convert value to a character string
C
      SMALL = .TRUE.
      IF     ( TYPE .EQ. VTINT ) THEN
        WRITE(UNIT=CNUMBER,FMT='(I10,14X)')  IIVAL
      ELSEIF ( TYPE .EQ. VTLOG ) THEN
        IF ( LLVAL ) THEN
        WRITE(UNIT=CNUMBER,FMT='(5X,A5,14X)') ' TRUE'
        ELSE
        WRITE(UNIT=CNUMBER,FMT='(5X,A5,14X)') 'FALSE'
        ENDIF
      ELSEIF ( TYPE .EQ. VTREAL ) THEN
        WRITE(UNIT=CNUMBER,FMT='(F10.4,14X)') RRVAL
      ELSEIF ( TYPE .GT. VTCHR ) THEN
        CNUMBER = CVAL(1:LCVAL)
      ELSE
        CNUMBER = ' '
      ENDIF
C
C ****  Checking the type of parameter to determine the
C ****  appropriate format 
C
      FIRSTFMT = .FALSE.
      IF ( (PARAM(1:4) .EQ. 'NAME') .OR. 
     &     (PARAM(1:7) .EQ. '%ACTION').OR.
     &     (PARAM(1:6) .EQ. '%TITLE' ) )THEN
        FIRSTFMT = .TRUE.
      ENDIF
C
C ****  Insert quotations marks around parameter names and
C ****  remarks, and also values when it is a character value
C
      IF( ( LREM .GT. 40 ) .AND. ( FIRSTFMT ) )THEN
        LR = 40
      ELSEIF( ( LREM .GT. 34) .AND. ( .NOT. FIRSTFMT ) ) THEN
        LR = 34
      ELSE
        LR = LREM
      ENDIF
      PARANAME = QUOTE//PARANAME(1:LPAR)//QUOTE
      REMARK   = QUOTE//REMARK(1:LR)//QUOTE
      IF ( TYPE .GT. VTCHR ) THEN       ! Character
        CNUMBER= QUOTE//CNUMBER(1:LCVAL)//QUOTE
      ENDIF
C
C ****  Write the parameter
C
      LINE = ' '
      IF ( FIRSTFMT ) THEN
        WRITE(LINE,200) PARANAME,CNUMBER,REMARK
      ELSE
        WRITE(LINE,300) PARANAME,CNUMBER,REMARK
      ENDIF
      IF ( LUN .GT. 0 ) THEN
        WRITE(LUN,100) LINE
      ELSE
        TEMP = ' '//LINE
        CALL INTMSG(TEMP)
      ENDIF
C
  999 RETURN
C
C ****  FORMATS
C
  100 FORMAT(A)
  200 FORMAT(1X,A9 ,1X,A24,2X,A42)
  300 FORMAT(1X,A20,1X,A20,2X,A36)
      END
