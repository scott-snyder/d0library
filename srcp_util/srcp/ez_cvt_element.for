      SUBROUTINE EZ_CVT_ELEMENT
     &  (PARAM,LPAR,IVAL,CVAL,LCVAL,REM,LREM,ITYPE,RECORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert an array element with the format:
C-       Name      Value      Remark
C-        :         :           :
C-   to a character string.
C-
C-   Inputs  : PARAM    [C*]    Name of parameter
C-             LPAR     [I]     Length of PARAM string
C-             IVAL     [I]     Value (uses equivalences)
C-             CVAL     [C*]    Value (character)
C-             LCVAL    [I]     Length of CVAL
C-             REM      [C*]    Value (character)
C-             LREM     [I]     Length of REM
C-             ITYPE    [I]     Value type
C-
C-   Outputs : RECORD   [C*]    Character string
C-
C-   Created 30-MAY-1991   Lupe Howell   Harrison B. Prosper
C-   Updated  7-AUG-1991   Lupe Howell  The format for NAME was adjusted so it
C-   can include %ACTION(s) and %TITLE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      INTEGER LPAR
      INTEGER IVAL
      CHARACTER*(*) CVAL
      INTEGER LCVAL
      CHARACTER*(*) REM
      INTEGER LREM
      INTEGER ITYPE
      CHARACTER*(*) RECORD
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      CHARACTER*42 PARANAME,CNUMBER,REMARK
      CHARACTER*132 LINE
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
      IF     ( ITYPE .EQ. VTINT ) THEN
        WRITE(UNIT=CNUMBER,FMT='(I10,14X)')  IIVAL
      ELSEIF ( ITYPE .EQ. VTLOG ) THEN
        IF ( LLVAL ) THEN
        WRITE(UNIT=CNUMBER,FMT='(5X,A5,14X)') ' TRUE'
        ELSE
        WRITE(UNIT=CNUMBER,FMT='(5X,A5,14X)') 'FALSE'
        ENDIF
      ELSEIF ( ITYPE .EQ. VTREAL ) THEN
        WRITE(UNIT=CNUMBER,FMT='(F10.4,14X)') RRVAL
      ELSEIF ( ITYPE .GT. VTCHR ) THEN
        CNUMBER = CVAL(1:LCVAL)
      ELSE
        CNUMBER = ' '
      ENDIF
C
C ****  Insert quotations marks around parameter names and
C ****  remarks, and also values when it is a character value
C
      IF ( LREM .GT. 40 ) THEN
        LR = 40
      ELSE
        LR = LREM
      ENDIF
      PARANAME = QUOTE//PARANAME(1:LPAR)//QUOTE
      REMARK   = QUOTE//REMARK(1:LR)//QUOTE
      IF ( ITYPE .GT. VTCHR ) THEN       ! Character
        CNUMBER= QUOTE//CNUMBER(1:LCVAL)//QUOTE
      ENDIF
C
C ****  Write the parameter
C
      LINE = ' '
      IF ( ( PARAM(1:4) .EQ. 'NAME' ) .OR.
     &     ( PARAM(1:7) .EQ. '%ACTION').OR.
     &      (PARAM(1:6) .EQ. '%TITLE' ) )THEN
        WRITE(LINE,200) PARANAME,CNUMBER,REMARK
      ELSE
        WRITE(LINE,300) PARANAME,CNUMBER,REMARK
      ENDIF
      RECORD = LINE
C
  999 RETURN
C
C ****  FORMATS
C
  100 FORMAT(A)
  200 FORMAT(1X,A9 ,1X,A24,2X,A42)
  300 FORMAT(1X,A20,1X,A20,2X,A36)
      END
