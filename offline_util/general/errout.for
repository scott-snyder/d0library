C DEC/CMS REPLACEMENT HISTORY, Element ERROUT.FOR
C *1    11-MAY-1988 10:47:34 HARRY "COMPACK routine to display error code in upper window"
C DEC/CMS REPLACEMENT HISTORY, Element ERROUT.FOR
      SUBROUTINE ERROUT (LUN,text,IOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write error message and number to unit LUN 
C-                         if LUN > 0, or to screen using INTMSG 
C-                         if LUN < 1.
C-
C-   Inputs  : LUN       Unit number (If zero ---> write with INTMSG)
C-             Text      Text to be displayed
C-             IOS       Error number (usually from ZEBRA)
C-   Outputs : None
C-
C-   Created  17-FEB-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER         LUN,IOS,N,K
      CHARACTER*(*)   Text
      CHARACTER*2     A
      CHARACTER*16    FORM
      CHARACTER*80    string
C----------------------------------------------------------------------
C
      N = LEN (TEXT)
      WRITE(UNIT=A,FMT='(I2)') N
      IF ( N.LE.9 ) THEN
        K = 2
      ELSE
        K = 1
      END IF
      FORM = '(1X,A'//A(K:2)//',I2)'
      WRITE (UNIT=string,FMT=FORM) Text(1:N),IOS
C
      IF ( LUN.LE.0 ) THEN
        CALL INTMSG (string)
      ELSE
        WRITE(UNIT=LUN,FMT='(A80)') string
      ENDIF
C
  999 RETURN
      END
