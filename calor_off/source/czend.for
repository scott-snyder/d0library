      SUBROUTINE CZEND (INUNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Terminate and close a ZEBRA input file and
C-                         release the unit number.
C-
C-   Inputs  : INUNIT           Unit number to be closed and released
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-JAN-1989   Harrison B. Prosper, John Womersley
C-   Updated   6-FEB-1990   Harrison B. Prosper
C-      Added ERR=900 in CLOSE command 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUNIT,IER
      INCLUDE 'D0$PARAMS:CALID.DEF'
C----------------------------------------------------------------------
      CALL FZENDI (INUNIT,'TU')
      CLOSE (UNIT=INUNIT,ERR=900)
      CALL RLUNIT (87,INUNIT,IER)
      IF(IER.NE.0) GOTO 900
      GOTO 999
C
C ****  Error in closing
C
  900 CONTINUE
      CALL ERRMSG('CALOR_OFF','CZEND',
     &  'Error in closing datafile','W')
      CALL RLUNIT (CALID,INUNIT,IER)
  999 RETURN
      END
