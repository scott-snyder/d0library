      FUNCTION ISADMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       dump ISAJET event banks
C-
C-   Created   6-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISADMP
      CHARACTER*4 PATH
      INTEGER PRUNIT,DMPUNI,LESUM,GZESUM
      EXTERNAL PRCAEP
C----------------------------------------------------------------------
C
      PRUNIT=DMPUNI()
      CALL PRTEVZ(PRUNIT)
      LESUM=GZESUM('ISAE')
      IF(LESUM.GT.0) CALL PRESUM ( PRUNIT, LESUM, 0, 'ONE', 0)
      CALL PATHGT(PATH)
      IF(PATH.EQ.'FAKE') THEN
        PRUNIT=DMPUNI()
        WRITE(PRUNIT,100) PATH
        CALL DMPANY('CAEP',PRCAEP)
      ENDIF
      ISADMP=.TRUE.
  999 RETURN
  100 FORMAT(//,'***********  CAEP dumped for path= ',A)
      END
