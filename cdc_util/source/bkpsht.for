      SUBROUTINE BKPSHT(LPSHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book PSHT (preshower header) bank
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-OCT-1995   Hailin Li
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPSHT.LINK'
      INTEGER LPSHT,LUPGD,GZUPGD
      INTEGER MPPSHT(5)
      DATA MPPSHT/4HPSHT,1,1,1,1/
      SAVE MPPSHT
C----------------------------------------------------------------------
      LUPGD = GZUPGD()
      IF (LUPGD.LE.0) CALL BKUPGD(LUPGD)
      IF ( LUPGD .LE. 0 ) THEN
        CALL ERRMSG('Unable to book UPGD','BKPSHT',' ','W')
        LPSHT = 0
        RETURN
      ENDIF
C
C ****  Book PSHT bank
C
      CALL MZLIFT ( IXMAIN, LPSHT, LUPGD, -IZPSHT, MPPSHT, 0 )
      IF ( LPSHT .LE. 0 ) THEN
        CALL ERRMSG('Unable to book PSHT','BKPSHT',' ','W')
        LPSHT = 0
        RETURN
      ENDIF
C
      IQ(LPSHT + 1) = 1
  999 RETURN
      END
