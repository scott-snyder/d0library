      LOGICAL FUNCTION FFISAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize SAMUS FFREAD cards
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  20-SEP-1990   A.Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER I
      LOGICAL PRT_FFISAM
C----------------------------------------------------------------------
      FFISAM = .TRUE.
      IF ( DSAM .LE. 0 ) GOTO 999
C
      DO I = 1, 10
        SSAM(I) = 0.
      ENDDO
C
      CALL FFKEY('SSAM',SSAM,10,'REAL')
C
      ENTRY PRT_FFISAM
C
      PRT_FFISAM = .TRUE.
      WRITE (LOUT,9000) SSAM
 9000 FORMAT(' FFISAM ** SSAM ',10F6.1)
C
  999 RETURN
      END
