      LOGICAL FUNCTION INISAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization for SAMUS
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  27-SEP-1990   A.Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER IERR
C----------------------------------------------------------------------
      INISAM = .TRUE.
      IF ( DSAM .LE. 0 ) GOTO 999
C
C ****  Initialize the SAMUS geometry in the store /ZEBSTP/
C
      CALL SAISTP ( 'SAM_STPFILE', IERR )
      IF (IERR .NE. 0 ) THEN
        CALL ERRMSG ('D0GEANT','INISAM',
     &  'Error SAMUS geometry initialization','F')  
      ENDIF
C
  999 RETURN
      END
