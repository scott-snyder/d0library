      SUBROUTINE GTMSHT( ITRAK, NMSCT, IMSCT ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get MSHT bank contents
C-
C-   Inputs  : ITRAK : MUOT track ID
C-   Outputs : NMSCT : number of scinti hit belong the track
C-             IMSCT : scinti hit pointer in MSCT
C-   Controls: None
C-
C-   Created  22-FEB-1994   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ITRAK, NMSCT, IMSCT(*)
C-- includes
      INCLUDE 'D0$INC:ZEBCOM.INC'
C_- Local
      INTEGER LM, I, GZMSHT
C----------------------------------------------------------------------
C
      NMSCT = 0
      LM = GZMSHT(ITRAK)
      IF ( LM.EQ.0 ) GOTO 999
C
      DO I=1,4
        IF ( IQ(LM+I).NE.0 ) THEN
          NMSCT = NMSCT + 1
          IMSCT(NMSCT) = IQ(LM+I) 
        END IF
      END DO
C
  999 RETURN
      END
