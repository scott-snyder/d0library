C------------------------------------------------------------------------
      LOGICAL FUNCTION GOOD_PARTICLE(LBANK,CNAME)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Test a particle bank to see if it is contained
C-     in the list of good particles found by PARTICLE_SELECT.
C-
C-   Inputs  : CNAME - Name of selection (from PARTICLE_SELECT,RCP)
C-             LBANK - Index of bank to check
C-   Outputs : None
C-   Controls:
C-
C-   Created   2-Feb-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      CHARACTER*8 CNAME
      INTEGER     LBANK
      INTEGER     NPMAX
      PARAMETER(NPMAX=50)
      INTEGER NGOOD,GOOD_LINKS(NPMAX),J
C-----------------------------------------------------------------------
      GOOD_PARTICLE=.FALSE.
      NGOOD=0
      CALL GTSLINK(CNAME,NPMAX,NGOOD,GOOD_LINKS)
      IF( NGOOD.GT.0 ) THEN
        DO J=1,NGOOD
          IF( LBANK.EQ.GOOD_LINKS(J) ) THEN
            GOOD_PARTICLE=.TRUE.
            GOTO 999
          ENDIF
        ENDDO
      ENDIF  
C
 999  RETURN
      END
