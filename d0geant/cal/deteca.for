      SUBROUTINE DETECA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Defines hit and digitization parameters
C-                         for the end cap calorimeter
C-
C-   Inputs  :None
C-   Outputs :None
C-   Controls:None
C-
C-   Created  28-Oct-1985 Stephan Linn
C-   Updated  20-Nov-1985 Rajendran Raja
C-   Updated  14-SEP-1988 Rajendran Raja
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INTEGER ISET,IDET
C
C----------------------------------------------------------------------
C
        CALL SETDET('IUSET_END_CALORIMETER+Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_END_MASSLESS_GAPS+Z',SCAL,ISET,IDET)
C
        CALL SETDET('IUSET_END_CALORIMETER-Z',SCAL,ISET,IDET)
        CALL SETDET('IUSET_END_MASSLESS_GAPS-Z',SCAL,ISET,IDET)
C
      END
