      INTEGER FUNCTION GZC2EM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get link to C2EM bank
C-
C-   Returned value  : Link to first C2EM bank
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  29-APR-1993   James T. McKinley
C-   Modified  7-JUN-1993   James T. McKinley - make sure FILT bank version
C-                          is .GE. 5 so it has a link to C2EM, otherwise 
C-                          return 0.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZC2EM.LINK/LIST'
      INTEGER LFILT,GZFILT
C----------------------------------------------------------------------
C
C--   initialize
      GZC2EM = 0
C
C--   get link to supporting FILT bank
      LFILT = GZFILT()
C
      IF(LFILT.NE.0)THEN    ! check version .GE. 5
        IF(IQ(LFILT+1).GE.5) GZC2EM = LQ(LFILT - IZC2EM)
      ENDIF
C
  999 RETURN
      END
