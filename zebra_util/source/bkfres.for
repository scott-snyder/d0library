      SUBROUTINE BKFRES(LFRES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Lift FRES bank if needed
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  28-SEP-1990  Rich Astur
C-   Updated   6-MAR-1992   James T. Linnemann  add info on last tool in script 
C-   Updated  18-JUN-1994   Lewis Taylor Goss--changed NL, NS from 15 to 20
C-   Updated  13-MAY-1995   James T. Linnemann  NL,NS from 20 to 25 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LFRES,LFILT,GZFILT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFRES.LINK'
C----------------------------------------------------------------------
      LFILT = GZFILT()
      IF (LFILT .LE. 0) CALL BKFILT(LFILT)
      LFRES = LQ( LFILT - IZFRES)
      IF ( LFRES .EQ. 0 ) THEN
        CALL MZBOOK(IXMAIN,LFRES,LFILT,-IZFRES,'FRES',25,25,65,2,0)
        IQ(LFRES+1) = 2                 ! version number
      END IF
  999 RETURN
      END
