C DEC/CMS REPLACEMENT HISTORY, Element LASTFDCSTP.FOR
C *2    15-SEP-1992 10:57:49 AVERY "Put in extra check on LSFDC"
C *1    10-SEP-1992 10:14:23 AVERY "updates for FDC"
C DEC/CMS REPLACEMENT HISTORY, Element LASTFDCSTP.FOR
      FUNCTION LASTFDCSTP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns the last (most recent) fdc stpfile version
C-
C-   Returned value  : LASTFDCSTP= last (most recent) fdc stpfile version
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created  17-AUG-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LASTFDCSTP
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFGEH.LINK'
      INTEGER GZSFDC 
C----------------------------------------------------------------------
      LASTFDCSTP=0
      LSFDC = GZSFDC() 
      IF (LSFDC.GT.0) THEN 
        LFGEH = LC(LSFDC-IZFGEH)   
        IF ( LFGEH.GT.0 ) THEN
          LASTFDCSTP = IC(LFGEH-5)
        END IF   
      END IF 
  999 RETURN
      END
