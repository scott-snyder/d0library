      SUBROUTINE CLBRUN(NCRT,CRATES,PEDRUN,GAINRUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the run number for the pedestal and
C-                           pulser runs
C-
C-   Inputs  : NCRT - Number of ADC crates being read
C-             CRATES = Array containing the ADC crates to be read
C-                      If NCRT=12, then assumes all 12 D0 ADC crates
C-   Outputs : PEDRUN - Array of run numbers for ped run/crate (0-11)
C-             GAINRUN - Array of run numbers for ped run/crate (0-11)
C-   Controls:
C-
C-   Created  26-JUN-1990   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
C
      INTEGER BNKNUM1,BNKNUM2
      INTEGER CRT,CRTID,NCRT,CRATES(*)
      INTEGER LZFIND,LBANK
      INTEGER PEDRUN(0:11),GAINRUN(0:11)
      PARAMETER (BNKNUM1 = 7)
      PARAMETER (BNKNUM2 = 8)
C----------------------------------------------------------------------
      LCPDH = LC(LSCAL-IZCPDH)
      LCGNH = LC(LSCAL-IZCGNH)
C
      DO CRT = 0,NCRT-1
        PEDRUN(CRT) = 0
        GAINRUN(CRT) = 0
        IF (NCRT.EQ. 12) THEN
          IF(CRT.LT.6)THEN
            CRTID = 10*CRT + BNKNUM1
          ELSE
            CRTID = 10*(CRT-6) + BNKNUM2
          ENDIF
        ELSE
          CRTID = CRATES(CRT+1)
        ENDIF
        LBANK = LZFIND(IXSTP,LCPDH,CRTID,9)
        PEDRUN(CRT) = IC(LBANK+6)
        LBANK = LZFIND(IXSTP,LCGNH,CRTID,9)
        GAINRUN(CRT) = IC(LBANK+6)
      ENDDO
  999 RETURN
      END
