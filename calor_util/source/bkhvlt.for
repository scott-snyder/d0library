C DEC/CMS REPLACEMENT HISTORY, Element BKHVLT.FOR
C *2     3-MAY-1990 16:22:53 DEMARTEAU "Booking routines"
C *1     8-MAR-1990 21:56:18 FUESS "Zebra routines for TB90"
C DEC/CMS REPLACEMENT HISTORY, Element BKHVLT.FOR
      SUBROUTINE BKHVLT(NCH,LHVLT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books bank HVLT for high voltage info
C-
C-   Inputs  : NCH   = Number of channels
C-   Outputs : LHVLT = Address of created bank HVLT
C-
C-   Created   5-MAR-1990   Marcel Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZHVLT.LINK/LIST'
      INTEGER NCH,NALOC
      INTEGER LHVLT, IOHVLT, LTRUN, GZTRUN 
      INTEGER NLNKS
      PARAMETER (NLNKS=1)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        CALL MZFORM('HVLT','-I',IOHVLT)   ! format for HVLT
        FIRST=.FALSE.
      ENDIF
C
      LTRUN=GZTRUN()
      IF ( LTRUN.LE.0 ) CALL BKTRUN(LTRUN)
      LHVLT=LQ(LTRUN-IZHVLT)
      IF (LHVLT .NE. 0) GOTO 999        ! return if HVLT exists already
C
C
C
C **** Create HVLT bank
C
      NALOC=NCH+2
      CALL MZBOOK(IXMAIN,LHVLT,LTRUN,-IZHVLT,
     +            'HVLT',NLNKS,NLNKS,NALOC,IOHVLT,-1)
C
      IQ(LHVLT+1) = 1           ! version number
      IQ(LHVLT+2) = NCH         ! number of channels 
C
  999 RETURN
      END
