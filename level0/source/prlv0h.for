      SUBROUTINE PRLV0H(PRUNIT,LJLV0H,NLV0H,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints out LV0H (Level 0 Detector hits Header)
C-                         bank
C-
C-   Inputs  : PRUNIT = unit number for printout
C-             LKLV0H = bank address
C-             NLV0H  = bank number
C-             CFL    = only one bank per path (not necessary)
C-             IFL    = level of printout
C-                 IFL  = 0 no printout
C-                 IFL  = 1 prints number of hits in LV0
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER PRUNIT, NLV0H, IFL, LKLV0H, LJLV0H
      INTEGER NSHORT, NALL,IVERS
      INTEGER CBUNCH, NBUNCH
      INTEGER GZLV0H
      EXTERNAL GZLV0H
C
      REAL DSHORT
C
      CHARACTER*3 CFL
C----------------------------------------------------------------------
C
      IF ( IFL.LE.0 ) GOTO 999
      LKLV0H=LJLV0H
      IF ( LKLV0H.LE.0) LKLV0H=GZLV0H()
      IF ( LKLV0H.LE.0 ) THEN
        WRITE (PRUNIT,201) LKLV0H
        GOTO 999
      ENDIF
C
      IVERS=IBITS(IQ(LKLV0H),13,5)
      CALL GTLV0H(CBUNCH,NBUNCH)
      IF ( IFL.GE.1) WRITE(PRUNIT,101) IVERS, CBUNCH, NBUNCH
C
  101 FORMAT(/' Level 0 Detector Hits Header LV0H - Version',I3/
     $       '  Correct bunch =',I3, '  Number of bunches =',I3)
  201 FORMAT(/' WRONG ADDRESS, LKLV0H = ',I10)
C----------------------------------------------------------------------
  999 RETURN
      END
