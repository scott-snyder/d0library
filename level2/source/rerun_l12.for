      LOGICAL FUNCTION RERUN_L12()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set up zebra so level 1/level 2 can be run on data
C-    with existing output banks 
C-
C-   Inputs  :   ESUM and FILT banks if any
C-   Outputs :  rename ESUM and new FILT bank and path
C-   Controls: 
C-
C-   Created  18-FEB-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZESUM,LESUM,LFILT,WORD,LMUHT,GZMUHT
      CHARACTER*4 OLD_PATH
C----------------------------------------------------------------------
      LESUM = GZESUM('TRGR')
      IF (LESUM.GT.0) THEN
        CALL UCTOH('OTRG',IQ(LESUM+30),4,4)
      ENDIF
      LESUM = GZESUM('TRG0')
      IF (LESUM.GT.0) THEN
        CALL UCTOH('OTR0',IQ(LESUM+30),4,4)
      ENDIF
C *** Added for TR15 ESUM bank
      LESUM = GZESUM('TR15')
      IF (LESUM.GT.0) THEN
        CALL UCTOH('OT15',IQ(LESUM+30),4,4)
      ENDIF
C ***
      LESUM = GZESUM('FILT')
      IF (LESUM.GT.0) THEN
        CALL UCTOH('OFLT',IQ(LESUM+30),4,4)
      ENDIF
      LMUHT = GZMUHT()  ! won't run on MUD if found MUHT
      IF (LMUHT.GT.0) THEN
        CALL MZDROP(IXMAIN,LMUHT,' ')
      ENDIF
      CALL PATHGT(OLD_PATH)
      CALL PATHST('FILT')
      CALL BKFILT(LFILT)  !this will book a SECOND FILT bank to hold new results
      CALL MKPATH !create new path now to fill with banks
      CALL PATHST(OLD_PATH)
      RERUN_L12 = .TRUE.
  999 RETURN
      END
