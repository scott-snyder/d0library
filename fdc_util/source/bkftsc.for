      SUBROUTINE BKFTSC(HALF,QUAD,SECTOR,NHITS,LKFTSC)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Books the bank "FTSC" for a specified
C-                        FDC Theta  Sector - Version 0
C-
C-  Inputs: HALF,QUAD,SECTOR = FDC Theta Sector address
C-          NHITS            = Number of hits to store in bank
C-  Output: LKFTSC = Link to "FTSC" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987   Tom Trippe
C-   Updated   7-OCT-1988   Jeffrey Bantly  for use with new hit format
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   2-JUL-1990   Jeffrey Bantly  use FDCLNK,use ISETVN,add SAVE 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFTSC.LINK'
      INTEGER HALF,QUAD,SECTOR,LOGCHA
      INTEGER LKFTSC,LKFTQD,IXFTSC,IVERS
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFTSC,NHITS,LHFTSC
      INTEGER GZFTQD,GZFTSC,ISETVN
      CHARACTER*20 FMFTSC
      CHARACTER*80 VARMSG
      LOGICAL FSFTSC
      SAVE LHFTSC,NWDSHT,IXFTSC,NPTWHT,IVERS
      DATA FSFTSC/.TRUE./
      DATA IVERS / 0 /
C------------------------------------------------------------------------
C
C  Book 'FTQD', if needed.
C
      LKFTQD=GZFTQD(HALF,QUAD)
      IF(LKFTQD.EQ.0) CALL BKFTQD(HALF,QUAD,LKFTQD)
C
      IF(FSFTSC) THEN
        FSFTSC=.FALSE.
        NWDSHT=IQ(LFDCH+2)
        NPTWHT=IQ(LFDCH+3)
        LHFTSC=3+NPTWHT*2
        WRITE(FMFTSC,100) LHFTSC
  100   FORMAT(I2,'I / 1B 7F 1B 3I')
        CALL MZFORM('FTSC',FMFTSC,IXFTSC)
      ENDIF
C
C  Book 'FTSC', if needed.
C
C
      LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
      IF(LKFTSC.EQ.0)THEN   ! OK to book sector
        MXFTSC=LHFTSC+NWDSHT*NHITS
        CALL MZBOOK(IXMAIN,LFTSC(SECTOR,QUAD,HALF),LFTQD(QUAD,HALF),
     +      -IZFTSC-SECTOR,'FTSC',1,1,MXFTSC,IXFTSC,LHFTSC)
        LKFTSC=LFTSC(SECTOR,QUAD,HALF)
        CALL FCODER(LOGCHA,HALF,0,QUAD,SECTOR,0,0,2)
        IQ(LKFTSC-5)= LOGCHA           ! set numeric bank id
        IQ(LKFTSC)  = ISETVN( IQ(LKFTSC), IVERS )     ! version number
        IQ(LKFTSC+2)= NPTWHT           ! IQ(LFDCH+3)
        IQ(LKFTSC+3)= NWDSHT           ! IQ(LFDCH+2)
      ELSE
        WRITE(VARMSG,10)
   10   FORMAT(' *** BKFTSC *** - ATTEMPTED REBOOKING OF',
     +          ' PREVIOUSLY BOOKED SECTOR')
        CALL ERRMSG('FDC-ALREADY BOOKED','BKFTSC',VARMSG,'I')
        LKFTSC=0
      ENDIF
C
C---------------------------------------------------------------------
      RETURN
      END
