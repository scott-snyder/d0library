      SUBROUTINE BKFTDA(HALF,QUAD,SECTOR,NHITS,LKFTDA)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Books the bank "FTDA" for a specified
C-                        FDC Theta Sector - Version 0
C-
C-  Inputs: HALF,QUAD,SECTOR = FDC Theta Sector address
C-          NHITS            = Number of hits to store in bank
C-  Output: LKFTDA = Link to "FTDA" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987   Tom Trippe
C-   Updated   7-OCT-1988   Jeffrey Bantly  for use with new hit format
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   2-JUL-1990   Jeffrey Bantly  use FDCLNK,use ISETVN,add SAVE 
C-   Updated  21-APR-1992   Susan K. Blessing  Need separate MZFORM calls
C-    for GEAN, regular data analysis, and when DO_RISE=.TRUE.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFTDA.LINK'
C
      INTEGER HALF,QUAD,SECTOR,LOGCHA
      INTEGER LKFTDA,LKFTSC,IXFTDA,IVERS,NHITS
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFTDA,LHFTDA
      INTEGER GZFTDA,GZFTSC,ISETVN
      INTEGER IER
C
      CHARACTER*20 FMFTDA
      CHARACTER*80 VARMSG
      CHARACTER*4 PATH
C
      LOGICAL FIRST
      LOGICAL DO_RISE
C
      SAVE LHFTDA,IXFTDA,NPTWHT,NWDSHT,IVERS
C
      DATA FIRST/.TRUE./
      DATA IVERS / 0 /
      DATA DO_RISE/.FALSE./
C------------------------------------------------------------------------
C
C  Book 'FTSC', if needed.
C
      LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
      IF(LKFTSC.EQ.0) CALL BKFTSC(HALF,QUAD,SECTOR,NHITS,LKFTSC)
C
      IF(FIRST) THEN
C
        NWDSHT=IQ(LFDCH+4)
        NPTWHT=IQ(LFDCH+5)
        LHFTDA=3+NPTWHT*2
C
        CALL PATHGT(PATH)
        IF( PATH .EQ. 'GEAN' ) THEN
          WRITE(FMFTDA,100) LHFTDA
  100     FORMAT(I2,'I / 1B 6F 2B')
        ELSE
C
          CALL EZPICK('FTRAKS_RCP')
          CALL EZGET('DO_RISE',DO_RISE,IER)
          CALL EZRSET
C
          IF (DO_RISE) THEN
            WRITE(FMFTDA,101) LHFTDA
  101       FORMAT(I2,'I / 1B 6F 1B 3F')
          ELSE
            WRITE(FMFTDA,102) LHFTDA
  102       FORMAT(I2,'I / 1B 6F 1B')
          END IF
        END IF
C
        CALL MZFORM('FTDA',FMFTDA,IXFTDA)
        FIRST = .FALSE.
      ENDIF
C
C  Book 'FTDA', if needed.
C
      LKFTDA=GZFTDA(HALF,QUAD,SECTOR)
      IF(LKFTDA.EQ.0)THEN   ! OK to book sector "data"
        MXFTDA = LHFTDA+NWDSHT*NHITS
        CALL MZBOOK(IXMAIN,LFTDA(SECTOR,QUAD,HALF),
     +  LFTSC(SECTOR,QUAD,HALF),-IZFTDA,'FTDA',0,0,MXFTDA,IXFTDA,LHFTDA)
        LKFTDA=LFTDA(SECTOR,QUAD,HALF)
        CALL FCODER(LOGCHA,HALF,0,QUAD,SECTOR,0,0,2)
        IQ(LKFTDA-5)= LOGCHA           ! set numeric bank id
        IQ(LKFTDA)  = ISETVN( IQ(LKFTDA), IVERS )     ! version number
        IQ(LKFTDA+2)= NPTWHT           ! IQ(LFDCH+5)
        IQ(LKFTDA+3)= NWDSHT           ! IQ(LFDCH+4)
      ELSE
        WRITE(VARMSG,10)
   10   FORMAT(' *** BKFTDA *** - ATTEMPTED REBOOKING OF',
     +          ' PREVIOUSLY BOOKED SECTOR')
        CALL ERRMSG('FDC-ALREADY BOOKED','BKFTDA',VARMSG,'I')
        LKFTDA=0
      ENDIF
C--------------------------------------------------------------------------
      RETURN
      END
