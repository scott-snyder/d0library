      SUBROUTINE BKFPDA(HALF,SECTOR,NHITS,LKFPDA)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Books the bank "FPDA" for a specified
C-                        FDC Phi sector - Version 0
C-
C-  Inputs: HALF,SECTOR = FDC Phi Sector address
C-          NHITS       = Number of hits to be stored in bank
C-  Output: LKFPDA = Link to "FPDA" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987   Tom Trippe
C-   Updated   x-MAR-1987   Daria Zieminska
C-   Updated   7-OCT-1988   Jeffrey Bantly  for use with new hit format
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated  20-JUN-1990   S. Protopopescu call MZFORM before pointers
C-   Updated   2-JUL-1990   Jeffrey Bantly  use ISETVN,add SAVE,use FDCLNK 
C-   Updated  21-APR-1992   Susan K. Blessing  Need separate MZFORM calls
C-    for GEAN, regular data analysis, and when DO_RISE=.TRUE.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFPDA.LINK'
C
      INTEGER HALF,SECTOR,LOGCHA
      INTEGER LKFPDA,LKFPSC,IXFPDA,LHFPDA
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFPDA,IVERS,NHITS
      INTEGER GZFPDA,GZFPSC,ISETVN
      INTEGER IER
C
      CHARACTER*20 FMFPDA
      CHARACTER*80 VARMSG
      CHARACTER*4 PATH
C
      LOGICAL FIRST
      LOGICAL DO_RISE
C
      SAVE LHFPDA,IXFPDA,NPTWHT,NWDSHT,IVERS,FIRST
C
      DATA FIRST/.TRUE./
      DATA IVERS / 0 /
      DATA DO_RISE/.FALSE./
C-------------------------------------------------------------------------
C
C  Book supporting bank 'FPSC' if needed.
C
      LKFPSC=GZFPSC(HALF,SECTOR)
      IF(LKFPSC.EQ.0) CALL BKFPSC(HALF,SECTOR,NHITS,LKFPSC)
C
      IF (FIRST) THEN
C
        NWDSHT=IQ(LFDCH+8)
        NPTWHT=IQ(LFDCH+9)
        LHFPDA=3+NPTWHT*2
C
        CALL PATHGT(PATH)
        IF( PATH .EQ. 'GEAN' ) THEN
          WRITE(FMFPDA,100) LHFPDA
  100     FORMAT(I2,'I / 1B 6F 2B')
        ELSE
C
          CALL EZPICK('FTRAKS_RCP')
          CALL EZGET('DO_RISE',DO_RISE,IER)
          CALL EZRSET
C
          IF (DO_RISE) THEN
            WRITE(FMFPDA,101) LHFPDA
  101       FORMAT(I2,'I / 1B 6F 1B 3F')
          ELSE
            WRITE(FMFPDA,102) LHFPDA
  102       FORMAT(I2,'I / 1B 6F 1B')
          END IF
        END IF
C          
        CALL MZFORM('FPDA',FMFPDA,IXFPDA)
        FIRST=.FALSE.
      ENDIF
C
C  Book 'FPDA' if needed.
C
      LKFPDA=GZFPDA(HALF,SECTOR)
      IF(LKFPDA.EQ.0)THEN   ! OK to book sector "data"
        MXFPDA = LHFPDA+NWDSHT*NHITS
        CALL MZBOOK(IXMAIN,LFPDA(SECTOR,HALF),LFPSC(SECTOR,HALF),
     +            -IZFPDA,'FPDA',0,0,MXFPDA,IXFPDA,LHFPDA)
        LKFPDA=LFPDA(SECTOR,HALF)
        CALL FCODER(LOGCHA,HALF,1,0,SECTOR,0,0,2)
        IQ(LKFPDA-5)= LOGCHA
        IQ(LKFPDA)  = ISETVN( IQ(LKFPDA), IVERS )     ! version number
        IQ(LKFPDA+2)= NPTWHT           ! IQ(LFDCH+9)
        IQ(LKFPDA+3)= NWDSHT           ! IQ(LFDCH+8)
      ELSE
        WRITE(VARMSG,10)
   10   FORMAT(' *** BKFPDA *** - ATTEMPTED REBOOKING OF',
     +          ' PREVIOUSLY BOOKED SECTOR')
        CALL ERRMSG('FDC-ALREADY BOOKED','BKFPDA',VARMSG,'I')
        LKFPDA=0
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
