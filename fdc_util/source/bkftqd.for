      SUBROUTINE BKFTQD(HALF,QUAD,LKFTQD)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Books the bank "FTQD" for a specified
C-                        FDC Theta Quadrant - Version 0
C-
C-  Inputs: HALF,QUAD = FDC Quadrant address
C-  Output: LKFTQD = Link to "FTQD" bank, 0 if error detected.
C-
C-   T. Trippe, 4 Jan. 1987
C-   Updated  19-OCT-1988   Jeffrey Bantly  retrofitted to specs
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated   2-JUL-1990   Jeffrey Bantly  use FDCLNK,use ISETVN,add SAVE
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE   
      INCLUDE 'D0$INC:FDCLNK.INC' 
      INCLUDE 'D0$INC:ZEBCOM.INC' 
      INCLUDE 'D0$LINKS:IZFTQD.LINK' 
      INTEGER HALF,QUAD,LOGCHA
      INTEGER LKFTQD,LKFTHE,IVERS
      INTEGER GZFTQD,GZFDUN,ISETVN
C
      CHARACTER*80 VARMSG
C
      SAVE IVERS
      DATA IVERS / 0 /
C------------------------------------------------------------------------
C
C  Book 'FTHE', if needed.
C
      LKFTHE=GZFDUN(HALF,0)
      IF(LKFTHE.EQ.0) CALL BKFTHE(HALF,LKFTHE)
C
C  Book 'FTQD', if needed.
C
      LKFTQD=GZFTQD(HALF,QUAD)
      IF (LKFTQD.LE.0) THEN
         CALL MZBOOK(IXMAIN,LFTQD(QUAD,HALF),
     +            LFDUN(0,HALF),-IZFTQD-QUAD,'FTQD',6,6,1,2,0)
         LKFTQD=LFTQD(QUAD,HALF)
         CALL FCODER(LOGCHA,HALF,0,QUAD,0,0,0,2)
         IQ(LKFTQD-5)= LOGCHA           ! set numeric bank id
         IQ(LKFTQD)  = ISETVN( IQ(LKFTQD), IVERS )     ! version number
      ELSE
         WRITE(VARMSG,10)
   10    FORMAT(' *** BKFTQD *** - ATTEMPTED REBOOKING OF',
     +          ' PREVIOUSLY BOOKED QUADRANT')
         CALL ERRMSG('FDC-ALREADY BOOKED','BKFTQD',VARMSG,'I')
         LKFTQD=0
      ENDIF
C
C------------------------------------------------------------------------
      RETURN
      END
