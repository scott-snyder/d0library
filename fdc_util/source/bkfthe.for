      SUBROUTINE BKFTHE(HALF,LKFTHE)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Books the bank "FTHE" for a specified
C-                        FDC Theta Unit - Version 0
C-
C-  Inputs: HALF   = FDC Half
C-  Output: LKFTHE = Link to "FTHE" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987   Tom Trippe
C-   Updated  12-MAY-1989   Jeffrey Bantly  retrofitted to specs 
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated   3-MAY-1990   Jeffrey Bantly  add two words to marking
C-                                          sectors if hitfinding done 
C-   Updated   2-JUL-1990   Jeffrey Bantly  use FDCLNK,use ISETVN,add SAVE 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE   
      INCLUDE 'D0$INC:FDCLNK.INC' 
      INCLUDE 'D0$INC:ZEBCOM.INC' 
      INCLUDE 'D0$LINKS:IZFDUN.LINK' 
      INTEGER HALF,UNIT,LOGCHA,LKFTHE,LKFHLF,IVERS
      INTEGER GZFDUN,GZFHLF,ISETVN
C
      CHARACTER*80 VARMSG
C
      SAVE UNIT,IVERS
      DATA UNIT, IVERS / 0, 0 /
C-----------------------------------------------------------------------
C
C  Book 'FHLF' if needed.
C
      LKFHLF=GZFHLF(HALF)
      IF(LKFHLF.EQ.0) CALL BKFHLF(HALF,LKFHLF)
C
C  Book 'FTHE' if needed.
C
      LKFTHE=GZFDUN(HALF,UNIT)
      IF (LKFTHE.LE.0) THEN
         CALL MZBOOK(IXMAIN,LFDUN(UNIT,HALF),
     +            LFHLF(HALF),-IZFDUN-UNIT,'FTHE',8,8,3,2,0)
         LKFTHE=LFDUN(UNIT,HALF)
         CALL FCODER(LOGCHA,HALF,UNIT,0,0,0,0,2)
         IQ(LKFTHE-5)=LOGCHA            ! set numeric bank id
         IQ(LKFTHE)  =ISETVN( IQ(LKFTHE), IVERS )     ! version number
      ELSE
         WRITE(VARMSG,10)
   10    FORMAT(' *** BKFTHE *** - ATTEMPTED REBOOKING OF',
     +          ' PREVIOUSLY BOOKED THETA')
         CALL ERRMSG('FDC-ALREADY BOOKED','BKFTHE',VARMSG,'I')
         LKFTHE=0
      ENDIF
C
C--------------------------------------------------------------------------
      RETURN
      END
