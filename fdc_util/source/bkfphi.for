      SUBROUTINE BKFPHI(HALF,LKFPHI)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Books the bank "FPHI" for a specified
C-                        FDC Half - Version 0
C-
C-  Input : HALF
C-  Output: LKFPHI = Link to "FPHI" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987   Tom Trippe 
C-   Updated  12-MAY-1989   Jeffrey Bantly  retrofitted to specs 
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated   3-MAY-1990   Jeffrey Bantly  add two words to mark if  
C-                                          hitfinding done
C-   Updated   2-JUL-1990   Jeffrey Bantly  use FDCLNK,use ISETVN,add SAVE 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE   
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFDUN.LINK'
      INTEGER HALF,UNIT,LOGCHA,LKFPHI,LKFHLF,IVERS 
      INTEGER GZFDUN,GZFHLF,ISETVN
C
      CHARACTER*80 VARMSG
C
      SAVE UNIT,IVERS
      DATA UNIT,IVERS / 1, 0 / 
C-----------------------------------------------------------------------
C  Book 'FHLF' if needed.
C
      LKFHLF=GZFHLF(HALF)
      IF(LKFHLF.EQ.0) CALL BKFHLF(HALF,LKFHLF)
C
C  Book 'FPHI' if needed.
C
      LKFPHI=GZFDUN(HALF,UNIT)
      IF (LKFPHI.LE.0) THEN
         CALL MZBOOK(IXMAIN,LFDUN(UNIT,HALF),LFHLF(HALF),
     +            -IZFDUN-UNIT,'FPHI',36,36,3,2,0)
         LKFPHI=LFDUN(UNIT,HALF)
         CALL FCODER(LOGCHA,HALF,UNIT,0,0,0,0,2)
         IQ(LKFPHI-5)=LOGCHA                 ! set numeric bank id
         IQ(LKFPHI)  =ISETVN( IQ(LKFPHI), IVERS )     ! version number
      ELSE
         WRITE(VARMSG,10)
   10    FORMAT(' *** BKFPHI *** - ATTEMPTED REBOOKING OF',
     +          ' PREVIOUSLY BOOKED PHI')
         CALL ERRMSG('FDC-ALREADY BOOKED','BKFPHI',VARMSG,'I')
         LKFPHI=0
      ENDIF
C----------------------------------------------------------------------
      RETURN
      END
