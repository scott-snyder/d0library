      SUBROUTINE BKFHLF(HALF,LKFHLF)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Subroutine BKFHLF books the bank "FHLF" 
C-                        for a specified FDC Half - Version 0
C-  
C-  Input : HALF   = FDC Half
C-  Output: LKFHLF = Link to "FHLF" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987   Tom Trippe 
C-   Updated  15-OCT-1988   Jeffrey Bantly  retrofit of rules
C-   Updated   2-JUL-1990   Jeffrey Bantly  use ISETVN, add SAVE 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE   
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFHLF.LINK'
      INTEGER HALF,LOGCHA,LKFHLF,LKFDCH,IVERS
      INTEGER GZFDCH,ISETVN
C
      SAVE IVERS
      DATA IVERS / 0 /
C-----------------------------------------------------------------------
C
C  Book supporting bank 'FDCH' if needed.
C
      LKFDCH=GZFDCH()
      IF(LKFDCH.EQ.0) CALL BKFDCH(LKFDCH)
C
C  Book FHLF bank, if needed
C
      IF (LFHLF(HALF).LE.0) THEN
         CALL MZBOOK(IXMAIN,LFHLF(HALF),LFDCH,
     +            -IZFHLF-HALF,'FHLF',2,2,1,2,0)
         LKFHLF=LFHLF(HALF)
         CALL FCODER(LOGCHA,HALF,0,0,0,0,0,2)
         IQ(LKFHLF-5)=LOGCHA            ! set numeric bank id
         IQ(LKFHLF)  =ISETVN( IQ(LKFHLF), IVERS )  ! version number
      ENDIF
C
C------------------------------------------------------------------------
      RETURN
      END
