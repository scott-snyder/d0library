      SUBROUTINE PRFHLF(PRUNIT,LJFHLF,MFHLF,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out FHLF (Hit bank for forward/backward "half" of FDC)
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LJFHLF = bank address
C-  NFHLF = HALF number
C-  CFL   = not used
C-
C-  IFL   = 0  no printout
C-  IFL  >= 1  prints number of hits in FDC Half
C-
C-   Created  xx-JAN-1987   Daria Zieminska
C-   Updated  14-MAR-1989   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C-------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER PRUNIT,LKFHLF,LJFHLF,NFHLF,MFHLF,IFL
      INTEGER IHALF,MINHLF,MAXHLF
      INTEGER NHITS,IVERS
      INTEGER GZFHLF
C
      CHARACTER CFL*(*)
C------------------------------------------------------------------
C
      IF (IFL.LE.0) GOTO 999
      IF (CFL.EQ.'ALL') THEN
        MINHLF=0
        MAXHLF=1
      ELSEIF (CFL.EQ.'ONE') THEN
        CALL FCODER(MFHLF,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        MINHLF=HALF
        MAXHLF=HALF
      ENDIF
C
      LKFHLF=LJFHLF
      DO 10 IHALF=MINHLF, MAXHLF
        IF (CFL.NE.'ONE' .OR. LKFHLF.LE.0) LKFHLF=GZFHLF(IHALF)
        IF (LKFHLF.LE.0) THEN
          WRITE(PRUNIT,1011) LKFHLF
          GO TO 999
        END IF
C
        NFHLF = IQ(LKFHLF-5)
        CALL FCODER(NFHLF,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
        IVERS=IBITS(IQ(LKFHLF),13,5)
        NHITS =IQ(LKFHLF+1)    ! number of hits in this half
        IF ( IFL .GE. 1 ) WRITE(PRUNIT,101) IVERS,HALF,NHITS
   10 CONTINUE
C
  101 FORMAT(/' Hit bank for FDC half FHLF  - Version',I3/,
     $' Half #  =',I3,'    Number of hits in this half =',I5)
 1011 FORMAT(/' WRONG ADDRESS, LKFHLF =',I10)
C-------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
