      SUBROUTINE PRFTQD(PRUNIT,LJFTQD,MFTQD,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out FTQD (Hit bank for quadrant of theta unit of FDC)
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LJFTQD = bank address
C-  NFTQD = numerical bank identifier (not used)
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
      INTEGER PRUNIT,LFTQD,LJFTQD,NFTQD,MFTQD,IFL
      INTEGER NHITS,IVERS
      INTEGER IHALF,MINHLF,MAXHLF,IQUAD,MINTQD,MAXTQD
      INTEGER GZFTQD
C
      CHARACTER CFL*(*)
C-------------------------------------------------------------------
C
      IF (IFL.LE.0) GOTO 999
      IF (CFL.EQ.'ALL') THEN
        MINHLF=0
        MAXHLF=1
        MINTQD=0
        MAXTQD=7
      ELSEIF (CFL.EQ.'ONE') THEN
        CALL FCODER(MFTQD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        MINHLF=HALF
        MAXHLF=HALF
        MINTQD=QUAD
        MAXTQD=QUAD
      ENDIF
C
      LFTQD=LJFTQD
      DO 10 IHALF=MINHLF,MAXHLF
          DO 30 IQUAD=MINTQD,MAXTQD
C
            IF (CFL.NE.'ONE' .OR. LFTQD.LE.0)
     &                  LFTQD=GZFTQD(IHALF,IQUAD)
            IF (LFTQD.LE.0) THEN
              WRITE(PRUNIT,1011) LFTQD
              GO TO 999
            END IF
C
            NFTQD = IQ(LFTQD-5)
            CALL FCODER(NFTQD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
            IVERS=IBITS(IQ(LFTQD),13,5)
            NHITS =IQ(LFTQD+1)    ! number of hits in this quadrant
            IF ( IFL .GE. 1 ) WRITE(PRUNIT,101) IVERS,HALF,
     &                        UNIT,QUAD,NHITS
C
   40       CONTINUE
   30     CONTINUE
   10 CONTINUE
C
  101 FORMAT(/' Hit bank for quadrant FTQD - Version',I3/,
     $' Half/Unit/Quad =',3I2,'  # hits in this quadrant =',I5)
 1011 FORMAT(/' WRONG ADDRESS, LFTQD =',I10)
C------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
