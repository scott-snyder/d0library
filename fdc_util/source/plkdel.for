      SUBROUTINE PLKDEL(HALF,SECTOR)
C------------------------------------------------------------------
C
C  Delete used links (and mirrors) in FDC Phi sector (HALF,SECTOR)
C
C  Inputs: HALF,SECTOR
C
C-   Created  xx-JAN-1987   Daria Zieminska
C-   Updated  16-MAR-1990   Jeffrey Bantly  general cleanup 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine blocks. 
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C
C------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF,SECTOR,LLINK,NLINK,ILINK,LOC,I1,I2
      INTEGER IQHIT(18),NEL,NWORDS
      INTEGER LZFIND
      REAL QHIT(18)
      EQUIVALENCE (IQHIT,QHIT)
C------------------------------------------------------------------
      LLINK=LQ(LFLOC-1)
      NLINK=IQ(LFLOC+1)
      DO 200 ILINK=1,NLINK        ! loop over links
        LOC=LZFIND(IXCOM,LLINK,ILINK,-5)
        IF (LOC.LE.0) GO TO 200
        I1=IQ(LOC+1)/2            ! pointer to 1-st hit on link ILINK
        CALL GTFPSC(HALF,SECTOR,'HIT',I1,NEL,NWORDS,QHIT)
        I1=IQHIT(9)
        IF (BTEST(I1,2)) THEN     ! check if 1-st hit used
          CALL MZDROP(IXCOM,LOC,' ')
          GO TO 200
        END IF
        I2=IQ(LOC+2)/2            ! pointer to 2-nd hit on link ILINK
        CALL GTFPSC(HALF,SECTOR,'HIT',I2,NEL,NWORDS,QHIT)
        I2=IQHIT(9)
        IF (BTEST(I2,2)) CALL MZDROP(IXCOM,LOC,' ')
  200 CONTINUE
C----------------------------------------------------------------------
      RETURN
      END
