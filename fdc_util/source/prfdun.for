      SUBROUTINE PRFDUN(PRUNIT,LJFDUN,MFDUN,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out FTHE or FPHI (Hit bank for theta or phi unit of FDC)
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LJFDUN= bank address
C-  MFDUN = numerical bank identifier - logical address, -5 word
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
      INTEGER PRUNIT,LJFDUN,LKFDUN,NFDUN,MFDUN,IFL
      INTEGER IHALF,MINHLF,MAXHLF,IUNIT,MINUNT,MAXUNT
      INTEGER NHITS,IVERS
      INTEGER GZFDUN
C
      CHARACTER CFL*(*)
C------------------------------------------------------------------
C
      IF (IFL.LE.0) GOTO 999
      IF (CFL.EQ.'ALL') THEN
        MINHLF=0
        MAXHLF=1
        MINUNT=0
        MAXUNT=1
      ELSEIF (CFL.EQ.'ONE') THEN
        CALL FCODER(MFDUN,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        MINHLF=HALF
        MAXHLF=HALF
        MINUNT=UNIT
        MAXUNT=UNIT
      ENDIF
C
      LKFDUN=LJFDUN
      DO 10 IHALF=MINHLF,MAXHLF
        DO 20 IUNIT=MINUNT,MAXUNT
          IF ((CFL.NE.'ONE') .OR. (LKFDUN.LE.0)) 
     &                              LKFDUN=GZFDUN(IHALF,IUNIT)
          IF (LKFDUN.LE.0) THEN
            WRITE(PRUNIT,1011) LKFDUN
            GO TO 999
          END IF
C
          NFDUN = IQ(LKFDUN-5)
          CALL FCODER(NFDUN,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
          IVERS=IBITS(IQ(LKFDUN),13,5)
          NHITS =IQ(LKFDUN+1)    ! number of hits in this unit
          IF ( IFL .GE. 1 ) THEN
            IF(UNIT.EQ.0) WRITE(PRUNIT,101) IVERS,HALF,NHITS
            IF(UNIT.EQ.1) WRITE(PRUNIT,102) IVERS,HALF,NHITS
          ENDIF
   20   CONTINUE
   10 CONTINUE
C
  101 FORMAT(/' Hit bank for theta unit FTHE - Version',I3/,
     &' Half=',I3,'  # hits in this unit =',I5)
  102 FORMAT(/' Hit bank for phi   unit FPHI - Version',I3/,
     &' Half=',I3,'  # hits in this unit =',I5)
 1011 FORMAT(/' WRONG ADDRESS, LFDUN =',I10)
C------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
