      SUBROUTINE PRFSG5( PRUNIT, KTSEG, NTSEG, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank FSG5
C-
C-   Inputs  : PRUNIT Logical unit for output
C-             KTSEG  (not used)
C-             NTSEG  (not used)
C-             CFL  - can be set to 'ALL' (ifl=3), 'NONE' (ifl=0)
C-             IFL  = 0, no printout
C-                  = 1, number of segments in module
C-                  = 2, lists segments
C-                  = 3, full dump of banks
C-
C-   Updated  28-FEB-1990   Daria Zieminska 
C-   Updated  28-FEB-1990   Jeffrey Bantly update to use logical,CFL,IFL  
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT array
C-    and include additional output from bank.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER PRUNIT,KTSEG,NTSEG,IFL,IADD,NHITS,HALF,UNIT,QUAD,SECTOR
      INTEGER ISEG,NSEG,LFTRH,LSEG,MODULE,LAYER,WIRE,UB
      INTEGER IHIT,IWIRE,LR,IPTR
      INTEGER GZFSEG,NZBANK
C
      REAL SLOPE,INTER,CHISQ,AVE_ION
      REAL CONT(62),PHI,DR1,Z1,FIADD,FNHITS,RESID
C
      CHARACTER*(*) CFL
C
      EQUIVALENCE(IADD,FIADD)
      EQUIVALENCE(NHITS,FNHITS)
C----------------------------------------------------------------------------
      IF( IFL .EQ. 0 ) GOTO 999
      HALF=1
      UNIT=1
      LAYER=2
      MODULE=HALF*3+LAYER
      LSEG=GZFSEG(HALF,LAYER)
      IF(LSEG.LE.0) THEN
        WRITE(PRUNIT,*) ' FSG5 - NO SEGMENTS FOUND'
        GOTO 999
      ENDIF
      NSEG=NZBANK(IXCOM,LSEG)
      IF( IFL.EQ.1 ) THEN
        WRITE(PRUNIT,103) NSEG
        GOTO 999
      ENDIF
      IF( IFL.EQ.2 ) WRITE (PRUNIT,101)
      IF( IFL.EQ.3 ) WRITE (PRUNIT,104)
      DO 100 ISEG=1,NSEG
        CALL GTFSEG(MODULE,ISEG,CONT)
        FIADD=CONT(2)
        FNHITS=CONT(3)
        CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        PHI=CONT(36)
        DR1=CONT(37)
        Z1=CONT(38)
        SLOPE = CONT(55)
        INTER = CONT(56)
        CHISQ = CONT(57)
        AVE_ION = CONT(58)
        WRITE( PRUNIT,102) ISEG,HALF,UNIT,SECTOR,NHITS,PHI,
     &             DR1,Z1,SLOPE,INTER,CHISQ,AVE_ION
        IF( IFL.EQ.2 ) GOTO 100
        DO 200 IHIT=1, NHITS
          IWIRE=INT(CONT(3+IHIT)/2.)
          LR   =INT(CONT(3+IHIT))-IWIRE*2
          IPTR =INT(CONT(3+IHIT+16))
          RESID=CONT(3+IHIT+35)
          WRITE(PRUNIT,105) IWIRE,LR,IPTR,RESID
  200   CONTINUE
  100 CONTINUE
C
  101 FORMAT(/,' FSG5  HALF UNIT SECT  NHITS  PHI   DR1     Z1',
     X'     SLOPE  INTER  CHISQ/DOF  IONIZ/HIT ')
  102 FORMAT(4I5,1X,I5,2X,2F6.2,2X,F7.2,2F7.2,2X,F7.3,5X,F7.3)
  103 FORMAT(/,' FSG5  - ',I4,'  SEGMENTS FOUND')
  104 FORMAT(/,' FSG5  HALF UNIT SECT  NHITS  PHI   DR1     Z1',
     X'     SLOPE  INTER  CHISQ/DOF  IONIZ/HIT',
     X'  WIRE   +/-   PTR IN FPSC  RESID')
  105 FORMAT(86X,I4,4X,I2,6X,I4,F10.4)
C--------------------------------------------------------------------------
  999 RETURN
      END
