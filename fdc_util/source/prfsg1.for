      SUBROUTINE PRFSG1( PRUNIT, KTSEG, NTSEG, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print bank FSG1
C-
C-   Inputs  : PRUNIT Logical unit for output
C-             KTSEG  (not used)
C-             NTSEG  (not used)
C-             CFL    (not used)
C-             IFL  = 0, no printout
C-                  = 1, number of segments in module
C-                  = 2, lists segments
C-                  = 3, full dump of banks
C-
C-   Updated  28-FEB-1990   Daria Zieminska 
C-   Updated  28-FEB-1990   Jeffrey Bantly update to use logical,CFL,IFL  
C-   Updated  29-APR-1991   Jeffrey Bantly  remove obs options 
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
      INTEGER IHIT,IWIRE,LR,IPTR,ICALL,IER
      INTEGER GZFSEG,NZBANK
C
      REAL SLOPE,INTER,CHISQ,AVE_ION
      REAL CONT(62),PHI,THETA,FIADD,FNHITS,RESID
      EQUIVALENCE(IADD,FIADD)
      EQUIVALENCE(NHITS,FNHITS)
C
      CHARACTER*(*) CFL
C
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF( IFL .EQ. 0 ) GOTO 999
      IF( ICALL.EQ.0 ) THEN
        ICALL=1
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
      ENDIF
      HALF=0
      UNIT=0
      LAYER=1
      MODULE=LAYER+3*HALF
      LSEG=GZFSEG(HALF,LAYER)
      IF(LSEG.LE.0) THEN
        WRITE(PRUNIT,*) ' FSG1 - NO SEGMENTS FOUND'
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
        PHI=CONT(20)
        THETA=CONT(21)
        SLOPE = CONT(30)
        INTER = CONT(31)
        CHISQ = CONT(32)
        AVE_ION = CONT(33)
        WRITE (PRUNIT,102) ISEG,HALF,UNIT,QUAD,SECTOR,NHITS,
     &                 PHI,THETA,SLOPE,INTER,CHISQ,AVE_ION
        IF( IFL.EQ.2 ) GOTO 100
        DO 200 IHIT=1, NHITS
          IWIRE=INT(CONT(3+IHIT)/2.)
          LR   =INT(CONT(3+IHIT))-IWIRE*2
          IPTR =INT(CONT(3+IHIT+8))
          RESID=CONT(3+IHIT+18)
          WRITE(PRUNIT,105) IWIRE,LR,IPTR,RESID
  200   CONTINUE
  100 CONTINUE
C
  101 FORMAT(/,' FSG1  HALF UNIT QUAD SECT  NHITS   PHI ',
     & '  THETA  SLOPE  INTER  CHISQ/DOF  IONIZ/HIT')
  102 FORMAT(5I5,2X,I4,2X,F6.2,F6.2,F7.2,1X,F7.2,2X,F7.3,5X,F7.3)
  103 FORMAT(/,' FSG1  - ',I4,'  SEGMENTS FOUND')
  104 FORMAT(/,' FSG1  HALF UNIT QUAD SECT  NHITS  PHI ',
     & '  THETA  SLOPE  INTER  CHISQ/DOF  IONIZ/HIT 
     &  WIRE   +/-   PTR IN FTSC  RESID')
  105 FORMAT(84X,I4,4X,I2,6X,I4,F10.4)
C--------------------------------------------------------------------------
  999 RETURN
      END
