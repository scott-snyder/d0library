      SUBROUTINE FGTLTM(HA,UN,QU,SE,WI,ETZERO,ATZERO,VELOP,VELOM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract Time information from 
C-   logical STP banks
C-
C-   Inputs  :  HA = Half for desired channel.
C-              UN = Unit for desired channel.
C-              QU = Quadrant for desired channel.
C-              SE = Sector for desired channel.
C-              WI = Wire for desired channel.
C-   Outputs : ETZERO = Electronics zero time offset 
C-             ATZERO = Absolute zero time offset 
C-             VELOP  = Drift gas velocity for x>0
C-             VELOM  = Drift gas velocity for x<0
C-
C-   Created   8-APR-1991   Robert E. Avery, replaces calls 
C-                              to fgttms with ifl=2,3
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER HA,UN,QU,SE,WI
      INTEGER NWIRE,NDELAY,WPARAM
      INTEGER LKFTSE,GZFTSE
C
      REAL ETZERO,ATZERO,VELOP,VELOM
C----------------------------------------------------------------------
C
      IF(WI.GE.0) THEN
        LKFTSE = GZFTSE(HA,UN,QU,SE)         ! Pointer to Logical sector
        IF (LKFTSE.NE.0) THEN
          NWIRE   = IC(LKFTSE+3)
          NDELAY  = IC(LKFTSE+5)*2
          IF(WI.LE.NWIRE+NDELAY) THEN 
            WPARAM  = IC(LKFTSE+4)              ! assumes DLPARAM=WPARAM
            ETZERO = C(LKFTSE+6+WPARAM*WI+1)    ! Read electronic t-zero value
            ATZERO = C(LKFTSE+6+WPARAM*WI+2)    ! Read absolute t-zero value
            VELOP  = C(LKFTSE+6+WPARAM*WI+3)    ! Read velocity +drift side
            VELOM  = C(LKFTSE+6+WPARAM*WI+4)    ! Read velocity -drift side
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
      ETZERO  = -1.                     ! Return -1 if error
      ATZERO  = -1.                     ! Return -1 if error
      VELOP   = -1.                     ! Return -1 if error
      VELOM   = -1.                     ! Return -1 if error
C--------------------------------------------------------------------------
  999 RETURN
      END
