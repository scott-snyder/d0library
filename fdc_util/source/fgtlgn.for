      SUBROUTINE FGTLGN(HA,UN,QU,SE,WI,GAIN,MIPCONV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract Gain and M.I.P. conversion from 
C-   logical STP banks
C-
C-   Inputs  :  HA = Half for desired channel.
C-              UN = Unit for desired channel.
C-              QU = Quadrant for desired channel.
C-              SE = Sector for desired channel.
C-              WI = Wire for desired channel.
C-   Outputs : GAIN    = Gain value for address ID
C-             MIPCONV = M.I.P. conversion value for address ID
C-
C-   Created   8-APR-1991   Robert E. Avery, replaces calls 
C-                              to fgtpds with ifl=2,3
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER HA,UN,QU,SE,WI
      INTEGER NWIRE,NDELAY,WPARAM
      INTEGER LKFGSE,GZFGSE
C
      REAL GAIN,MIPCONV
C
C----------------------------------------------------------------------
C
      IF(WI.GE.0) THEN
        LKFGSE = GZFGSE(HA,UN,QU,SE)         ! Pointer to Logical sector
        IF (LKFGSE.NE.0) THEN
          NWIRE   = IC(LKFGSE+3)
          NDELAY  = IC(LKFGSE+5)*2
          IF(WI.LE.NWIRE+NDELAY) THEN 
            WPARAM  = IC(LKFGSE+4)             ! assumes DLPARAM=WPARAM
            GAIN    = C(LKFGSE+6+WPARAM*WI+1)  ! Read GAIN value
            MIPCONV = C(LKFGSE+6+WPARAM*WI+2)  ! Read M.I.P. conversion ratio
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
      GAIN    = -1.                     ! Return -1 if error
      MIPCONV = -1.                     ! Return -1 if error
C-------------------------------------------------------------------------
  999 RETURN
      END
