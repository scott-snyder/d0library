      SUBROUTINE FGTLPD(HA,UN,QU,SE,WI,PEDAVG,PEDSIG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract Pedestal average and sigma from 
C-   logical STP banks
C-
C-   Inputs  :  HA = Half for desired channel.
C-              UN = Unit for desired channel.
C-              QU = Quadrant for desired channel.
C-              SE = Sector for desired channel.
C-              WI = Wire for desired channel.
C-   Outputs : PEDAVG,PEDSIG : Ped average and sigma corresponding to channel
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
      INTEGER LKFPSE,GZFPSE
C
      REAL PEDAVG,PEDSIG
C
C----------------------------------------------------------------------
C
      IF(WI.GE.0) THEN
        LKFPSE = GZFPSE(HA,UN,QU,SE)         ! Pointer to Logical sector
        IF (LKFPSE.NE.0) THEN
          NWIRE   = IC(LKFPSE+3)
          NDELAY  = IC(LKFPSE+5)*2
          IF(WI.LE.NWIRE+NDELAY) THEN 
            WPARAM  = IC(LKFPSE+4)             ! assumes DLPARAM=WPARAM
            PEDAVG = C(LKFPSE+6+WPARAM*WI+1)   ! Read pedestal value
            PEDSIG = C(LKFPSE+6+WPARAM*WI+2)   ! Read sigma
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
      PEDAVG = -1.                    ! Return -1 if error
      PEDSIG = -1.                    ! Return -1 if error
C---------------------------------------------------------------------------
  999 RETURN
      END
