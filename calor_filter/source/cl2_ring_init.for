      SUBROUTINE CL2_RING_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize CL2_RINGS.INC common block arrays
C-
C-   Inputs  : none
C-   Outputs : /cl2_rings/
C
C      BYTE L1_JETA                   ! L1 eta coord to quantized eta
C      BYTE JETA_L1                   ! quantized eta to L1 eta coord
C      BYTE L2_JETA                   ! offline eta coord to quantized eta
C      BYTE JETA_L2                   ! quantized eta to offline eta coord
C      BYTE L1_ETA2LO                 ! L1 eta coord to lowest offline eta
C      BYTE L1_ETA2HI                 ! L1 eta coord to highest offline eta
C      BYTE L1_JPHI                   ! wrapped-around L1 phi
C      BYTE L2_JPHI                   ! wrapped-around offline phi
C      BYTE DETA3,DPHI3               !offsets in EM3 space
C      INTEGER*2 L3_DPHI              ! delta phi in EM3 space
C-   Controls: BORDER1 and BORDER2 parameters in cl2_rings.inc
C-                      control the size of the safety region
C-
C-   Created  14-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMs:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
      INTEGER I                       ! loop index
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA DETA3/0,0,1,1/,DPHI3/0,1,0,1/ !EM3 offsets in EM3 space
C----------------------------------------------------------------------
C
      IF (FIRST) THEN

C
C...The idea of these arrays is to map IETATT to consecutive integers, and
C       calculate the ring in consecutive integers, and then map the endpoints
C       back to IETATT (trigger tower coordinates)
C
        L1_JETA(0) = 1
        DO I = 1, NETAL1
          L1_JETA(I) = I
          L1_JETA(-I) = -I + 1
        ENDDO
        DO I  = NETAL1 + 1, NETAL1 + BORDER1 + 1
          L1_JETA(I) = NETAL1             ! only towers in trigger bank
          L1_JETA(-I) = -NETAL1 + 1
        ENDDO
        DO I  = 1,NETAL1
          JETA_L1(I) = I
          JETA_L1(-I+1) = -I
        ENDDO
        DO I = NETAL1 + 1, NETAL1 + BORDER1 + 1
          JETA_L1(I) = NETAL1
          JETA_L1(-I+1) = -NETAL1
        ENDDO
        JETA_L1(-NETAL1-1-BORDER1) = -NETAL1
C
C...The idea of these arrays is to map IETAC to consecutive integers, and
C       calculate the ring in consecutive integers, and then map the endpoints
C       back to IETAC, (offline coordinates)
C
        L2_JETA(0) = 1
        DO I = 1, NETAL
          L2_JETA(I) = I
          L2_JETA(-I) = -I + 1
        ENDDO
        DO I  = NETAL + 1, NETAL + BORDER2
          L2_JETA(I) = NETAL             ! only towers in trigger bank
          L2_JETA(-I) = -NETAL + 1
        ENDDO
        DO I  = 1,NETAL
          JETA_L2(I) = I
          JETA_L2(-I+1) = -I
        ENDDO
        DO I = NETAL + 1, NETAL + BORDER2 
          JETA_L2(I) = NETAL
          JETA_L2(-I+1) = -NETAL
        ENDDO
        JETA_L2(-NETAL-BORDER2) = -NETAL
C
C...The idea of these arrays is to define the smallest and largest offline
C       towers contained in the trigger tower

        DO I = 1 , MNCTTE - 1             ! fine trigger towers
          L1_ETA2LO(I) = 2*I - 1
          L1_ETA2LO(-I) = -2*I
          L1_ETA2HI(I) = L1_ETA2LO(I) + 1
          L1_ETA2HI(-I) = L1_ETA2LO(-I) + 1
        ENDDO
        DO I = MNCTTE,NETAL1              ! coarse trigger towers
          L1_ETA2LO(I) = MNCTTE + I - 1
          L1_ETA2HI(I) = L1_ETA2LO(I)
          L1_ETA2LO(-I) = -L1_ETA2LO(I)
          L1_ETA2HI(-I) = L1_ETA2LO(-I)
        ENDDO
        DO I = NETAL1 + 1, NETAL1 + BORDER1 + 1   ! beyond the edge
          L1_ETA2LO(I) = NETAL
          L1_ETA2HI(I) = NETAL
          L1_ETA2LO(-I) = -NETAL
          L1_ETA2HI(-I) = -NETAL
        ENDDO
C
C...Now PHI arrays: idea is to include wraparound so that can look up
C       the correct boundary corresponding to ieta +/- ringsize.  If the max is
C       less than the min, you know you've wrapped around
C
        DO I = 1,NPHIL1                   ! LV1 indices
          L1_JPHI(I) = I
        END DO
        DO I = 1,BORDER1                   ! Do wrap around
          L1_JPHI(NPHIL1+ I) = L1_JPHI(I)
          L1_JPHI(1 - I)     = L1_JPHI(NPHIL1- I + 1)
        END DO
        DO I = 1,NPHIL                    ! Offline indices
          L2_JPHI(I) = I
        END DO
        DO I = 1,BORDER2                   ! Do wrap around
          L2_JPHI(NPHIL + I) = L2_JPHI(I)
          L2_JPHI(1 - I)     = L2_JPHI(NPHIL - I + 1)
        END DO
        L3_DPHI(0) = 0
C...the range of possible values in em3 space is 2...2*NPHIL+1
C...so the range of the differences is -(2*NPHIL-1)...(2*NPHIL-1)
C...this adding of 1.5 periods and subtracting .5 period puts things in the
C...range -.5 period... .5 period, the closest match mod the period
        DO I = 1,2*NPHIL
          L3_DPHI(I) = MOD(I+3*NPHIL,2*NPHIL) - NPHIL
          L3_DPHI(-I) = MOD(-I+3*NPHIL,2*NPHIL) - NPHIL
        ENDDO
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
