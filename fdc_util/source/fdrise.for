      SUBROUTINE FDRISE(IFIRST,ITAIL,IPEAK,IPEV,NPULSE,WIRE,PED,A)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate rise time, fall time, overshoot
C-
C-   Inputs  : IFIRST - beginning of hit
C-             IPEAK -  top of hit
C-             IPEV - offset to get to hit (since using clusters in FDPULS)
C-             NPULSE - number of this hit on wire
C-             WIRE - wire number
C-             PED - value of pedestal
C-             A - data
C-   Outputs : elements of HITS array
C-   Controls:
C-
C-   Created  27-FEB-1990   Susan K. Blessing
C-   Updated  26-APR-1991   Jeffrey Bantly  use new PARAMS file 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
      INCLUDE 'D0$INC:FDEVNT.INC/LIST'
C
      INTEGER I,J,K
      INTEGER IFIRST,ITAIL,IPEAK,IPEV
      INTEGER I10,I90                   ! LOCATION OF 10% AND 90% POINTS
      INTEGER A(0:LFADC-1)              ! DATA
      INTEGER NPULSE                    ! PULSE NUMBER
      INTEGER WIRE                      ! WIRE NUMBER
      INTEGER IER
C
      REAL PED                          ! VALUE OF PEDESTAL
      REAL THRES10,THRES90              ! 10% AND 90% POINTS
      REAL RISETIME,FALLTIME,OVERSHOOT
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      THRES90 = 0.9 * (A(IPEAK+IPEV)-PED) + PED
      THRES10 = 0.1 * (A(IPEAK+IPEV)-PED) + PED
C
C ****  get the sigma of the rise time by taking 90% - 10% bin/ 2.2
C
      I90 = 0
      I10 = 0
      DO 131 I = IPEAK-1, IFIRST, -1
        IF (A(I+IPEV) .GT. THRES90) THEN
          GO TO 131
        ELSE    !so has dropped below the 90% point, save this and look
C               ! for 10% point
          I90 = I
          IF (I90-1 .GE. 1) THEN
            DO 111 J = I90-1, IFIRST, -1
              IF(A(J+IPEV) .GT. THRES10) THEN
                GOTO 111
              ELSE        !have dropped below 10% point so save
                I10 = J
                GOTO 121  !get out and quit
              ENDIF
  111       CONTINUE
          ENDIF
        ENDIF
  131 CONTINUE
  121 CONTINUE
      IF (I90.NE.0 .AND. I10.NE.0) THEN
        RISETIME = (I90-I10)* NBPBIN / 2.2
      ELSE
        RISETIME = 0.0
      ENDIF
C
C ****  get the sigma of the fall time by taking 90% - 10% bin/ 2.2
C
      I90 = 0
      I10 = 0
      DO 130 I = IPEAK+1, ITAIL
        IF (A(I+IPEV) .GT. THRES90) THEN
          GO TO 130
        ELSE    !so has dropped below the 90% point, save this and look
C               ! for 10% point
          I90 = I
          IF (I90+1 .LE. ITAIL) THEN
            DO 110 J = I90+1, ITAIL
              IF(A(J+IPEV) .GT. THRES10) THEN
                GOTO 110
              ELSE        !have dropped below 10% point so save
                I10 = J
                GOTO 120  !get out and quit
              ENDIF
  110       CONTINUE
          ENDIF
        ENDIF
  130 CONTINUE
  120 CONTINUE
      IF (I90.NE.0 .AND. I10.NE.0) THEN
        FALLTIME = (I10-I90) * NBPBIN / 2.2
      ELSE
        FALLTIME = 0.0
      ENDIF
C
C ****  now look for area in overshoot bins
C
      OVERSHOOT = 0.0
C
C      IF (.NOT.ZEROSUP) THEN
C        DO 140 I = ITAIL, LFADC-2
C if next 2 successive bins below pedestal level, then assume in overshoot
C          IF(A(I+IPEV) .LE. PED) THEN
C            OVERSHOOT = OVERSHOOT + A(I+IPEV) - PED
C            IF (A(I+1+IPEV) .GT. PED) GO TO 150
C          ELSE
C if above ped and next is also, probably next to another hit
C            IF (A(I+1+IPEV) .GT. PED) GO TO 150
C
C          ENDIF
C  140   CONTINUE
C  150   CONTINUE
C      END IF
C
C SAVE INFORMATION
C
      HITS(9,NPULSE,WIRE) = RISETIME
      HITS(10,NPULSE,WIRE) = FALLTIME
      HITS(11,NPULSE,WIRE) = OVERSHOOT
C----------------------------------------------------------------------------
  999 RETURN
      END
