      SUBROUTINE MTOFFL(IMUON,LMTOF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank MTOF.
C-
C-   Inputs  :  IMUON 'MUON' Track number
C-              LMTOF = link of bank to be filled.
C-              LMTOF < 0, routine will get link using GZMTOF
C-              LMTOF = 0, routine will book bank.
C-
C-   Outputs :
C-   Controls:
C-
C-    Created 25-FEB-1994 10:44:19.81  Acharya
C-   Modified 19-DEC-1994 R. Markeloff. Added IHIT argument to GTMSCT,
C-                        subtract wavelength shifter delay from scint. time
C-   Modified 06-FEB-1995 D. Wood, fix bug in mixing MUON and MUOT indices
C-   Modified 01-MAR-1995 R. Markeloff. Added WLS_TIME argument to GTMSCT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMTOF
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER GZMTOF,GZMUON
      INTEGER LMUON
      INTEGER  IMUON, NMSCT, IMSCT(4)
      INTEGER  IADD, IFLAG, IMUOT, IHIT
      INTEGER I,J,NR
      INTEGER NMUOT,IT,NS,LMUOT,LMUOT_TEST
      REAL   TOF, TXYZ(3), XYZ(3), DXYZ(3), WLS_TIME
      INTEGER GZMUOT
C----------------------------------------------------------------------
      LOGICAL FIRST, ACTIVE, MNACTIVE
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C     -- number of MUOT tracks...
      CALL GTMTRH(NMUOT)
C
C find the index of the MUOT track associated with this MUON bank
      LMUON = GZMUON(IMUON)
      IF(LMUON.LE.0) GOTO 999
      NS = IQ(LMUON - 2)
      LMUOT = LQ(LMUON - NS - 1)
      IF(LMUOT.LE.0) GOTO 999
      IMUOT = 0
      DO IT = 1,NMUOT
        LMUOT_TEST = GZMUOT(IT)
        IF(LMUOT_TEST.EQ.LMUOT) IMUOT = IT
      ENDDO
      IF(IMUOT.LE.0) GOTO 999
C
C -- Get the pointer to MSCT Bank from a given track --
C
      CALL GTMSHT( IMUOT, NMSCT, IMSCT )
C
      IF(NMSCT.EQ.0)GO TO 999     !No Scint Hits for this track, so do not
C                                 ! Book and Fill MTOF bank
C -- Verify that at least one MSCT bank corresponds to an active scint.
C
      ACTIVE = .FALSE.
      DO I=1,NMSCT
        CALL GTMSCT(IMSCT(I),IADD,IFLAG,IMUOT,IHIT,TOF,TXYZ,XYZ,DXYZ,
     &    WLS_TIME)
        ACTIVE = ACTIVE .OR. MNACTIVE(IADD/256)
      ENDDO
      IF (.NOT. ACTIVE) GOTO 999
C
      IF ( LMTOF .LT. 0 ) THEN
        LMTOF = GZMTOF(IMUON)    ! GET LINK.
      ENDIF
C
C Book the bank if argument = 0.
C
      IF ( LMTOF .EQ. 0 ) THEN
        LMUON = GZMUON(IMUON)    ! Link to Supporting Parent Bank
        IF(LMUON.LE.0)GO TO 999
        CALL BKMTOF(LMUON,IMUON,NMSCT,LMTOF)
      ENDIF
C
C-- Fill MTOF with contents from MSCT bank
C
      NR =  IQ(LMTOF+2)              ! Repitition number

      DO I=1,NMSCT
        CALL GTMSCT(IMSCT(I),IADD,IFLAG,IMUOT,IHIT,TOF,TXYZ,XYZ,DXYZ,
     &    WLS_TIME)
        IQ(LMTOF+(I-1)*NR+4) = IADD                ! scintillator hit address
        IQ(LMTOF+(I-1)*NR+5) = IFLAG               ! quality flag
        IQ(LMTOF+(I-1)*NR+6) = IMUOT               ! MUOT track number
C
C--   Subtract wavelength shifter delay
C
        Q(LMTOF+(I-1)*NR+8) = TOF - WLS_TIME       ! Time of flight
        DO J=1,3
          Q(LMTOF+(I-1)*NR+8+J)  =  TXYZ(J)       ! track position
          Q(LMTOF+(I-1)*NR+11+J) =  XYZ(J)        ! scintillator center
          Q(LMTOF+(I-1)*NR+14+J) =  DXYZ(J)       ! half width
        END DO
      ENDDO
  999 RETURN
      END
