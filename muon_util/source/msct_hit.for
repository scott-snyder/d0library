      SUBROUTINE MSCT_HIT(ITRACK,SCINT_TIME,IHIT,SCINT_ADDR,TXYZ_BEST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get info about the best scint. hit from the MSCT
C-                         bank
C-
C-   Inputs  : ITRACK           [I] : MUOT Track number
C-   Outputs : SCINT_TIME       [R] : Scintillator time (corrected for
C-                                    wavelength shifter delay)
C-             IHIT             [I] : Pointer to MUHP bank
C-             SCINT_ADDR       [I] : Scintillator address
C-             TXYZ_BEST(3)     [R] : Track coords. at scintillator
C-   Controls:
C-
C-   Created  14-NOV-1994   R. Markeloff
C-   Modified  3-MAR-1995   R. Markeloff WLS time is now stored in MSCT bank
C-   Modified  4-APR-1995   R. Markeloff Fix bug so multiple hits are handled
C-                                       correctly
C-   Modified 12-APR-1995   R. Markeloff No MSCT info returned if scint. not
C-                                       active
C-   Modified  3-MAY-1995   R. Markeloff Fix bug
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER ITRACK, NMSCT, IMSCT(4)
      INTEGER I, J, SCINT_ADDR, IMOD
      INTEGER  IADD, IFLAG, IMUOT, IHIT, JHIT
      REAL   TIME, TXYZ(3), XYZ(3), DXYZ(3)
      REAL   SCINT_TIME, TXYZ_BEST(3), WLS_TIME, WLS_BEST
      REAL   DX2, DY2, DZ2, TRACK_TO_PMT_DIST, DIST
      LOGICAL MNACTIVE
C----------------------------------------------------------------------
      SCINT_TIME = -999.0
C
C--   Get the pointer to MSCT Bank for a given track
C
      CALL GTMSHT(ITRACK,NMSCT,IMSCT)
C
      IF (NMSCT .LT. 1) GOTO 999    ! No Scint hits for this track
C
      CALL GTMSCT(IMSCT(1),SCINT_ADDR,IFLAG,IMUOT,IHIT,SCINT_TIME,
     &  TXYZ_BEST,XYZ,DXYZ,WLS_BEST)
C
C--   If the scintillator is not flagged as active,
C--   then the run is uncalibrated
C
      IMOD = SCINT_ADDR/256
      IF (.NOT. MNACTIVE(IMOD)) THEN
        SCINT_TIME = -999.
        GOTO 999
      ENDIF
C
      IF (NMSCT .EQ. 1) THEN        ! Only one time exists
        GOTO 800
      ELSE                          ! More Scint hits exist
C
C--     Accept the time corresponding to the scintillator nearest to the track
C
        DX2=(TXYZ_BEST(1)-XYZ(1))*(TXYZ_BEST(1)-XYZ(1))
        DY2=(TXYZ_BEST(2)-XYZ(2))*(TXYZ_BEST(2)-XYZ(2))
        DZ2=(TXYZ_BEST(3)-XYZ(3))*(TXYZ_BEST(3)-XYZ(3))
        TRACK_TO_PMT_DIST = SQRT(DX2+DY2+DZ2)
        DO I = 2, NMSCT
          CALL GTMSCT(IMSCT(I),IADD,IFLAG,IMUOT,JHIT,TIME,TXYZ,XYZ,DXYZ,
     &      WLS_TIME)
          DX2 = (TXYZ(1)-XYZ(1))*(TXYZ(1)-XYZ(1))
          DY2 = (TXYZ(2)-XYZ(2))*(TXYZ(2)-XYZ(2))
          DZ2 = (TXYZ(3)-XYZ(3))*(TXYZ(3)-XYZ(3))
          DIST = SQRT(DX2+DY2+DZ2)
          IF (DIST .LT. TRACK_TO_PMT_DIST) THEN
            TRACK_TO_PMT_DIST = DIST
            SCINT_TIME = TIME
            SCINT_ADDR = IADD
            IHIT = JHIT
            WLS_BEST = WLS_TIME
            DO J = 1, 3
              TXYZ_BEST(J) = TXYZ(J)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C
  800 CONTINUE
C
C--   Correct for wavelength shifter delay
C
      SCINT_TIME = SCINT_TIME - WLS_BEST
C
  999 RETURN
      END
