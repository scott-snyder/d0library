      SUBROUTINE MSCINT_TOF(ITRACK,SC_TOF,EXPECTED_TOF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Findout the Good TOF from MTOF bank 
C-
C-   Inputs  : ITRACK       [I] : MUON Track number
C-   Outputs : SC_TOF       [R] : Scintillator Time of flight
C-             EXPECTED_TOF [R] : Expected Time of Flight from Trajectory
C-   Controls: 
C-
C-   Created  27-FEB-1994   Acharya
C-   Modified 10-APR-1995   D. Wood : call MNETOF with MUOT index instead of
C-                                    MUON index
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-- Arguemnets--
      INTEGER ITRACK
      REAL SC_TOF,EXPECTED_TOF
C- Local Variables
      INTEGER  I
      INTEGER  NMTOF,NMSCT,IADD,IFLAG,IMUOT,IER
      REAL     TOF,TXYZ(3), XYZ(3), DXYZ(3) 
      REAL DX2, DY2, DZ2, TRACK_TO_PMT_DIST, DIST
      REAL MNETOF
C----------------------------------------------------------------------
      SC_TOF = -999.0
      EXPECTED_TOF = -999.0
      NMTOF=1       ! first TOF
      CALL GTMTOF( ITRACK, NMTOF,NMSCT,IADD,IFLAG,IMUOT, TOF,
     1                   TXYZ, XYZ, DXYZ, IER )
      IF(IER.EQ.1)GO TO 999
      IF(NMSCT.LT.1)GO TO 999
      IF(NMSCT.EQ.1)THEN
        SC_TOF = TOF   ! Only one TOF exists per track
        EXPECTED_TOF=MNETOF(IMUOT,TXYZ)
        GO TO 999
      ELSE                ! More TOF exists per track
C
C-- Accept the TOF corresponding to the scintillator nearest to TRACK
C
        DX2=(TXYZ(1)-XYZ(1))*(TXYZ(1)-XYZ(1))
        DY2=(TXYZ(2)-XYZ(2))*(TXYZ(2)-XYZ(2))
        DZ2=(TXYZ(3)-XYZ(3))*(TXYZ(3)-XYZ(3))
        TRACK_TO_PMT_DIST=SQRT(DX2+DY2+DZ2)
        SC_TOF = TOF   
        EXPECTED_TOF=MNETOF(IMUOT,TXYZ)
        DO I=1,NMSCT-1
         NMTOF=NMTOF+1
         CALL GTMTOF( ITRACK, NMTOF,NMSCT,IADD,IFLAG,IMUOT, TOF,
     1                   TXYZ, XYZ, DXYZ, IER )
         DX2=(TXYZ(1)-XYZ(1))*(TXYZ(1)-XYZ(1))
         DY2=(TXYZ(2)-XYZ(2))*(TXYZ(2)-XYZ(2))
         DZ2=(TXYZ(3)-XYZ(3))*(TXYZ(3)-XYZ(3))
         DIST=SQRT(DX2+DY2+DZ2)
         IF(DIST.LT.TRACK_TO_PMT_DIST)THEN
            TRACK_TO_PMT_DIST = DIST
            SC_TOF = TOF   
         ENDIF
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
