      SUBROUTINE NEARJET(ETA,PHI,DR_MIN,LJETS_MIN_DR,DPHI_MIN,
     1  LJETS_MIN_DPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find closest Reconstructed Jet to input 
C-                         axis. Does dR_min and dPhi_min separately
C-
C-   Inputs  : 
C-              Eta      - eta of track direction
C-              Phi      - phi of track direction
C-
C-   Outputs : 
C-              DR_MIN         - min dR Jet-track
C-              LJETS_MIN_DR   - LJETS pointer for Jet
C-              DPHI_MIN       - min dPhi Jet-track
C-              LJETS_MIN_DPHI - LJETS pointer for Jet
C- 
C-   Controls: None
C-
C-   Created  17-AUG-1992   Stephen J. Wimpenny
C-   Modified 12-Oct-1992   Uses Good Jet Logical
C-   Modified 21-Nov-1992   Bug in dPhimin calc for large dPhi fixed
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL GOOD_JET,ISKIP_DR,ISKIP_DPHI
      INTEGER LJETS,GZJETS
      INTEGER LJETS_MIN_DR,LJETS_MIN_DPHI
      REAL ETA,PHI,DR,DETA,DPHI,DR_MIN,DPHI_MIN
      REAL PI,TWOPI
C
      DATA PI,TWOPI/ 3.1415927,6.2831853/
C
      ISKIP_DR=.FALSE.
      ISKIP_DPHI=.FALSE.
      IF(ETA.LT.-7.) ISKIP_DR=.TRUE.
      IF(PHI.LT.-7.) ISKIP_DPHI=.TRUE.
      DR_MIN=9999.
      DPHI_MIN=9999.
      LJETS_MIN_DR=-1
      LJETS_MIN_DPHI=-1
C
C *** Loop over JETS to find which Jet is closest to the input direction
C
      LJETS=GZJETS()
      IF(LJETS.EQ.0) GO TO 999
      DO WHILE (LJETS.NE.0)
        IF(GOOD_JET(LJETS)) THEN
          IF(.NOT.ISKIP_DPHI) THEN
            DPHI=ABS(Q(LJETS+8)-PHI)
            IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
            IF(DPHI.LT.DPHI_MIN) THEN
              DPHI_MIN=DPHI
              LJETS_MIN_DPHI=LJETS
            ENDIF
          ENDIF
          IF(.NOT.ISKIP_DR) THEN
            DETA=ABS(Q(LJETS+9)-ETA)
            DR=SQRT(DETA**2+DPHI**2)
            IF(DR.LT.DR_MIN) THEN
              DR_MIN=DR
              LJETS_MIN_DR=LJETS
            ENDIF
          ENDIF
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
