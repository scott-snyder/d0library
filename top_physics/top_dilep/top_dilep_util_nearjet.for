      SUBROUTINE TOP_DILEP_UTIL_NEARJET(LPMUO,OPP_PHI,DR_MIN,
     1  LJETS_MIN_DR,DPHI_MIN,LJETS_MIN_DPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find closest Reconstructed Jet to input
C-                         axis. Does dR_min and dPhi_min separately
C-
C-   Inputs  :
C-              LPMUO          - pointer to muon of intrest
C-
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
C-   Modified 15-Mar-1993   Change in Good_Jet logical name
C-   Modified 19-Mar-1993   Routine name changed for library compatibility
C-   Modified 29-Nov-1994   RE HALL: use pmuo pointer get muon eta and phi
C-            29-Nov-1994   change name for library compatability
C-            29-Nov-1994   -> now uses PARTICLE_SELECT for jet selection:
C-            29-Nov-1994   jet type defined as "TOP_JETS"
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LJETS,LPMUO
      INTEGER LJETS_MIN_DR,LJETS_MIN_DPHI
      INTEGER NJET,I
C
      REAL ETA,PHI,DR,DETA,DPHI,DR_MIN,DPHI_MIN
      REAL PI,TWOPI
C
      LOGICAL OPP_PHI
C
C *** stuff for particle select calls
      INTEGER ntot_jet, nwant_jet
      PARAMETER( nwant_jet = 20 )
      INTEGER  jets_link(nwant_jet)
C
      DATA PI,TWOPI/ 3.1415927,6.2831853/
C
      DR_MIN=9999.
      DPHI_MIN=9999.
      LJETS_MIN_DR=-1
      LJETS_MIN_DPHI=-1
C
C *** get muon eta and phi
C
      ETA=Q(LPMUO+16)
      PHI=Q(LPMUO+17)
      IF (OPP_PHI) THEN
        PHI = PHI + PI
        IF(PHI.GT.TWOPI) PHI=PHI-TWOPI
      ENDIF

C
C *** Loop over JETS to find which Jet is closest to the input direction
C
      CALL GTSLINK('TOP_JETS', NWANT_JET, NTOT_JET, JETS_LINK)
      NJET = MIN(NWANT_JET, NTOT_JET)
      DO I=1,NJET
        LJETS = JETS_LINK(I)
        DPHI=ABS(Q(LJETS+8)-PHI)
        IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
        IF(DPHI.LT.DPHI_MIN) THEN
          DPHI_MIN=DPHI
          LJETS_MIN_DPHI=LJETS
        ENDIF
        DETA=ABS(Q(LJETS+9)-ETA)
        DR=SQRT(DETA**2+DPHI**2)
        IF(DR.LT.DR_MIN) THEN
          DR_MIN=DR
          LJETS_MIN_DR=LJETS
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
