      SUBROUTINE L2_EM_ISOL(IETAC_IN,IPHIC_IN,LYRC_IN,EFLOOR,CONE_R,
     &  CONE_FR_MAX,ISO_RATIO,PASSED)
C----------------------------------------------------------------------
C-
C-   Purpose And Methods : make isolation cut on EM candidates
C-   Inputs  : IETAC_IN of candidate (offline towers)
C-             IPHIC_IN of candidate
C-             LYRC_IN  of candidate
C-             EFLOOR the floor sums of the candidate
C-             CONE_R radius of the cone ( recommend > .2 since smaller will hit
C-                  the core region)
C-             CONE_FR_MAX  max fraction of ECAND found outside cone
C-   Outputs : ISO_RATIO fraction of extry energy(ET) around the 3x3 readout
C-                tower core
C-               PASSED =.TRUE. a success; otherwise indicates cut failed
C-   Controls:  which layers make up core and cone come from
C-    the STP file and are found in /L2_EM_STP/
C-
C-   CreAted  15-NOV-1991   Yi  XiA
C-   Modified 29-MAR-1993   James T. McKinley add correction for shower
C-                          leakage out of nominal 3x3 L2_EM cluster
C-                          correction routine supplied by M. Tartaglia
C-   Updated   6-NOV-1993   James T. Linnemann   allow for minimum cell Et(nom)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! zebra main store /ZEBCOM/
      INCLUDE 'D0$INC:ZLINKC.INC'       ! zebra link area to store links
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
      INTEGER IETAC_IN,IPHIC_IN,LYRC_IN
      REAL EFLOOR(8),CONE_R,CONE_FR_MAX,OFFSET
      REAL SUM_CAND,ISO_RATIO,ETE,SUM_CONE
      REAL RCONE2,DETA,DPHI,LEAK_CORR
      INTEGER IBOUND,GZCAEP,IPOINT,NR,ETA,PHI,I
      INTEGER CONE_RING,LYR,ETAL,ETAH,PHIL(2),PHIH(2),NPHI
      INTEGER*2 L2_DPHI(-NPHIL:NPHIL) ! delta phi in readout tower space
      LOGICAL PASSED, FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C initialize an array that precomputes delta phi wraparound
      IF (FIRST) THEN
        L2_DPHI(0) = 0
C...the range of possible values in readout tower space is 1...NPHIL
C...so the range of the differences is -(NPHIL-1)...(NPHIL-1)
C...this adding of 1.5 periods and subtracting .5 period puts things in the
C...range -.5 period... .5 period, the closest match mod the period
        DO I = 1,NPHIL
          L2_DPHI(I) = MOD(I+NPHIL+NPHIL/2,NPHIL) - NPHIL/2
          L2_DPHI(-I) = MOD(-I+NPHIL+NPHIL/2,NPHIL) - NPHIL/2
        ENDDO
        FIRST = .FALSE.
      ENDIF

C----------------------------------------------------------------------
      CONE_RING = INT(CONE_R*10.0 + 0.4999999)  !square ring to contain cone
      RCONE2 = 100.*CONE_R**2     !in readout tower units for defining cone
C
C    get the boundaries of a ring of size of RCONE
C
      CALL CL2_RING22(IETAC_IN,IPHIC_IN,CONE_RING,ETAL,ETAH,
     &  PHIL,PHIH,NPHI)
C
C    Unpacking the L2 data
C
      CALL CL2_ROTOW_ETNOM(ETAL,ETAH,PHIL(1),PHIH(1))
      IF (NPHI.GT.1) CALL CL2_ROTOW_ETNOM(ETAL,ETAH,PHIL(2),
     &  PHIH(2))
C
      LCAEP = GZCAEP()
      NR = IQ(LCAEP + 2)
      SUM_CONE = 0.
      DO ETA = ETAL,ETAH
        DETA = L2_JETA(ETA)-L2_JETA(IETAC_IN)  ! this crosses eta = 0 smoothly
        DO IBOUND = 1,NPHI
          DO PHI = PHIL(IBOUND),PHIH(IBOUND)
            DPHI = L2_DPHI(PHI-IPHIC_IN)       !use array to do wraparound
            IF ( (DETA**2 + DPHI**2) .LE. RCONE2) THEN  !it's really a cylinder
              DO LYR = LO_CONE_LAYER,HI_CONE_LAYER    !limits from STP file
                IF (PTCAEP2(LYR,PHI,ETA) .GT. 0) THEN
                  IF((LYR .LT.MNLYMG ).OR. (LYR .GT. MXLYMG) ) THEN
                    IPOINT = (PTCAEP2(LYR,PHI,ETA) - 1)*NR    !normal channels
                    ETE = Q(LCAEP + IPOINT + 5)
                    IF (ETE.GE.ETMIN_CELL) SUM_CONE = SUM_CONE + ETE
                  ELSE
                    IF (CONE_USE_ICD) THEN
                      IPOINT = (PTCAEP2(LYR,PHI,ETA) - 1)*NR
                      ETE = Q(LCAEP + IPOINT + 5)
                      IF (ETE.GE.ETMIN_CELL) SUM_CONE = SUM_CONE + ETE
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C...add offset translated to Et back into CONE, since CORE has it
      IF (LO_GAMMA_FLOOR.EQ.1) THEN
        CALL L2_EM_GET_OFFSET(IETAC_IN,IPHIC_IN,LYRC_IN,OFFSET) !Zero, or offset
        SUM_CONE = SUM_CONE + OFFSET
      ENDIF
C
      SUM_CAND = 0
      DO I = LO_GAMMA_FLOOR,HI_GAMMA_FLOOR    ! limits come from STP file
        SUM_CAND = EFLOOR(I) + SUM_CAND
      ENDDO
      CALL L2_EM_LEAK_FIX(IETAC_IN,HI_GAMMA_FLOOR,LEAK_CORR)
      SUM_CAND = SUM_CAND*LEAK_CORR
      IF (SUM_CAND.LE.0) SUM_CAND = 1.E-5
C
      ISO_RATIO = (SUM_CONE - SUM_CAND)/SUM_CAND

      PASSED = (ISO_RATIO .LE. CONE_FR_MAX)
C
  990 RETURN
      END
