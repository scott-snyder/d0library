      SUBROUTINE L2_EM_GET_CAND(ICAND,BIT_MASK,ZVTX,DO_ZCORR,
     &  DO_LEAK_CORR,IETA,IPHI,LYR,EFLOOR,ET_VTX0,ET_CAND,E_EM,EM3,E3,
     &  E5,E7,L1ETA,L1PHI,PASSED,AETA,APHI,XYZ_CLUS,SIG_ETA,
     &  SIG_PHI,ETA_PHYS,ET_Z_E_CORR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack (if necessary) this EM candidate
C-
C-   Inputs  : ICAND         candidate number
C-             BIT_MASK      32 bit mask to AND with the hot tower relevance
C-                mask if they have no common bits, the candidate is irrelevant
C-             ZVTX          z-coordinate of the vertex
C-             DO_ZCORR
C-             DO_LEAK_CORR
C-   Outputs : IETA,IPHI,LYR coordinates of peak EM3 cell of shower
C-             EFLOOR(1...8) sum in EM1,2,3,4, and FH1, FH2,3 CH, ICD/MG
C-             ET_VTX0       ET of the candidate (calorimeter only)
C-             ET_CAND
C-             E_EM          E of the candidate (both calculated in 3x3 towers)
C-             EM3           5x5 EM3 cells around peak EM3 cell
C-             E3,E5,E7      Et in 3x3, 5x5, 7x7 around the peak
C-             L1ETA         Level 1 eta of the hit
C-             L1PHI         Level 1 phi of the hit
C-             PASSED        .TRUE. if a good and relevant candidate
C-             AETA          detector-based eta of the hit
C-             APHI          detector-based phi of the hit
C-             XYZ_CLUS      x,y,z of cluster centroid (based on EM3)
C-             SIG_ETA       estimated error in eta
C-             SIG_PHI       estimated error in phi
C-             ETA_PHYS      vertex corrected eta
C-             ET_Z_E_CORR   correction for leakage out of 3X3 cluster
C-   Controls:
C-
C-   Created  29-JAN-1992   James T. Linnemann
C-   Updated  26-SEP-1994   Lewis T. Goss moved code from L2_EM, added memory,
C-                          to decrease net processing time
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER BIT_MASK,DEPTH,LEMSV
      REAL THETA_PHYS,ZVTX,ET_ZCORR,LEAK_CORR,CL2_ET_CORR_FINE
      LOGICAL DO_ZCORR,DO_LEAK_CORR,ALREADY_EXIST
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2JETS_HOT.INC'            ! The Hot Towers from L1
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:DBITT.INC'                 ! hot tower to L1 eta,phi
      INCLUDE 'D0$INC:L2_EM_STP.INC'
      INCLUDE 'D0$INC:EMSV_VALUES.INC'
      INCLUDE 'D0$INC:EMSV_LINK.INC'
      PARAMETER( DEPTH = 4 )
C----------------------------------------------------------------------
      ALREADY_EXIST = .FALSE.
      PASSED = IAND(BIT_MASK,IHOT_MSK_EM(ICAND)).NE.0
      IF (PASSED) THEN
        IF (ICAND.LE.EMCAND_MAX) THEN
          IF (L2EMSV(ICAND).GT.0) THEN
            LEMSV = L2EMSV(ICAND)
            CALL GTEMSV(LEMSV,ICAND,IETA,IPHI,LYR,EFLOOR,ET_VTX0,
     &        ET_CAND,E_EM,EM3,E3,E5,E7,L1ETA,L1PHI,PASSED,AETA,APHI,
     &        XYZ_CLUS,SIG_PHI,SIG_ETA,ETA_PHYS,ET_Z_E_CORR)
            ALREADY_EXIST = .TRUE.
          ENDIF
        ENDIF
C if the em candidate hasn't been processed alredy, do so
        IF (.NOT.ALREADY_EXIST) THEN
          L1ETA =  DBITT(IHOT_ADR_EM(ICAND),1)
          L1PHI =  DBITT(IHOT_ADR_EM(ICAND),2)
C
C...unpack 3x3 trigger towers; find peak EM3 cell in central trigger tower
          CALL L2_EM3_MAX(L1ETA,L1PHI,IETA,IPHI,LYR)
          PASSED = IETA.NE.0            ! require some energy in EM3
          IF (PASSED) THEN
C
            CALL L2_EM_UNPACK(IETA,IPHI,LYR,EFLOOR,ET_VTX0,E_EM,EM3,E3,
     &        E5,E7)
            PASSED = (EFLOOR(3).GT.0.).AND.(ET_VTX0.GT.0.)
            IF (PASSED) THEN
              CALL L2_EM_XYZ_POSITION(IETA,IPHI,LYR,EM3,E_EM,AETA,
     &          APHI,XYZ_CLUS,SIG_ETA,SIG_PHI)
C...z correction
              CALL ETA_ZCORR(XYZ_CLUS,ZVTX,ETA_PHYS,THETA_PHYS)
              ET_ZCORR = ET_VTX0*CL2_ET_CORR_FINE(IETA,IPHI,LYR,        
     &          XYZ_CLUS)
              ET_CAND = ET_VTX0       !for online until Zvtx certified
              IF(DO_ZCORR) ET_CAND = ET_ZCORR      !for offline testing
C...shower leakage correction
              CALL L2_EM_LEAK_FIX(IETA,DEPTH,LEAK_CORR) !correction for leakage
              ET_Z_E_CORR = ET_ZCORR*LEAK_CORR          !out of 3x3 cluster
              IF(DO_LEAK_CORR) ET_CAND = ET_Z_E_CORR    !for offline testing
            ENDIF
C put into memory, if permissible
            IF (ICAND.LE.EMCAND_MAX) THEN
              CALL BKEMSV(ICAND,LEMSV)
              L2EMSV(ICAND) = LEMSV
              CALL EMSVFL(LEMSV,ICAND,IETA,IPHI,LYR,EFLOOR,ET_VTX0,
     &          ET_CAND,E_EM,EM3,E3,E5,E7,L1ETA,L1PHI,PASSED,AETA,APHI,
     &          XYZ_CLUS,SIG_PHI,SIG_ETA,ETA_PHYS,ET_Z_E_CORR)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
