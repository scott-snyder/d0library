      SUBROUTINE GTEMSV(LEMSV,ICAND,IETA,IPHI,LYR,EFLOOR,ET_VTX0,
     &  ET_CAND,E_EM,EM3,E3,E5,E7,L1ETA,L1PHI,PASSED,AETA,APHI,XYZ_CLUS,
     &  SIG_PHI,SIG_ETA,ETA_PHYS,ET_Z_E_CORR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  get contents of EMSV banks
C-
C-   Inputs  : LEMSV - pointer to EMSV bank
C-   Outputs : ICAND         em candidate number
C-             IETA,IPHI,LYR coordinates of peak EM3 cell of shower
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
C-
C-   Created  30-SEP-1994   Lewis Taylor Goss
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LEMSV,I,J
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:EMSV.PARAMS'
      INCLUDE 'D0$INC:EMSV_VALUES.INC'
C----------------------------------------------------------------------
      IF (LEMSV.LE.0) THEN
        CALL ERRMSG('EMSV','EMSV','no EMSV bank created','E')
        GOTO 999
      ELSE
        ICAND        = IQ(LEMSV + NMAIN_EMSV + 1)
        IETA         = IQ(LEMSV + NMAIN_EMSV + 2)
        IPHI         = IQ(LEMSV + NMAIN_EMSV + 3)
        LYR          = IQ(LEMSV + NMAIN_EMSV + 4)
        DO I = 1, 8
          EFLOOR(I)  = Q(LEMSV + NMAIN_EMSV + 4 + I)
        ENDDO
        ET_VTX0      = Q(LEMSV + NMAIN_EMSV + 13)
        ET_CAND      = Q(LEMSV + NMAIN_EMSV + 14)
        E_EM         = Q(LEMSV + NMAIN_EMSV + 15)
        DO J = -2, 2
          DO I = -2, 2
            EM3(I,J) = Q(LEMSV + NMAIN_EMSV + I + 18 + (J+2) * 5 )
          ENDDO
        ENDDO
        E3           = Q(LEMSV + NMAIN_EMSV + 41)
        E5           = Q(LEMSV + NMAIN_EMSV + 42)
        E7           = Q(LEMSV + NMAIN_EMSV + 43)
        L1ETA        = IQ(LEMSV + NMAIN_EMSV + 44)
        L1PHI        = IQ(LEMSV + NMAIN_EMSV + 45)
        IF (IQ(LEMSV + NMAIN_EMSV + 46).EQ.1) THEN
          PASSED     = .TRUE.
        ELSE
          PASSED     = .FALSE.
        ENDIF
        AETA         = Q(LEMSV + NMAIN_EMSV + 47)
        APHI         = Q(LEMSV + NMAIN_EMSV + 48)
        XYZ_CLUS(1)  = Q(LEMSV + NMAIN_EMSV + 49)
        XYZ_CLUS(2)  = Q(LEMSV + NMAIN_EMSV + 50)
        XYZ_CLUS(3)  = Q(LEMSV + NMAIN_EMSV + 51)
        SIG_PHI      = Q(LEMSV + NMAIN_EMSV + 52)
        SIG_ETA      = Q(LEMSV + NMAIN_EMSV + 53)
        ETA_PHYS     = Q(LEMSV + NMAIN_EMSV + 54)
        ET_Z_E_CORR  = Q(LEMSV + NMAIN_EMSV + 55)
      ENDIF
  999 RETURN
      END
