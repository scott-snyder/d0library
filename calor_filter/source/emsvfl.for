      SUBROUTINE EMSVFL(LEMSV,ICAND,IETA,IPHI,LYR,EFLOOR,ET_VTX0,
     &  ET_CAND,E_EM,EM3,E3,E5,E7,L1ETA,L1PHI,PASSED,AETA,APHI,XYZ_CLUS,
     &  SIG_PHI,SIG_ETA,ETA_PHYS,ET_Z_E_CORR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book (if necessary) and Fill EMSV bank
C-
C-   Inputs  :  
C-   Outputs : EMSV bank
C-   Controls: 
C-
C-   Created  30-SEP-1994   Lewis Taylor Goss
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:EMSV_VALUES.INC'
      INCLUDE 'D0$PARAMS:EMSV.PARAMS'
      INTEGER I,J,LEMSV
C----------------------------------------------------------------------
      IF (LEMSV .LE. 0) THEN
        CALL ERRMSG('EMSV','EMSV','no EMSV bank created','I')
        CALL BKEMSV(ICAND)   ! book EMSV bank
      ENDIF
      IQ(LEMSV + NMAIN_EMSV + 1) = ICAND
      IQ(LEMSV + NMAIN_EMSV + 2) = IETA
      IQ(LEMSV + NMAIN_EMSV + 3) = IPHI
      IQ(LEMSV + NMAIN_EMSV + 4) = LYR
      DO I = 1, 8
        Q(LEMSV + NMAIN_EMSV + 4 + I) = EFLOOR(I)
      ENDDO
      Q(LEMSV + NMAIN_EMSV + 13) = ET_VTX0
      Q(LEMSV + NMAIN_EMSV + 14) = ET_CAND
      Q(LEMSV + NMAIN_EMSV + 15) = E_EM
      DO J = -2, 2
        DO I = -2, 2
          Q(LEMSV + NMAIN_EMSV + I + 18 + (J+2) * 5 ) = EM3(I,J)
        ENDDO
      ENDDO
      Q(LEMSV + NMAIN_EMSV + 41)  = E3
      Q(LEMSV + NMAIN_EMSV + 42)  = E5
      Q(LEMSV + NMAIN_EMSV + 43)  = E7
      IQ(LEMSV + NMAIN_EMSV + 44) = L1ETA
      IQ(LEMSV + NMAIN_EMSV + 45) = L1PHI
      IF (PASSED) THEN
        IQ(LEMSV + NMAIN_EMSV + 46) = 1
      ELSE
        IQ(LEMSV + NMAIN_EMSV + 46) = -1
      ENDIF
      Q(LEMSV + NMAIN_EMSV + 47) = AETA
      Q(LEMSV + NMAIN_EMSV + 48) = APHI
      Q(LEMSV + NMAIN_EMSV + 49) = XYZ_CLUS(1)
      Q(LEMSV + NMAIN_EMSV + 50) = XYZ_CLUS(2)
      Q(LEMSV + NMAIN_EMSV + 51) = XYZ_CLUS(3)
      Q(LEMSV + NMAIN_EMSV + 52) = SIG_PHI
      Q(LEMSV + NMAIN_EMSV + 53) = SIG_ETA
      Q(LEMSV + NMAIN_EMSV + 54) = ETA_PHYS
      Q(LEMSV + NMAIN_EMSV + 55) = ET_Z_E_CORR
  999 RETURN
      END
