      SUBROUTINE TOP_LEPTONS_EM_JET_MATCH(NOEL,NOPH,LJETS,IMATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match Jets with Good Photon and ELectron
C-                               Candidates
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  12-JUL-1993   Stephen J. Wimpenny
C-   Modified 10-Sep-1993   Error in Phi Matching Corrected
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
C
      LOGICAL TOP_LEPTONS_GOOD_ELECTRON,TOP_LEPTONS_GOOD_PHOTON
C
      INTEGER NOEL,NOPH,IMATCH
      INTEGER LJETS,LPELC,LPPHO,GZPELC,GZPPHO
C
      REAL DPHI_MAX,DETA_MAX
      REAL PROXIM
C
      DATA DPHI_MAX,DETA_MAX/ 0.25,0.25/
C
      IMATCH=0
C
C *** Flag 'electron' jets where the jet axis is within deta_max of deta
C *** and dphi_max of dphi with a 'good' PELC electron which has already passed
C *** the electron selection. 
C
      IF(NOEL.GT.0) THEN
        LPELC=GZPELC()
        IF(LPELC.NE.0) THEN
          DO WHILE (LPELC.NE.0)
            IF(TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
              IF((ABS(Q(LJETS+9)-Q(LPELC+9)).LT.DETA_MAX).AND.
     1         (ABS(PROXIM(Q(LJETS+8)-Q(LPELC+10),0.)).LT.DPHI_MAX))THEN
                 IMATCH=IMATCH+1
              ENDIF
            ENDIF
            LPELC=LQ(LPELC)
          ENDDO
        ENDIF
      ENDIF
C
C *** Flag 'photon' jets where the jet axis is within deta_max of deta
C *** and dphi_max of dphi with a 'good' PPHO photon which has already passed
C *** the photon selection. 
C
      IF(NOPH.GT.0) THEN
        LPPHO=GZPPHO()
        IF(LPPHO.NE.0) THEN
          DO WHILE (LPPHO.NE.0)
            IF(TOP_LEPTONS_GOOD_PHOTON(LPPHO)) THEN
              IF((ABS(Q(LJETS+9)-Q(LPPHO+9)).LT.DETA_MAX).AND.
     1         (ABS(PROXIM(Q(LJETS+8)-Q(LPPHO+10),0.)).LT.DPHI_MAX))THEN
                  IMATCH=IMATCH+1
              ENDIF
            ENDIF
            LPPHO=LQ(LPPHO)
          ENDDO
        ENDIF
      ENDIF
C
      IF(IMATCH.EQ.0) IMATCH=-1
C
C----------------------------------------------------------------------
  999 RETURN
      END
