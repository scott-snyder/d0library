      SUBROUTINE MUON_CALOR_CONFIRM(IETA,IPHI,THRESH,ESUM,PASS,REGION)
C----------------------------------------------------------------------
C-
C-   PURPOSE : GET ENERGY ON TRACK
C-
C-
C-
C-   Inputs  : IETA,IPHI INDICES FOR TRACK IN QUESTION
C-             NETA      NUMBER OF NEAREST NEIGHBORS IN ETA
C-             THRESH    ENERGY FOR PASSING EVENT
C-   Outputs : ESUM,PASS
C-   Controls: none
C-
C-   Created  14-OCT-1993   j.balderston
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF' ! pointer array,
C      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTR2.DEF' ! THIS GIVES THE
C      ! FUNCTION VERSION!!!!!!!!
      !perhaps zero'd here
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! location of current event
      INCLUDE 'D0$INC:PI.DEF'      ! number
C
      INTEGER  IER,PTR,REGION
      INTEGER  LCAEP,GZCAEP,IETA,IPHI
      INTEGER  I,L,E,P,EMIN,EMAX,PMIN,PMAX
      INTEGER  ACT_PHI,PHIL(2),PHIH(2),CONE_RING,ETAL,ETAH,NPHI,IBOUND
C
      LOGICAL  PASS,FIRST
      DATA FIRST/.TRUE./
C
      REAL     ESUM,CL2_ET_TO_E
      REAL     MU_ENRG_CUT,ETA
      REAL     ENRG,ETCOR,HYPOT
      REAL     THETA,PHI
C The following is for looking at ones with E LT 2
      CHARACTER*4 OLD_PATH
C
      INTEGER LVERT,LPMUO,GZPMUO,PMUO_QUAL
C
      REAL VVERT,PMUO_ETA,PMUO_PHI,PMUO_NN_NRG,ZVTX,L2_VERT
C------------------------------------------------------------- JB 10-14-93
      INTEGER PHILO(2),PHIHI(2),ETALO,ETAHI,ILYR,ARLYR(14)
      DATA ARLYR/1,2,3,4,5,6,7,11,12,13,14,15,16,17/
C
      REAL THRESH
      INTEGER J,K,NETA,NUMPHI,PHI_L(2),PHI_H(2),MIN_LYR,MAX_LYR
C----------------------------------------------------------------------
      PASS=.FALSE.
C
C Get the boundaries of a ring of size of RCONE
C
      IF(REGION.EQ.1.OR.REGION.EQ.3.) THEN
        MIN_LYR=1
        MAX_LYR=14
        THRESH=0.5
      ELSE
        MIN_LYR=8
        MAX_LYR=14
      ENDIF
C      NETA=1
C      NPHI=1
C      MIN_LYR=1
C      MAX_LYR=14
      CONE_RING=1                       ! 1NN IN ETA, 1NN IN PHI
      CALL CL2_RING22(IETA,IPHI,CONE_RING,ETALO,ETAHI,
     &  PHIL,PHIH,NPHI)
C      CONE_RING=2                       ! 2NN IN ETA, 2NN IN PHI
C      CALL CL2_RING22(IETA,IPHI,CONE_RING,ETALO(2),ETAHI(2),
C     &  PHILO,PHIHI,J)
C
C Unpack et
C
      IF(NPHI.EQ.1) THEN
        DO K=1,2
          PHI_L(K)=PHIL(K)
          PHI_H(K)=PHIH(K)
        ENDDO
      ELSE
        DO K=1,2
          PHI_L(K)=PHILO(K)
          PHI_H(K)=PHIHI(K)
        ENDDO
        NPHI=J
      ENDIF
      CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHI_L(1),PHI_H(1))
      IF(NPHI.GT.1.) CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,
     &  PHI_L(2),PHI_H(2))

C
C Now sum Et from CAEP for this road
C
      ESUM=0
      LCAEP=GZCAEP()
      IF(LCAEP.GT.0) THEN ! Loop over hit cells
        DO IETA=ETALO,ETAHI
          DO IBOUND = 1,NPHI
            DO IPHI=PHI_L(IBOUND),PHI_H(IBOUND)
              DO ILYR=MIN_LYR,MAX_LYR
                PTR = PTCAEP2(ARLYR(ILYR),IPHI,IETA)
                IF(PTR.GT.0) THEN
                  ENRG=Q(LCAEP+(PTR-1)*2+5)
                  IF(ENRG.GT.0) THEN
                    ETCOR=CL2_ET_TO_E(IETA)
                    IF(ETCOR.GT.0) THEN
                      ESUM=ESUM+(ENRG*ETCOR)
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        IF(ESUM.GE.THRESH) PASS=.TRUE.
      ENDIF                           ! CAEP2 has been filled
  999 RETURN
      END
