      SUBROUTINE ISBBAR(NANAL,NB,PT,ETA,PHI,NBB,PTB,ETAB,PHIB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MC_TREE.INC'
C
      INTEGER NB,NBB,J,IDABS,NANAL
      REAL    PT(5),ETA(5),PHI(5),PTB(5),ETAB(5),PHIB(5)
      REAL    PTSQRT
C----------------------------------------------------------------------
C
c      CALL ISFILL()
      NB=0
      NBB=0
      CALL VZERO(PT,5)
      CALL VZERO(ETA,5)
      CALL VZERO(PHI,5)
      CALL VZERO(PTB,5)
      CALL VZERO(ETAB,5)
      CALL VZERO(PHIB,5)
      DO J=1,NMC
        IDABS=ABS(IMCDAT(JMC_ID,J))
        IF(IDABS.EQ.5) THEN
          IF(ABS(IMCDAT(JMC_ID,IMCDAT(JMC_PAR,J))).EQ.6) THEN
            NB=NB+1
            IF(NB.LE.5) THEN
              PTSQRT=SQRT(RMCDAT(JMC_PX,J)**2.+RMCDAT(JMC_PY,J)**2.)
              PT(NB) =PTSQRT
              ETA(NB)=RMCDAT(JMC_ETA,J)
              PHI(NB)=RMCDAT(JMC_PHI,J)
C                WRITE(61,1001) NANAL,J,IMCDAT(JMC_ID,J),PTSQRT,
C     &            IMCDAT(JMC_PAR,J),IMCDAT(JMC_ID,IMCDAT(JMC_PAR,J)),
C     &            RMCDAT(JMC_ETA,J),RMCDAT(JMC_PHI,J)
            ENDIF
          ENDIF
          IF(ABS(IMCDAT(JMC_ID,IMCDAT(JMC_PAR,J))).NE.6) THEN
            NBB=NBB+1
            IF(NBB.LE.5) THEN
              PTSQRT=SQRT(RMCDAT(JMC_PX,J)**2.+RMCDAT(JMC_PY,J)**2.)
              PTB(NBB) =PTSQRT
              ETAB(NBB)=RMCDAT(JMC_ETA,J)
              PHIB(NBB)=RMCDAT(JMC_PHI,J)
C                WRITE(61,1002) NANAL,J,IMCDAT(JMC_ID,J),PTSQRT,
C     &            IMCDAT(JMC_PAR,J),IMCDAT(JMC_ID,IMCDAT(JMC_PAR,J)),
C     &            RMCDAT(JMC_ETA,J),RMCDAT(JMC_PHI,J)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
 1001 FORMAT(' Event',I4,'   Index',I4,'   PartID',I5,
     &    '   Et',F8.2,'   W Index',I3,'  W ID',I5,
     &    '   Eta,Phi',2F8.2)
 1002 FORMAT(' Event',I4,'   Index',I4,'   PartID',I5,
     &    '   Et',F8.2,'   B Index',I3,'  B ID',I5,
     &    '   Eta,Phi',2F8.2)
C
  999 RETURN
      END
