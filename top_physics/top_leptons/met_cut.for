      LOGICAL FUNCTION MET_CUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-NOV-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MET.INC'
      INCLUDE 'D0$INC:JET.INC'
C
      INTEGER BAD,I,IER
      REAL    MET,MET_CORR,METPHI_CORR,FEMNN,ETNN,PHINN,ETANN
      REAL    METCUT,ETX,ETY
      LOGICAL FIRST,REQ_FEMNN,REQ_METCUT
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('REQ_FEMNN',REQ_FEMNN,IER)
        CALL EZGET('REQ_METCUT',REQ_METCUT,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
      MET_CUT = .TRUE.
C
      IF(REQ_FEMNN)THEN
        MET = META(1,2)
        MET_CORR = META(4,2)     !From PNUT2
        METPHI_CORR = META(5,2)     !From PNUT2
        BAD = 0
        IBAD = 0
        DO I=1,NJETNN
          FEMNN = NNJET(5,I)
          IF(FEMNN .LE. 0.02)THEN
            BAD=1
            ETNN  = NNJET(1,I)
            PHINN = NNJET(2,I)
            ETANN = NNJET(3,I)
            ETX=ETX+ETNN*COS(PHINN)
            ETY=ETY+ETNN*SIN(PHINN)
            IBAD = IBAD+1
            BJETA(IBAD) = ETANN
            BJPHI(IBAD) = PHINN
            BJTHETA(IBAD) = 2*ATAN(EXP(-ETANN))
          ENDIF
        ENDDO
        IF(BAD.EQ.1)THEN
          MET_CORR  = SQRT((MET_CORR*COS(METPHI_CORR)+ETX)**2
     &      +(MET_CORR*SIN(METPHI_CORR)+ETY)**2)
          METPHI_CORR = (MET_CORR*SIN(METPHI_CORR)+ETY)/
     &      (MET_CORR*COS(METPHI_CORR)+ETX)
          METPHI_CORR =ATAN(METPHI_CORR)
          META(4,2) = MET_CORR         !put back corrected
          META(5,2) = METPHI_CORR      !value in PNUT2
        ENDIF
      ENDIF
C
      IF(REQ_METCUT)THEN
        IF(META(4,2).LT.METCUT)MET_CUT=.FALSE.
      ENDIF
C
  999   RETURN
        END
