      LOGICAL FUNCTION WZ_HOT_CELL(E_HOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : tag hot cell jets
C-
C-   Returned value  : true if a hot cell was found
C-   Inputs  :         JETS bank, NN algorithm
C-   Outputs :         E_HOT - total E-vector of hot cell jets
C-   Controls:  WZ.RCP
C-
C-   Created  17-FEB-1993   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      INTEGER IER,I,LJETS,GZJETS,NHOTJ
      REAL    HOTJ_ET_CUT,HOTJ_RATIO_CUT,E_HOT(3),RHOTJ,JET_ET
      REAL TEMP(5)
      DATA TEMP/ 2.,7.,2.,8.,2. /                    ! NN
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('WZ_RCP')
        CALL EZGET('W_E_HOTJET_ET_CUT',HOTJ_ET_CUT,IER)
        IF(IER.EQ.0)CALL EZGET('W_E_HOTJET_RATIO_CUT',HOTJ_RATIO_CUT,
     &    IER)
        IF(IER.NE.0)CALL ERRMSG('WZ_RCP','WZ_HOT_CELL',
     &    'error gtting parameters','F')
        CALL EZRSET
      ENDIF
C
      DO I=1,3
        E_HOT(I)=0.
      ENDDO
      NHOTJ=0
      CALL SET_CAPH('NN_JET',TEMP,IER)
      LJETS = GZJETS()
      IF(LJETS.GT.0)THEN
        IF(IQ(LJETS+1).GE.3)THEN    ! check on bank version number
          DO WHILE (LJETS.GT.0)
            RHOTJ = Q(LJETS+19)     ! Ratio of hottest to next-hottest cell
            JET_ET = Q(LJETS+6)
            IF (JET_ET.GT.HOTJ_ET_CUT.AND.RHOTJ.GT.HOTJ_RATIO_CUT)THEN
              NHOTJ=NHOTJ+1
              DO I=1,3
                E_HOT(I)=E_HOT(I)+Q(LJETS+1+I)
              ENDDO
            ENDIF
            LJETS = LQ(LJETS)
          ENDDO
        ELSE
          CALL ERRMSG('JETS bank version<3','WZ_HOT_CELL',
     &      'skip checking for hot cells','W')
        ENDIF
      ENDIF
      CALL RESET_CAPH
      WZ_HOT_CELL=NHOTJ.GT.0
C----------------------------------------------------------------------
  999 RETURN
      END
