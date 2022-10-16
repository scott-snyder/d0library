      FUNCTION NP_MUNU()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter for 2nd gen leptoquark search in
C-                         muon-neutrino channel
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-NOV-1993   DOUGLAS M. NORMAN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPNUT.LINK'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
C
      LOGICAL NP_MUNU,NP_MUNU_EOJ
      LOGICAL FIRST
C
      INTEGER LJETS,GZJETS,ILI,NCJ,IER,IVERS
      INTEGER LPNUT,GZPNUT
      INTEGER LPMUO,LPARH,MSTAT,NMUO,MUO_COUNT,IOK,I
      INTEGER GZPARH,MUON_MAX
C
      REAL CJ_PT(25),CJ_JE(25)
      REAL E_GRAND(7),CJ_THETA(25),CJ_PHI(25),CJ_ETA(25)
      REAL TEMPLATE(20),DUMMY(25)
      REAL ETMISS3
      REAL MUON_PT,MUON_ETA,MU_PT_CUT,MU_ETA_CUT,MET_CUT,JET_PT_CUT
      REAL RSUM(20),RSUMM(20)
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NP_MUNU=.FALSE.
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('NP_MUNU_RCP',IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK('NP_MUNU_RCP')
          CALL EZGET('MU_PT_CUT',MU_PT_CUT,ier)
          CALL EZGET('JET_PT_CUT',JET_PT_CUT,ier)
          CALL EZGET('MET_CUT',MET_CUT,ier)
          CALL EZGET('MU_ETA_CUT',MU_ETA_CUT,ier)
          CALL EZGET_i('MUON_MAX',MUON_MAX,ier)
          CALL EZRSET
        ENDIF
        CALL VZERO(RSUM,20)
      ENDIF
C ****  SET PATH TO THE DEFAULT JET FINDING ALGORITHM
C ****       ( CONE WITH R=0.7, ET=8GEV )
C
c
      TEMPLATE(1)=2.0
      TEMPLATE(2)=6.0
      TEMPLATE(3)=0.7
      TEMPLATE(4)=7.0
      TEMPLATE(5)=8.0
      CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
      IF ( IER.NE.0 ) THEN
        CALL RESET_CAPH
        GO TO 999
      ENDIF
c
      CALL VZERO(CJ_PT,25)
C
      CALL GTJETS_TOTAL(NCJ,IER)
      DO ILI=1,NCJ
        IER=0
        CALL GTJETS(ILI,IVERS,E_GRAND,CJ_THETA(ILI),CJ_PHI(ILI),
     &    CJ_ETA(ILI),IER)
C
        CJ_PT(ILI) = E_GRAND(5)
C
C
      END DO
      CALL RESET_CAPH
C
c ***   sort by descending order  ******
      if (ncj.le.0) goto 791
      CALL SORT_DESCEND(CJ_PT,CJ_ETA,CJ_PHI,DUMMY,
     &DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,NCJ)
 791  continue
C ****  GET MISSING ET - muon correction
C
C
      LPNUT = GZPNUT(3)
      IF(LPNUT.GT.0) THEN
        ETMISS3 = Q(LPNUT+7)
      ENDIF
c
      LPARH=GZPARH()
      LPMUO=LQ(LPARH-IZPMUO)
      NMUO=0
      MUO_COUNT=0
      IF (LPMUO.LE.0) RETURN
 99   CONTINUE
      NMUO=NMUO+1
      MUON_PT=Q(LPMUO+14)
      MUON_ETA=Q(LPMUO+16)
      CALL MULQ_MUON_SELECT(LPMUO,MSTAT,IOK)
      IF (MUON_PT.GT.MU_PT_CUT.AND.MUON_ETA.LT.MU_ETA_CUT.AND.IOK.GT.0) 
     &  MUO_COUNT=MUO_COUNT+1
      LPMUO=LQ(LPMUO)
      IF (LPMUO.GT.0) GOTO 99
C
      IF (NMUO.GT.MUON_MAX) RETURN
      RSUM(1) = RSUM(1) + 1
      IF (MUO_COUNT .GE. 1) RSUM(2) = RSUM(2) + 1
      IF (CJ_PT(2) .GT. JET_PT_CUT) RSUM(3) = RSUM(3) + 1
      IF (ETMISS3 .GT. MET_CUT) RSUM(4) = RSUM(4) + 1
      IF (MUO_COUNT.GE.1.AND.CJ_PT(2).GT.JET_PT_CUT.AND.
     &ETMISS3.GT.MET_CUT) NP_MUNU=.TRUE.
      IF (NP_MUNU) RSUM(5) = RSUM(5) + 1
C
  999 RETURN
C
C   
      ENTRY NP_MUNU_EOJ(RSUMM)
C
      DO I = 1,20
        RSUMM(I) = RSUM(I)
      ENDDO
C
      RETURN
      END
