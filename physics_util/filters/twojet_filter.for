      LOGICAL FUNCTION TWOJET_FILTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select twojet events
C-
C-   Returned value  : TRUE
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-MAR-1994   j.balderston
C----------------------------------------------------------------------
C-Algorithm :
C-        1.) See if the leading two jets are approx back-to-back
C-        2.) Require both jets to have abs(eta).lt.1.0  
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER NFILT,LJ_FIRST
      PARAMETER (NFILT=7)
      INTEGER LJETS,L2FILT(NFILT),IL2BIT1
      DATA L2FILT/15,16,17,18,19,20,21/
      INTEGER IER,GZJETS,LJETS_F,I
C
      REAL DELPHI,MIN_DELPHI
      PARAMETER (MIN_DELPHI=2.3562)
      REAL TEMPLATE(20)
      DATA TEMPLATE / 1., 6., .7, 17*0.0 /
      REAL MAX_ET,FIRST_PHI,SEC_PHI
      REAL FIRST_ETA,SEC_ETA
C
C----------------------------------------------------------------------
      TWOJET_FILTER=.FALSE.
C
      CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
      LJETS_F=GZJETS()
C
C  First find highest TWO Et jets
C 
      MAX_ET=10.
      LJETS=LJETS_F
      DO WHILE(LJETS.GT.0.AND.LJETS.LT.2500000)
        IF(Q(LJETS+6).GT.MAX_ET) THEN
          MAX_ET=Q(LJETS+6)
          LJ_FIRST=LJETS
          FIRST_PHI = Q(LJ_FIRST+8)
          FIRST_ETA = ABS(Q(LJ_FIRST+9))
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      MAX_ET=10.
      LJETS=LJETS_F
      DO WHILE(LJETS.GT.0.AND.LJETS.LT.2500000)
        IF(LJETS.NE.LJ_FIRST) THEN
          IF(Q(LJETS+6).GT.MAX_ET) THEN
            MAX_ET=Q(LJETS+6)
            SEC_PHI = Q(LJETS+8)
            SEC_ETA = ABS(Q(LJETS+9))
          ENDIF
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      DELPHI=ABS(FIRST_PHI-SEC_PHI)
      IF(DELPHI.GT.PI) DELPHI=TWOPI-DELPHI
      IF(DELPHI.GT.MIN_DELPHI) THEN
        IF(FIRST_ETA.LT.1.AND.SEC_ETA.LT.1.) THEN
          IL2BIT1=IQ(LHEAD+15)
          DO I=1,NFILT
            IF(BTEST(IL2BIT1,L2FILT(I))) TWOJET_FILTER=.TRUE.
          ENDDO
        ENDIF
      ENDIF
      CALL RESET_CAPH
  999 RETURN
      END
