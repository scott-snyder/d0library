      LOGICAL FUNCTION THREEJET_FILTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select three jet events
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-MAR-1994   j.balderston
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER NFILT,LJ_FIRST,LJ_SEC
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
      REAL FIRST_ETA,SEC_ETA,THIRD_ETA
      REAL FIRST_P(2),SEC_P(2),THIRD_P(2)
      REAL FIRSTPTOTSQ,PSUM(2),PSUMSQ
C
      LOGICAL THIRD
C----------------------------------------------------------------------
C Algorithm - 
C        1.) Check that the first three jets match up in ET to 20%ET1
C        2.) Check the first three jets are all with abs(eta).lt.1
C----------------------------------------------------------------------
      THREEJET_FILTER=.FALSE.
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
          FIRST_P(1)=Q(LJETS+2)
          FIRST_P(2)=Q(LJETS+3)
          FIRST_ETA=ABS(Q(LJETS+9))
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      MAX_ET=10.
      LJETS=LJETS_F
      DO WHILE(LJETS.GT.0.AND.LJETS.LT.2500000)
        IF(LJETS.NE.LJ_FIRST) THEN
          IF(Q(LJETS+6).GT.MAX_ET) THEN
            MAX_ET=Q(LJETS+6)
            LJ_SEC=LJETS
            SEC_P(1)=Q(LJETS+2)
            SEC_P(2)=Q(LJETS+3)
            SEC_ETA=ABS(Q(LJETS+9))
          ENDIF
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      MAX_ET=10.
      THIRD=.FALSE.
      LJETS=LJETS_F
      DO WHILE(LJETS.GT.0.AND.LJETS.LT.2500000)
        IF(LJETS.NE.LJ_FIRST.AND.LJETS.NE.LJ_SEC) THEN
          IF(Q(LJETS+6).GT.MAX_ET) THEN
            THIRD=.TRUE.
            MAX_ET=Q(LJETS+6)
            THIRD_P(1)=Q(LJETS+2)
            THIRD_P(2)=Q(LJETS+3)
            THIRD_ETA=ABS(Q(LJETS+9))
          ENDIF
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(THIRD) THEN
        IF(FIRST_ETA.LT.1.AND.SEC_ETA.LT.1.AND.THIRD_ETA.LT.1.) THEN
          FIRSTPTOTSQ=FIRST_P(1)**2+FIRST_P(2)**2
          PSUM(1)=FIRST_P(1)+SEC_P(1)+THIRD_P(1)
          PSUM(2)=FIRST_P(2)+SEC_P(2)+THIRD_P(2)
          PSUMSQ=PSUM(1)**2+PSUM(2)**2
          IF((PSUMSQ/FIRSTPTOTSQ).LT.0.2) THEN
            IL2BIT1=IQ(LHEAD+15)
            DO I=1,NFILT
              IF(BTEST(IL2BIT1,L2FILT(I))) THREEJET_FILTER=.TRUE.
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      CALL RESET_CAPH
  999 RETURN
      END
