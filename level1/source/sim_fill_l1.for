      SUBROUTINE SIM_FILL_L1(NTRIG_S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill histos with information from simulator
C-
C-   Inputs  : NTRIG_S - multiplicity for the 7 trigger regions
C- 
C-   Controls:
C-
C-   Adapted  28-FEB-1994   Wagner Carvalho   from subroutine TRGR.FOR
C-  _______________________________________________________________________
C- | TRGR.FOR                                                              |
C- | Created  02-SEP-1992   Guilherme Lima                                 |
C- | Updated  30-Dec-1992   K. Bazizi   Modified for 7 trigger regions     |
C- | Updated  09-MAR-1993   Guilherme Lima   Add output for SIM_LATCH bits |
C- | Updated   2-FEB-1994   Jussara M. de Miranda                          |
C- |_______________________________________________________________________|
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER L1OCT(40),NTRIG_S(7)
      INTEGER I,J,II,IBIT
      INTEGER Y1,Y2,Y3,Y4,X1,X2,X3
      LOGICAL S_L1BIT(16),L1_PHYS(16)
      INTEGER S_CCT_LATCH(7)         ! ,LATCH_MASK(7)
C----------------------------------------------------------------------

C.. Gets CCT Latch bits from simulator
      CALL MU_CCT_LATCH(S_CCT_LATCH)

C.. Gets raw L1 muon raw trigger bits from simulator
      CALL MU_L1_RAW_BITS(S_L1BIT)

C.. Fill raw bits histo
      DO I=0,15
        L1_PHYS(I+1)=.FALSE.
        IF(S_L1BIT(I+1)  ) CALL HFILL(1002,FLOAT(I+1),0.,1.)
      ENDDO

C
C.. Fill octant histos
C

      CALL MU_L1BITS_TO_OCT(S_CCT_LATCH,L1OCT)
      DO I=1,40
        IF(L1OCT(I).NE.0) CALL HFILL(1015,FLOAT(I-1),0.,1.)
      ENDDO

C- Central region
      DO IBIT=0,7
        IF(IBITS(S_CCT_LATCH(1),IBIT+12,1).NE.0)       ! CF octants
     &      CALL HFILL(1011,FLOAT(IBIT),0.,1.)
      ENDDO

C- WAMUS
      DO IBIT=0,3
        IF(IBITS(S_CCT_LATCH(2),IBIT+12,1).NE.0)       ! WN octants
     &      CALL HFILL(1012,FLOAT(IBIT+1),0.,1.)
        IF(IBITS(S_CCT_LATCH(3),IBIT+12,1).NE.0)       ! WS octants
     &      CALL HFILL(1012,FLOAT(IBIT+6),0.,1.)
      ENDDO

C- Overlap
      DO IBIT=0,3
        IF(IBITS(S_CCT_LATCH(4),IBIT+12,1).NE.0)       ! ON octants
     &      CALL HFILL(1013,FLOAT(IBIT+1),0.,1.)
        IF(IBITS(S_CCT_LATCH(5),IBIT+12,1).NE.0)       ! OS octants
     &      CALL HFILL(1013,FLOAT(IBIT+6),0.,1.)
      ENDDO

C- SAMUS
      DO IBIT=0,3
        IF(IBITS(S_CCT_LATCH(6),IBIT+2,1).NE.0)       ! SN octants
     &      CALL HFILL(1014,FLOAT(IBIT+1),0.,1.)
        IF(IBITS(S_CCT_LATCH(7),IBIT+2,1).NE.0)       ! SS octants
     &      CALL HFILL(1014,FLOAT(IBIT+6),0.,1.)
      ENDDO


C.. Overall WAMUS and SAMUS counters
C   NTRIG_S vector calculated in L1BIT_COUNTERS.FOR

      Y1 = NTRIG_S(1)
      Y2 = Y1 + NTRIG_S(2)+NTRIG_S(3)
      Y3 = Y2 + NTRIG_S(4)+NTRIG_S(5)
      Y4 = Y3 + NTRIG_S(6)+NTRIG_S(7)
 
      X3 = NTRIG_S(6)+NTRIG_S(7)
      X2 = X3 + NTRIG_S(4)+NTRIG_S(5)
      X1 = X2 + NTRIG_S(2)+NTRIG_S(3)

      CALL HFILL(1021,FLOAT(Y1),0.,1.)

      CALL HFILL(1022,FLOAT(Y2),0.,1.)

      CALL HFILL(1023,FLOAT(Y3),0.,1.)

      CALL HFILL(1024,FLOAT(Y4),0.,1.)

      CALL HFILL(1025,-4.0,0.,FLOAT(NTRIG_S(6)))
      CALL HFILL(1025,-3.0,0.,FLOAT(NTRIG_S(4)))
      CALL HFILL(1025,-2.0,0.,FLOAT(NTRIG_S(2)))
      CALL HFILL(1025,-1.0,0.,FLOAT(NTRIG_S(1))/2.0)
      CALL HFILL(1025, 0.0,0.,FLOAT(NTRIG_S(1))/2.0)
      CALL HFILL(1025, 1.0,0.,FLOAT(NTRIG_S(3)))
      CALL HFILL(1025, 2.0,0.,FLOAT(NTRIG_S(5)))
      CALL HFILL(1025, 3.0,0.,FLOAT(NTRIG_S(7)))

      IF(NTRIG_S(6).NE.0) CALL HFILL(1026,-4.0,0.,1.0)
      IF(NTRIG_S(4).NE.0) CALL HFILL(1026,-3.0,0.,1.0)
      IF(NTRIG_S(2).NE.0) CALL HFILL(1026,-2.0,0.,1.0)
      IF(NTRIG_S(1).NE.0) CALL HFILL(1026,-1.0,0.,1.0/2.0)
      IF(NTRIG_S(1).NE.0) CALL HFILL(1026, 0.0,0.,1.0/2.0)
      IF(NTRIG_S(3).NE.0) CALL HFILL(1026, 1.0,0.,1.0)
      IF(NTRIG_S(5).NE.0) CALL HFILL(1026, 2.0,0.,1.0)
      IF(NTRIG_S(7).NE.0) CALL HFILL(1026, 3.0,0.,1.0)

C
C- Fill histos with 32 bits CCT latch words for all regions

      DO I=1,7
         DO IBIT=0,31
           IF(IBITS(S_CCT_LATCH(I),IBIT,1).NE.0)       
     &         CALL HFILL(1050+I,FLOAT(IBIT),0.,1.)
         ENDDO
      ENDDO

C- 
      CALL CC_DISTS(S_L1BIT)


  999 RETURN

      END
