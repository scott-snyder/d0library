      SUBROUTINE HARD_FILL_L1(NTRIG_H)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill histograms with HARDWARE information
C-
C-   Inputs  : NTRIG_H - multiplicity for the 7 triger regions
C-   Outputs : 
C-   Controls:
C-
C-   Adapted from TRGR_FILL.FOR 
C-
C-   Created  02-SEP-1992   Guilherme Lima
C-   Updated  30-Dec-1992   K. Bazizi   Modified for 7 trigger regions
C-   Updated  09-MAR-1993   Guilherme Lima   Add output for CCT_LATCH bits
C----------------------------------------------------------------------
C-   Updated   2-FEB-1994   Jussara M. de Miranda   

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ST_BIT(0:31),SPEC_TRIG
      INTEGER L1OCT(40),NTRIG_H(7)
      INTEGER I,J,II,IBIT
      INTEGER Y1,Y2,Y3,Y4,X1,X2,X3
      LOGICAL H_L1BIT(16),HFW_L1BIT(0:31)
      INTEGER H_CCT_LATCH(7),CCT_LATCH1(5),CCT_LATCH2(5)

C-----------------------------------------------------------------

C.. Find specific trigger bit fired
      IF(LHEAD.NE.0) THEN
        SPEC_TRIG=IQ(LHEAD+11)
      ELSE
        RETURN
      ENDIF
      DO I=31,0,-1
        SPEC_TRIG = ISHFTC( SPEC_TRIG , 1 , 32)
        ST_BIT(I) = IAND  ( SPEC_TRIG , 1 )
      ENDDO
      DO I=0,31
        IF(ST_BIT(I).NE.0) CALL HFILL(2003,FLOAT(I),0.,1.)
      ENDDO

C--  Get level 1 muon RAW information and and the CCT_LATCH
C    information calculated in HARD_CCT_L1.FOR
      CALL HARD_INFO(H_CCT_LATCH,H_L1BIT,HFW_L1BIT)

C.. Fill raw bits histos
      DO I=1,16
        IF(HFW_L1BIT(I-1)) CALL HFILL(2001,FLOAT(I-1),0.,1.)
        IF(H_L1BIT(I))   CALL HFILL(2002,FLOAT(I),0.,1.)
      ENDDO
C
C.. Fill octant histos
C
      CALL MU_L1BITS_TO_OCT(H_CCT_LATCH,L1OCT)
      DO I=1,40
        IF(L1OCT(I).NE.0) CALL HFILL(2015,FLOAT(I-1),0.,1.)
      ENDDO

C- Central region
      DO IBIT=0,7
        IF(IBITS(H_CCT_LATCH(1),IBIT+12,1).NE.0)       ! CF octants
     &      CALL HFILL(2011,FLOAT(IBIT),0.,1.)
      ENDDO

C- WAMUS
      DO IBIT=0,3
        IF(IBITS(H_CCT_LATCH(2),IBIT+12,1).NE.0)       ! WN octants
     &      CALL HFILL(2012,FLOAT(IBIT+1),0.,1.)
        IF(IBITS(H_CCT_LATCH(3),IBIT+12,1).NE.0)       ! WS octants
     &      CALL HFILL(2012,FLOAT(IBIT+6),0.,1.)
      ENDDO

C- Overlap
      DO IBIT=0,3
        IF(IBITS(H_CCT_LATCH(4),IBIT+12,1).NE.0)       ! ON octants
     &      CALL HFILL(2013,FLOAT(IBIT+1),0.,1.)
        IF(IBITS(H_CCT_LATCH(5),IBIT+12,1).NE.0)       ! OS octants
     &      CALL HFILL(2013,FLOAT(IBIT+6),0.,1.)
      ENDDO

C- SAMUS
      DO IBIT=0,3
        IF(IBITS(H_CCT_LATCH(6),IBIT+2,1).NE.0)       ! SN octants
     &      CALL HFILL(2014,FLOAT(IBIT+1),0.,1.)
        IF(IBITS(H_CCT_LATCH(7),IBIT+2,1).NE.0)       ! SS octants
     &      CALL HFILL(2014,FLOAT(IBIT+6),0.,1.)
      ENDDO


C.. Overall WAMUS and SAMUS counters
C   NTRIG_H vector calculated in L1BIT_COUNTERS.FOR

      Y1 = NTRIG_H(1)
      Y2 = Y1 + NTRIG_H(2)+NTRIG_H(3)
      Y3 = Y2 + NTRIG_H(4)+NTRIG_H(5)
      Y4 = Y3 + NTRIG_H(6)+NTRIG_H(7)
 
      X3 = NTRIG_H(6)+NTRIG_H(7)
      X2 = X3 + NTRIG_H(4)+NTRIG_H(5)
      X1 = X2 + NTRIG_H(2)+NTRIG_H(3)

      CALL HFILL(2021,FLOAT(Y1),0.,1.)

      CALL HFILL(2022,FLOAT(Y2),0.,1.)

      CALL HFILL(2023,FLOAT(Y3),0.,1.)

      CALL HFILL(2024,FLOAT(Y4),0.,1.)

      CALL HFILL(2025,-4.0,0.,FLOAT(NTRIG_H(6)))
      CALL HFILL(2025,-3.0,0.,FLOAT(NTRIG_H(4)))
      CALL HFILL(2025,-2.0,0.,FLOAT(NTRIG_H(2)))
      CALL HFILL(2025,-1.0,0.,FLOAT(NTRIG_H(1))/2.0)
      CALL HFILL(2025, 0.0,0.,FLOAT(NTRIG_H(1))/2.0)
      CALL HFILL(2025, 1.0,0.,FLOAT(NTRIG_H(3)))
      CALL HFILL(2025, 2.0,0.,FLOAT(NTRIG_H(5)))
      CALL HFILL(2025, 3.0,0.,FLOAT(NTRIG_H(7)))

      IF(NTRIG_H(6).NE.0) CALL HFILL(2026,-4.0,0.,1.0)
      IF(NTRIG_H(4).NE.0) CALL HFILL(2026,-3.0,0.,1.0)
      IF(NTRIG_H(2).NE.0) CALL HFILL(2026,-2.0,0.,1.0)
      IF(NTRIG_H(1).NE.0) CALL HFILL(2026,-1.0,0.,1.0/2.0)
      IF(NTRIG_H(1).NE.0) CALL HFILL(2026, 0.0,0.,1.0/2.0)
      IF(NTRIG_H(3).NE.0) CALL HFILL(2026, 1.0,0.,1.0)
      IF(NTRIG_H(5).NE.0) CALL HFILL(2026, 2.0,0.,1.0)
      IF(NTRIG_H(7).NE.0) CALL HFILL(2026, 3.0,0.,1.0)

C
C- Fill histos with 32 bits CCT latch words for all regions

      DO I=1,7
         DO IBIT=0,31
           IF(IBITS(H_CCT_LATCH(I),IBIT,1).NE.0)       
     &         CALL HFILL(2050+I,FLOAT(IBIT),0.,1.)
         ENDDO
      ENDDO


  999 RETURN

      END
