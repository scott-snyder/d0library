      SUBROUTINE HARD_INFO(H_CCT_LATCH,H_L1BIT,HFW_L1BIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get hardware cct_latch form TRGR and built h_libit
C-                  Mask cct_latch to get only bits descreibed in d0note 1587
C-   Inputs  :
C-   Outputs : H_CCT_LATCH cct latch word form trgr
C-             HL1BIT hardware bits unfolded form cctlatch
C-             HFW_L1BIT Hardware L1 BITS seen by the framework
C-   Controls:
C-
C-   Created  16-FEB-1994   Jussara M. de Miranda
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER H_CCT_LAT(7),H_CCT_LATCH(7)
      LOGICAL H_L1BIT(16),HFW_L1BIT(0:31)
C--   Mask the unused bits form CCT_LATCH
      INTEGER LATCH_MASK(7)
      DATA LATCH_MASK/z'1FF003',z'10F003',z'10F003',
     &                z'10F3FF',z'10F3FF',z'13F03F',z'13F03F'/
C-- TRGR variables
      INTEGER IREG(6),CRATEWORDS(5),TRIG_NUM(5),CRATE_ID(5),
     &        NOTC_CARDS(5),ACTOTC(5),VERSION(5),L15OTC(5),
     &        LONGTO(5),TABTYP(5),TRGBITS(5),MERR(5),KERR(5),
     &        OTCNUM(5,16),OTCSTAT(5,16),MGRSTAT(5),NOTCWD(5),
     &        KTABLE(5,130,2,4),ITFWORD(2),
     &        TRAILWC(5),TRAILCR(5),TRAILTN(5),
     &        CCT_LATCH1(5),CCT_LATCH2(5)
C-- Counters
      INTEGER I,II

C----------------------------------------------------------------------
C-- Initialize
      DO I=1,16
        H_L1BIT(I)=.FALSE.
        HFW_L1BIT(I-1)=.FALSE.
      ENDDO

C.. Get TRGR information
      CALL GTTRGR2(IREG,CRATEWORDS,TRIG_NUM,CRATE_ID,NOTC_CARDS,
     &  ACTOTC,VERSION,L15OTC,LONGTO,TABTYP,TRGBITS,MERR,KERR,
     &  CCT_LATCH1,CCT_LATCH2,
     &  OTCNUM,OTCSTAT,MGRSTAT,NOTCWD,KTABLE,ITFWORD,
     &  TRAILWC,TRAILCR,TRAILTN)
C-
C- rearange the CCT latch words to go from 1 to 7
C- in order to match : CF, WN, WS, ON, OS, SN, and SS
C-
      H_CCT_LATCH(1)=CCT_LATCH1(1)          ! CF
      H_CCT_LATCH(2)=CCT_LATCH1(2)          ! WN
      H_CCT_LATCH(3)=CCT_LATCH1(3)          ! WS
      H_CCT_LATCH(4)=CCT_LATCH2(2)          ! ON
      H_CCT_LATCH(5)=CCT_LATCH2(3)          ! OS
      H_CCT_LATCH(6)=CCT_LATCH1(4)          ! SN
      H_CCT_LATCH(7)=CCT_LATCH1(5)          ! SS

C.. Mask off unused Latch bits
      DO I=1,7
        H_CCT_LATCH(I)=IAND(H_CCT_LATCH(I),LATCH_MASK(I))
      ENDDO


C.. Put together muon raw bits from hardware as found from CCTLATCH
      DO I=1,7
        DO II=0,1
          IF(IBITS(H_CCT_LATCH(I),II,1).NE.0) H_L1BIT(2*I+II)=.TRUE.
        ENDDO
      ENDDO

      DO I=0,7
        IF(IBITS(ITFWORD(1),I,1).NE.0) HFW_L1BIT(I)=.TRUE.
        IF(IBITS(ITFWORD(1),I+16,1).NE.0)HFW_L1BIT(I+8)=.TRUE.
      ENDDO
 
  
  999 RETURN

      END
