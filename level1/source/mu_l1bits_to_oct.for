      SUBROUTINE MU_L1BITS_TO_OCT(CCT_WORD,L1OCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To extract the muon Level 1.0 Trigger Octant
C-                         Fired from the CCT Latch Words
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-- CF ==>  IOCT = 1-8
C-- WN ==>  IOCT = 11,13,15,17
C-- WS ==>  IOCT = 12,14,16,18
C-- ON ==>  IOCT = 21,23,25,27
C-- OS ==>  IOCT = 22,24,26,28
C-- SN ==>  IOCT = 31,33,35,37
C-- SS ==>  IOCT = 32,34,36,38
C-
C-   Created  11-OCT-1992   Kamel A. Bazizi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOCT,CCT_WORD(7),IBIT,BIT,TRIG_REG
      LOGICAL L1OCT(40),LATCH_BITS(0:31,7)
C
C-- Unpack the CCT_WORDS into LATCH_BITS
C
      DO TRIG_REG=1,7
        DO IBIT=0,31
          LATCH_BITS(IBIT,TRIG_REG)=.FALSE.
          BIT=IBITS(CCT_WORD(TRIG_REG),IBIT,1)
          IF (BIT.NE.0) LATCH_BITS(IBIT,TRIG_REG)=.TRUE. 
        ENDDO
      ENDDO
C
C-- Extract The triggered octant information
C
      DO IOCT=1,8

        L1OCT(IOCT) = LATCH_BITS(11+IOCT,1)           ! CF octants

        IF(IOCT.LE.4) THEN

          L1OCT(2*IOCT-1+10) = LATCH_BITS(11+IOCT,2)  ! WN quadrants
          L1OCT(2*IOCT  +10) = LATCH_BITS(11+IOCT,3)  ! WS quadrants

          L1OCT(2*IOCT-1+20) = LATCH_BITS(11+IOCT,4)  ! ON quadrants
          L1OCT(2*IOCT  +20) = LATCH_BITS(11+IOCT,5)  ! OS quadrants

          L1OCT(2*IOCT-1+30) = LATCH_BITS( 1+IOCT,6)  ! SN quadrants
          L1OCT(2*IOCT  +30) = LATCH_BITS( 1+IOCT,7)  ! SS quadrants

        ENDIF

      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
