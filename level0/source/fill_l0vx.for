      SUBROUTINE FILL_L0VX(IBUNCH,FASTZ_ID,VERTEX_BOARD,VERTEX_INFO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills out L0VX Zebra banks
C-
C-   Inputs  : IBUNCH - bunch number
C-             FASTZ_ID = (0=vertex, 1=halo)
C-             VERTEX_BOARD = (0=North, 1=South)
C-             VERTEX_INFO(bunch,vword#,unpacked data#)
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER IBUNCH, IBOARD, LKL0VX
      INTEGER FASTZ_ID(2),VERTEX_BOARD(2)
      INTEGER VERTEX_INFO(2,6,6,8)
      INTEGER GZL0VX_BUNCH
      EXTERNAL GZL0VX_BUNCH
C
C----------------------------------------------------------------------
C
C ****  Fetch L0VX link
C
      LKL0VX=GZL0VX_BUNCH(IBUNCH)
      IF ( LKL0VX.LE.0 ) CALL BKL0VX(LKL0VX,IBUNCH)
C
C ****  Fill L0VX bank
C
      DO IBOARD=1,2
        IQ(LKL0VX+2)=FASTZ_ID(IBOARD)
        IQ(LKL0VX+3)=VERTEX_BOARD(IBOARD)
C
        IQ(LKL0VX+4)=VERTEX_INFO(IBOARD,IBUNCH,1,1)
        IQ(LKL0VX+5)=VERTEX_INFO(IBOARD,IBUNCH,1,2)
C
        IQ(LKL0VX+6)=VERTEX_INFO(IBOARD,IBUNCH,2,1)
        IQ(LKL0VX+7)=VERTEX_INFO(IBOARD,IBUNCH,2,2)
        IQ(LKL0VX+8)=VERTEX_INFO(IBOARD,IBUNCH,2,3)
C
        IQ(LKL0VX+9)=VERTEX_INFO(IBOARD,IBUNCH,3,1)
        IQ(LKL0VX+10)=VERTEX_INFO(IBOARD,IBUNCH,3,2)
C
        IQ(LKL0VX+11)=VERTEX_INFO(IBOARD,IBUNCH,4,1)
        IQ(LKL0VX+12)=VERTEX_INFO(IBOARD,IBUNCH,4,2)
        IQ(LKL0VX+13)=VERTEX_INFO(IBOARD,IBUNCH,4,3)
C
        IQ(LKL0VX+14)=VERTEX_INFO(IBOARD,IBUNCH,5,1)
        IQ(LKL0VX+15)=VERTEX_INFO(IBOARD,IBUNCH,5,2)
        IQ(LKL0VX+16)=VERTEX_INFO(IBOARD,IBUNCH,5,3)
        IQ(LKL0VX+17)=VERTEX_INFO(IBOARD,IBUNCH,5,4)
        IQ(LKL0VX+18)=VERTEX_INFO(IBOARD,IBUNCH,5,5)
C
        IQ(LKL0VX+19)=VERTEX_INFO(IBOARD,IBUNCH,6,1)
        IQ(LKL0VX+20)=VERTEX_INFO(IBOARD,IBUNCH,6,2)
        IQ(LKL0VX+21)=VERTEX_INFO(IBOARD,IBUNCH,6,3)
        IQ(LKL0VX+22)=VERTEX_INFO(IBOARD,IBUNCH,6,4)
        IQ(LKL0VX+23)=VERTEX_INFO(IBOARD,IBUNCH,6,5)
        IQ(LKL0VX+24)=VERTEX_INFO(IBOARD,IBUNCH,6,6)
        IQ(LKL0VX+25)=VERTEX_INFO(IBOARD,IBUNCH,6,7)
        IQ(LKL0VX+26)=VERTEX_INFO(IBOARD,IBUNCH,6,8)
C
        LKL0VX = LKL0VX+26
C
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
