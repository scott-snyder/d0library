      SUBROUTINE TRCROSS(ICH,IN,OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flag the TRD celles crossed by a track
C-
C-   Inputs  : ich:layer nb, in=entering cell, out:exit cell
C-   Outputs : Fill quantitities in common tr_cross_cell
C-   Controls:
C-
C-   Created  11-FEB-1993   A. Zylberstejn
C-   Updated  20-SEP-1993   A. Zylberstejn  Updated for 512 cells in layer 3 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICH,IN,OUT,IM1,IM2,IM,JM
      INCLUDE 'D0$INC:tr_cross_cell.INC'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
C----------------------------------------------------------------------
      NCROSS(ICH)=0
c        print*,' dans trcross,ich',ich,'in,out',in,out
      IF(IN.EQ.OUT)THEN
        NCROSS(ICH)=NCROSS(ICH)+1
        ICROSS(NCROSS(ICH),ICH)=IN
c        print*,' ncross',ncross(ich),' icrosss',ICROSS(NCROSS(ICH),ICH)
        GO TO 999
      END IF
      IM1=MIN0(IN,OUT)
      IM2=IN+OUT-IM1
      IF(IM2-IM1.GT.10)THEN
        IM=IM2
        IM2=IM1+nwire_per_layer(ich)
        IM1=IM
      END IF
      DO IM=IM1,IM2
        JM=IM
        IF(IM.GT.nwire_per_layer(ich))JM=IM-nwire_per_layer(ich)
        NCROSS(ICH)=NCROSS(ICH)+1
        ICROSS(NCROSS(ICH),ICH)=JM
c        print*,' ncross',ncross(ich),' icrosss',ICROSS(NCROSS(ICH),ICH)
        IF(NCROSS(ICH).EQ.5)GO TO 999
      END DO
  999 RETURN
      END
