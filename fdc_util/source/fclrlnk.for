      SUBROUTINE FCLRLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set all link values to zero in FDCLNK.INC
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  17-SEP-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FDCLNK.INC'
C----------------------------------------------------------------------
      FDCLNK(1)=0
      FDCLNK(2)=0
      LFDCH=0
      CALL VZERO(LFHLF(0),2)
      CALL VZERO(LFDUN(0,0),4)
      CALL VZERO(LFTQD(0,0),16)
      CALL VZERO(LFTSC(0,0,0),96)
      CALL VZERO(LFTDA(0,0,0),96)
      CALL VZERO(LFPSC(0,0),72)
      CALL VZERO(LFPDA(0,0),72)
      LCDD3=0
C----------------------------------------------------------------------
  999 RETURN
      END
