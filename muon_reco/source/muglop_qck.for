      SUBROUTINE MUGLOP_QCK(LMUON,P,DP) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find momentum for a muon track 
C-
C-   Inputs  : LMUON  muon track bank address
C-   MUON bank contains a first estimate of momentum and muon track
C-   parameters after global fit
C- 
C-   Outputs : P,DP  momentum and error
C-   Controls: 
C-
C-   Created  12-JUN-1992   Daria Zieminska   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
C----------------------------------------------------------------------
      INTEGER  LMUON,LMUOT,I
      REAL     XYZ(3),DXYZ(3),P,DP,EC,EF,BDL
C
      LMUOT=LQ(LMUON-11)
      CALL VSCALE(Q(LMUON+11),1./Q(LMUON+14),DXYZ,3)
      CALL MUPQCK(IQ(LMUOT+3),Q(LMUON+37),Q(LMUON+38),Q(LMUON+39),
     1            Q(LMUOT+11),Q(LMUOT+12),Q(LMUOT+13),
C     1            Q(LMUON+47),Q(LMUOT+48),Q(LMUON+49),
     2            DXYZ(1),DXYZ(2),DXYZ(3), 
     3            Q(LMUOT+17),Q(LMUOT+18),Q(LMUOT+19),
     4            P,DP,EC,EF,BDL)
        P=ABS(P)   
  999 RETURN
      END
