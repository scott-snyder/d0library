      SUBROUTINE PC3DCEN(LCACL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Mark the center of an CACL energy cluster
C-
C-   Inputs  : LCACL - LINK TO CACL BANK
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-MAY-1990   S. Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCACL
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C----------------------------------------------------------------------
      REAL XC,YC,ZC
      REAL DEL/10./
C----------------------------------------------------------------------
      CALL PXCOLR('GRE')
      XC=Q(LCACL+14)
      YC=Q(LCACL+15)
      ZC=Q(LCACL+16)
      CALL J3MOVE(XC,YC,ZC)
      CALL J3DRAW(XC+DEL,YC+DEL,ZC)
      CALL J3MOVE(XC,YC,ZC)
      CALL J3DRAW(XC,YC+DEL,ZC+DEL)
      CALL J3MOVE(XC,YC,ZC)
      CALL J3DRAW(XC+DEL,YC,ZC+DEL)
      CALL J3MOVE(XC,YC,ZC)
      CALL J3DRAW(XC-DEL,YC-DEL,ZC)
      CALL J3MOVE(XC,YC,ZC)
      CALL J3DRAW(XC,YC-DEL,ZC-DEL)
      CALL J3MOVE(XC,YC,ZC)
      CALL J3DRAW(XC-DEL,YC,ZC-DEL)
  999 RETURN
      END
