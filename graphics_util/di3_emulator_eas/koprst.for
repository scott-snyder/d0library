      SUBROUTINE KOPRST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To reset parameters when JROPEN is called
C-
C-   Inputs  :  none
C-   Outputs :  none
C-   Controls:  none
C-
C-   Created  10-SEP-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
C
      CPINTR = DPINTR
      CPIDCO = DPIDCO
      CPIDIN = DPIDIN
      CPEDGE = DPEDGE
C
  999 CONTINUE
      RETURN
      END
