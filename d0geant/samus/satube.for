      SUBROUTINE SATUBE ( DTUB,RIN,ROUT,DEND, TUPAR,VOPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Defines geometric parameters of a drift tube
C-                         and of a drift volume inside it
C-
C-   Inputs  : DTUB - half length of drift tube
C-             RIN  - inner radius of tube (or outer radius of volume)
C-             ROUT - outer radius of tube
C-             DEND - length of tube end (where drift volume is absent)
C-   Oitputs : TUPAR - geometric parameters of a tube ('TUBE')
C-             VOPAR - geometric parameters of drift volume ('TUBE')
C-
C-   Created  29-SEP-1990   A.Kiryunin
C-   Updated  18-OCT-1990   A.Kiryunin  New set-up of SAMUS geometry
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    DTUB,RIN,ROUT,DEND, TUPAR(3),VOPAR(3)
C----------------------------------------------------------------------
      TUPAR(1)=RIN
      TUPAR(2)=ROUT
      TUPAR(3)=DTUB
C
      VOPAR(1)=0.0
      VOPAR(2)=RIN
      VOPAR(3)=DTUB-DEND
C
  999 RETURN
      END
