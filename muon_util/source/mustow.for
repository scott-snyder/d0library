      SUBROUTINE MUSTOW(ITRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : project a SAMUS track to WAMUS, add WAMUS
C-   hits, refit and update MUOT bank
C-
C-   Inputs  : ITRAK - track number
C-   Outputs : updated MUOT bank
C-   Controls: 
C-
C-   Created  20-JUN-1991   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRAK,NPTRAK,NSAMUS,IFW1,IFW2,IFW3,QUAD,ISPARE
      REAL XMAGC,YMAGC,ZMAGC,XI,YI,ZI,SPARE1,SPARE2
      REAL XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM
      REAL CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE
C----------------------------------------------------------------------
      CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,SPARE2)
  999 RETURN
      END
