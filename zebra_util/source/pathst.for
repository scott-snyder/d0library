      SUBROUTINE PATHST(PATH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Set ZEBRA path to follow for hits and processed event
C-      information.
C-   Inputs  : PATH= character*4 'GEAN' or 'RECO'
C-             if 'GEAN' pointer subroutines GZHITS and GZPROC
C-             assume one is looking for GEAN banks
C-             ditto for 'RECO'
C-
C-   ENTRY PATHDF(PATH)  set default (if not called default is 'RECO')
C-
C-   ENTRY PATHGT(PATH)  get PATH value
C-
C-   ENTRY PATHRS        reset PATH to default
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 PATH,PATDEF,PATSET
      SAVE PATDEF,PATSET
      DATA PATDEF,PATSET/'RECO','RECO'/
C----------------------------------------------------------------------
C
      PATSET=PATH
      RETURN
C
      ENTRY PATHGT(PATH)
      PATH=PATSET
      RETURN
C
      ENTRY PATHDF(PATH)
      PATDEF=PATH
      RETURN
C
      ENTRY PATHRS
      PATSET=PATDEF
      RETURN
C
      END
