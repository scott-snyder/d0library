C     FDEVNT.INC
C----------------------------------------------------------------------
C-
C-   Created   8-NOV-1988   Jeffrey Bantly
C-   Updated  12-MAY-1992   Susan K. Blessing  Remove a bunch of unused
C-    stuff.
C-
C-   Requires FDPARA.PARAMS for dimensions.
C----------------------------------------------------------------------
C
      COMMON /FDEVNT/
     &         HITS(LPULSE,MX_HIT_WIRE,0:NBPSEN-1),
     &         TMPUBN,
     &         FINDTP
      REAL    HITS, TMPUBN
      LOGICAL FINDTP
