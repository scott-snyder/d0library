C DEC/CMS REPLACEMENT HISTORY, Element GDIVOL.INC
C *1     2-NOV-1988 17:14:02 ABACHI ""
C DEC/CMS REPLACEMENT HISTORY, Element GDIVOL.INC
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
C-   S. ABACHI  2-NOV-1988
C-
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      INTEGER   ISHAPE,IDININ,IDNPAR,IDIAXI,IDNDIV,IVOCUR,NLECUR,
     1   INDCUR,NLEAAA,NMSCUR
      REAL      GDIPAR(50),GDICOS,GDSTEP,GDIIVO,GDIROT,GDIXVO,
     1   GDIYVO,GDIZVO
      COMMON/GDIVOL/ISHAPE,IDININ,IDNPAR,GDIPAR,IDIAXI,IDNDIV
     1   ,GDICOS,GDSTEP,GDIIVO,GDIROT,GDIXVO,GDIYVO,GDIZVO
     2   ,IVOCUR,NLECUR,INDCUR,NLEAAA,NMSCUR
