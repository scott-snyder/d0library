C DEC/CMS REPLACEMENT HISTORY, Element GDIVWT.INC
C *2     3-NOV-1988 02:10:30 ABACHI ""
C *1     2-NOV-1988 17:14:04 ABACHI ""
C DEC/CMS REPLACEMENT HISTORY, Element GDIVWT.INC
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
C-   S. ABACHI  2-NOV-1988
C-
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      INTEGER   IDPERS,IDIVWT,NCHCOM,IDITIT,IDSCAL,IDSCAA,IDITSC,
     1    IDIOAS,IDIBUF(49),IOPRDR
      REAL      DIXVRP,DIYVRP,DIZVRP,DIDIST,YANGLE,XZANGL,VANGLE,
     1    DIXEYE,DIYEYE,DIZEYE,DIDXPV,DIDYPV,DIDZPV,DIDXNO,
     2    DIDYNO,DIDZNO,DIUMIN,DIUMAX,DIVMIN,DIVMAX,DIDNPE,DIDUPE,
     3    DIDVPE,PLDIST,DHDIST,DYDIST,GDIMXV,GDIMYV,GDIMZV,XDISCL,
     4    YDISCL,ZDISCL
      CHARACTER *80 COMMEN
      COMMON/GDIVWT/DIXVRP,DIYVRP,DIZVRP,DIDIST,YANGLE,XZANGL,VANGLE,
     1    DIXEYE,DIYEYE,DIZEYE,IDPERS,IDIVWT,
     2    DIDXPV,DIDYPV,DIDZPV,DIDXNO,DIDYNO,DIDZNO,DIUMIN,DIUMAX,
     3    DIVMIN,DIVMAX,DIDNPE,DIDUPE,DIDVPE,
     4    PLDIST,DHDIST,DYDIST,
     5    COMMEN,NCHCOM,IDITIT,
     6    GDIMXV,GDIMYV,GDIMZV,
     7    XDISCL,YDISCL,ZDISCL,IDSCAL,IDSCAA,IDITSC,IDIOAS,IDIBUF,
     8    IOPRDR