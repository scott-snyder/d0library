C----------------------------------------------------------------------
C-
C-   Created  30-JUN-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      INTEGER MAXITM
      PARAMETER (MAXITM=100)
      INTEGER MAXUSE
      CHARACTER*132 TITLE(3,MAXITM)
      CHARACTER*2048 HLPTXT(MAXITM)
      CHARACTER*64 FILUSE,FILOUT,FILNAM
      CHARACTER*32 ADDNAM
      LOGICAL SORTOK,NEWFLG,DSPFLG,WRIFLG,SETWRI,MENWRI,LISTON
      COMMON /MENMAK/ MAXUSE,TITLE,HLPTXT,SORTOK,FILUSE,NEWFLG,DSPFLG,
     &                WRIFLG,FILOUT,SETWRI,MENWRI,LISTON,FILNAM,ADDNAM
