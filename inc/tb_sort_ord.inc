C----------------------------------------------------------------------
C-    TB_SORT_ORD.INC
C-
C-   COMMON for TB90 ordered address tables
C-
C-   Created  14-DEC-1989   
C-    by Andrew P. White
C-   Updated  30-MAR-1990   Stuart Fuess   Change SIZE to 5653
C-   Updated  13-APR-1990   Stuart Fuess   Change SIZE to 5654
C----------------------------------------------------------------------
      INTEGER SIZE
      INTEGER SEQADC,ADCADD,CARD,BLS,TOWER,DEPTH
      INTEGER IFTB,CON,PIN,ING,BMOD
      INTEGER POINT,ETA,PHI,SECT,LAYER
      PARAMETER (SIZE=5654)
      COMMON /TB_SORT_ORD/ SEQADC(SIZE,2),ADCADD(SIZE,2),
     C CARD(SIZE,2),BLS(SIZE,2),TOWER(SIZE,2),DEPTH(SIZE,2),
     C IFTB(SIZE,2),CON(SIZE,2),PIN(SIZE,2),ING(SIZE,2),BMOD(SIZE,2),
     C POINT(SIZE,2),SECT(SIZE,2),ETA(SIZE,2),PHI(SIZE,2),
     C LAYER(SIZE,2)
C
