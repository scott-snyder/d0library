C  TB_SORT.INC
C----------------------------------------------------------------------
C-
C-   Created  27-NOV-1989   
C-    by Andrew P. White
C-   Updated  30-MAR-1990   Stuart Fuess   Change SIZE to 5653
C-   Updated  13-APR-1990   Stuart Fuess   Change SIZE to 5654
C----------------------------------------------------------------------
C
      INTEGER SIZE,IFTB,CON,PIN,ING,BMOD,SECT,ETA,PHI,LAYER,ADC
      PARAMETER (SIZE=5654)
      COMMON/TABLE/IFTB(SIZE),CON(SIZE),PIN(SIZE),ING(SIZE),BMOD(SIZE),
     C            SECT(SIZE),ETA(SIZE),PHI(SIZE),LAYER(SIZE),ADC(SIZE)
