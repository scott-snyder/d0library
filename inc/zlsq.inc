C----------------------------------------------------------------------
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
C
      INTEGER LSQ_MAX            
      PARAMETER( LSQ_MAX = 200 )
      COMMON /ZLSQ/LSTLNK(LSQ_MAX),LRFLNK(LSQ_MAX)
      INTEGER LSTLNK,LRFLNK
C
