C----------------------------------------------------------------------
C-   Filename  FLAGS.INC
C-   Purpose   Storage for logical flags, managed by the FLAG facility.
C-   Created   9-JUL-1988   Harrison B. Prosper
C----------------------------------------------------------------------
      INTEGER MAXFLG,ERRFLG
      PARAMETER( MAXFLG = 200 )
      INTEGER NUMFLG
      LOGICAL BOOLE(MAXFLG)
      COMMON /FLAGS/ NUMFLG,ERRFLG,BOOLE
