      INTEGER FUNCTION SAGTYP (N_TUBE_TYPES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get number of SAMUS tube types
C-
C-   Inputs  : None
C-   Outputs : Number of tube types
C-   Controls: 
C-
C-   Created  16-OCT-1990   A. Efimov
C-   Updated  30-APR-1991   Andrei Kiryunin: geometry from banks SSTH, etc. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER N_TUBE_TYPES
      INTEGER LSSTH,GZSSTH
C----------------------------------------------------------------------
C
      SAGTYP = -1
      LSSTH = GZSSTH()
      IF (LSSTH.EQ.0) GOTO 999
      N_TUBE_TYPES = IC(LSSTH+20) + IC(LSSTH+23)
      SAGTYP = +1
C
  999 RETURN
      END
