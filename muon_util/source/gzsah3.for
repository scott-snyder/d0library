      INTEGER FUNCTION GZSAH3(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS 3-hits bank    
C-
C-   Returned value  : SAH3 bank address (or zero if something is bad)
C-   Inputs  : I - link number.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  23-NOV-1993   Vladimir Podstavkov
C-   Updated  16-FEB-1994   Alexander Efimov  -  check link number.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N_STATIONS, N_PLANES
      PARAMETER (N_STATIONS=6, N_PLANES=3)
      INTEGER N_LINKS
      PARAMETER (N_LINKS=N_STATIONS*N_PLANES)
      INTEGER GZSAHS, LSAHS, I
      GZSAH3 = 0
      IF (I .GE. 1 .AND. I .LE. N_LINKS) THEN
        LSAHS = GZSAHS()
        IF(LSAHS .NE. 0) THEN
          GZSAH3 = LQ(LSAHS-18-I) 
        ENDIF
      ENDIF
  999 RETURN
      END
