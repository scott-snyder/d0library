      INTEGER FUNCTION GZSAHT(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS hits bank    
C-
C-   Returned value  : SAHT bank address (or zero if something is bad)
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  23-NOV-1993   Vladimir Podstavkov
C-   Updated  16-FEB-1994   Alexander Efimov  - check the link number. 
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
      GZSAHT = 0
      IF (I .GE. 1 .AND. I .LE. N_LINKS) THEN
        LSAHS = GZSAHS()
        IF(LSAHS .NE. 0) THEN
          GZSAHT = LQ(LSAHS-I) 
        ENDIF
      ENDIF
  999 RETURN
      END
