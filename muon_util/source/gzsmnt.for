      INTEGER FUNCTION GZSMNT (NSTA, NSEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS minimum time bank 
C-                         for station NSTA, section NSEC
C-
C-   Returned value  : 
C-   Inputs  : Station number, Section number
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-   Updated  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov   
C-   Updated  14-DEC-1992   Alexander Efimov: change the structure
C-                          of the ZEBRA banks tree.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMTH.LINK'
      INTEGER N_STATION, N_SECTION
      PARAMETER (N_STATION=6, N_SECTION=6)
      INTEGER KSSAM, KSMTH, KSTSH, NSTA, NSEC, IL
      INTEGER GZSSAM
      EXTERNAL GZSSAM
C
      GZSMNT = 0
      KSSAM = GZSSAM()
      IF (KSSAM .NE. 0) THEN
        KSMTH = LC(KSSAM-IZSMTH)
        IF (KSMTH .NE. 0) THEN
          IL = (NSTA - 1) * N_SECTION + NSEC
          GZSMNT = LC(KSMTH-IL)
        ENDIF
      ENDIF
C
      RETURN
      END
