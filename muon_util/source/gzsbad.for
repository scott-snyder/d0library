      INTEGER FUNCTION GZSBAD (NST, NSEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to SAMUS bad channels bank
C-
C-   Returned value  : index of SAMUS bad channels bank
C-   Inputs  : NST - station number
C-             NSEC - section number
C-   Outputs : None
C-   Controls: 
C-
C-   Created  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSBCH.LINK'
      INTEGER KSSAM,KSBCH,KSBSH,NST,NSEC
      INTEGER GZSSAM
      EXTERNAL GZSSAM
C
      GZSBAD = 0
      KSSAM = GZSSAM()
      IF (KSSAM .NE. 0) THEN
        KSBCH = LC(KSSAM-IZSBCH)
        IF (KSBCH .NE. 0) THEN
          KSBSH = LC(KSBCH-NST)
          IF (KSBSH .NE. 0) THEN
            GZSBAD = LC(KSBSH-NSEC)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
