C+
      INTEGER FUNCTION GZSELH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to Samus ELectronics Header 
C-                         SELH bank 
C-
C-   Returned value  : bank SELH address (or zero if something is bad)
C-
C-   Inputs  : none 
C-   Outputs : None
C-   Controls: 
C-
C-   Created  20-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-                                        
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSELH.LINK'
      INTEGER KSSAM 
      INTEGER GZSSAM
      EXTERNAL GZSSAM
C
      GZSELH = 0
      KSSAM = GZSSAM()
      IF (KSSAM .NE. 0) THEN
        GZSELH = LC(KSSAM-IZSELH)
      END IF
C
      RETURN
      END
