      INTEGER FUNCTION GZSSAM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS header bank.
C-
C-   Returns : pointer to SAMUS constants header bank
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created 28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov   
C-   Updated  21-APR-1991   Vladimir Glebov  ! Move SSAM under STPC 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSSAM.LINK'
      INTEGER KSTPC
C
      GZSSAM = 0
      IF (LSTPH .NE. 0) THEN
        KSTPC = LC(LSTPH-IZSTPC)
        IF (KSTPC .NE. 0) THEN
          GZSSAM = LC(KSTPC-IZSSAM)
        ENDIF
      ENDIF
      RETURN
      END
