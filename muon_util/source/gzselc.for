      INTEGER FUNCTION GZSELC (NST, NSEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS electronic constant 
C-                         SELC bank for station NST, section NSEC
C-
C-   Returned value  : bank SELC address (or zero if something is bad)
C-
C-   Inputs  : NST  - Station number
C-             NSEC - Section number
C-   Outputs : None
C-   Controls: 
C-
C-   Created  20-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-   Updated  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov   
C-   Updated  30-JUN-1992   Vladimir Glebov: Change structure to 
C-                          SAMCON_TREE.ZEB Version 2.0 
C-                                        
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSELH.LINK'
      INTEGER KSSAM,KSELH,NL,NST,NSEC
      INTEGER GZSSAM
      EXTERNAL GZSSAM
C
      GZSELC = 0
      KSSAM = GZSSAM()
      IF (KSSAM .NE. 0) THEN
        KSELH = LC(KSSAM-IZSELH)
        IF (KSELH .NE. 0) THEN
          NL = 6 * (NST-1) + NSEC
          GZSELC = LC(KSELH-NL)
        ENDIF
      END IF
C
      RETURN
      END
