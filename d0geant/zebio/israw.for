      SUBROUTINE ISRAW(FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop all banks under LHEAD *except* the raw data
C-                              banks.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: FLAG = Operation flag, for now it's just non-zero to drop banks.
C-
C-   Created  30-JUL-1990   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    FLAG
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER NFIRST
      DATA    NFIRST/10/
      INTEGER NS,I
C
C----------------------------------------------------------------------
C
      IF ( FLAG.NE.0 ) THEN
        IF ( LHEAD.NE.0 ) THEN
          NS    = IQ(LHEAD-2)           ! number of structural links
          DO I =  NFIRST, NS
            IF ( LQ(LHEAD-I).NE.0 ) CALL MZDROP(IXCOM,LQ(LHEAD-I),'L')
          ENDDO
        ENDIF
      ENDIF
C
  999 RETURN
      END
