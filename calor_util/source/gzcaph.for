      FUNCTION GZCAPH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return address of currentlty selected CAPH
C-   bank.
C-
C-   Returned value  : Address of CAPH bank
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  20-MAR-1989   Z. Wolf
C-   Updated  17-JAN-1990   Harrison B. Prosper
C-      Return address of currently selected CAPH bank. Use
C-      link area LKCAPH.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKCAPH.INC'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
C
      INTEGER GZCAPH
      INTEGER GZPROC
      LOGICAL FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL CAPH_LINK
      END IF
      IF ( JCAPH .LE. 0 ) THEN
        JPROC  = GZPROC()
        IF ( JPROC .GT. 0 ) THEN
          JCAPH = LQ(JPROC-IZCAPH)
        ELSE
          JCAPH = 0
        ENDIF
      ENDIF
      GZCAPH = JCAPH
  999 RETURN
      END
