      INTEGER FUNCTION GZCPTB()

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPTB
C-                            Calorimeter trigger pedestals bad channels
C-   Returned value  : Link to bank
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  13-JUL-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPTR.LINK'
      INCLUDE 'D0$LINKS:IZCPTB.LINK'
      INTEGER GZCPDH,GZCPTR,LCPTR
C----------------------------------------------------------------------
      GZCPTB = 0
      LCPDH = GZCPDH()
      IF ( LCPDH.GT.0 ) THEN
        LCPTR = LC(LCPDH-IZCPTR)
        IF ( LCPTR.GT.0 ) THEN
          GZCPTB = LC(LCPTR-IZCPTB)
        ENDIF
      ENDIF
  999 RETURN
      END
