      INTEGER FUNCTION GZMSTH(IDUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get pointer to MSTH bank
C-
C-   Returned value  : ZEBRA pointer to MSTH bank in ZEBSTP 
C-   Inputs  : IDUM   ! dummy argument to make GZMSTH look like other GZ 
C                     ! routines
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUL-1993   J.Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMSTH.LINK'
      INTEGER KSTPC,KSMUO,KMSTH,IDUM
C----------------------------------------------------------------------
      GZMSTH=0
      KSTPC=LC(LSTPH-IZSTPC)
      IF(KSTPC.NE.0) THEN
        KSMUO=LC(KSTPC-IZSMUO)
        IF(KSMUO.NE.0) THEN
          GZMSTH=LC(KSMUO-IZMSTH)
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
