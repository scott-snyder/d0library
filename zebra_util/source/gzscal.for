      INTEGER FUNCTION GZSCAL(CHSTP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find address of Calorimeter Static
C-                         Parameters Zebra bank, SCAL, which hangs
C-                         from the bank given by the argument string
C-                         CHSTP which may be any one of the banks
C-                         STPO, STPC, or STPN
C-
C-   Inputs  :  CHSTP           character name of supporting bank,
C-                              which must be one of the choices
C-                              'STPO', 'STPC', or 'STPN'
C-   Outputs :  GZSCAL          SCAL bank address
C-                                -1 --> SCAL bank not found because
C-                                       STPH bank does not exist
C-                                 0 --> SCAL bank not found because
C-                                       'CHSTP' bank does not exist
C-   Controls:  none
C-   Requires:  ZEBSTP          SCAL bank plus supporting structure
C-   Fills   :  none
C-   Calls   :  none
C-
C-   Created   7-JUL-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
C
      CHARACTER*(*) CHSTP
C
      INTEGER LSTP
C
C ****  Check for existence of STPH header bank
C
      IF ( LSTPH .LE. 0 ) THEN
        GZSCAL = -1
        GO TO 999
      ENDIF
C
C ****  Get link to STP? bank, which supports SCAL bank
C
      LSTP = 0
      IF ( CHSTP .EQ. 'STPO' ) THEN
        LSTP = LC(LSTPH - IZSTPO)
      ELSEIF ( CHSTP .EQ. 'STPC' ) THEN
        LSTP = LC(LSTPH - IZSTPC)
      ELSEIF ( CHSTP .EQ. 'STPN' ) THEN
        LSTP = LC(LSTPH - IZSTPN)
      ENDIF
C
C ****  Check for existence of STP? bank
C
      IF ( LSTP .LE. 0 ) THEN
        GZSCAL = 0
        GO TO 999
      ENDIF
C
C ****  Get address of SCAL bank
C
      GZSCAL = LC(LSTP - IZSCAL)
C
  999 RETURN
      END
