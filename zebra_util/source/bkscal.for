      SUBROUTINE BKSCAL(CHSTP,LLSCAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Calorimeter Static Parameters 
C-                         Zebra bank, SCAL, which hangs from the
C-                         bank given by the argument string CHSTP
C-                         which may be any one of the banks STPO,
C-                         STPC, or STPN
C-
C-   Inputs  :  CHSTP           character name of supporting bank,
C-                              which must be one of the choices
C-                              'STPO', 'STPC', or 'STPN'
C-   Outputs :  LLSCAL          SCAL bank address
C-                                -1 --> SCAL bank creation fails
C-                                       because STPH bank does
C-                                       not exist
C-                                 0 --> SCAL bank creation fails
C-                                       because 'CHSTP' bank does
C-                                       not exist
C-   Controls:  none
C-   Requires:  ZEBSTP          LSTPH link to STPH bank
C-   Fills   :  ZEBSTP          SCAL bank, SCAL link in STP? bank
C-   Calls   :  MZBOOK          Zebra bank creation
C-              IDATE           Date intrinsic function
C-              TIME            Time intrinsic function
C-
C-   Created   6-JUL-1988   Stuart Fuess
C-   Updated   5-JUL-1990   Jan Guida  Chip Stewart -CAP BANK LINKS
C-   Updated  27-APR-1992   Jan Guida  Add MZFORM 
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
      CHARACTER*8 STIME
C
      INTEGER LLSCAL
      INTEGER LSTP
      INTEGER RUN, RUNLO, RUNHI
      INTEGER I, J, K, JDATE, JTIME
      INTEGER NIO
C
      PARAMETER( RUN = 0 )
      PARAMETER( RUNLO = 0 )
      PARAMETER( RUNHI = 99999 )
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  SET FORMAT of SCAL bank to make exchange mode happy
C
      IF (FIRST) THEN
        CALL MZFORM('SCAL','8I 1F -I',NIO)
        FIRST = .FALSE.
      ENDIF
C
C ****  Check for existence of STPH header bank
C
      IF ( LSTPH .LE. 0 ) THEN
        LLSCAL = -1
        GO TO 999
      ENDIF
C
C ****  Get link to STP? bank, which supports SCAL bank
C
      LSTP = 0
      IF     ( CHSTP .EQ. 'STPO' ) THEN
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
        LLSCAL = 0
        GO TO 999
      ENDIF
C
C ****  Book Calorimeter Static Parameters bank SCAL
C
      CALL MZBOOK(IDVSTP,LLSCAL,LSTP,-IZSCAL,'SCAL',6,6,10,NIO,0)
C
C ****  Get date and time and pack into SCAL data
C
      CALL IDATE(I,J,K)
      JDATE = K + J*100 + I*10000
      CALL TIME(STIME)
      READ(UNIT=STIME,FMT='(I2,2(1X,I2))') I,J,K
      JTIME = K + J*100 + I*10000
C
      IC(LLSCAL+4) = RUNLO
      IC(LLSCAL+5) = RUNHI
      IC(LLSCAL+6) = RUN
      IC(LLSCAL+7) = JDATE
      IC(LLSCAL+8) = JTIME
C
  999 RETURN
      END
