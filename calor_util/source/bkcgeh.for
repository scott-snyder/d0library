      SUBROUTINE BKCGEH (CHSTP,LLCGEH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Calorimeter Geometry Parameters 
C-                         header bank, CGEH, which hangs beneath the
C-                         bank given by the argument string CHSTP
C-                         which may be any one of the banks STPO,
C-                         STPC, or STPN. The sequence of banks is
C-                         STPx---SCAL---CGEH.
C-
C-   Inputs  :  CHSTP           character name of supporting bank,
C-                              which must be one of the choices
C-                              'STPO', 'STPC', or 'STPN'
C-   Outputs :  LLCGEH          CGEH bank address
C-                                -1 --> CGEH bank creation fails
C-                                       because STPH bank does
C-                                       not exist
C-                                 0 --> CGEH bank creation fails
C-                                       because 'CHSTP' bank does
C-                                       not exist
C-   Controls:  none
C-   Calls   :  MZBOOK          Zebra bank creation
C-              IDATE           Date intrinsic function
C-              TIME            Time intrinsic function
C-
C-   Created   15-JAN-1989   Harrison B. Prosper
C-   Updated   17-Sep-1992   Herbert Greenlee
C-      Fix I/O characteristic
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) CHSTP
      CHARACTER*8 STIME
C
      INTEGER LLCGEH
      INTEGER LSTP
      INTEGER RUN, RUNLO, RUNHI
      INTEGER I, J, K, JDATE, JTIME
      REAL    VERSN
C
      PARAMETER( VERSN = 2.0 )
      PARAMETER( RUN = 0 )
      PARAMETER( RUNLO = 0 )
      PARAMETER( RUNHI = 99999 )
C
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER IXIO
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('CGEH', '9I 1F', IXIO)
      ENDIF
C
C ****  Check for existence of STPH header bank
C
      IF ( LSTPH .LE. 0 ) THEN
        LLCGEH = -1
        GO TO 999
      ENDIF
C
C ****  Get link to STP? bank, which supports CGEH, via SCAL
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
        LLCGEH = 0
        GO TO 999
      ENDIF
C
C ****  Create SCAL bank if it does not yet exist
C
      LSCAL = LC(LSTP-IZSCAL)
      IF ( LSCAL .LE. 0 ) CALL BKSCAL (CHSTP,LSCAL)
      IF ( LSCAL .LE. 0 ) THEN
        LLCGEH = 0
        GOTO 999
      ENDIF
C
C ****  Book Calorimeter GEOMETRY Parameters bank CGEH
C
      CALL MZBOOK(IDVSTP,LLCGEH,LSCAL,-IZCGEH,'CGEH',10,10,10,IXIO,0)
C
C ****  Get date and time and pack into CGEH data
C
      CALL IDATE(I,J,K)
      JDATE = K + J*100 + I*10000
      CALL TIME(STIME)
      READ(UNIT=STIME,FMT='(I2,2(1X,I2))') I,J,K
      JTIME = K + J*100 + I*10000
C
      IC(LLCGEH+4) = RUNLO
      IC(LLCGEH+5) = RUNHI
      IC(LLCGEH+6) = RUN
      IC(LLCGEH+7) = JDATE
      IC(LLCGEH+8) = JTIME
      C(LLCGEH+10) = VERSN ! Version number
C
  999 RETURN
      END
