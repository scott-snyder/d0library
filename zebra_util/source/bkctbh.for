      SUBROUTINE BKCTBH (CHSTP,LLCTBH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Test Beam Geometry Parameters 
C-                         header bank, CTBH, which hangs beneath the
C-                         bank given by the argument string CHSTP
C-                         which may be any one of the banks STPO,
C-                         STPC, or STPN. The sequence of banks is
C-                         STPx---SCAL---CTBH.
C-
C-   Inputs  :  CHSTP           character name of supporting bank,
C-                              which must be one of the choices
C-                              'STPO', 'STPC', or 'STPN'
C-   Outputs :  LLCTBH          CTBH bank address
C-                                -1 --> CTBH bank creation fails
C-                                       because STPH bank does
C-                                       not exist
C-                                 0 --> CTBH bank creation fails
C-                                       because 'CHSTP' bank does
C-                                       not exist
C-   Controls:  none
C-   Calls   :  MZBOOK          Zebra bank creation
C-              IDATE           Date intrinsic function
C-              TIME            Time intrinsic function
C-
C-   Created   12-OCT-1989   Stuart Fuess  From BKCGEH 
C-   Updated   16-MAR-2004   sss - use idate2k instead of idate.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) CHSTP
      CHARACTER*8 STIME
C
      INTEGER LLCTBH
      INTEGER LSTP
      INTEGER RUN, RUNLO, RUNHI
      INTEGER I, J, K, JDATE, JTIME
      REAL    VERSN
      INTEGER NIO
C
      PARAMETER( VERSN = 1.0 )
      PARAMETER( RUN = 0 )
      PARAMETER( RUNLO = 0 )
      PARAMETER( RUNHI = 99999 )
C
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCTBH.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  SET FORMAT of CTBH bank to make exchange mode happy
C
      IF (FIRST) THEN
        CALL MZFORM('CTBH','9I 1F -I',NIO)
        FIRST = .FALSE.
      ENDIF
C
C ****  Check for existence of STPH header bank
C
      IF ( LSTPH .LE. 0 ) THEN
        LLCTBH = -1
        GO TO 999
      ENDIF
C
C ****  Get link to STP? bank, which supports CTBH, via SCAL
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
        LLCTBH = 0
        GO TO 999
      ENDIF
C
C ****  Create SCAL bank if it does not yet exist
C
      LSCAL = LC(LSTP-IZSCAL)
      IF ( LSCAL .LE. 0 ) CALL BKSCAL (CHSTP,LSCAL)
      IF ( LSCAL .LE. 0 ) THEN
        LLCTBH = 0
        GOTO 999
      ENDIF
C
C ****  Book Calorimeter GEOMETRY Parameters bank CTBH
C
      CALL MZBOOK(IDVSTP,LLCTBH,LSCAL,-IZCTBH,'CTBH',7,7,10,NIO,0)
C
C ****  Get date and time and pack into CTBH data
C
      CALL IDATE2k(I,J,K)
      JDATE = K + J*100 + I*10000
      CALL TIME(STIME)
      READ(UNIT=STIME,FMT='(I2,2(1X,I2))') I,J,K
      JTIME = K + J*100 + I*10000
C
      IC(LLCTBH+4) = RUNLO
      IC(LLCTBH+5) = RUNHI
      IC(LLCTBH+6) = RUN
      IC(LLCTBH+7) = JDATE
      IC(LLCTBH+8) = JTIME
      C(LLCTBH+10) = VERSN ! Version number
C
  999 RETURN
      END
