      SUBROUTINE BKUser(idet)
C======================================================================
C
C   Purpose and Methods :  Books the bank "USER" for my own devious
C                          purposes.
C
C-  Inputs : integer idet = detector id (1:VTX, 2:CDC, 3:FDC, 4:TRD)
C-  Output : books USER bank.
C-
C    Created   4-JAN-1987  T. Trippe
C             20-APR-1987  G. Rahal   adapted to CDC
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau   use ERRMSG and put in
C-                                                 version number
C    mod. for USER bank sept 91, CK
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INTEGER MPCDDN(5), ISETVN, luser
      LOGICAL FIRST
      integer idet
      character*4 cdet(4)
      data cdet / 'CDD1', 'CDD2', 'CDD3', 'CDD4' /
      DATA FIRST / .TRUE. /
      DATA MPCDDN / 0, 0, 0, 10000, 1 /
C
C======================================================================
C
C      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( cdet(idet), MPCDDN, 4, 4 )
C      ENDIF
      IF ( LHEAD .EQ. 0 ) THEN
        CALL ERRMSG('DTRAKS','BKUSER',
     &      ' Header bank LHEAD not booked ','W')
        GO TO 999
      ENDIF
C
C ****  Book USER
C
      LUSER = LQ ( LHEAD - IZUSER )
      IF ( Luser .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, Luser, LHEAD, -IZuser, MPCDDN, 3 )
      ENDIF
C
      IQ(Luser) = ISETVN(IQ(Luser),0)
  999 CONTINUE
      RETURN
      END
