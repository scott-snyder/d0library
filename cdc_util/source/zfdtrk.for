      SUBROUTINE ZFDTRK(PARFIT, CHISQ, NDEGF, LABEL, RESID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store a new track in the DTRK banks...
C-
C-   Inputs  : PARFIT(9) : Parameters of the fit:x,y,z,theta,phi,errd,z,tet,phi
C-             CHISQ(2)  : Chisquare of xy, rz fits
C-             NDEGF(2)  : Degree of freedom of the fit
C-             LABEL(0:27): Label of the hits
C-             RESID(0:27,2) residuals of the xy and rz fits
C-   Outputs : a new DTRK bank...
C-
C-   Created  14-MAR-1988   Olivier Callot
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau  fix bug in MZFORM 
C-   Updated  27-JUL-1989   Qizhong Li-Demarteau  use modified bank DTRH 
C-   Updated  14-OCT-1991   Qizhong Li-Demarteau  added a reference link in
C-                                                DTRK bank to ZTRK
C-   Updated  24-AUG-1992   Qizhong Li-Demarteau  added covariance term in
C-                                      track parameters
C-   Updated  11-FEB-1993   Qizhong Li-Demarteau  fill word +7 in DTRH
C-   Updated  15-MAY-1993   Qizhong Li-Demarteau  added words 23-26 in DTRK
C-   Updated  17-NOV-1993   Stefano Lami          changed DTRK bank version
C-                                                number (from 0 to 1)
C-   Updated  20-MAY-1994   Srini Rajagopalan  Pass 0 to MZLIFT to reset bank 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      REAL    PARFIT(10), CHISQ(2), RESID(0:27,2)
      INTEGER NDEGF(2), LABEL(0:27), MPDTRK(5), MPDTTH(5), LDTTH, IP, IW
      INTEGER ISETVN
      LOGICAL FIRST
      DATA MPDTRK / 0, 2, 1, 27, 0 /
      DATA MPDTTH / 0, 0, 0,  0, 0 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'DTRK', MPDTRK(1), 4, 4 )
        CALL MZFORM('DTRK', '1B 4I 8F 2I 12F', MPDTRK(5) )
        CALL UCTOH( 'DTTH', MPDTTH(1), 4, 4 )
        CALL MZFORM('DTTH', '/ 1B 1F', MPDTTH(5) )
      ENDIF
      IF ( LDTRK .EQ. 0 ) THEN
C
C ****  first track. Book from DTRH
C
        IF (LDTRH .LE. 0) CALL BKDTRH
        CALL MZLIFT( IXMAIN, LDTRK, LDTRH, -1, MPDTRK, 0 )
        IQ( LDTRK-5) = 1
      ELSE
        CALL MZLIFT( IXMAIN, LDTRK, LDTRK, 0, MPDTRK, 0 )
      ENDIF
      IQ(LDTRK) = ISETVN(IQ(LDTRK),0)
C
C ****  Update track counter
C
      IQ(LDTRH+2) = IQ(LDTRH+2) + 1
      IQ(LDTRH+7) = IQ(LDTRH+7) + 1
C
C ****  Now fill the values
C
C
      IQ( LDTRK   ) = IBSET(IQ(LDTRK),13)   ! bank version set to 1 
      IQ( LDTRK+1 ) = 0                 ! status
      IQ( LDTRK+2 ) = NDEGF(1)+2        ! nb xy hits
      IQ( LDTRK+3 ) = 0
      IQ( LDTRK+4 ) = 0
      IQ( LDTRK+5 ) = NDEGF(2)+2
      Q(  LDTRK+6 ) = PARFIT(5)
      Q(  LDTRK+7 ) = PARFIT(1)
      Q(  LDTRK+8 ) = PARFIT(2)
      Q(  LDTRK+9 ) = PARFIT(4)
      Q(  LDTRK+10) = SQRT( PARFIT(1)**2+PARFIT(2)**2 )
      Q(  LDTRK+11) = PARFIT(3)
      Q(  LDTRK+12) = CHISQ(1)
      Q(  LDTRK+13) = CHISQ(2)
      IQ( LDTRK+14) = NDEGF(1)+NDEGF(2)
      IQ( LDTRK+15) = 0
      Q(  LDTRK+16) = PARFIT(9)
      Q(  LDTRK+17) = PARFIT(6)
      Q(  LDTRK+18) = PARFIT(8)
      Q(  LDTRK+19) = PARFIT(7)
      Q(LDTRK + 22) = PARFIT(10)
C
      MPDTTH(4) = 2*( IQ(LDTRK+2) + IQ(LDTRK+5) )
      CALL MZLIFT( IXMAIN, LDTTH, LDTRK, -1, MPDTTH, -1)
      IQ(LDTTH) = ISETVN(IQ(LDTTH),0)
      IP = LDTTH
      DO 100 IW = 0, 27
        IF( LABEL(IW) .EQ. 0 ) GOTO 100
        IQ( LDTRK+3 ) = IBSET( IQ(LDTRK+3) , IW )
        IQ( IP+1 ) = LABEL(IW)
        Q(  IP+2 ) = RESID( IW, 1 )
        IP = IP + 2
  100 CONTINUE
      DO 110 IW = 0, 27
        IF( LABEL(IW) .EQ. 0 .OR. RESID(IW,2) .GT. 900. ) GOTO 110
        IQ( LDTRK+4 ) = IBSET( IQ(LDTRK+4) , IW )
        IQ( IP+1 ) = LABEL(IW)
        Q(  IP+2 ) = RESID( IW, 2 )
        IP = IP + 2
  110 CONTINUE
  999 RETURN
      END
