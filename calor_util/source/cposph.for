      SUBROUTINE CPOSPH(X,Y,Z,IETAC,IPHIC,LAYERC,ARGSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-                  ( Find eta, phi, layer for a space point )
C-   Inputs  : X,Y,Z = Cartesian coordinates of a space point
C-   Outputs : IETAC,IPHIC,LAYERC = Physics variables of space point
C-   Controls: ARGSOK = Error flag, 0 means no error
C-
C-   Created  19-NOV-1990   John M.Balderston
C-   Modified 26-Nov-1990   J.B.
C-   Modified 22-JAN-1991   J.B.
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL X,Y,Z
      INTEGER IETAC,IPHIC,LAYERC
      INTEGER ETAERR,PHIERR,LYRERR,AROKIN,ARGSOK
      INTEGER ETA_LO_HI,PHI_LO_HI
      LOGICAL CEXIST
      REAL RADIUS,CENRAD,DELRAD,CENZED,DELZED,RMIN,RMAX
      REAL TILT
      REAL ZMAXEC,ZMAXCC
      INTEGER MNLYOH(8:14)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA MNLYOH/5*15,16,17/
C
      IF(FIRST) THEN
        CALL CALZED(25,15,CENZED,DELZED,CENRAD,DELRAD,TILT,AROKIN)
        IF(AROKIN.NE.0) CALL ERRMSG('BAD CALZED','CPOPHQ','CPOSPH BAD 
     &CALZED 1','F')
        ZMAXEC=CENZED+DELZED/2.
        CALL CALZED(11,17,CENZED,DELZED,CENRAD,DELRAD,TILT,AROKIN)
        IF(AROKIN.NE.0) CALL ERRMSG('BAD CALZED','CPOPHQ','CPOSPH BAD 
     &CALZED 2','F')
        RMAX=CENRAD+DELRAD/2.
        CALL CALZED(37,13,CENZED,DELZED,CENRAD,DELRAD,TILT,AROKIN)
        IF(AROKIN.NE.0) CALL ERRMSG('BAD CALZED','CPOPHQ','CPOSPH BAD 
     &CALZED 3','F')
        RMIN=CENRAD-DELRAD/2.
        FIRST=.FALSE.
      ENDIF
      RADIUS=SQRT(X**2+Y**2)
      ARGSOK=1
      IF(ABS(Z).GT.ZMAXEC .OR. RADIUS.GT.RMAX .OR. RADIUS.LT.RMIN)
     1   GO TO 999
      CALL CGET_ETA(RADIUS,Z,IETAC,ETA_LO_HI,ETAERR)
      IF(ETAERR.NE.0) THEN
        ARGSOK=1
        GOTO 999
      ENDIF
      CALL CGET_PHI(X,Y,IPHIC,PHI_LO_HI,PHIERR)
      IF(PHIERR.NE.0) THEN
        ARGSOK=2
        GOTO 999
      ENDIF
      CALL CGET_LAYER(RADIUS,Z,IETAC,ETA_LO_HI,IPHIC,PHI_LO_HI,
     1               LAYERC,LYRERR)
      IF(LYRERR.NE.0) THEN
        ARGSOK=3
        GOTO 999
      ENDIF
      ARGSOK=0
  999 RETURN
      END
