      SUBROUTINE GTSSTG(ISTA,ISEC,ITUBE,RTUBE,VTUBE,TLEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SAMUS tubes geometry parameters
C-
C-   Inputs  : ISTA  - station number
C-             ISEC  - section number
C-             ITUBE - tube number
C-                     if negative, -ITUBE is the offset to SSTG bank
C-
C-   Outputs : RTUBE(3,2) - vectors coordinates of tube center from
C-                           center of station
C-             VTUBE(3,2) - tube axis vectors, |VTUBE| = 1
C-             TLEN(2)    - tube length
C-
C-   Controls:
C-
C-   Created  8-SEP-1994  M. Fortner
C-   Modified 15-FEB-1995 M. Fortner  Use -ITUBE to pass SSTG reference
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER ISTA,ISEC,ITUBE
      REAL    RTUBE(3,2),VTUBE(3,2),TLEN(2)
      INTEGER LSSEC,LSSTG,ISSTG,JSSTG
      INTEGER GZSSEC
      EXTERNAL GZSSEC
C
C ****  Get addresses of the geometry banks SSEC and SSTG
C
      TLEN(1) = 0.0
      LSSEC = GZSSEC(ISTA,ISEC)
      IF (LSSEC.EQ.0) GOTO 999
      LSSTG = LC(LSSEC-1)
      IF (LSSTG.EQ.0) GOTO 999
      IF (ITUBE.GE.0) THEN
        ISSTG = IC(LSSTG+ITUBE)
      ELSE
        ISSTG = -ITUBE
      ENDIF
      IF (ABS(ISSTG).GE.IC(LSSTG-1)) GOTO 999
      IF (ISSTG.GT.0) THEN
        JSSTG = LSSTG + ISSTG
      ELSE
        JSSTG = LSSTG - ISSTG
      ENDIF
C
C ****  Get constants for first tube
C
      RTUBE(1,1) = C(JSSTG+1)
      RTUBE(2,1) = C(JSSTG+2)
      RTUBE(3,1) = C(JSSTG+3)
      VTUBE(1,1) = C(JSSTG+4)
      VTUBE(2,1) = C(JSSTG+5)
      VTUBE(3,1) = C(JSSTG+6)
      TLEN(1) = C(JSSTG+7)
C
C ****  Get constants for split tube
C
      TLEN(2) = 0.0
      IF (ISSTG.LT.0) THEN
        JSSTG = JSSTG + 7
        RTUBE(1,2) = C(JSSTG+1)
        RTUBE(2,2) = C(JSSTG+2)
        RTUBE(3,2) = C(JSSTG+3)
        VTUBE(1,2) = C(JSSTG+4)
        VTUBE(2,2) = C(JSSTG+5)
        VTUBE(3,2) = C(JSSTG+6)
        TLEN(2) = C(JSSTG+7)
      END IF
C
  999 CONTINUE
      RETURN
      END
