      SUBROUTINE VTXYZ(ITRACK,LVTXT,X0,Y0,Z0)
C-----------------------------------------------------------------------
C
C  Returns a point ("center of gravity") on a VTX track 
C  given by track id, ITRACK or location, LVTXT
C  If in the r-z projection the chisq/d.f. is greater than 5, or theta=0,
C  Z0 is set to -999.
C
C  Input:   ITRACK       track number
C           LVTXT        track bank location
C
C  Output:  X0,Y0,Z0     point coordinates
C
C  Daria Zieminska Feb 1990
C-   Updated  23-OCT-1991   P. Grudberg (from A. Mayorov)
C-                    Correct calculation of Z0: QTRAK(9) = theta, not
C-                    tan(theta).  Also add protection from /0. 
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER ITRACK,LVTXT,LOC,GZVTXT,IQTRAK(21) 
      REAL QTRAK(21),CONT(21),QHSEC(4,24),QHZLA(3,6),X0,Y0,Z0,CHIDFZ
      EQUIVALENCE(IQTRAK,QTRAK)
C                      
      IF (ITRACK+LVTXT.EQ.0) THEN
        CALL ERRMSG(' both ITRACK and LVTXT are zero','VTXYZ',
     &  ' cannot calculate point coordinates','W')
        GO TO 1000
      ELSE IF (ITRACK.GT.0) THEN
        LOC=GZVTXT(ITRACK)
        CALL GTVTXT_LINK(LOC,CONT,QHSEC,QHZLA)
      ELSE IF (LVTXT.GT.0) THEN
        CALL GTVTXT_LINK(LVTXT,CONT,QHSEC,QHZLA)
      END IF
      CALL UCOPY(CONT,QTRAK,21)
      X0=QTRAK(7)
      Y0=QTRAK(8)
      IF ( IQTRAK(5) .GT. 0 ) THEN
        CHIDFZ=QTRAK(13)/FLOAT(IQTRAK(5))
        IF (CHIDFZ.GT.5.OR.QTRAK(9).EQ.0) THEN
          Z0=-999.
        ELSE
          Z0=QTRAK(11)-QTRAK(10)/TAN(QTRAK(9))
        END IF
      ELSE
        Z0 = -999.
      ENDIF
 1000 RETURN
      END
