      REAL FUNCTION ENDSEG1(XG,YG,AL,XG1,YG1,AL1)
C-------------------------------------------------------------------------
C
C  Check matching two track segments at the ends.
C  Return MATCH=.TRUE. if distance is less than TOLDIS
C
C  Input: XG,YG,AL     center of gravity and asimuth of 1-st segment
C         XG1,YG1,AL1  same for 2-nd segment
C  Return
C
C  Daria Zieminska , May 1987
C     Modified 24-SEP-1992     M. Pang   Rewrote the formular
C     Modified 12-OCT-1992     M. Pang   Removed unnecessary sigular point
C-   Updated   5-OCT-1993   Ed Oltman    Rename ENDSEG and make into a real 
C-                                          function -- remove arg.
C
C-------------------------------------------------------------------------
      IMPLICIT NONE
      REAL  XG,YG,AL,XG1,YG1,AL1,DIS
      REAL X1,X2,Y1,Y2
      REAL TANAL, TANAL1, XDIFF, YDIFF, XDIFF2, DENOM
C-------------------------------------------------------------------------
C
      ENDSEG1=9.e19
C
      IF (XG*XG1.LT.0..AND.YG*YG1.LT.0) GO TO 1000
C
      TANAL = TAN(AL)
      TANAL1 = TAN(AL1)
      XDIFF = XG1 - XG
      XDIFF2 = XG1**2 - XG**2
      YDIFF = YG1 - YG
C
C ****  Project first segment onto perp bisector
C
      DENOM = TANAL*YDIFF + XDIFF
      IF ( DENOM .EQ. 0. ) GO TO 1000
      X1 = ((XG*TANAL + YDIFF*0.5)*YDIFF + 0.5*XDIFF2) / DENOM
C
      Y1 = (X1-XG)*TANAL + YG
C
C ****  Project second segment
C
      DENOM = TANAL1*YDIFF + XDIFF
      IF ( DENOM .EQ. 0. )GO TO 1000
      X2 = ((XG1*TANAL1 - YDIFF*0.5)*YDIFF + 0.5*XDIFF2) / DENOM
C
      Y2 = (X2-XG1)*TANAL1 + YG1
C
C ****  Calc distance between projected points
C
      ENDSEG1 = SQRT((X2-X1)**2 + (Y2-Y1)**2)
C
 1000 RETURN
      END
