      SUBROUTINE VERTE2(Z0AVR,NENTRY)
C------------------------------------------------------------------------
C
C  Get VTX tracks and check if they point to (x,y)=(0,0) within tolerance.
C  Find z(r=0) and fill a histogram of z(r=0)
C
C  Daria Zieminska  2-MAR-1991
C-   Updated   7-DEC-1992   Qizhong Li-Demarteau  added EZRSET and EZERROR,
C-                                                also added SAVE statement
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
      REAL ZLOW,ZHI,DELTAZ,XG,YG,ZG,SG,RG,PHIG,THETA,PHI,Z0,Z0AVR
      REAL IMPACT,TOL2
      INTEGER NENTRY,NBIN2,LVTRH,GZVTRH,LVTXT,NID
      INTEGER IER,ICALL
      LOGICAL OK,VTRAKS
      LOGICAL EZERROR
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','VERTE2',
     &       'Unable to find bank VERTEX_RCP','W')
          GOTO 1000
        ENDIF
        CALL EZGET('TOL2',TOL2,IER)
        CALL EZGET('ZHI',ZHI,IER)
        CALL EZGET('NBIN2',NBIN2,IER)
        CALL EZRSET
        ZLOW=-ZHI
        DELTAZ=(ZHI-ZLOW)/FLOAT(NBIN2)
        ICALL=1
      END IF
      OK=VTRAKS()
      LVTRH=GZVTRH()
      LVTXT=LQ(LVTRH-IZVTXT)
  100 CONTINUE
      IF (LVTXT.LE.0) GO TO 300
      XG=Q(LVTXT+7)
      YG=Q(LVTXT+8)
      PHIG=ATAN2(YG,XG)
      IF (PHIG .LT. 0.) PHIG=PHIG+TWOPI
      SG=Q(LVTXT+10)
      RG=SQRT(XG**2+YG**2)+SG
      PHI=Q(LVTXT+6)
      IMPACT=ABS(RG*SIN(PHIG-PHI))
      IF (IMPACT.GT.TOL2) GO TO 200
      THETA=Q(LVTXT+9)
      IF (THETA.EQ.0.) GO TO 200
      ZG=Q(LVTXT+11)
      Z0=ZG-RG/TAN(THETA)
      IF (ABS(Z0).LT.ZHI) THEN
        Z0AVR=(Z0AVR*FLOAT(NENTRY)+Z0)/FLOAT(NENTRY+1)
        IF (NENTRY.EQ.0) THEN
          CALL HBOOK1(1,' z(r=0) from VTX hits $',NBIN2,ZLOW,ZHI)
        END IF
        CALL HFF1(1,NID,Z0,1.)
        NENTRY=NENTRY+1
      END IF
  200 CONTINUE
      LVTXT=LQ(LVTXT)
      GO TO 100
C
  300 CONTINUE
 1000 RETURN
      END
