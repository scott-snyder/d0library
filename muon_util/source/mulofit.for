      SUBROUTINE MULOFIT(IMUON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get hit position in global coordinate with
C-                         error and fit them with straight line,
c-                         giving errors on parameters.
C-
C-   Inputs  : IMUON - number of the track
C-   Outputs : MSEG banks booked and filled with the following contents:
C-             IABC   : 0 A, 1 BC
C-             NHIT   : number of hit on given track
C-             X,Y,Z  : hit position in global coordinate
C-             XERR,YERR,ZERR : error of hit
C-             AY, Y0 - parameters of mufseg : Y=AY*X+Y0
C-             DY- error matrix for AY, Y0
C-             AZ, Z0 - parameters of mufseg : Z=AZ*X+Z0
C-             DZ- error matrix for AZ, Z0
C-
C-
C-
C-   Controls:
C-
C-   Created  09-MAR-1993 Regina Demina
C-
C-   V-1.0 Currently all errors are same value and hard wired.
C-   Updated   7-JUL-1994   Daria Zieminska protect against 0 errors
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZMSEG.LINK'
C
      INTEGER LMUON, IMUON, GZMUON,NS
      INTEGER LMUOT
      INTEGER LMSEG
      INTEGER NHIT_MAX
      PARAMETER(NHIT_MAX=32)
      INTEGER  IHIT, ISEG, K
      INTEGER IORIENT, MUORIENT
      REAL     X(NHIT_MAX), Y(NHIT_MAX), Z(NHIT_MAX)
      REAL     XERR(NHIT_MAX), YERR(NHIT_MAX), ZERR(NHIT_MAX)
      REAL SUBS
      REAL XSUB, YSUB, ZSUB
      INTEGER IPOINT
      REAL Y0, AY, DY(2,2), CHI2Y, SMALL
      REAL Z0, AZ, DZ(2,2), CHI2Z
      INTEGER NSM,NHM
      PARAMETER (NSM=10)       ! MAXIMUM NUMBER OF SEGMENTS
      PARAMETER (NHM=20)       ! MAXIMUM NUMBER OF HITS/SEGEMENT
      INTEGER NSEG,NHIT(NSM),MMOD(NSM,NHM),IMUOT,IABC
      REAL XYZ(NSM,NHM,3),DXYZ(NSM,NHM,3),CXYZ(NSM,NHM,3)
      DATA XSUB, YSUB, ZSUB/300.,300.,400./
      DATA SMALL/1.0E-8/
C
      LMUON = GZMUON(IMUON)
      NS = IQ(LMUON-2)
      LMUOT = LQ(LMUON-NS-1)
      IMUOT = IQ(LMUOT-5)
      LMSEG = LQ(LMUON-IZMSEG)
      IF(LMSEG.GT.0) GOTO 999
      DO IABC = 0,1
        CALL MULOCAL(IMUOT,IABC,NSEG,NHIT,XYZ,DXYZ,MMOD,CXYZ)
C
        DO ISEG = 1,NSEG
          CALL VZERO(X,NHIT_MAX)
          CALL VZERO(Y,NHIT_MAX)
          CALL VZERO(Z,NHIT_MAX)
          CALL VZERO(YERR,NHIT_MAX)
          CALL VZERO(ZERR,NHIT_MAX)
          IORIENT = MUORIENT(MMOD(ISEG,1))
C
          DO IHIT = 1, NHIT(ISEG)
            X(IHIT) = XYZ(ISEG,IHIT,1)
            Y(IHIT) = XYZ(ISEG,IHIT,2)
            Z(IHIT) = XYZ(ISEG,IHIT,3)
            XERR(IHIT) = SQRT( DXYZ(ISEG,IHIT,1)**2+CXYZ(ISEG,IHIT,1)**
     &        2)
            YERR(IHIT) = SQRT( DXYZ(ISEG,IHIT,2)**2+CXYZ(ISEG,IHIT,2)**
     &        2)
            ZERR(IHIT) = SQRT( DXYZ(ISEG,IHIT,3)**2+CXYZ(ISEG,IHIT,3)**
     &        2)
          ENDDO
C
C
          IF(IORIENT.EQ.1)THEN
            IF(Y(1).GT.0) THEN
              SUBS = YSUB
            ELSE
              SUBS = - YSUB
            ENDIF
            IF (IABC.EQ.0) SUBS=Q(LMUOT+9)
            IF (IABC.EQ.1) SUBS=Q(LMUOT+12)
C
            CALL MUFSEG(NHIT(ISEG), Y, SUBS, X, XERR, AY, Y0, DY, CHI2Y)
            CALL MUFSEG(NHIT(ISEG), Y, SUBS, Z, ZERR, AZ, Z0, DZ, CHI2Z)
C
          ELSEIF(IORIENT.EQ.2)THEN
C
            IF(X(1).GT.0) THEN
              SUBS = XSUB
            ELSE
              SUBS = - XSUB
            ENDIF
            IF (IABC.EQ.0) SUBS=Q(LMUOT+8)
            IF (IABC.EQ.1) SUBS=Q(LMUOT+11)
C
            CALL MUFSEG(NHIT(ISEG), X, SUBS, Y, YERR, AY, Y0, DY, CHI2Y)
            CALL MUFSEG(NHIT(ISEG), X, SUBS, Z, ZERR, AZ, Z0, DZ, CHI2Z)
C
          ELSEIF(IORIENT.EQ.3.OR.IORIENT.EQ.4)THEN
C
            IF(Z(1).GT.0) THEN
              SUBS = ZSUB
            ELSE
              SUBS = - ZSUB
            ENDIF
            IF (IABC.EQ.0) SUBS=Q(LMUOT+10)
            IF (IABC.EQ.1) SUBS=Q(LMUOT+13)
C
            CALL MUFSEG(NHIT(ISEG), Z, SUBS, X, XERR, AY, Y0, DY, CHI2Y)
            CALL MUFSEG(NHIT(ISEG), Z, SUBS, Y, YERR, AZ, Z0, DZ, CHI2Z)
C
          ENDIF
C
C  booking MSEG
C
          CALL BKMSEG(IMUON,IPOINT)
          IF(IPOINT.EQ.0) GOTO 555
C
          DO K=1,2
            IF (DY(K,K).LT.SMALL) DY(K,K)=999999.
            IF (DZ(K,K).LT.SMALL) DZ(K,K)=999999.
          END DO
          IQ(IPOINT + 1) = IABC
          IQ(IPOINT + 3) = NHIT(ISEG)
          IQ(IPOINT + 4) = IORIENT
          Q(IPOINT + 10) = SUBS
          Q(IPOINT + 11) = AY
          Q(IPOINT + 12) = Y0
          Q(IPOINT + 13) = DY(1,1)
          Q(IPOINT + 14) = DY(1,2)
          Q(IPOINT + 15) = DY(2,1)
          Q(IPOINT + 16) = DY(2,2)
          Q(IPOINT + 17) = CHI2Y
          Q(IPOINT + 18) = AZ
          Q(IPOINT + 19) = Z0
          Q(IPOINT + 20) = DZ(1,1)
          Q(IPOINT + 21) = DZ(1,2)
          Q(IPOINT + 22) = DZ(2,1)
          Q(IPOINT + 23) = DZ(2,2)
          Q(IPOINT + 24) = CHI2Z
          IF (IABC.EQ.0) THEN
            IF (IQ(LMUOT+4).EQ.5) THEN
C              WRITE(0,101) AY,Y0,AZ,Z0,CHI2Y,CHI2Z
C  101         FORMAT(' ASTUB MSEG',6F12.3)
            ELSE
C              WRITE(0,102) AY,Y0,AZ,Z0,CHI2Y,CHI2Z
C  102         FORMAT(' FULL TRACK',6F12.3)
            END IF
          END IF
C
  555   ENDDO
      ENDDO
  999 RETURN
      END
