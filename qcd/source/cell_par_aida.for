      SUBROUTINE CELL_PAR_AIDA(IETA,IPHI,ILYR,IVER,CELENG,
     &  COORD,DIRECT,VECENG,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATE CELL COORDINATE (X, Y, Z) AND CELL
C-   DIRECTION (ETA, PHI) AND (EX, EY, EZ) WITH RESPECT TO A GIVEN VERTEX
C-
C-   Inputs  : IETA OF THE CELL
C-             IPHI OF THE CELL
C-             ILYR OF THE CELL
C-             IVER: VERTEX NUMBER
C-             CELENG: CELL ENERGY
C-
C-   Outputs : COORD(3) (X, Y, Z) OF THE CELL
C-             DIRECT(2) (ETA, PHI) OF THE CELL
C-             VECENG(3) (EX,EY,EZ) OF THE CELL
C-             IERR = 0    OK
C-             IERR = -1   INPUT ERROR ON IETA, IPHI, ILYR
C-             IERR = -3,-4 COORDINATE CALULATION ERROR
C-
C-   Created   4-OCT-1995   Frank Hsieh
C-            22-OCT-1995   F. Hsieh    add IVER input, VECENG output
C-            07-DEC-1995   Andrew Brandt Use 0,0,slowz if no vertex found
C-            08-MAR-1996   Andrew Brandt Rename to CELL_PAR_AIDA
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INTEGER IETA,IPHI,ILYR,IER,IERR,LVERT,IVER,GZVERT
      EXTERNAL GZVERT
      REAL COORD(3),DIRECT(2),XCEL,YCEL,ZCEL,XVER,YVER,ZVER,XX,YY,ZZ,PI,
     &  ETA,PHI,RR,THETA,DUMM,VECENG(3),CELENG
      PARAMETER (PI = 3.141592654)
C----------------------------------------------------------------------
      IERR = 0
      CALL VZERO(COORD,3)
      CALL VZERO(DIRECT,2)
      CALL VZERO(VECENG,3)
      IF (IETA.EQ.0) IERR = -1
      IF ((IPHI.LT.1).OR.(IPHI.GT.64)) IERR = -1
      IF ((ILYR.LT.1).OR.(ILYR.GT.17)) IERR = -1
      IF (IERR.NE.0) GOTO 999

      CALL CELXYZ(IETA,IPHI,ILYR,XCEL,YCEL,ZCEL,IER)
      IF (IER.NE.0) THEN
        IERR = -1
        CALL ERRMSG('CELL_PAR_AIDA','CELL_PAR_AIDA',
     &   'CELL ERROR DURING AIDA UNPACK ','W')
        GOTO 999
      END IF

C
C If no vertex x=y=0 and for z use  JUTL or SLOWZ or 0
C
      LVERT = GZVERT(IVER)
      IF (LVERT.LE.0) THEN
        CALL ERRMSG('CELL_PAR_AIDA','CELL_PAR_AIDA',
     &   'NO VERTEX FOUND FOR AIDA UNPACK--USE L0 OR DEFAULT ','W')
        XVER = 0.
        YVER = 0.
        IF(TRK_NV.GT.0) THEN
          ZVER = TRK_Z(1)
        ELSE IF (L0_ZF(1).EQ.1) THEN
          ZVER = L0_Z(1)
        ELSE
          ZVER=0
        END IF
      ELSE
        XVER = Q(LVERT + 3)
        YVER = Q(LVERT + 4)
        ZVER = Q(LVERT + 5)
      END IF

      XX = XCEL - XVER
      YY = YCEL - YVER
      ZZ = ZCEL - ZVER
C
C CALCULATE CELL ETA, PHI W.R.T. VERTEX
      IF ((XX.EQ.0.).AND.(YY.EQ.0.)) THEN
        IERR = -3
        GOTO 999
      END IF

      IF (YY.EQ.0.) THEN
        PHI = PI * ( -SIGN(0.5, XX) + 0.5 )
      ELSE
        PHI = ATAN2(YY,XX)
        IF (PHI.LT.0.) PHI = PHI + 2.* PI
      END IF

      RR = SQRT(XX**2 + YY**2 + ZZ**2)
      THETA = ACOS( ZZ / RR )
      DUMM = TAN( 0.5 * THETA )
      IF (DUMM.LE.0.) THEN
        IERR = -4
        GOTO 999
      END IF
      ETA = -LOG(DUMM)

      COORD(1) = XX
      COORD(2) = YY
      COORD(3) = ZZ
      DIRECT(1) = ETA
      DIRECT(2) = PHI
      VECENG(1) = CELENG * SIN(THETA) * COS(PHI)
      VECENG(2) = CELENG * SIN(THETA) * SIN(PHI)
      VECENG(3) = CELENG * COS(THETA)

  999 RETURN
      END
