      SUBROUTINE MTC_GET_XYZ(IETAIN,IPHIIN,ILYR,XPOINT,YPOINT,ZPOINT,OK)
C----------------------------------------------------------------------
C- MTC_GET_XYZ: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : get the x,y,z position of the cell
C-      designated by the input
C-      IETAIN,IPHIIN (the relative ieta and iphi relative to the tower
C-              eta and phi of the last call to MTC_MUCALEN) and
C-      ILYR (layer number 1:17)
C-
C-   Inputs  : IETAIN,IPHIIN,ILYR
C-   Outputs : xpoint,ypoint,zpoint - the x,y,z position
C-             ok = 0 means position found successfully
C-                = -1 means cell does not exist
C-
C-   Created  12-JAN-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
C- input
      INTEGER IETAIN,IPHIIN,ILYR
C- output
      REAL XPOINT,YPOINT,ZPOINT
      INTEGER OK
C- local
      INTEGER IETA,IPHI, IETA2,IPHI2, JPHI
      REAL X,Y,Z
C----------------------------------------------------------------------
      IETA = 0
      IPHI = 0
      IF(ILYR.LE.0 .OR. ILYR.GE.18) GO TO 666
      IF(ABS(IETAIN).GT.ITTHI .OR. ABS(IPHIIN).GT.ITTHI) GO TO 666

      IETA = IETATOWER(ILYR)
      IPHI = IPHITOWER(ILYR)

      IETA2 = IETA + IETAIN
      IF(IETA2.EQ.0) IETA2 = IETA2 + IETAIN
      IF(ABS(IETA2).GE.38) GO TO 666

      IPHI2 = IPHI + IPHIIN
      JPHI = IPHI2
      IF (IPHI2.GE.65) JPHI = MOD(IPHI2,64)
      IF (IPHI2.LE.0)  JPHI = 64 + IPHI2
      IPHI2 = JPHI

      IF(ILYR.NE.3) THEN
C- if not EM3
        CALL CELXYZ(IETA2, IPHI2, ILYR, X, Y, Z, OK)
      ELSE
C- if EM3, then energy weight the position of the energy deposition ...
        CALL MTC_EXYZEM3(IETA2,IPHI2, X,Y,Z, OK)
      END IF
      IF(OK.NE.0) GO TO 666

      XPOINT = X
      YPOINT = Y
      ZPOINT = Z
      OK     = 0
      GO TO 999
C----------------------------------------------------------------------
  666 CONTINUE
      XPOINT = 0.
      YPOINT = 0.
      ZPOINT = -1000.
      OK     = -1
      WRITE(6,88) IETAIN,IPHIIN,ILYR,IETA,IPHI
C----------------------------------------------------------------------
   88 FORMAT(' MTC_GET_XYZ: error iein,ipin,il,ie,ip=',5I3)
  999 RETURN
      END
