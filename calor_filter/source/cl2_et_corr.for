      FUNCTION CL2_ET_CORR(IETA_IN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : correction factor from
C-    L2 CAEP (nominal ET) to true ET, based on level 2 vertex
C-      a value of 0.0 is returned for the correction if for an illegal cell
C-
C-   Inputs  : IETA_IN  Calorimeter readout tower number
C-   Outputs : correction factor so ETtrue = ETnominal*CL2_ET_CORR(IETA_IN)
C-   Controls: none
C-
C         ENTRY CL2_ET_CORR_FINE(IETA_IN,IPHI_IN,LYR_IN,XYZ_IN)
C
C       the same, except that the cell and position of the cluster is specified
C               
C----------------------------------------------------------------------
C-   Created  25-JUL-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA_IN,IPHI,LYR,IER,IPHI_IN,LYR_IN
      REAL    CL2_ET_CORR,ZVTX,L2_VERT,CL2_SNTH,XCELL,YCELL,ZCELL
      REAL    CL2_ET_CORR_FINE,XCLUS,YCLUS,ZCLUS,XYZ_IN(3)
      LOGICAL CEXIST
      INTEGER CURRENT_EVENT             ! the event for which CAEP is valid
      SAVE CURRENT_EVENT
      DATA CURRENT_EVENT/-987654/
C----------------------------------------------------------------------
      IPHI = 1  !always exists
      LYR = 11  !almost always exixts
      CL2_ET_CORR = 0.0
      IF (IETA_IN.EQ.0) RETURN
      IF (ABS(IETA_IN).GE.NETAL) THEN
        IF (ABS(IETA_IN).EQ.NETAL) THEN
          LYR = 13  ! last cell has no layer 11
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL CELXYZ(IETA_IN,IPHI,LYR,XCELL,YCELL,ZCELL,IER)
C
C...check if information is valid for current event
      IF (CURRENT_EVENT.NE.IQ(LHEAD+7)) THEN
        ZVTX = L2_VERT()                  !this might be slow
        CURRENT_EVENT = IQ(LHEAD+7)
      ENDIF
      CL2_ET_CORR= SQRT( (1.0+     (ZCELL)**2/(XCELL**2+YCELL**2)) /
     &                   (1.0+(ZVTX-ZCELL)**2/(XCELL**2+YCELL**2)) )
      RETURN
C----------------------------------------------------------------------
      ENTRY CL2_ET_CORR_FINE(IETA_IN,IPHI_IN,LYR_IN,XYZ_IN)
      CL2_ET_CORR_FINE = 0.0
      IF (.NOT.CEXIST(IETA_IN,IPHI_IN,LYR_IN)) RETURN
      CALL CELXYZ(IETA_IN,IPHI_IN,LYR_IN,XCELL,YCELL,ZCELL,IER)
      XCLUS = XYZ_IN(1)
      YCLUS = XYZ_IN(2)
      ZCLUS = XYZ_IN(3)
C
C...check if information is valid for current event
      IF (CURRENT_EVENT.NE.IQ(LHEAD+7)) THEN
        ZVTX = L2_VERT()                  !this might be slow
        CURRENT_EVENT = IQ(LHEAD+7)
      ENDIF
      CL2_ET_CORR_FINE= SQRT( (1.0+ (ZCELL)**2/(XCELL**2+YCELL**2)) /
     &                    (1.0+(ZVTX-ZCLUS)**2/(XCLUS**2+YCLUS**2)) )
C----------------------------------------------------------------------
  999 RETURN
      END
