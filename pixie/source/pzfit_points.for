      SUBROUTINE PZFIT_POINTS(  ID_CDC,ID_FDC,
     &                          PHI,XC,YC,THETA,ZC,
     &                          XPOS1,YPOS1,ZPOS1,
     &                          XPOS2,YPOS2,ZPOS2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return first and last point of track to be
C-      displayed. 
C-
C-   Inputs  :  ID_CDC,ID_FDC        ID of CDC and FDC tracks (if they exist)
C-              PHI,XC,YC,THETA,Z0   Center of gravity track definition.
C-   Outputs :  XPOS1,YPOS1,ZPOS1    Begining point of track
C-              XPOS2,YPOS2,ZPOS2    Ending point of track
C-
C-   Created   8-AUG-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ID_FDC,ID_CDC
      REAL    PHI,XC,YC
      REAL    THETA,ZC
      REAL    XPOS1, YPOS1, ZPOS1
      REAL    XPOS2, YPOS2, ZPOS2
C
      INTEGER LFDCT,GZFDCT
      INTEGER IER 
      INTEGER FDC_SIDE   
      REAL    X0,Y0,DX,DY
      REAL    RAD_FDC,LENGTH_FDC
      REAL    RAD_CDC,LENGTH_CDC
      REAL    RAD_VTX,LENGTH_VTX
      REAL    X1,Y1,Z1,X2,Y2,Z2
      REAL    PHIC, PHI1, PHI2 
C
      LOGICAL COSMIC
C
      DATA    RAD_FDC, LENGTH_FDC /70.0,140.0/
      DATA    RAD_CDC, LENGTH_CDC /80.0,100.0/
      DATA    RAD_VTX, LENGTH_VTX /19.0,60.0/
C----------------------------------------------------------------------
      CALL EZPICK('ZTRAKS_RCP')
      CALL EZGET('COSMIC',COSMIC,IER)
      CALL EZRSET
C
      CALL PZTRK_CONV( PHI,XC,YC,
     &                 THETA,ZC,
     &                 X0,Y0,DX,DY)
      IF ( COSMIC .AND. ID_FDC .GT.0 ) THEN
C
C  Special for cosmics, draw FDC-CDC tracks all the way through both
C
        LFDCT=GZFDCT(ID_FDC)
        FDC_SIDE = (-1) ** ( IQ(LFDCT+1) + 1)
C
C  First point is at outside boundary of FDC:
C
        CALL PZCYL_INT(X0,Y0,DX,DY,RAD_FDC,LENGTH_FDC,
     &                    X1,Y1,Z1,X2,Y2,Z2)
        IF ( FDC_SIDE*Z1 .GT. FDC_SIDE*Z2 ) THEN
          XPOS1 = X1
          YPOS1 = Y1
          ZPOS1 = Z1
        ELSE
          XPOS1 = X2
          YPOS1 = Y2
          ZPOS1 = Z2
        ENDIF
C
C  Last point is at outside boundary of central detector
C
        IF ( ID_CDC .GT. 0 ) THEN
          CALL PZCYL_INT( X0,Y0,DX,DY,
     &                    RAD_CDC,LENGTH_CDC,
     &                    X1,Y1,Z1,X2,Y2,Z2)
        ELSE
          CALL PZCYL_INT( X0,Y0,DX,DY,
     &                    RAD_VTX,LENGTH_VTX,
     &                    X1,Y1,Z1,X2,Y2,Z2)
        ENDIF
        IF ( FDC_SIDE*Z1 .LT. FDC_SIDE*Z2 ) THEN
          XPOS2 = X1
          YPOS2 = Y1
          ZPOS2 = Z1
        ELSE
          XPOS2 = X2
          YPOS2 = Y2
          ZPOS2 = Z2
        ENDIF
      ELSE
C
C  Standard for Collider data (and cosmic CDC-VTX tracks)
C
C  First point is at point of closest approach:
C
        IF ( ABS(DX*DY).GT.0 )  THEN
          ZPOS1 = -(DX*X0 +DY*Y0) / (DX**2. + DY**2.)
        ELSE
          ZPOS1 = 0.
        ENDIF
        XPOS1= X0 + ZPOS1*DX
        YPOS1= Y0 + ZPOS1*DY
C
C  Last point is at outside boundary of FDC or CDC
C
        IF ( ID_FDC .GT. 0 ) THEN
          CALL PZCYL_INT( X0,Y0,DX,DY,
     &                    RAD_FDC,LENGTH_FDC,
     &                    X1,Y1,Z1,X2,Y2,Z2)
        ELSE
          CALL PZCYL_INT( X0,Y0,DX,DY,
     &                    RAD_CDC,LENGTH_CDC,
     &                    X1,Y1,Z1,X2,Y2,Z2)
        ENDIF
C
C   Pick right direction (based on on track center of gravity)
C
        PHIC = ATAN2(XC,YC)
        PHI1 = ATAN2(X1,Y1)
        PHI2 = ATAN2(X2,Y2)
        IF (  ABS(SIN(2*(PHI1-PHIC))) .LT. 
     &        ABS(SIN(2*(PHI2-PHIC))) ) THEN
          XPOS2 = X1
          YPOS2 = Y1
          ZPOS2 = Z1
        ELSE
          XPOS2 = X2
          YPOS2 = Y2
          ZPOS2 = Z2
        ENDIF
      ENDIF
  999 CONTINUE
      RETURN
      END
