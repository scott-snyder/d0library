      SUBROUTINE MU_TRIG_REGION(THETA,PHI,IOCT,TRREG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Evaluates which trigger region a muon should be
C-                         found at.
C-
C-   Inputs  : THETA, PHI - Muon angles
C-
C-   Outputs : IOCT - Trigger Region, and  TRREG - CCT Trigger region
C-             
C-            -999     Error                -999
C-              -1     Beam hole              -1     Beam hole
C-
C-               0     CF Oct.  0              1     Central
C-               1              1
C-              ...            ...
C-               7              7
C-
C-              10     WN Quad. 1              2     WAMUS North
C-              11     WS       1              3     WAMUS South
C-              12     WN       2
C-             ....            ...
C-              17     WS       4
C-
C-              20     ON Quad. 1              4     Overlap North
C-              21     OS       1              5     Overlap South
C-              22     ON       2
C-             ....            ...
C-              27     OS       4
C-
C-              30     SN Quad. 1              6     SAMUS North
C-              31     SS       1              7     SAMUS South
C-              32     SN       2
C-             ....            ...
C-              37     SS       4
C-
C-
C-   Controls: 
C-
C-   Created   9-DEC-1992   Guilherme Lima, Kamel A. Bazizi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL    THETA,PHI,THD,PHID
      INTEGER IOCT,TRREG
      LOGICAL CF        
      REAL    RHO,X,Y

C.. Geometry constants
      REAL    ZREF,DL1,DL2,DL3,DL4
      DATA ZREF / 430.   /
      DATA DL1  /  25.50 /  ! Beam hole
      DATA DL2  /  76.   /  ! Pure SAMUS
      DATA DL3  / 168.   /  ! SAMUS-WAMUS overlap
      DATA DL4  / 315.   /  ! Pure WAMUS
C----------------------------------------------------------------------

      IOCT=-999
      TRREG=-999
      THD=THETA/RADIAN
      PHID=PHI/RADIAN
      CF=.FALSE.

C.. Rough check in Theta, to avoid overflows
      IF(ABS(THD-90.).LE.40.) THEN
        CF=.TRUE.
        GOTO 500
      ENDIF

C.. Get x,y for a given Z=Zref (EFA-layer position)
      RHO=ZREF*ABS(TAN(THETA))
      X=RHO*COS(PHI)
      Y=RHO*SIN(PHI)

      IF (ABS(X).LE.DL1 .AND. ABS(Y).LE.DL1) THEN       ! Beam hole
        IOCT=-1
        TRREG=-1

      ELSE IF (ABS(X).LE.DL2 .AND. ABS(Y).LE.DL2) THEN  ! Pure SAMUS
        IF( THD .GT. 90.) THEN   ! North
          TRREG=6
          IF( X .GT.  DL1 .AND. Y .GT. -DL1 ) IOCT = 30
          IF( X .LT.  DL1 .AND. Y .GT.  DL1 ) IOCT = 32
          IF( X .LT. -DL1 .AND. Y .LT.  DL1 ) IOCT = 34
          IF( X .GT. -DL1 .AND. Y .LT. -DL1 ) IOCT = 36
        ENDIF
        IF( THD .LT. 90.) THEN   ! South
          TRREG=7
          IF( X .LT. -DL1 .AND. Y .GT. -DL1 ) IOCT = 31
          IF( X .GT. -DL1 .AND. Y .GT.  DL1 ) IOCT = 33
          IF( X .GT.  DL1 .AND. Y .LT.  DL1 ) IOCT = 35
          IF( X .LT.  DL1 .AND. Y .LT. -DL1 ) IOCT = 37
        ENDIF

      ELSE IF (ABS(X).LE.DL3 .AND. ABS(Y).LE.DL3) THEN  ! SAMUS-WAMUS Overlap
        IF( THD .GT. 90.) THEN   ! North
          TRREG=4
          IF( PHID .GE.   0. .AND. PHID .LE.  90.) IOCT = 20
          IF( PHID .GT.  90. .AND. PHID .LE. 180.) IOCT = 22
          IF( PHID .GT. 180. .AND. PHID .LE. 270.) IOCT = 24
          IF( PHID .GT. 270. .AND. PHID .LE. 360.) IOCT = 26
        ENDIF
        IF( THD .LT. 90.) THEN   ! South
          TRREG=5
          IF( PHID .GE.   0. .AND. PHID .LE.  90.) IOCT = 21
          IF( PHID .GT.  90. .AND. PHID .LE. 180.) IOCT = 23
          IF( PHID .GT. 180. .AND. PHID .LE. 270.) IOCT = 25
          IF( PHID .GT. 270. .AND. PHID .LE. 360.) IOCT = 27
        ENDIF

      ELSE IF (ABS(X).LE.DL4 .AND. ABS(Y).LE.DL4) THEN  ! Pure WAMUS
        IF( THD .GT. 90.) THEN   ! North
          TRREG=2
          IF( PHID .GE.   0. .AND. PHID .LE.  90.) IOCT = 10
          IF( PHID .GT.  90. .AND. PHID .LE. 180.) IOCT = 12
          IF( PHID .GT. 180. .AND. PHID .LE. 270.) IOCT = 14
          IF( PHID .GT. 270. .AND. PHID .LE. 360.) IOCT = 16
        ENDIF
        IF( THD .LT. 90.) THEN   ! South
          TRREG=3
          IF( PHID .GE.   0. .AND. PHID .LE.  90.) IOCT = 11
          IF( PHID .GT.  90. .AND. PHID .LE. 180.) IOCT = 13
          IF( PHID .GT. 180. .AND. PHID .LE. 270.) IOCT = 15
          IF( PHID .GT. 270. .AND. PHID .LE. 360.) IOCT = 17
        ENDIF

      ELSE
        CF=.TRUE.
      ENDIF

  500 IF ( CF ) THEN
        TRREG=1
        IF( PHID .GE.   0. .AND. PHID .LE.  45.) IOCT =  0
        IF( PHID .GT.  45. .AND. PHID .LE.  90.) IOCT =  1
        IF( PHID .GT.  90. .AND. PHID .LE. 135.) IOCT =  2
        IF( PHID .GT. 135. .AND. PHID .LE. 180.) IOCT =  3
        IF( PHID .GT. 180. .AND. PHID .LE. 225.) IOCT =  4
        IF( PHID .GT. 225. .AND. PHID .LE. 270.) IOCT =  5
        IF( PHID .GT. 270. .AND. PHID .LE. 315.) IOCT =  6
        IF( PHID .GT. 315. .AND. PHID .LE. 360.) IOCT =  7
      ENDIF                                        

      IF(IOCT.EQ.-999) THEN
        PRINT *,' *** Bad inputs for MU_TRIG_REGION!'
        PRINT *,'       TH,PHI = ',THETA,PHI
      ENDIF
        
  999 RETURN
      END
