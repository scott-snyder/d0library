      SUBROUTINE PF_ISA_SECTORS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action Routine to draw a multiple window 
C-                         display of an FDC R-Z view and a single 
C-                         isajet track's sector plots.
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-FEB-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LISAE,GZISAE
      INTEGER LFTRH,GZFTRH
      INTEGER LFITR,GZFITR
      INTEGER JCALL
      INTEGER TRKNUM
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER LAYER, QD
C
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    X_TRK,Y_TRK,DX_TRK,DY_TRK
      REAL    X_POS,Y_POS 
      REAL    Z0(0:1)
      REAL    TAN_THETA, Z_VTX 
      REAL    TRKDAT(9)
C
      SAVE JCALL
      DATA JCALL/3/
C----------------------------------------------------------------------
C
C
      IF(JCALL.EQ.3) THEN
        JCALL=1
      ELSEIF(JCALL.EQ.1) THEN
        JCALL=2
      ELSEIF(JCALL.EQ.2) THEN
        JCALL=3
      ENDIF
C
      LISAE = GZISAE()
      IF ( LISAE .LE. 0 ) THEN
        IF(JCALL.EQ.1) CALL INTMSG(' No ISAJET bank present.')
        GOTO 999
      ENDIF
C
      LFTRH=GZFTRH()
      IF(LFTRH.GT.0) THEN
        Z0(0)=Q(LFTRH+3)
        Z0(1)=Q(LFTRH+4)
      ELSE
        IF(JCALL.EQ.1) CALL INTMSG(' No FDC track bank present.')
        GOTO 999
      ENDIF
C
C  List all of the FDC tracks shown and choose track desired.
C
      IF (JCALL.EQ.1) THEN
        TRKNUM = 0
C
        CALL FDCISA
        LFITR = GZFITR()
        IF ( LFITR .LE. 0 ) GOTO 999
C
        CALL PFPICK_ISAJET(TRKNUM)
        IF(TRKNUM.LE.0) GOTO 999
C
C  Get isajet track parameters.
C
        CALL GTFITR(TRKNUM, TRKDAT)
        TAN_THETA = TAN(TRKDAT(5)) 
        IF ( TAN_THETA .GT. 0 ) THEN
          HALF = 1
        ELSE
          HALF = 0
        ENDIF
        DX_TRK = TAN_THETA * COS(TRKDAT(4))
        DY_TRK = TAN_THETA * SIN(TRKDAT(4))
        Z_VTX = TRKDAT(3) 
        X_TRK = TRKDAT(1) - (Z_VTX - Z0(HALF) ) * DX_TRK 
        Y_TRK = TRKDAT(2) - (Z_VTX - Z0(HALF) ) * DY_TRK 
      ENDIF
      IF(TRKNUM.LE.0) GOTO 999
C        
      LAYER = JCALL-1
      UNIT = 0
      QD = 0
      IF ( LAYER .EQ. 2 ) UNIT=1
      IF ( LAYER .EQ. 1 ) QD = 4
      CALL GTFALH( HALF,UNIT,QD,0,0,
     &               XWIRE,YWIRE,ZWIRE)
      X_POS = X_TRK + DX_TRK * (ZWIRE - Z0(HALF) ) 
      Y_POS = Y_TRK + DY_TRK * (ZWIRE - Z0(HALF) )
C
      CALL FGET_SECTOR(X_POS,Y_POS,HALF,LAYER,QUAD,SECTOR)
      IF ( SECTOR .GT. -1 ) THEN
        IF(UNIT.EQ.0) THEN
          CALL PF_THETA_SECT_VIEW(HALF,QUAD,SECTOR)
        ELSEIF(UNIT.EQ.1) THEN
          CALL PF_PHI_SECT_VIEW(HALF,SECTOR)
        ENDIF
      ELSEIF(UNIT.EQ.1) THEN
        CALL GTFALH( HALF,UNIT,0,0,15,
     &               XWIRE,YWIRE,ZWIRE)
        X_POS = X_TRK + DX_TRK * (ZWIRE - Z0(HALF) ) 
        Y_POS = Y_TRK + DY_TRK * (ZWIRE - Z0(HALF) )
        CALL FGET_SECTOR(X_POS,Y_POS,HALF,LAYER,QUAD,SECTOR)
        IF ( SECTOR .GT. -1 ) THEN
          CALL PF_PHI_SECT_VIEW(HALF,SECTOR)
        ELSE
          CALL PF_PR_MISS(LAYER,X_POS,Y_POS)
        ENDIF
      ELSE
        CALL GTFALH( HALF,UNIT,QD,0,7,
     &               XWIRE,YWIRE,ZWIRE)
        X_POS = X_TRK + DX_TRK * (ZWIRE - Z0(HALF) ) 
        Y_POS = Y_TRK + DY_TRK * (ZWIRE - Z0(HALF) )
        CALL FGET_SECTOR(X_POS,Y_POS,HALF,LAYER,QUAD,SECTOR)
        IF ( SECTOR .GT. -1 ) THEN
          CALL PF_THETA_SECT_VIEW(HALF,QUAD,SECTOR)
        ELSE
          CALL PF_PR_MISS(LAYER,X_POS,Y_POS)
        ENDIF
      ENDIF
C
C  Done.
C
C----------------------------------------------------------------------
  999 RETURN
      END
