      SUBROUTINE PF_PR_SEGMENTS(HALF,LADDER,QTRAK,IQTRAK,CHINORM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print information about chosen segments to 
C-   screen.
C-
C-   Inputs  : HALF,LADDER(0:2),CHINORM
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-MAY-1991   Robert E. Avery
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated   9-SEP-1991   Robert E. Avery  Correction in use of 
C-                              status word of FDCT track (bit 0 = half). 
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to 
C-     26 (two errors and two spares).
C-   Updated   7-NOV-1991   Robert E. Avery  Compute correct CHINORM. 
C-   Updated  18-NOV-1991   Robert E. Avery  Change Segment information
C-                                              to print out. 
C-   Updated  24-JAN-1992   Lupe Howell  Remove the use of EXTERNAL in machine
C-                                       block 
C-   Updated   2-MAR-1993   Robert E. Avery  Allow alternate information 
C-                              to be printed 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:PXPARA.INC'
C  Input:
      INTEGER HALF,LADDER(0:2)
      REAL QTRAK(26)
      INTEGER IQTRAK(26)
      REAL    CHINORM
C  Local:
      INTEGER UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER IER,LRCP
      INTEGER MODULE,LAYER 
      INTEGER ISTRQU
      INTEGER IWORD
C
      REAL    XPOS ,YPOS 
      REAL    CONT(62),PHI,THETA,RESID
      REAL    DR1,Z1
      REAL    RADIUS,FSTAGR,SLOPE,INTERCEPT,CHISQ,AVEION
      REAL    XC,YC,ZC
      REAL    BEAM_POS(3),Z0(0:1)
      REAL    XFDC, YFDC, RFDC, PHI_FDC 
      REAL    DX, DY 
      REAL    XIMP, YIMP, ZIMP, RIMP 
C
      REAL    CSIZE
      PARAMETER( CSIZE = 1.5 )
C
      CHARACTER*100 TEXT
      CHARACTER*100 TEXT1
      CHARACTER*100 TEXT2
      CHARACTER*4 TITCLR
      CHARACTER*20 LAYER_LABEL(0:2)
      CHARACTER*1 CHALF(0:1)
      CHARACTER*5 CUNIT(0:1)
C
      LOGICAL EZERROR
      LOGICAL FIRST
      LOGICAL ALTERNATE_INFO 
C
      REAL    FIADD,FNHITS
      INTEGER IADD,NHITS
      EQUIVALENCE(IADD,FIADD)
      EQUIVALENCE(NHITS,FNHITS)
C
      DATA FIRST/.TRUE./
      DATA TITCLR/'    '/
      DATA LAYER_LABEL /' Inner Theta:',' Outer Theta:',' Phi:'/
      DATA CHALF/'N','S'/
      DATA CUNIT/'THETA',' PHI '/
      DATA ALTERNATE_INFO /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZLOC('FTRAKS_RCP',LRCP)
        IF(LRCP.GT.0) THEN                                   
          CALL EZPICK('FTRAKS_RCP')
          CALL EZGET('Z0',Z0,IER)
          CALL EZRSET
        ENDIF
        CALL EZLOC('VERTEX_RCP',LRCP)                        
        IF(LRCP.GT.0) THEN                                   
          CALL EZPICK('VERTEX_RCP')
          CALL EZGET('BEAM_POS',BEAM_POS,IER)
          CALL EZRSET
        ENDIF
        FIRST = .FALSE.
      END IF
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF_PR_SEGMENTS',
     &      'Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUOPEN
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      CALL PXCOLR(TITCLR)
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
      CALL JJUST( 1, 2)
C
C First print out info on fitted track:
C
      HALF = IAND(1,IQTRAK(1))
      NHITS = IQTRAK(2)
      TEXT = ' Parameters of Fitted Track:'
      IF ( ALTERNATE_INFO ) THEN
        TEXT1 = ' Half Nhit R-FDC  Phi-FDC Theta  R-imp Z-imp'//
     &                  '  Chi (norm) TrMean'
        XFDC = QTRAK(4) 
        YFDC = QTRAK(5) 
        RFDC = SQRT(XFDC**2 + YFDC**2)
        PHI_FDC = ATAN2(YFDC,XFDC)
        IF (PHI_FDC .LT.0.)
     &          PHI_FDC = PHI_FDC + TWOPI
        THETA = QTRAK(22)  
C
        DX = QTRAK(7)
        DY = QTRAK(8)
        IF ( ABS(DX*DY).GT.0 )  THEN
          XFDC = XFDC - DX*Z0(HALF) - BEAM_POS(2) 
          YFDC = YFDC - DY*Z0(HALF) - BEAM_POS(1)
          ZIMP = -(DX*XFDC +DY*YFDC) / (DX**2. + DY**2.)
          XIMP = XFDC + ZIMP * DX
          YIMP = YFDC + ZIMP * DY
          RIMP = SQRT( XIMP**2. + YIMP**2. )
        ELSE
          ZIMP = 0.0
          RIMP = 999.0
        ENDIF
C
        WRITE (TEXT2,121) CHALF(HALF),NHITS,
     &      RFDC, PHI_FDC, THETA, RIMP, ZIMP,
     &      CHINORM,QTRAK(20)
      ELSE
        TEXT1 = '  Half Nhit  X0    Y0     Phi   Theta'//
     &                  ' Chi (norm) TrMean'
        WRITE (TEXT2,121) CHALF(HALF),NHITS,
     &      (QTRAK(IWORD),IWORD=4,6),QTRAK(22),
     &      CHINORM,QTRAK(20)
      ENDIF
  121 FORMAT(1X,A3,I5,5F7.2,2F8.2)
  122 FORMAT(1X,A3,I5,4F7.2,2F8.2)
C
      CALL INTMSG(' ')
      CALL INTMSG(TEXT)
      CALL INTMSG(TEXT1)
      CALL INTMSG(TEXT2)
C
      YPOS = YWIND2*0.90
      XPOS = -XWIND2
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT)
      YPOS = YPOS - 2*CSIZE
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT1)
      YPOS = YPOS - CSIZE
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT2)
C
C Then print out info on segments:
C
      DO LAYER =  0, 2
        YPOS = YPOS - 2*CSIZE
        CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, LAYER_LABEL(LAYER))
C
        IF ( LADDER(LAYER).NE.0 ) THEN
          IF(LAYER.LE.1) THEN
            TEXT = '  Seg  Hlf Quad  Sct Nhit   Phi  Theta  Slope'//
     &       ' XDrift  Chisq AveIon'
          ELSE
            TEXT = '  Seg  Hlf  Sct Nhit   Phi  Slope'//
     &       ' XDrift  Chisq AveIon'
          ENDIF
          YPOS = YPOS - CSIZE
          CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
C
          MODULE=HALF*3+LAYER
          CALL GTFSEG(MODULE,LADDER(LAYER),CONT)
          FIADD=CONT(2)
          FNHITS=CONT(3)
          CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
          CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
          IF(UNIT.EQ.0) THEN
            PHI=CONT(20)
            THETA=CONT(21)
            SLOPE = CONT(30)
            RADIUS = ((XC)**2. + (YC)**2.)**.5
     &               - FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE) 
            INTERCEPT = CONT(31) + ZC * SLOPE- RADIUS 
            CHISQ = MIN( 999.0, CONT(32) )
            AVEION = CONT(33)
            WRITE( TEXT,101) LADDER(LAYER),CHALF(HALF),QUAD,SECTOR,
     &          NHITS,PHI,THETA,SLOPE,INTERCEPT,CHISQ,AVEION
          ELSE
            PHI=CONT(36)
            SLOPE = CONT(55)
            INTERCEPT = CONT(56) + ZC * SLOPE
            CHISQ = MIN( 999.0, CONT(57) )
            AVEION = CONT(58)
            WRITE( TEXT,111) LADDER(LAYER),CHALF(HALF),SECTOR,
     &          NHITS,PHI,SLOPE,INTERCEPT,CHISQ,AVEION
          ENDIF
          YPOS = YPOS - CSIZE
          CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )

        ELSE
          TEXT = ' No Segment chosen in this layer'
          YPOS = YPOS - CSIZE
          CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
        ENDIF
      ENDDO
      CALL JRCLOS
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
      CALL EZRSET
C
  101 FORMAT(X,I4,A4,3I5,6F7.2)
  111 FORMAT(X,I4,A4,2I5,5F7.2)
C----------------------------------------------------------------------
  999 RETURN
      END
