      SUBROUTINE PF_PR_TRACKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action Routine to display information
C-                         about all tracks in an event.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  11-JUN-1991   Robert E. Avery
C-   Updated   9-SEP-1991   Robert E. Avery  Correction in use of 
C-                              status word of FDCT track (bit 0 = half). 
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to 
C-     26 (two errors and two spares).
C-   Updated   7-NOV-1991   Robert E. Avery  Compute correct CHINORM. 
C-   Updated  24-JAN-1992   Lupe Howell  Removed the machine block for external 
C-   Updated   2-MAR-1993   Robert E. Avery  Allow alternate information 
C-                              to be printed 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'

      INTEGER LFTRH,GZFTRH
      INTEGER NTRK,IER
      INTEGER ISTRQU
      INTEGER ITRK
      INTEGER LADDER(0:2)
      INTEGER NHITS,NFIT
      INTEGER IWORD
      INTEGER HALF
C
      REAL    CHINORM
      REAL    XPOS, YPOS 
      REAL    XFDC, YFDC, RFDC, PHI_FDC 
      REAL    XIMP, YIMP, ZIMP, RIMP 
C
      LOGICAL ALTERNATE_INFO 
      LOGICAL EZERROR
C
      CHARACTER*100 TEXT
      CHARACTER*4 TITCLR
      CHARACTER*1 CHALF(0:1)
C
      REAL    CSIZE
      PARAMETER( CSIZE = 1.5 )
C
      REAL QTRAK(26),QHTRK(3,34)        
      INTEGER IQTRAK(26)
      EQUIVALENCE (IQTRAK,QTRAK)
C  
      DATA ALTERNATE_INFO /.TRUE./
      DATA TITCLR/'    '/
      DATA CHALF/'N','S'/
C----------------------------------------------------------------------
      LFTRH=GZFTRH()
      IF(LFTRH.LE.0) THEN
        CALL INTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
C
      NTRK=IQ(LFTRH+2)
      IF (NTRK.LE.0) THEN
        CALL INTMSG(' No tracks found in FDC, try next event.')
        GO TO 999
      END IF
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF_PR_TRACKS','Cannot find PX_FDCDIS_RCP',
     &       'W')
        GOTO 999
      ENDIF
C
      CALL PUOPEN
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      CALL PXCOLR(TITCLR)
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
      CALL JJUST( 1, 2)
C
      IF ( ALTERNATE_INFO ) THEN
        TEXT = 
     &  ' Trk Half Nhit  R-FDC Phi-FDC R-imp Z-imp  Ladder '//
     &  '   Chsq   Trmean'
      ELSE
        TEXT = 
     &  ' Trk Half Nhit  X0    Y0     Phi   Theta   Ladder '//
     &  '   Chsq   Trmean'
      ENDIF
      YPOS = YWIND2*0.80
      XPOS = -XWIND2
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
      TEXT = 
     &  '                                           0  1  2'//
     &  '  (norm)  (MIP)'
      YPOS = YPOS - CSIZE
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
C
      DO ITRK=1,NTRK
        CALL GTFDCT(ITRK,QTRAK,QHTRK,LADDER)
        HALF = IAND(1,IQTRAK(1))
        NHITS = IQTRAK(2)
        NFIT = IQTRAK(25)
        IF ( (NFIT.NE.NHITS).AND.
     &       ( (NFIT.LT.4) .OR. (NFIT.GT.9) )  ) THEN
          NFIT = NHITS             ! In case of old version of FDCT 
        ENDIF
        IF ( NFIT.GT.4 ) THEN
          CHINORM=SQRT(2*QTRAK(19))-SQRT(2*(FLOAT(NFIT)-4.)-1.)
        ELSE
          CHINORM = 0.
        ENDIF
C
        IF ( ALTERNATE_INFO) THEN
          XFDC = QTRAK(4) 
          YFDC = QTRAK(5) 
          RFDC = SQRT(XFDC**2 + YFDC**2)
          PHI_FDC = ATAN2(YFDC,XFDC)
          IF (PHI_FDC .LT.0.)
     &          PHI_FDC = PHI_FDC + TWOPI
          CALL FGET_CLOSE(ITRK,XIMP,YIMP,ZIMP,RIMP)
C
          WRITE (TEXT,101) ITRK,CHALF(HALF),NHITS,
     &      RFDC, PHI_FDC, RIMP, ZIMP,
     &      LADDER,CHINORM,QTRAK(20)
  101     FORMAT(1X,I3,A3,I5,4F7.2,1X,3I3,2F7.2)
        ELSE
          WRITE (TEXT,102) ITRK,CHALF(HALF),NHITS,
     &      (QTRAK(IWORD),IWORD=4,6),QTRAK(22),
     &      LADDER,CHINORM,QTRAK(20)
  102     FORMAT(1X,I3,A3,I5,2F7.2,2F7.3,1X,3I3,2F7.2)
        ENDIF
        YPOS = YPOS - CSIZE
        CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
      END DO
      CALL JRCLOS
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
      CALL EZRSET
C
  999 RETURN
      END
