       SUBROUTINE PFHLF4( HALF )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display one half of the Forward Drift Chambers
C-                         with hits and tracks, if requested
C-
C-   Inputs  : HALF - FDC Half
C-   Outputs : draws display
C-   Controls: 
C-
C-   Created  21-OCT-1988   Jeffrey Bantly
C-   Updated   7-FEB-1990   Jeffrey Bantly  general cleanup 
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated  14-MAY-1991   Susan K. Blessing  Make the user use N or S 
C-                                             for half value
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER HALF, QUARTR
      INTEGER LEN, GZFGEH, IERR, IER, II, JJ
C
      REAL    XMIN, XMAX, YMIN, YMAX, XMID, YMID, XQUAR
      REAL    XMINW, XMAXW, YMINW, YMAXW, XADJ
C
      LOGICAL EZERROR
C
      CHARACTER*3 TMPCOL
      CHARACTER*4 TITCLR
      CHARACTER*13 QUTITL(0:3)
      CHARACTER*15 TEXT
      CHARACTER*60 PROM, ANS
C
      DATA PROM /' Enter HALF - N(orth) or S(outh)>'/
      DATA TMPCOL /'FOR'/
      DATA QUTITL /' Qu0/Phi\Qu4 ',' Qu1/Phi\Qu5 ',
     &             ' Qu2/Phi\Qu6 ',' Qu3/Phi\Qu7 '/
C----------------------------------------------------------------------
C
C  Check for presence of STP banks
C
      IF( LSTPH .LE. 0 ) CALL INZSTP
      IF( LSFDC .LE. 0 ) CALL FDISTP('FDC_STPFILE', IERR)
      IF( LFGEH .LE. 0 ) LFGEH = GZFGEH()
      IF( LFGEH .LE. 0 ) GOTO 999
C
C  Get window parameters for display
C
      XMINW = -50.
      XMAXW = +50.
      YMINW = -50.
      YMAXW = +50.
      CALL OUTMSG('1')
      CALL GETPAR(1,PROM,'U',ANS)
      CALL SWORDS(ANS,II,JJ,LEN)
      IF ( LEN .NE. 0 ) THEN
        IF (ANS(1:1).EQ.'N'.OR.ANS(1:1).EQ.'n') THEN
          HALF = 0
        ELSE IF (ANS(1:1).EQ.'S'.OR.ANS(1:1).EQ.'s') THEN
          HALF = 1
        ELSE
          READ ( ANS(1:LEN),*,ERR=700) HALF 
        END IF
      END IF
      IF(HALF.NE.0 .AND. HALF.NE.1) THEN
        HALF=0
        IF(LEN.NE.0) WRITE(*,*) ' PFHLF4 - Half value bad '
      ENDIF
C
      CALL PUOPEN
      CALL PFUMES(' ')
      CALL PFUMES('FDC HITS ')
      IF (HALF.EQ.0) THEN
        WRITE(TEXT,100)
  100   FORMAT(' North FDC ')
      ELSE
        WRITE(TEXT,101)
  101   FORMAT(' South FDC ')
      ENDIF
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFHLF4','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETA('FDC COLR LABELS',TITCLR)
      CALL EZRSET
      CALL PXCOLR( TITCLR )
      CALL JJUST(2,2)
      XMID = 0.
      YMID = 48.
      CALL PUVSTR( XMID, YMID, 3., 1.5, TEXT )
C
C  Generate the display for one quarter of the Half at a time
C
      XQUAR = 25.
      DO 10 QUARTR = 0, 3
        XMIN = XQUAR*0.05 + QUARTR * XQUAR + XMINW
        XMAX = XMIN + XQUAR*0.90
        YMIN = YMINW
        YMAX = (YMAXW-YMINW)*0.90 + YMINW
        XMID = (XMIN+XMAX)/2.
        YMID = YMAX*1.065
        XADJ = (XMAX-XMIN)*.375
        CALL JJUST(2,2)
        CALL PXRECT(TMPCOL,XMID,YMID,0.,((XMAX-XMIN)/2.),YMAX*0.04)
        CALL PXCOLR( TITCLR )
        CALL PUVSTR( XMID, YMID, 1., 2., QUTITL(QUARTR))
        CALL PUVSTR( XMID-XADJ, YMID, .75, 1.5, 'INR')
        CALL PUVSTR( XMID+XADJ, YMID, .75, 1.5, 'OUTR')
        CALL PFQUAR ( HALF, QUARTR, XMIN, XMAX, YMIN, YMAX )
   10 CONTINUE
      CALL JRCLOS
      GO TO 999
  700 PRINT *,' Error in HALF value '
C----------------------------------------------------------------------
  999 RETURN
      END
