      SUBROUTINE PFPICK_TRACK(TRKNUM,HALF,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Requests which track the user wants 
C-                         FDC track display
C-
C-   Inputs  : none
C-   Outputs : TRKNUM = Track number
C-             HALF   = FDC Half of track
C-   Controls : IFL   = 1 if want track and view, 2 if track only
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated   7-AUG-1990   Jeffrey Bantly  put in 3 views
C-   Updated   5-APR-1991   Jeffrey Bantly  add IFL control 
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup using new Compack 
C-   Updated  14-MAY-1991   Susan K. Blessing  Fix EZRSET bug 
C-   Updated  12-JUN-1991   Robert E. Avery  Change format of track list. 
C-   Updated   9-SEP-1991   Robert E. Avery  Correction in use of 
C-                              status word of FDCT track (bit 0 = half). 
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to 
C-     26 (two errors and two spares).
C-   Updated   7-NOV-1991   Robert E. Avery  Compute correct CHINORM. 
C-   Updated  25-JAN-1992   Robert E. Avery  Only display segment information 
C-                              the first time called in event.
C-   Updated  17-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  17-FEB-1992   Robert E. Avery  If hardcopy, don't ask questions.
C-   Updated  18-AUG-1992   Robert E. Avery  Add track quality to printout. 
C-   Updated   2-APR-1993   Robert E. Avery  Don't assume tracks are
C-                              consequtively numbered.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$LINKS:IZFDCT.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER TRKNUM,HALF,ICONT(10),IVIEW
      INTEGER LEN,ITRK,NTRK,IWORD
      INTEGER IQTRAK(26),IER,LADDER(0:2)
      INTEGER TYP,IVAL,II,JJ,IFL
      INTEGER LFTRH,GZFTRH
      INTEGER LFDCT,GZFDCT
      INTEGER NHITS,NFIT
      INTEGER RUNSAV, IDSAV, RUN, ID
      INTEGER LZLAST
C
      REAL    XMIN, XMAX, YMIN, YMAX, X02, Y02, Z0(2)
      REAL    CHINORM
      REAL QTRAK(26),QHTRK(3,34)
      EQUIVALENCE(QTRAK,IQTRAK)
C
      CHARACTER*60 PROM,PROM1
      CHARACTER*80 STRING
      CHARACTER*112 FTEXT
      CHARACTER*1 CHALF(0:1)
C
      LOGICAL FIRST
      LOGICAL EZERROR
      LOGICAL PRINT_LIST, PRINT_LIST_RCP
      LOGICAL FLGVAL,HARDCOPY 
C
      DATA FIRST/.TRUE./
      DATA PROM1/' Enter view type (default Prev or 1)>'/
      DATA PROM/' Enter Track Number (default prev or 1 )>'/
      DATA IVIEW/1/
      DATA CHALF/'N','S'/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        TRKNUM = 1
        FIRST = .FALSE.
      END IF
C
      LFTRH=GZFTRH()
      IF(LFTRH.GT.0) THEN
        Z0(1)=Q(LFTRH+3)
        Z0(2)=Q(LFTRH+4)
      ELSE
        TRKNUM = 0
        CALL INTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
C
      CALL GTFTRH(ICONT)
      NTRK=ICONT(2)
      IF (NTRK.LE.0) THEN
        TRKNUM = 0
        CALL INTMSG(' No tracks found in FDC, try next event.')
        GOTO 999
      END IF
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFPICK_TRACK','Cannot find PX_FDCDIS_RCP',
     &       'W')
        GOTO 999
      ENDIF
C
C  Make choice of how to view display
C
      HARDCOPY = FLGVAL('HARDCOPY')
      IF (IFL.EQ.1) THEN
C
        IF ( .NOT.HARDCOPY ) THEN
          CALL OUTMSG('1')
          CALL OUTMSG('    Choose a viewpoint to see FDC Half in 3-D')
          CALL OUTMSG(
     &      ' ( 1=above center, 2=along track, 3=along beam )')
          STRING=' '
          LEN=0
          CALL GETPAR(1,PROM1,'U',STRING)
          CALL SWORDS(STRING,II,JJ,LEN)
          IF (LEN.NE.0) READ(STRING(1:LEN),*,ERR=980) IVIEW
        ENDIF
C
        IF(IVIEW.LT.1 .OR. IVIEW.GT.3) THEN
C  Bad choice of view
          IVIEW=1
          CALL OUTMSG(' Bad choice of view, set to default value of 1.')
        ENDIF
        CALL EZ_SET_ARRAY('PXPARAMS','FDC 3D VIEW',IVIEW,IER)
C
      ENDIF
C
C  Decide whether or not to print full list:
C
      CALL PUGETV('FDC PRINT LIST',PRINT_LIST_RCP)
      IF ( HARDCOPY ) THEN
        PRINT_LIST = .FALSE.
      ELSEIF ( PRINT_LIST_RCP ) THEN
        PRINT_LIST = .TRUE.
      ELSE
        CALL EVNTID(RUN,ID)
        IF (RUN.NE.RUNSAV .OR. ID.NE.IDSAV) THEN
          RUNSAV=RUN
          IDSAV=ID
          PRINT_LIST = .TRUE.
        ELSE
          PRINT_LIST = .FALSE.
        ENDIF
      ENDIF
C
C  Display listing of tracks.
C
      IF ( PRINT_LIST ) THEN
        CALL INTMSG(' ')
        FTEXT = 
     &  ' Trk Half Nhit  X0    Y0     Phi   Theta   Ladder '//
     &  '   Chsq   Trmean Quality'
        CALL INTMSG(FTEXT)
C
C Loop backwards:
C
        LFDCT = LZLAST(IXCOM,LFTRH-IZFDCT)
        DO WHILE ( LFDCT.NE.LFTRH-IZFDCT ) 
          ITRK = IQ(LFDCT-5)
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
          WRITE (FTEXT,102) ITRK,CHALF(HALF),NHITS,
     &      (QTRAK(IWORD),IWORD=4,6),QTRAK(22),
     &      LADDER,CHINORM,QTRAK(20),IQTRAK(1)
          CALL INTMSG(FTEXT)
C
          LFDCT = LQ(LFDCT+2)
        ENDDO
      ENDIF
C
C  Make choice of track to display
C
      LFDCT = LZLAST(IXCOM,LFTRH-IZFDCT)
      ITRK = IQ(LFDCT-5)
C
      IF ( NTRK.EQ.1) THEN
        TRKNUM = ITRK
      ELSE
        IF ( .NOT.HARDCOPY ) THEN
          CALL OUTMSG('1')
          CALL OUTMSG(' Choose an FDC Track to Display ')
          STRING=' '
          LEN=0
          CALL GETPAR(1,PROM,'U',STRING)
          CALL SWORDS(STRING,II,JJ,LEN)
          IF(LEN.NE.0) READ(STRING(1:LEN),*,ERR=980) TRKNUM
        ENDIF
        LFDCT = GZFDCT(TRKNUM)
        IF (LFDCT.LE.0) THEN
          TRKNUM = ITRK
          CALL OUTMSG(' Choice outside limits, use first track.')
        END IF
      ENDIF
      WRITE(FTEXT,100) TRKNUM
      CALL OUTMSG(FTEXT)
C
      HALF = IAND(1,IQ(LFDCT+1))
      GOTO 990
C
  100 FORMAT('  Track number chosen is ',I4,'  ')
  102 FORMAT(1X,I3,A3,I5,2F7.2,2F7.3,1X,3I3,2F7.2,Z7.5)
C----------------------------------------------------------------------
C
  980 CONTINUE
      CALL OUTMSG(' Error reading input.')
C
  990 CONTINUE
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
