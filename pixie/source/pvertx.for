      SUBROUTINE PVERTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw VTX y-z view
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-MAY-1989   Sharon Hagopian
C-   Modified 17-DEC-1989   Peter Grudberg - handle unmatched hits
C-   Updated  24-JAN-1990   Lupe Howell  Implementing color Table
C- Remodified 20-FEB-1990   P.G. - color table implemented on wrong version
C-   Updated  24-SEP-1990   Lupe Howell  Implementing PIXIE_RCP
C-   Updated   7-MAY-1991   Lupe Howell  Putting a EZRSET for VTRAKS_RCP
C-   Updated  20-SEP-1991   Lupe Howell  Adding error check for ezpick 
C-   Updated  17-MAR-1993   Alexandre Zinchenko - handle compressed hits
C-   Updated  27-June-1994  Danilo Puseljic - Added call to VCHT_UNPACK
C                           to handle VCHT banks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL MED, MED2, X, Y
      INTEGER ISEC,ILAY,NHITS,IH,NH,NEL,NWORDS,NGOOD,NVMAX,NV
      PARAMETER (NVMAX=5)
      REAL YCEN(256),YERR(256),ZCEN(256),ZERR(256),CONT(18)
      INTEGER NSEC(3),POS(256),DRWNOZ
      REAL ZZ,YY,ZVER(NVMAX),DZVER(NVMAX)
      CHARACTER*4 CVAL, REM
      INTEGER TYP
      LOGICAL EZERROR
      INTEGER LVHIT, GZVHIT, WORDS(3), WIRE, JHIT, IENDS, ONTRK
      INTEGER ISIDE, IZTRK, MIRR, MARK, IHIT
      INTEGER LVCHT, GZVCHT, ISTAT, LVLAY0, GZVLAY
      REAL SIZE, XPOS(2), YPOS(2), ZPOS, AREA
C----------------------------------------------------------------------
      DATA YY,ZZ/0.,0./
      PARAMETER (SIZE = 0.15)
C----------------------------------------------------------------------
      INTEGER IER,ICALL,LVERH,GZVERH
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        IF ( .NOT. EZERROR(IER) ) THEN
          CALL EZGET('NSEC',NSEC,IER)
          ICALL=1
          CALL EZRSET
        ENDIF
      END IF
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVERTX','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some VTX constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','DRAW NO Z MATCH',1,DRWNOZ,
     &       CVAL,TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PVERTX',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
C
      CALL PUOPEN
      CALL ZYAXIS
C
C Use compressed hits bank, if it exists
C
      LVHIT = GZVHIT()
      IF (LVHIT.LE.0) GO TO 490
      CALL GTVHIT(0,WORDS,ILAY,ISEC,WIRE,JHIT,IENDS,
     &            ONTRK,ISIDE,IZTRK,XPOS,YPOS,ZPOS,AREA)
      NHITS = WORDS(1)
      DO 400 IHIT = 1, NHITS
        CALL GTVHIT(IHIT,WORDS,ILAY,ISEC,WIRE,JHIT,IENDS,
     &              ONTRK,ISIDE,IZTRK,XPOS,YPOS,ZPOS,AREA)
        MARK = 2
        IF (ONTRK.EQ.1) THEN
          MIRR = 1
          CALL PXCOLR('CYA')
        ELSE
          MIRR = 2
          CALL PXCOLR('RED')
        ENDIF
        IF (IENDS.NE.3) THEN ! no Z-match
          IF (DRWNOZ.EQ.0) GO TO 400
C          CALL PXCOLR('YEL')
          MARK = 1
        ENDIF
        CALL JCMARK(MARK)
        DO 410 IH = 1, MIRR
          IF (ZPOS.LT.99.) THEN
            IF (XPOS(IH).LT.99.AND.YPOS(IH).LT.99) THEN
              CALL J3MARK(ZPOS,YPOS(IH),0.)
            ENDIF
          ENDIF
  410   CONTINUE 
  400 CONTINUE
      GO TO 210
C
C draw normal hits, check if VSEC banks are there if not look for VCHT
C
  490 CONTINUE
      LVLAY0 = GZVLAY(0)
      IF(LVLAY0.EQ.0)THEN
        LVCHT = GZVCHT()

        IF (LVCHT .NE. 0) THEN
          DO ILAY=0,2
            DO ISEC=0,NSEC(ILAY+1)
              CALL VCHT_UNPACK(ILAY,ISEC,ISTAT)
            ENDDO
          ENDDO
        ENDIF

      ENDIF
      
      DO 200 ILAY=0,2
        CALL GTVLAY( ILAY,NH )
        IF ( NH .LT. 8 ) GO TO 200
        DO 100 ISEC=0,NSEC(ILAY+1)
          CALL GTVSEC( ILAY,ISEC,'SEC',0,NEL,NWORDS,CONT )
          CALL UCOPY( CONT,NH,1 )
          IF ( NH .LT. 8 ) GO TO 100
C
C            VCHEKH is called for `ideal' Monte Carlo data
C            to simulate two-hit resolution by flagging hits
C            that have drift time within 84 nsec from previous
C            hit on the same wire and to smear the drift and z
C            coordinates.
          CALL VCHEKH( 3,ILAY,ISEC,NH,NGOOD )
          CALL PVGTHT(ILAY,ISEC,NHITS,YCEN,YERR,ZCEN,ZERR,POS)
          IF(NHITS.EQ.0)GO TO 100
          YY=0.
          ZZ=0.
          DO 50 IH=1,NHITS
            IF ( POS(IH) .EQ. 1 ) THEN
              CALL PXCOLR('CYA')
              MED2=( (ZCEN(IH)-ZERR(IH)/2.)+(ZCEN(IH)+ZERR(IH)/2.) )/2.
              CALL JCMARK(2)
              CALL J3MARK(MED2,YCEN(IH),0. )
            ELSEIF ( POS(IH) .EQ. 0 ) THEN      ! unmatched z hit
              IF ( DRWNOZ .EQ. 1 ) THEN
                CALL PXCOLR('YEL')
                CALL JCMARK(1)
                CALL J3MARK(ZCEN(IH),YCEN(IH),0.)
              ENDIF
              GO TO 50
            ELSE
              CALL PXCOLR('RED')
            ENDIF
            CALL J3MOVE(ZCEN(IH)-ZERR(IH)/2.,YCEN(IH),0.)
            CALL J3DRAW(ZCEN(IH)+ZERR(IH)/2.,YCEN(IH),0.)
            IF(ZCEN(IH).GT.ZZ)ZZ=ZCEN(IH)
            IF(YCEN(IH).GT.YY)YY=YCEN(IH)
   50     CONTINUE
  100   CONTINUE
  200 CONTINUE
C
C mark vertex; first check if vertexing was done
C
  210 CONTINUE
      LVERH=GZVERH()
      IF (LVERH.EQ.0) GO TO 300
      CALL ZVERTE(NV,ZVER,DZVER)
      IF(NV.EQ.0)GO TO 300
      CALL PXCOLR('GRE')
      CALL JSIZE(2.2,2.0)
      CALL JFONT(5)
      CALL JJUST(2,2)
      CALL JCMARK(1)
      X = (ZVER(1)+DZVER(1))-(ZVER(1)-DZVER(1))
      Y = .13
      CALL J3MOVE(ZVER(1)-DZVER(1),0.,0.)
      CALL JRRECT(X,Y)
      MED = ( (ZVER(1)-DZVER(1))+(ZVER(1)+DZVER(1)) )/2
      CALL J3MOVE(MED,0.,0.)
      CALL JHSTRG('X')
  300 CONTINUE
      CALL JRCLOS
  999 RETURN
      END
