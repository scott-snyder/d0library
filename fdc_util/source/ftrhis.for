      FUNCTION FTRHIS()
C----------------------------------------------------------------------
C
C    Purpose and Methods : Booking and filling histograms for FTRAKS package
C
C-   Created  30-MAR-1990   Jeffrey Bantly  taken from ZTRHIS
C-   Updated  23-JUL-1990   Jeffrey Bantly  add more histograms
C-   Updated   8-NOV-1990   Jeffrey Bantly  mo' histograms, mo' histograms 
C-   Updated  29-APR-1991   Jeffrey Bantly  add EZERROR check, use new 
C-                                          RCP, PARAMS 
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-    to accomodate theta and phi errors and two spare words.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FTRHIS
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDRESF.INC'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER H,U,QU,S,W,UB,MAXHIS,PREV_W,LAYER
      PARAMETER( MAXHIS = 31 )
      INTEGER ERR,IQTRAK(26),NID(MAXHIS),ID,ICONT(10)
      INTEGER ITRA,NFDC,NTRK
      INTEGER LKFDCT, LKFDTH, LOGCHA, LYR, MODULE, NSEG, LSEG
      INTEGER LKFDCH,NHITS,IADD,WPOS,I,NLYR,LSEGN(0:2)
      INTEGER LKFTSC,NSW0HT,IHIT
      INTEGER LADDER(0:2)
      INTEGER NZBANK,GZFSEG,GZFDCT,GZFDCH,GZFTSC
      EXTERNAL NZBANK,GZFSEG,GZFDCT,GZFDCH,GZFTSC
C
      REAL FHIST(4,MAXHIS),CONT(26),QFSEC(3,34),Z0(2)
      REAL CHI,QTRAK(26),XPOS,YPOS,ZPOS,BESDIF,RESID
      REAL COSMTHE
      EQUIVALENCE(QTRAK,IQTRAK)
      integer ifsec(3,34)
      equivalence (qfsec, ifsec)
C
      LOGICAL FIRST,FDC
      LOGICAL EZERROR
C
      CHARACTER*34 NAME(MAXHIS)
C
      SAVE FIRST,NAME,FHIST,Z0
      DATA FIRST/.TRUE./
      DATA NAME/' # FDC tracks',
     & ' # wires on FDC track',
     & ' Chi FDC tracks',' X0 for track',
     & ' Y0 for track',' Phi for track',' Theta for track',
     & ' dx/dz for track',' dy/dz for track',
     & ' X pos on chamber',' Y pos on chamber',' Num hits per event',
     & ' Num seg lyr 0',' Num seg lyr 1',' Num seg lyr 2',
     & ' Num seg lyr 3',' Num seg lyr 4',' Num seg lyr 5',
     & ' Track resid - All',' Track resid - Theta',' Track resid - Phi',
     & ' Besdif for DL',' Track resid - DL',' Wires with hits on track',
     & ' Segs per no track',' Timing pulse bin',' Tim pul bin & trk',
     & ' Ionization per hit on track',
     & ' Track ioniz vs track theta',' Track ioniz vs track theta',
     & ' Track ioniz * cos(theta)'/
C----------------------------------------------------------------------
C
      FTRHIS=.FALSE.
      CALL DHDIR('FTRAKS_RCP','HBOOK_DIRECTORY',ERR,' ')
      IF(ERR.NE.0) THEN
        CALL ERRMSG('FTRAKS','FTRHIS',' ERROR SETTING HBK DIR','W')
      ENDIF
C
      IF (FIRST) THEN    ! Book histograms
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('FTRAKS','FTRHIS','FTRAKS_RCP not found.','W')
        ELSE
          CALL EZGET('FHIST(1)',FHIST(1,1),ERR)
          CALL EZGET('Z0',Z0,ERR)
          CALL EZRSET
        ENDIF
        DO 100 ID=1,MAXHIS
          IF (FHIST(1,ID).NE.1.) GO TO 100
          CALL HBOOK1(ID,NAME(ID),
     &    NINT(FHIST(2,ID)),FHIST(3,ID),FHIST(4,ID),0.)
  100   CONTINUE
        DO 101 ID=1,MAXHIS
          IF (FHIST(1,ID).EQ.2.) THEN
            CALL HBOOK2(ID,NAME(ID),NINT(FHIST(2,ID)),
     &                  FHIST(3,ID),FHIST(4,ID),NINT(FHIST(2,ID+1)),
     &                  FHIST(3,ID+1),FHIST(4,ID+1),0.)
          ENDIF
  101   CONTINUE
        FIRST = .FALSE.
      END IF
C
      IF (FHIST(1,26).EQ.1.) CALL HFF1(26,NID(26),TMPUBN,1.)
      CALL GTFTRH(ICONT)
      NFDC=ICONT(2)
      IF (NFDC.LT. 0) GOTO 990
C
      NTRK=0
      IF (NFDC.EQ.0) GOTO 200
      IF (FHIST(1,27).EQ.1.) CALL HFF1(27,NID(27),TMPUBN,1.)
      DO 203 ITRA=1,NFDC
        CALL GTFDCT(ITRA,CONT,QFSEC,LADDER)
        CALL UCOPY(CONT,QTRAK,26)
        IF(IQTRAK(2).LE.0) GOTO 203
        NTRK=NTRK+1
        CHI=SQRT(2*QTRAK(19))-SQRT(2*(FLOAT(IQTRAK(2))-2.)-1.)
        IF (FHIST(1,2).EQ.1.) CALL HFF1(2,NID(2),FLOAT(IQTRAK(2)),1.)
        IF (FHIST(1,3).EQ.1.) CALL HFF1(3,NID(3),CHI,1.)
        IF (FHIST(1,4).EQ.1.) CALL HFF1(4,NID(4),QTRAK(4),1.)
        IF (FHIST(1,5).EQ.1.) CALL HFF1(5,NID(5),QTRAK(5),1.)
        IF (FHIST(1,6).EQ.1.) CALL HFF1(6,NID(6),QTRAK(6),1.)
        IF (FHIST(1,7).EQ.1.) CALL HFF1(7,NID(7),QTRAK(22),1.)
        IF (FHIST(1,8).EQ.1.) CALL HFF1(8,NID(8),QTRAK(7),1.)
        IF (FHIST(1,9).EQ.1.) CALL HFF1(9,NID(9),QTRAK(8),1.)
        IF (FHIST(1,28).EQ.1.) CALL HFF1(28,NID(28),QTRAK(20),1.)
        COSMTHE=ABS(COS(QTRAK(22)))*QTRAK(20)
        IF (FHIST(1,29).EQ.2.) CALL HFF2(29,NID(29),
     &               QTRAK(22),COSMTHE,1.)
        IF (FHIST(1,31).EQ.1.) CALL HFF1(31,NID(31),COSMTHE,1.)
C
        LKFDCT=GZFDCT(ITRA)
        IF(LKFDCT.LE.0) GOTO 203
        LKFDTH=LQ(LKFDCT-1)
        IF(LKFDTH.LE.0) GOTO 203
        LOGCHA=(IQ(LKFDTH+1))/2
        CALL FCODER(LOGCHA,H,U,QU,S,W,UB,1)
        ZPOS=104.*((-1.)**(H+1))-Z0(H+1)
        XPOS=QTRAK(4)+QTRAK(7)*ZPOS
        YPOS=QTRAK(5)+QTRAK(8)*ZPOS
        IF (FHIST(1,10).EQ.1.) CALL HFF1(10,NID(10),XPOS,1.)
        IF (FHIST(1,11).EQ.1.) CALL HFF1(11,NID(11),YPOS,1.)
        W=-1
        DO 204 IHIT=1,IQTRAK(2)
          CALL UCOPY_i(iFSEC(1,IHIT),IADD,1)
          resid = qfsec(3, ihit)
          !CALL UCOPY(QFSEC(3,IHIT),RESID,1)
          PREV_W=W
          CALL FCODER(IADD/2,H,U,QU,S,W,UB,1)
          LAYER=U*2 + QU/4
          WPOS =LAYER*9 + W
          IF(W.EQ.PREV_W .AND. W.EQ.0) THEN
            IF(FHIST(1,23).EQ.1.) CALL HFF1(23,NID(23),RESID,1.)
            WPOS=WPOS+8
            IF(FHIST(1,24).EQ.1.) CALL HFF1(24,NID(24),FLOAT(WPOS),1.)
            GOTO 204
          ENDIF
          IF (FHIST(1,19).EQ.1.) CALL HFF1(19,NID(19),RESID,1.)
          IF(U.EQ.0) THEN
            IF (FHIST(1,20).EQ.1.) CALL HFF1(20,NID(20),RESID,1.)
          ELSEIF(U.EQ.1) THEN
            IF (FHIST(1,21).EQ.1.) CALL HFF1(21,NID(21),RESID,1.)
          ENDIF
          IF(FHIST(1,24).EQ.1.) CALL HFF1(24,NID(24),FLOAT(WPOS),1.)
  204   CONTINUE
  203 CONTINUE
C
  200 CONTINUE                          ! End of tracks
      IF (FHIST(1,1).EQ.1.) CALL HFF1(1,NID(1),FLOAT(NTRK),1.)
C
      DO H=0,MXHALF
        DO LYR=0,2
          NSEG=0
          MODULE=H*3 + LYR
          LSEG=GZFSEG(H,LYR)
          IF(LSEG.GT.0) NSEG=NZBANK(IXCOM,LSEG)
          IF (FHIST(1,13+MODULE).EQ.1.) CALL HFF1(13+MODULE,
     &          NID(13+MODULE),FLOAT(NSEG),1.)
        ENDDO                           ! End loop over layers in half
      ENDDO                             ! End loop over halves in FDC
C
      IF(NFDC.EQ.0) THEN
        DO H=0,MXHALF
          NLYR=0
          DO LYR=0,2
            NSEG=0
            MODULE=H*3 + LYR
            LSEGN(LYR)=GZFSEG(H,LYR)
            IF(LSEGN(LYR).GT.0) THEN
              NLYR = NLYR + 1
              IF (FHIST(1,25).EQ.1.) CALL HFF1(25,NID(25),
     &                                                FLOAT(LYR+1),1.)
            ENDIF
          ENDDO
          IF (FHIST(1,25).EQ.1.) CALL HFF1(25,NID(25),FLOAT(-NLYR),1.)
        ENDDO
      ENDIF
C
      LKFDCH=GZFDCH()                   ! Number of hits in FDC
      NHITS=0
      IF(LKFDCH.GT.0) NHITS=IQ(LKFDCH+1)
      IF (FHIST(1,12).EQ.1.) CALL HFF1(12,NID(12),FLOAT(NHITS),1.)
C
      IF (FHIST(1,22).EQ.1.) THEN       ! Delay line mismatches in FDC
        DO 205 H=0,MXHALF
          DO 206 QU=0,MXQUAD
            DO 207 S=0,MXSECT
              LKFTSC=GZFTSC(H,QU,S)
              IF(LKFTSC.LE.5) GOTO 207
              NSW0HT=IQ(LKFTSC+4)
              IF(NSW0HT.LE.0) GOTO 207
              DO 208 IHIT=1,NSW0HT
                BESDIF=DLRES(IHIT,S,QU,H)
                IF(BESDIF .EQ. 1000.0) GOTO 208
                CALL HFF1(22,NID(22),BESDIF,1.)
  208         CONTINUE
  207       CONTINUE
  206     CONTINUE
  205   CONTINUE
      ENDIF
C
C  Done.
C
  990 CONTINUE
      FTRHIS=.TRUE.
C-------------------------------------------------------------------------
  999 RETURN
      END
