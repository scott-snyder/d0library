      SUBROUTINE DTRHBK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book histograms for CDC
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   5-JUN-1990   Qizhong Li-Demarteau
C-   Updated   5-FEB-1991   Qizhong Li-Demarteau  added histograms for
C-                                                wire hits efficiency 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  12-DEC-1991   Qizhong Li-Demarteau  "# of hits" histograms 
C-                                       are booked when (HISTON(5)>1) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CDPARA.INC'
C
      INTEGER HIST, HISTON(10), IER
      INTEGER ID, WIRE, PAR, SECTOR, LAYER, ERR, MAXLAY, MAXSEC
      INTEGER IDHLAY
      LOGICAL EZERROR
      REAL    HISPAR(3,8)
      CHARACTER*32 HITTIL 
      CHARACTER*30 CDHNAM
      CHARACTER*8 NUMLAY(0:3)
      CHARACTER*6 NUMWIR(0:MXFADC)
      CHARACTER*10 TITRE(8)
      DATA NUMLAY / 'Layer 0','Layer 1','Layer 2','Layer 3' /
      DATA NUMWIR /
     &  'Wire 0','Wire 1','Wire 2','Wire 3','Wire 4','Wire 5','Wire 6',
     &  'DL 0 -','DL 0 +','DL 1 -','DL 1 +'/
      DATA TITRE / 'Time    ','Charge  ','Width   ','Peak Amp',
     &             '# Hits  ','# Tracks','DL1 RES ','DL2 RES '/
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DTRHBK',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('HISTON(1)',HISTON(1),ERR)
      CALL EZGET_rarr('HISPAR(1)',HISPAR(1,1),ERR)
      CALL EZGET('MAXLAY',MAXLAY,ERR)
      IF (ERR .NE. 0) MAXLAY = 3
      CALL EZGET('MAXSEC',MAXSEC,ERR)
      IF (ERR .NE. 0) MAXSEC = 31
      CALL EZRSET
C
C         Create/Set HBOOK directory for DTRAKS
C
      CALL DHDIR('DTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER .NE. 0) THEN
        CALL ERRMSG('DTRAKS','DTRHBK',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      DO 10 LAYER = 0, MAXLAY
        DO 30 HIST = 1,5
          IF (HISTON (HIST).EQ.0) GOTO 30
          IF (HIST .EQ. 5 .AND. HISTON(5) .LE. 1) GOTO 30
C
C  Histograms are separated by sectors when less than six sectors are in test.
C  Otherwise histograms are not separated by sectors.
C
          IF (MAXSEC .LT. 5) THEN
            DO 60 SECTOR = 0, MAXSEC
              DO 40 WIRE = 0,MXFADC
                ID = 1000*HIST + 100*LAYER + SECTOR*20 + WIRE
                WRITE (CDHNAM,1000) TITRE(HIST), SECTOR, LAYER, WIRE
 1000           FORMAT (1X,A10,' SEC',I2,' LAY',I2,' W',I3,'$')
                CALL HBOOK1 (ID,CDHNAM, NINT(HISPAR(1,HIST)),
     &                              HISPAR(2,HIST), HISPAR(3,HIST),0.)
   40         CONTINUE
   60       CONTINUE
          ELSE
            DO 42 WIRE = 0,MXFADC
              ID = 1000*HIST + 100*LAYER + WIRE
              WRITE (CDHNAM,1002) TITRE(HIST), LAYER, WIRE
 1002         FORMAT (1X,A10,' LAY',I2,' W',I3,'$')
              CALL HBOOK1 (ID,CDHNAM, NINT(HISPAR(1,HIST)),
     &                              HISPAR(2,HIST), HISPAR(3,HIST),0.)
   42       CONTINUE
          ENDIF
   30   CONTINUE
        IF (HISTON(5).NE.0) THEN
          IDHLAY = 5400 + LAYER
          HITTIL = 'WIRE HITS EFFICIENCY ('//NUMLAY(LAYER)//')$'
          CALL HBOOK1(IDHLAY,HITTIL,352,-0.5,351.5,0.)
          IDHLAY = 5404 + LAYER
          HITTIL = 'WIRE HITS EFFICIENCY (SW '//NUMLAY(LAYER)//')$'
          CALL HBOOK1(IDHLAY,HITTIL,224,-0.5,223.5,0.)
          IDHLAY = 5408 + LAYER
          HITTIL = 'WIRE HITS EFFICIENCY (DL '//NUMLAY(LAYER)//')$'
          CALL HBOOK1(IDHLAY,HITTIL,128,-0.5,127.5,0.)
        ENDIF
        IF (HISTON(6).NE.0) THEN
          ID = 1000*HIST + 100*LAYER 
          CDHNAM = TITRE(HIST)//NUMLAY(LAYER)//'$'
          CALL HBOOK1 (ID,CDHNAM, NINT(HISPAR(1,6)), HISPAR(2,6),
     &                            HISPAR(3,6),0.)
        ENDIF
        DO 50 HIST = 7,8
          IF (HISTON(HIST).EQ.0) GOTO 50
          DO 100 SECTOR = 0, MAXSEC
            ID = 1000*HIST + 100*LAYER + SECTOR
            WRITE (CDHNAM,1001) TITRE(HIST), LAYER, SECTOR
 1001       FORMAT (1X,A10,' LAY',I3,' SECTOR',I3,'$')
            CALL HBOOK1 (ID,CDHNAM, NINT(HISPAR(1,HIST)), 
     &        HISPAR(2,HIST), HISPAR(3,HIST),0.)
  100     CONTINUE
   50   CONTINUE
   10 CONTINUE
C
      IF (HISTON(9).NE.0) THEN
        CALL HBOOK1(1991,'Trigger Signal (TIME)$',100,0.0,0.0,0.)
        CALL HBOOK1(1992,'Trigger Signal (WIDTH)$',10,0.0,100.0,0.)
        CALL HBOOK1(1993,'Trigger Signal (HEIGHT)$',100,0.0,800.0,0.)
      ENDIF
C
      CALL DHSTBK               !to book your own histograms
      CALL HIDOPT(0,'STAT')     !  get the statistics for all histograms
C
  999 RETURN
      END
