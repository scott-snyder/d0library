      FUNCTION GB_TRIGGER_HIST()
C----------------------------------------------------------------------
C
C-   Purpose and Methods : Booking and filling histograms for
C-                         GB_TRIGGER package
C-
C-   Returned value  : TRUE if histograms successfully filled.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  19-NOV-1992   Jeffrey Bantly
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GB_TRIGGER_HIST
      INTEGER L2_TRIGGER_WORD
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER MAXHIS
      PARAMETER( MAXHIS = 1 )
      INTEGER ID,NID(MAXHIS)
      INTEGER NFILTON,NTRIGON
      INTEGER TRIGBON(32),FILTBON(128),BITNUM
      INTEGER MAXFILT
      PARAMETER( MAXFILT = 16 )
      INTEGER NAME_MAP(MAXFILT)
      INTEGER ERR, IDF
      INTEGER RUN, EVT
      INTEGER NMATCH, MATCH_ID(MAXFILT), MATCH_IDF(MAXFILT)
      INTEGER L2_FILT_EXP_BITS
      INTEGER L2_TRIGGER_BITS
      INTEGER NUMFILT
      INTEGER IDX, LENGTH
C
      REAL    TRG_HIST(4,MAXHIS)
      REAL    FILT_BIT
C
      LOGICAL FIRST, EZERROR
C
      CHARACTER*27 NAME(MAXHIS)
      CHARACTER*16 TRIGNON(32)
      CHARACTER*12 FILTNON(128)
      CHARACTER*12 FILTNAMES(MAXFILT)
C
      SAVE FIRST,NAME,TRG_HIST,FILTNAMES
      DATA FIRST/.TRUE./
      DATA NAME/'Trig Filt Bit - All'/
C----------------------------------------------------------------------
C
      GB_TRIGGER_HIST=.FALSE.
      CALL DHDIR('GB_TRIGGER_RCP','HBOOK_DIRECTORY',ERR,' ')
      IF(ERR.NE.0) THEN
        CALL ERRMSG('GB_TRIGGER-bad-hbk-dir','GB_TRIGGER_HIST',
     &          ' ERROR SETTING HBK DIR','W')
      ENDIF
C
      IF (FIRST) THEN    ! Book histograms
        CALL EZPICK('GB_TRIGGER_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('GB_TRIGGER-no-rcp','GB_TRIGGER_HIST',
     &                        'GB_TRIGGER_RCP not found.','W')
        ELSE
          DO IDX=1,MAXFILT
            CALL EZGETS('FILTNAMES',IDX,FILTNAMES(IDX),LENGTH,ERR)
          ENDDO
          CALL EZGET('GB_TRIG_HIST(1)',TRG_HIST(1,1),ERR)
          CALL EZRSET
        ENDIF
C
        DO 100 ID=1,MAXHIS
          IF (TRG_HIST(1,ID).NE.1.) GO TO 100
          CALL HBOOK1(ID,NAME(ID),
     &      NINT(TRG_HIST(2,ID)),TRG_HIST(3,ID),TRG_HIST(4,ID),0.)
  100   CONTINUE
        NUMFILT = 0
        DO ID=1,MAXFILT
          IF ( FILTNAMES(ID).NE.'  ' ) NUMFILT=ID
        ENDDO
        FIRST = .FALSE.
      END IF
C
      CALL GTTSUM(NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
      NMATCH = 0
      DO IDF=1,NFILTON
        DO ID=1,NUMFILT
          IF ( FILTNAMES(ID).EQ.FILTNON(IDF) ) THEN
            NMATCH = NMATCH + 1
            MATCH_ID(NMATCH) = ID
            MATCH_IDF(NMATCH) = IDF
          ENDIF
        ENDDO
      ENDDO
C
      L2_FILT_EXP_BITS = 0
      DO ID=1,NMATCH
        L2_FILT_EXP_BITS = IBSET( L2_FILT_EXP_BITS, MATCH_ID(ID)-1 )
        FILT_BIT = FLOAT(MATCH_ID(ID)-1)
        IF(TRG_HIST(1,1).EQ.1.) CALL HFF1(1,NID(1),FILT_BIT,1.)
      ENDDO
C
C  Done.
C
  990 CONTINUE
      GB_TRIGGER_HIST=.TRUE.
      RETURN
C
C ****
C
      ENTRY L2_TRIGGER_WORD( L2_TRIGGER_BITS )
C
      L2_TRIGGER_WORD = L2_FILT_EXP_BITS
      L2_TRIGGER_BITS = L2_FILT_EXP_BITS
C
C-------------------------------------------------------------------------
  999 RETURN
      END
