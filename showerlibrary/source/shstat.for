      SUBROUTINE SHSTAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :    Subroutine called once to read the  shower
C-   library and fill debugging histograms.
C-   Intended to show whether adequate statistics have been
C-   obtained. The RZ file must already be declared and opened.
C-
C-   Created  21-FEB-1989   John Womersley
C-   Updated  11-FEB-1990   Rajendran Raja  FILE STATISTICS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:PAWC.INC'
C
      INTEGER ICYCLE
C
      EXTERNAL NZBANK
      INTEGER NZBANK

      INTEGER NVECT,NRET,NDIMEN
      PARAMETER (NVECT=250)
      REAL VECT(NVECT)
C
      INTEGER TOT_CYCLES
      INTEGER LW,NB
      INTEGER I1,I2,I3,I4,I5
      INTEGER IER,IJ
      LOGICAL DO_SHSTAT,COUNT_CYCLES
      INTEGER LSUP                      ! DUMMY ADDRESS
C
      INTEGER RFILE,IFILE,NFILES
      INTEGER RCPAR(20),RCPTYP(20),TOTAL,TOTAL4
      CHARACTER*80 FILNAM
C
      INTEGER DCYCLES(NDATA_CYCLES),NDCYCLES,NFILES_DUM
      INTEGER NTRACKS, TRULEN, NTOT_TRACKS
C
      INTEGER PRTUN,SSUNIT
      LOGICAL CYC
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER INDX_KEY
C-----------------------------------------------------------------------
      CALL EZPICK('SHOWERLIBRARY_RCP')
      CALL EZGET('DO_SHSTAT',DO_SHSTAT,IER)
      CALL EZGET('COUNT_CYCLES',COUNT_CYCLES,IER)
      CALL EZRSET
C
      IF(.NOT.DO_SHSTAT)RETURN
C
      PRTUN = SSUNIT()
C
      CALL DHDIR('SHOWERLIBRARY_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','SHLEVT',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
          CALL HBOOK1(1000,'TRACKS STORED PER BIN',50,-.5,49.5,0.)
      ENDIF
C
      IF(COUNT_CYCLES)THEN
        TOT_CYCLES = 0
C
C ****  loop over all entries in the library
C
        DO 100 I1=1,NVBIN
          DO 110 I2=1,NEBIN
            DO 120 I3=1,NMOM
              DO 130 I4 = 1,NPART_ID
                DO 140 I5 = 1,NPHI_ID
C
C ****  set key vector
C
                  KEY(1)=I1
                  KEY(2)=I2
                  KEY(3)=I3
                  KEY(4)=I4
                  KEY(5)=I5
C
C ****  read in the shower library array for each key
C
                  IF(CYCLES(INDX_KEY(KEY)).EQ.0)GOTO 140
                  NB = CYCLES(INDX_KEY(KEY))
                  TOT_CYCLES = TOT_CYCLES + NB
                  CALL HFILL(1000,FLOAT(NB),0.,1.0)
  140           CONTINUE
  130         CONTINUE
  120       CONTINUE
  110     CONTINUE
  105     CONTINUE
  100   CONTINUE
        WRITE(PRTUN,101)TOT_CYCLES,NCYCLES
  101   FORMAT(' NUMBER OF TRACKS FROM CYCLES ARRAY COUNT ',I8,/,
     &      ' NUMBER OF TRACKS EXPECTED FROM AUXILLIARY FILE ',I8)
        IF(TOT_CYCLES.NE.NCYCLES)THEN
          CALL ERRMSG('SHOWERLIBRARY ','SHSTAT',
     &    ' TRACK COUNT DISAGREEMENT ','W')
        ENDIF
      ENDIF
C
      WRITE(PRTUN,102)
  102 FORMAT(//' ******** RZ FILE STATISTICS *********** '//)
C
      CYC = .TRUE.
      CALL SHLAUX_READ(ISUNIT_AUX,1,NFILES,FILNAM,DCYCLES,NDCYCLES,
     &  NTRACKS,NTOT_TRACKS,CYC)
C
      WRITE(PRTUN,*) 'FIND',NFILES,' CYCLES OF FILENAMES'
      WRITE(PRTUN,*) 'FIND',NTOT_TRACKS,' NUMBER OF TRACKS IN TOTO'
C
      DO 200 IFILE  = 1 , NFILES
C
        CYC = .FALSE.
        CALL SHLAUX_READ(ISUNIT_AUX,IFILE,NFILES_DUM,FILNAM,DCYCLES,
     &    NDCYCLES,NTRACKS,NTOT_TRACKS,CYC)
C
        WRITE(PRTUN,*)'NOW READ CYCLE',IFILE,' CYCLES ARRAY'
C
        WRITE(PRTUN,103)IFILE,FILNAM(1:TRULEN(FILNAM)),NTRACKS
  103   FORMAT(' INPUT FILE ',I8,/, ' NAME: ',A,/,
     &    '  CUMULATIVE NUMBER OF TRACKS ',I8)
  200 CONTINUE
      RETURN
      END
