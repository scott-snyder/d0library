      PROGRAM D0HCMP_HST_RCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to scan given histogram file and produce
C-                         list of direcories/histograms to be used by
C-                         D0HCMP_ADD and D0HCMP_VERI
C-   Inputs  : the rcp file  D0HCMP_HST_RCP_RCP and files specified in it
C-   Outputs : the file specified in the rcp file
C-   Controls:
C-   CALLS   : SRCP UTIL and HBOOK routines plus
C-             D0HCMP_DIRSCAN
C-
C-   Created   9-MAY-1992   Krzysztof L. Genser
C-   Updated  31-AUG-1992   Krzysztof L. Genser
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PAWC.INC/LIST'
      INCLUDE 'D0$PARAMS:D0HCMP.DEF/LIST'
      INCLUDE 'D0$INC:D0HCMP_HST_RCP.INC/LIST'
C----------------------------------------------------------------------
      EXTERNAL D0HCMP_DIRSCAN
C----------------------------------------------------------------------

      CHARACTER*255 CSUBDIR

      INTEGER LLOGFID1
      INTEGER LPAWMID

      INTEGER IDIRN
      INTEGER LIDS1(MAXNCHISTD),NIDS1,IIDS1
      INTEGER LIDS2(MAXNCHISTD),NIDS2,IIDS2

      REAL    CONFLEV

      CHARACTER*255 CIHSF,CSRCPF
      INTEGER       LIHSF,LSRCPF
      LOGICAL       OK

C----------------------------------------------------------------------
C
C ****  initialize ZEBRA and PAW
C
      CALL MZEBRA(-3)
      CALL INZSTP
      CALL INPAWC
C
C ****  input rcp file
C
      CALL INRCP ('D0HCMP_HST_RCP_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG
     &      ('NO_RCP_FILE','D0HCMP_HST_RCP',
     &      'Problem accessing D0HCMP_HST_RCP_RCP','F')
      ENDIF
C
C ****  get the HBOOK RZ file to be scanned
C
      CALL EZGETS ('INPUT_HISTOGRAM_FILE',1,CIHSF,LIHSF,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Could not get INPUT_HISTOGRAM_FILE name','F')
      ENDIF
C
C ****  get the output semircp file name
C
      CALL EZGETS ('OUTPUT_SEMIRCP_FILE',1,CSRCPF,LSRCPF,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Could not get the OUTPUT_SEMIRCP_FILE name','F')
      ENDIF
C
C ****  get the print flag
C
      CALL EZGET ('LHPRINT', LHPRINT,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Could not get print flag','W')
      ENDIF

C
C ****  get the confidence level
C
      CALL EZGET ('CONFLEV',CONFLEV,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Could not get confidence level','W')
        CONFLEV = .5
      ENDIF

      CALL GTUNIT(IUSER,IUNIT1,IER)
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Can not get IUNIT1','F')
      ENDIF

      CALL HROPEN(IUNIT1,LOGFID1,CIHSF(1:LIHSF),' ',1024,IER)

      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Could not open histogram input file ','F')
      ENDIF

      CALL GTUNIT(IUSER,IUNIT2,IER)

      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Can not get IUNIT2','F')
      ENDIF

      CALL D0OPEN ( IUNIT2,CSRCPF(1:LSRCPF),'OFL',OK)

      IF ( .NOT.OK ) THEN
        CALL ERRMSG('D0HCMP_HST_RCP','D0HCMP_HST_RCP',
     &      'Could not open output file ','F')
      ENDIF
C
C ****  files opened, scann rz directory structure
C
      NDIRN = 0

      NCDIR = '//'//LOGFID1
      LLOGFID1 = LENOCC(NCDIR)

      CALL RZSCAN(NCDIR(1:LLOGFID1),D0HCMP_DIRSCAN)
C
C ****  write header on the output file
C
      WRITE (IUNIT2,'((A))')
     &  '\ARRAY HISTOGRAMS_TO_COMPARE'
      WRITE (IUNIT2,'((A))')
     &  ' '

C
C ****  now get the histograms in each directory
C
      DO 10 IDIRN =  1,NDIRN

        NCDIR  = HSUBDIRS(IDIRN)(1:LENOCC(HSUBDIRS(IDIRN)))
        LNCDIR = LENOCC(NCDIR)

        MCDIR  = '//'//PAWMID//NCDIR(LLOGFID1+1:LNCDIR)
        LMCDIR = LENOCC(MCDIR)

C        PRINT *,NCDIR(1:LNCDIR)
C        PRINT *,MCDIR(1:LMCDIR)

        CALL HCDIR( NCDIR(1:LNCDIR),' ')
C
C ****  create directory in memory, but skip top level one
C
        IF (IDIRN.NE.1) THEN

          CALL HMDIR( MCDIR(1:LMCDIR),'S')

        ENDIF

        CALL HRIN(0,99999,0)
C
C ****  use the hbook routine to get the 1 dim HIST ids
C
        NIDS1 = 0
        NIDS2 = 0

        CALL HID1(LIDS1,NIDS1)
        CALL HID2(LIDS2,NIDS2)

        IF ( NIDS1.GT.MAXNCHISTD ) THEN
          WRITE (IOUTPT,'(1X,(A))')
     &'Error, more 1-DIM histograms present than can be accomodated'
          WRITE (IOUTPT,'(1X,(A))') 'Program may be overritten'
        ENDIF

        IF ( NIDS2.GT.MAXNCHISTD ) THEN
          WRITE (IOUTPT,'(1X,(A))')
     &'Error, more 2-DIM histograms present than can be accomodated'
          WRITE (IOUTPT,'(1X,(A))') 'Program may be overritten'
        ENDIF

C
C ****  now print it out
C
        IF (LNCDIR.EQ.LLOGFID1) GOTO 10

        CSUBDIR = ''''//HSUBDIRS(IDIRN)
     &    (LLOGFID1+2:LENOCC(HSUBDIRS(IDIRN)))//''''

        DO 11 IIDS1 = 1,NIDS1

          WRITE (IUNIT2,'((A),I10,F10.5)')
     &      CSUBDIR(1:40),LIDS1(IIDS1), CONFLEV

   11   CONTINUE

        DO 12 IIDS2 = 1,NIDS2

          WRITE (IUNIT2,'((A),I10,F10.5)')
     &      CSUBDIR(1:40),LIDS2(IIDS2), CONFLEV

   12   CONTINUE


        IF ( LHPRINT ) THEN
          CALL HPRINT(0)
        ENDIF

        CALL HDELET (0)

   10 CONTINUE
C
C ****  file analysed;close
C

      WRITE (IUNIT2,'((A))')
     &  ' '

      WRITE (IUNIT2,'((A))')
     &  '\END'

      CLOSE (IUNIT2)
      CALL HREND (LOGFID1)

      END
