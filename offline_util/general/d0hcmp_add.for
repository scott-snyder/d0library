      PROGRAM D0HCMP_ADD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to sum up selected histograms from selected files
C-                         according to the info from the RCP file.
C-   Inputs  :
C-   Outputs : the rcp file D0HCMP_ADD_RCP and files specified in it
C-   Controls:
C-   CALLS   : srcp, hbook, cernlib routines plus 
C-             D0HCMP_GET_HPAR
C-             D0HCMP_CRE_PAWDIRS
C-
C-   Created  23-MAR-1992   Krzysztof L. Genser
C-   Updated  31-AUG-1992   Krzysztof L. Genser   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PAWC.INC/LIST'
      INCLUDE 'D0$PARAMS:D0HCMP.DEF/LIST'

      INTEGER NFILES
      CHARACTER*255 CHFILES(MAXCHFILES)

      CHARACTER*255 C_RFILE
      INTEGER N_RFILE

      INTEGER JFILE

      CHARACTER*80 CHTITL1
      INTEGER NX1,NY1,LOC1,NWT1
      REAL    XMI1,XMA1,YMI1,YMA1

      CHARACTER*80 CHTITL2
      INTEGER NX2,NY2,LOC2,NWT2
      REAL    XMI2,XMA2,YMI2,YMA2

      INTEGER ICYCLE
C----------------------------------------------------------------------
C
C ****  initialize zebra and paw
C
      CALL MZEBRA(-3)
      CALL INZSTP
      CALL INPAWC
C
C ****  input rcp file
C
      CALL INRCP ('D0HCMP_ADD_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG
     &      ('NO_RCP_FILE','D0HCMP_ADD',
     &      'Problem accessing D0HCMP_ADD_RCP','F')
      ENDIF
C
C ****  get directories, histogram ids and confidence levels
C
      CALL D0HCMP_GET_HPAR('HISTOGRAMS_TO_COMPARE',SUBDIRS,
     &                      IDS,CLEV,NIDS,IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG ('BAD_RCP_FILE','D0HCMP_ADD',
     &    ' Inconsistent HISTOGRAMS_TO_COMPARE in D0HCMP_ADD_RCP','F')
      ENDIF
C
      WRITE (IOUTPT,'(1X,(A))')' '
      WRITE (IOUTPT,'(1X,(A))')
     &  'The following histograms will be added:'
      WRITE (IOUTPT,'(1X,(A))')' '
C
      DO 10 IHIST =  1,NIDS
        WRITE (IOUTPT,'(1X,A40,I8,F10.4)')
     &    SUBDIRS(IHIST)(1:40),IDS(IHIST)
   10 CONTINUE
C
C ****  sort the directories and create the directory structure in the memory
C
      CALL D0HCMP_CRE_PAWDIRS(SUBDIRS,PAWMID,NIDS,JSORT,IER)
C
C ****  get the list of HBOOK RZ files to be add up
C
      CALL EZ_GET_CHARS('HISTOGRAM_FILES_TO_ADD',NFILES,CHFILES,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Could not get histogram input file names','F')
      ENDIF
C
C ****  loop over files, sum the histograms up
C
      WRITE (IOUTPT,'(1X,(A))')' '
      WRITE (IOUTPT,'(1X,(A))')'The histograms from the following '//
     &  'files will be added:'
      WRITE (IOUTPT,'(1X,(A))')' '
C
      DO 102 JFILE = 1, NFILES
        WRITE (IOUTPT,'(1X,(A))')CHFILES(JFILE)(1:
     &    LENOCC(CHFILES(JFILE)))
  102 CONTINUE

      DO 101 JFILE = 1, NFILES

        WRITE (IOUTPT,'(1X,(A))')' '
        WRITE (IOUTPT,'(1X,(A))')'Working on file:'
        WRITE (IOUTPT,'(1X,(A))')
     &    CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))
        WRITE (IOUTPT,'(1X,(A))')' '

        CALL GTUNIT(IUSER,IUNIT1,IER)
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Can not get IUNIT1','F')
        ENDIF

        CALL HROPEN(IUNIT1,LOGFID1,
     &       CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE))),' ',1024,IER)

        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Could not open histogram input file ','F')
        ENDIF
C
C ****  file opened, read histograms in
C

        OCDIR = ' '
        LOCDIR = 0

        DO 11 IHIST =  1,NIDS

          NCDIR = '//'//LOGFID1//'/'//
     &      SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
          LNCDIR = LENOCC(NCDIR)

          MCDIR = '//'//PAWMID//'/'//
     &      SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
          LMCDIR = LENOCC(MCDIR)

          IF( NCDIR(1:LNCDIR) .NE. OCDIR(1:LOCDIR) ) THEN

C            WRITE (IOUTPT,'(1X,(A))'),'Working in directory: '//
C     &        SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))

            CALL HCDIR( MCDIR(1:LMCDIR),' ')
            CALL HCDIR( NCDIR(1:LNCDIR),' ')

            OCDIR = NCDIR
            LOCDIR = LENOCC(OCDIR)

          ENDIF
C
C ****  read in next histogram, delete the old one if needed
C
          IF ( HEXIST(IDS(JSORT(IHIST))+IHOFFSET1) ) THEN
            CALL HDELET(IDS(JSORT(IHIST))+IHOFFSET1)
          ENDIF

          CALL HRIN(IDS(JSORT(IHIST)),9999,IHOFFSET1)

          IF ( .NOT.HEXIST(IDS(JSORT(IHIST))+IHOFFSET1) ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &        IDS(JSORT(IHIST)),
     &        ' from directory  '//SUBDIRS(JSORT(IHIST))(1:
     &        LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
            WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &        CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//' is missing'
C
C ****  if this is the first time get the information about the histogram
C
          ELSEIF ( .NOT.HEXIST(IDS(JSORT(IHIST))) ) THEN

            CALL HGIVE(IDS(JSORT(IHIST))+IHOFFSET1,
     &          CHTITL1,NX1,XMI1,XMA1,NY1,YMI1,YMA1,NWT1,LOC1)
C
C ****  book the histogram in the memory
C
            IF ( NY1.EQ.0 ) THEN
              CALL HBOOK1(IDS(JSORT(IHIST)),CHTITL1,
     &                    NX1,XMI1,XMA1,0.)
            ELSE
              CALL HBOOK2(IDS(JSORT(IHIST)),CHTITL1,
     &                    NX1,XMI1,XMA1,NY1,YMI1,YMA1,0.)
            ENDIF

          ENDIF
C
C ****  sum up the histograms
C
          IF ( HEXIST(IDS(JSORT(IHIST))+IHOFFSET1) ) THEN
C
C ****  check if the histograms are consistent
C
            CALL HGIVE(IDS(JSORT(IHIST)),
     &          CHTITL1,NX1,XMI1,XMA1,NY1,YMI1,YMA1,NWT1,LOC1)
C
            CALL HGIVE(IDS(JSORT(IHIST))+IHOFFSET1,
     &          CHTITL2,NX2,XMI2,XMA2,NY2,YMI2,YMA2,NWT2,LOC2)
C

            IF     ( CHTITL1(1:LENOCC(CHTITL1)).NE.
     &               CHTITL2(1:LENOCC(CHTITL2)) ) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
              WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &          CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//
     &          ' has different title'

            ELSEIF ( NX1.NE.NX2 ) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
              WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &          CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//
     &          ' has different NX'

            ELSEIF ( XMI1.NE.XMI2 ) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
              WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &          CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//
     &          ' has different XMI'

            ELSEIF ( XMA1.NE.XMA2 ) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
              WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &          CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//
     &          ' has different XMA'

            ELSEIF ( NY1.NE.NY2 ) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
              WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &          CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//
     &          ' has different NY'

            ELSEIF ( YMI1.NE.YMI2 ) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
              WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &          CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//
     &          ' has different YMI'

            ELSEIF ( YMA1.NE.YMA2 ) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' from file'
              WRITE (IOUTPT,'(1X,(A),I8,(A))')
     &          CHFILES(JFILE)(1:LENOCC(CHFILES(JFILE)))//
     &          ' has different YMA'

            ELSE

              CALL HOPERA(IDS(JSORT(IHIST)),'+',
     &          IDS(JSORT(IHIST))+IHOFFSET1,
     &          IDS(JSORT(IHIST)),1.,1.)

            ENDIF

          ENDIF

   11   CONTINUE
C
C ****  file read in; close it.
C
        CALL HREND (LOGFID1)

        CALL RLUNIT(IUSER,IUNIT1,IER)
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Can not release IUNIT1','F')
        ENDIF

  101 CONTINUE

C
C ****  create the output histogram file
C
      WRITE (IOUTPT,'(1X,(A))')' '
      WRITE (IOUTPT,'(1X,(A))')'Histograms added; creating output file'

      CALL EZGETS ( 'OUTPUT_HISTOGRAM_FILE',1,C_RFILE,N_RFILE,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Can not get histogram output file name','F')
      ENDIF

      WRITE (IOUTPT,'(1X,(A))')C_RFILE(1:N_RFILE)
      WRITE (IOUTPT,'(1X,(A))')' '

      CALL GTUNIT(IUSER,IUNIT2,IER)
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Can not get IUNIT2','F')
      ENDIF

      CALL HROPEN(IUNIT2,LOGFID2,C_RFILE(1:N_RFILE),'N',1024,IER)

      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Could not open histogram input file ','F')
      ENDIF
C
C ****  create the directory  structure on the output file
C
      CALL D0HCMP_CRE_PAWDIRS(SUBDIRS,LOGFID2,NIDS,JSORT,IER)
C
C ****  save the requested histograms
C
      OCDIR = ' '
      LOCDIR = 0

      DO 12 IHIST =  1,NIDS

        NCDIR = '//'//LOGFID2//'/'//
     &    SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
        LNCDIR = LENOCC(NCDIR)

        MCDIR = '//'//PAWMID//'/'//
     &    SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
        LMCDIR = LENOCC(MCDIR)

        IF( NCDIR(1:LNCDIR) .NE. OCDIR(1:LOCDIR) ) THEN

C          WRITE (IOUTPT,'(1X,(A))')'Working in directory: '//
C     &        SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))

          CALL HCDIR( MCDIR(1:LMCDIR),' ')
          CALL HCDIR( NCDIR(1:LNCDIR),' ')

          OCDIR = NCDIR
          LOCDIR = LENOCC(OCDIR)

        ENDIF

        IF ( HEXIST(IDS(JSORT(IHIST))) ) THEN
          CALL HROUT (IDS(JSORT(IHIST)),ICYCLE,' ')
        ELSE
          WRITE (IOUTPT,'(1X,(A))')' '
          WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &        IDS(JSORT(IHIST)),
     &        ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &        LENOCC(SUBDIRS(JSORT(IHIST))))//' is missing'
        ENDIF

   12 CONTINUE
C
C ****  close the histogram file
C
      CALL HREND (LOGFID2)

      WRITE (IOUTPT,'(1X,(A))')' '
      WRITE (IOUTPT,'(1X,(A))')'Output file created'
      WRITE (IOUTPT,'(1X,(A))')' '

      CALL EZGET ('LHPRINT', LHPRINT,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Can not get print flag','F')
      ENDIF

      IF ( LHPRINT ) THEN

        DO 112 IHIST =  1,NIDS

          MCDIR = '//'//PAWMID//'/'//
     &      SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
          LMCDIR = LENOCC(MCDIR)

          IF( NCDIR(1:LNCDIR) .NE. OCDIR(1:LOCDIR) ) THEN

            WRITE (IOUTPT,'(1X,(A))')'Working in directory: '//
     &        SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))

            CALL HCDIR( MCDIR(1:LMCDIR),' ')

            OCDIR = NCDIR
            LOCDIR = LENOCC(OCDIR)

          ENDIF

          IF ( HEXIST(IDS(JSORT(IHIST))) ) THEN
            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A))')'Directory '//
     &        SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
            CALL HPRINT(IDS(JSORT(IHIST)))
          ENDIF

  112   CONTINUE

      ENDIF

      END
