      PROGRAM D0HCMP_VERI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to compare histograms in two files
C-
C-   Inputs  : RCP file
C-   Outputs :
C-   Controls:
C-   CALLS   : SRCP and HBOOK routines plus
C-             D0HCMP_GET_HPAR
C-             D0HCMP_CRE_PAWDIRS
C-
C-   Created  21-MAR-1992   Krzysztof L. Genser
C-   Updated  31-AUG-1992   Krzysztof L. Genser
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PAWC.INC/LIST'
      INCLUDE 'D0$PARAMS:D0HCMP.DEF/LIST'

      CHARACTER*255 C_RFILE
      INTEGER N_RFILE

      CHARACTER*80 CHTITL1
      INTEGER NX1,NY1,LOC1,NWT1
      REAL    XMI1,XMA1,YMI1,YMA1

      CHARACTER*80 CHTITL2
      INTEGER NX2,NY2,LOC2,NWT2
      REAL    XMI2,XMA2,YMI2,YMA2

      DATA LHPRINT /.FALSE./

      LOGICAL LDHPRINT

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
      CALL INRCP ('D0HCMP_VERI_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG
     &      ('NO_RCP_FILE','D0HCMP_VERI',
     &      'Problem accessing D0HCMP_VERI_RCP','F')
      ENDIF
C
C ****  get directories, histogram ids and confidence levels
C
      CALL D0HCMP_GET_HPAR('HISTOGRAMS_TO_COMPARE',SUBDIRS,
     &                      IDS,CLEV,NIDS,IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG ('BAD_RCP_FILE','D0HCMP_VERI',
     &    ' Inconsistent HISTOGRAMS_TO_COMPARE in D0HCMP_VERI_RCP','F')
      ENDIF
C
      WRITE (IOUTPT,'(1X,(A))')' '
      WRITE (IOUTPT,'(1X,(A))')
     &  'The following histograms will be verified:'
      WRITE (IOUTPT,'(1X,(A))')' '
C
      DO 10 IHIST =  1,NIDS
        WRITE (IOUTPT,'(1X,A40,I8,F10.4)')
     &    SUBDIRS(IHIST)(1:40),
     &    IDS(IHIST),CLEV(IHIST)
   10 CONTINUE
C
C ****  sort the directories and create the directory structure in the memory
C
      CALL D0HCMP_CRE_PAWDIRS(SUBDIRS,PAWMID,NIDS,JSORT,IER)
C
C ****  gain access to the master paw rz disk file
C
      CALL GTUNIT(IUSER,IUNIT1,IER)
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_VERI','D0HCMP_VERI',
     &      'Can not get IUNIT1','F')
      ENDIF

      CALL EZGET ('LHPRINT', LHPRINT,IER)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_ADD','D0HCMP_ADD',
     &      'Can not get print flag','W')
      ENDIF

      CALL EZGETS ( 'MASTER_HISTOGRAM_FILE',1,C_RFILE,N_RFILE,IER)

      WRITE (IOUTPT,'(1X,(A))')' '
      WRITE (IOUTPT,'(1X,(A))')'Working on file:'
      WRITE (IOUTPT,'(1X,(A))') C_RFILE(1:N_RFILE)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_VERI','D0HCMP_VERI',
     &      'Could not get histogram input file name','F')
      ENDIF

      CALL HROPEN(IUNIT1,LOGFID1,C_RFILE(1:N_RFILE),' ',1024,IER)

      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_VERI','D0HCMP_VERI',
     &      'Could not open histogram input file ','F')
      ENDIF

C
C ****  file opened, read histograms in
C
      OCDIR = ' '
      LOCDIR = 0

      DO 11 IHIST =  1,NIDS

        NCDIR = '//'//LOGFID1//'/'//
     &    SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
        LNCDIR = LENOCC(NCDIR)

        MCDIR = '//'//PAWMID//'/'//
     &    SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))
        LMCDIR = LENOCC(MCDIR)

        IF( NCDIR(1:LNCDIR) .NE. OCDIR(1:LOCDIR) ) THEN

C          WRITE (IOUTPT,'(1X,(A))'),'Working in directory: '//
C     &        SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST))))

          CALL HCDIR( MCDIR(1:LMCDIR),' ')
          CALL HCDIR( NCDIR(1:LNCDIR),' ')

          OCDIR = NCDIR
          LOCDIR = LENOCC(OCDIR)

        ENDIF

        CALL HRIN(IDS(JSORT(IHIST)),9999,0)

        IF ( .NOT.HEXIST(IDS(JSORT(IHIST)))) THEN
          WRITE (IOUTPT,'(1X,(A))')' '
          WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram from directory '//
     &      SUBDIRS(JSORT(IHIST))(1:LENOCC(SUBDIRS(JSORT(IHIST)))),
     &      IDS(JSORT(IHIST)),' is missing'
        ENDIF

   11 CONTINUE
C
C ****  master file read close it.
C
      CALL HREND (LOGFID1)
C
C ****  get the file to be verified
C
      CALL GTUNIT(IUSER,IUNIT2,IER)
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_VERI','D0HCMP_VERI',
     &      'Can not get IUNIT2','F')
      ENDIF

      CALL EZGETS ( 'HISTOGRAM_FILE',1,C_RFILE,N_RFILE,IER)

      WRITE (IOUTPT,'(1X,(A))')' '
      WRITE (IOUTPT,'(1X,(A))')'Working on file:'
      WRITE (IOUTPT,'(1X,(A))') C_RFILE(1:N_RFILE)

      IF (EZERR(IER)) THEN
        CALL ERRMSG('D0HCMP_VERI','D0HCMP_VERI',
     &      'Could not get histogram input file name','F')
      ENDIF

      CALL HROPEN(IUNIT2,LOGFID2,C_RFILE(1:N_RFILE),' ',1024,IER)

      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('D0HCMP_VERI','D0HCMP_VERI',
     &      'Could not open histogram input file ','F')
      ENDIF

C
C ****  get the requested histograms
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

          CALL HCDIR( MCDIR(1:LMCDIR),' ')
          CALL HCDIR( NCDIR(1:LNCDIR),' ')

          OCDIR = NCDIR
          LOCDIR = LENOCC(OCDIR)

        ENDIF

        CALL HRIN(IDS(JSORT(IHIST)),9999,IHOFFSET1)

        IF ( .NOT.HEXIST(IDS(JSORT(IHIST))+IHOFFSET1) ) THEN

          WRITE (IOUTPT,'(1X,(A))')' '
          WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' is missing'

        ELSEIF ( HEXIST(IDS(JSORT(IHIST))) ) THEN


          CALL HGIVE(IDS(JSORT(IHIST)),
     &          CHTITL1,NX1,XMI1,XMA1,NY1,YMI1,YMA1,NWT1,LOC1)
C
          CALL HGIVE(IDS(JSORT(IHIST))+IHOFFSET1,
     &          CHTITL2,NX2,XMI2,XMA2,NY2,YMI2,YMA2,NWT2,LOC2)
C
          LDHPRINT=.FALSE.

          IF     ( CHTITL1(1:LENOCC(CHTITL1)).NE.
     &               CHTITL2(1:LENOCC(CHTITL2)) ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' has different title'
            LDHPRINT=.TRUE.

          ELSEIF ( NX1.NE.NX2 ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' has different NX'
            LDHPRINT=.TRUE.

          ELSEIF ( XMI1.NE.XMI2 ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' has different XMI'
            LDHPRINT=.TRUE.

          ELSEIF ( XMA1.NE.XMA2 ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' has different XMA'
            LDHPRINT=.TRUE.

          ELSEIF ( NY1.NE.NY2 ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' has different NY'
            LDHPRINT=.TRUE.

          ELSEIF ( YMI1.NE.YMI2 ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' has different YMI'
            LDHPRINT=.TRUE.

          ELSEIF ( YMA1.NE.YMA2 ) THEN

            WRITE (IOUTPT,'(1X,(A))')' '
            WRITE (IOUTPT,'(1X,(A),I8,(A))')'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))//' has different YMA'

          ELSE
C
C ****  verify histogram
C
            CALL D0VERI(IDS(JSORT(IHIST)),IDS(JSORT(IHIST))+IHOFFSET1,1,
     &        ' ',PROB,IER)

            IF (PROB.LT.CLEV(JSORT(IHIST))) THEN

              WRITE (IOUTPT,'(1X,(A))')' '
              WRITE (IOUTPT,'(1X,(A),I8,(A))') 'Histogram ',
     &          IDS(JSORT(IHIST)),
     &          ' from directory '//SUBDIRS(JSORT(IHIST))(1:
     &          LENOCC(SUBDIRS(JSORT(IHIST))))
              WRITE (IOUTPT,'(1X,(A))')
     &          'has changed signifficantly please check'
              WRITE (IOUTPT,'(1X,(A),F8.5,(A),F8.5)')
     &          'The resulting probability ',PROB,
     &          ' is smaler than the confidence level ',
     &          CLEV(JSORT(IHIST))


              IF ( LHPRINT ) THEN

                CALL HPRINT(IDS(JSORT(IHIST)))
                CALL HPRINT(IDS(JSORT(IHIST))+IHOFFSET1)

              ENDIF

            ENDIF

          ENDIF

          IF (LDHPRINT) THEN

            WRITE (6,'('' '')')
            WRITE (6,'('' The reference histogram parameters:'')')
            WRITE (6,'(1X,''The ID:                 '',I10)')
     &        IDS(JSORT(IHIST))
            WRITE (6,'(1X,''Titled: ''/1X,(A))')
     &        CHTITL1(1:LENOCC(CHTITL1))
            WRITE (6,'(1X,''With x axis parameters: '',I10,2E14.6)')
     &        NX1,XMI1,XMA1
            WRITE (6,'(1X,''With y axis parameters: '',I10,2E14.6)')
     &        NY1,YMI1,YMA1

            WRITE (6,'('' '')')
            WRITE (6,'('' The verified histogram parameters:'')')
            WRITE (6,'(1X,''The ID:                 '',I10)')
     &        IDS(JSORT(IHIST))+IHOFFSET1
            WRITE (6,'(1X,''Titled: ''/1X,(A))')
     &        CHTITL2(1:LENOCC(CHTITL2))
            WRITE (6,'(1X,''With x axis parameters: '',I10,2E14.6)')
     &        NX2,XMI2,XMA2
            WRITE (6,'(1X,''With y axis parameters: '',I10,2E14.6)')
     &        NY2,YMI2,YMA2

          ENDIF

        ENDIF

   12 CONTINUE

      CALL HREND (LOGFID2)

      END
