      FUNCTION l2em_ntuple_book()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the ntuple
C-                         ntuple = USR$OUT:L2EM_'RUN_NUM'.NTUP
C-                         'RUN_NUM' = first run seen
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-SEP-1992   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER runno
      INCLUDE 'd0$params:l2em_ntuple.def'
      LOGICAL l2em_ntuple_book,first
      DATA first/.TRUE./
      LOGICAL l2em_ntuple_store
      INTEGER me                        ! user id
      INTEGER rzunit                    ! unit number for rz file
      CHARACTER*80 ntuple_file,logname,logtrans
      DATA logname/'USR$OUT'/
      CHARACTER*8 labels(NTUPLE_SIZE)
      INTEGER icycle,ier,istat,length
      INTEGER trnlnm                    ! logical name translator function
C
C ****  define labels for ntuple variables.
C
      DATA labels/
     &  'RUN_NUM ','L1_TOTET','L1_EMET ','TETA    ','TPHI    ',
     &  'IETA    ','IPHI    ','LYR     ','EMET    ','SUMEM   ',
     &  'EM1FRAC ','EM12FRAC','EM3FRAC ','EM4FRAC ','FH1FRAC ', 
     &  'SIGMA3  ','SIGMA5  ','SIG3_MID',
     &  'SH13    ','SH24    ','SH35    ','SH57    ',
     &  'CONE_R  ','FCONE_ET',
     &  'DETA    ','DPHI    ','NTRACK  ',
     &  'IFAILED ','PAR_SET ',
     &  'AETA    ','APHI    ',
     &  'XCLUS   ','YCLUS   ','ZCLUS   ',
     &  'EMET_ZC ','CUTBITS ','ZETA    ',
     &  'VTX_Z   ','VTX_FLAG','VTX_TYPE',
     &  'NCAND_PS','EVT_NUM ','WEIGHT  '/

C----------------------------------------------------------------------
      IF( first )THEN
        me = 333
        istat=trnlnm(logname,logtrans,length)         ! check that USR$OUT
        IF((istat.LT.0) .AND. ( length .GT. 0 ) )THEN ! is defined
          WRITE (ntuple_file,1000) runno()
        ELSE                                          ! else write to default
          WRITE (ntuple_file,2000) runno()            ! directory
        ENDIF
        CALL gtunit(me,rzunit,ier)
        CALL hropen(rzunit,'NTUPLE',ntuple_file,'N',1024,ier)
        IF( ier .NE. 0 )THEN
          CALL errmsg('HROPEN PROBLEM','L2EM_NTUPLE_BOOK',
     &                'Cannot open RZ file','F')
          RETURN
        ENDIF
        CALL hcdir('//PAWC',' ')
        CALL hcdir('//NTUPLE',' ')
        CALL
     &      hbookn(4410,'L2EM ntuple',NTUPLE_SIZE,
     &                    'NTUPLE',1024,labels)
        CALL hcdir('//PAWC',' ')
        first = .FALSE.
      ENDIF
      l2em_ntuple_book = .TRUE.
      RETURN
      ENTRY l2em_ntuple_store
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write ntuple file to disk
C-
C-   Inputs  : none
C-   Outputs : USR$OUT:L2EM_'RUN_NUM'.NTUP, 'RUN_NUM' = 1st run seen
C-   Controls: none
C-
C-   Created  10-SEP-1992   James T. McKinley
C-
C----------------------------------------------------------------------
      l2em_ntuple_store = .TRUE.
      CALL hcdir('//PAWC',' ')
      CALL hcdir('//NTUPLE',' ')
      CALL hrout(0,icycle,' ')
      CALL hrend('NTUPLE')
      close(rzunit)
      CALL rlunit(me,rzunit,ier)
      CALL intmsg(' Ntuple stored in '//ntuple_file)
      CALL hcdir('//PAWC',' ')
      RETURN
 1000 FORMAT ('USR$OUT:L2EM_',I7.7,'.NTUP')
 2000 FORMAT ('L2EM_',I7.7,'.NTUP')
      END
