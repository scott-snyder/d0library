      FUNCTION TB90L2_NTUPLE_BOOK ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the ntuple
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER runno
      INCLUDE 'd0$params:tb90l2_ntuple.def'
      LOGICAL tb90l2_ntuple_book
      LOGICAL tb90l2_ntuple_store
      INTEGER me                        ! user id
      INTEGER rzunit                    ! unit number for rz file
      CHARACTER*25 ntuple_file
      CHARACTER*8 labels(NTUPLE_SIZE)
      INTEGER icycle, ier
      LOGICAL STORE
C
C ****  define labels for ntuple variables.
C
      DATA labels/'TAG_WORD',           ! tagwords
     &  'MIP_TDC ','MUON_TDC','CKV2_TDC','HALO_TDC','TOF1_TDC', ! reg tdc
     &  'TOF2_TDC',
     &  'LT_PRT  ','PEAK    ','BASE    ','ER_PRT  ','LT_VETO ', ! long
     &  'ER_VETO ','LDTDC7  ','PWC_GATE',     ! delay tdcs
     &  'EMC1    ','EMC2    ','EMC3    ','TOF1_ADC','TOF2_ADC', ! adc
     &  'ADC5    ','ADC6    ','MU_ADC  ','MIP_ADC ','CKV9_ADC',
     &  'CKVA_ADC','CKV2_ADC','B_GAUSS ',       ! end of camac (34 words)
     &  'BEAM_MOM','NORM_MOM',
     &  'XCP_Y   ','SLP_Y   ','CHI2_Y',
     &  'XCP_DX  ','SLP_DX  ','CHI2_DX',
     &  'XCP_UX  ','SLP_UX  ',
     &  'NY      ','NDX     ','NUX     ',!end fit
     &  'NCRYOX  ','NCRYOY  ',
     &  'CHANS   ','ENER_TOT','EM_TOT  ','FH_TOT  ',
     &  'CH_TOT  ','MH_TOT  ','OH_TOT  ',
     &  'EM1     ','EM1E    ','EM1P    ',       ! em layer 1 energy,eta,phi
     &  'EM2     ','EM2E    ','EM2P    ',       ! em layer 2 energy,eta,phi
     &  'EM3     ','EM3E    ','EM3P    ',       ! em layer 3 energy,eta,phi
     &  'EM4     ','EM4E    ','EM4P    ',       ! em layer 4 energy,eta,phi
     &  'FH1     ','FH1E    ','FH1P    ',       ! fh layer 1 energy,eta,phi
     &  'FH2     ','FH2E    ','FH2P    ',       ! fh layer 2 energy,eta,phi
     &  'FH3     ','FH3E    ','FH3P    ',       ! fh layer 3 energy,eta,phi
     &  'CH      ','CH_ETA  ','CH_PHI  ',       ! ch         energy,eta,phi
     &  'MH1     ','MH1E    ','MH1P    ',       ! mh layer 1 energy,eta,phi
     &  'MH2     ','MH2E    ','MH2P    ',       ! mh layer 2 energy,eta,phi
     &  'MH3     ','MH3E    ','MH3P    ',       ! mh layer 3 energy,eta,phi
     &  'MH4     ','MH4E    ','MH4P    ',       ! mh layer 4 energy,eta,phi
     &  'MH5     ','MH5E    ','MH5P    ',       ! mh layer 5 energy,eta,phi
     &  'OH1     ','OH1E    ','OH1P    ',       ! oh layer 1 energy,eta,phi
     &  'OH2     ','OH2E    ','OH2P    ',       ! oh layer 2 energy,eta,phi
     &  'OH3     ','OH3E    ','OH3P    ',       ! oh layer 3 energy,eta,phi
     &  'ICD     ','ICDE    ','ICDP    ',       ! ICD        energy,eta,phi
     &  'CCMG    ','CCMG_ETA','CCMGP   ',       ! ccmg       energy,eta,phi
     &  'ECMG    ','ECMG_ETA','ECMGP   ',       ! ecmg       energy,eta,phi
     &  'ICDMG   ','ICDMGE  ','ICDMGP  '/       ! icd mg     energy,eta,phi
C----------------------------------------------------------------------
      me = 134
      WRITE (ntuple_file,1000) runno()
      CALL gtunit(me,rzunit,ier)
      CALL hropen(rzunit,'NTUPLE',ntuple_file,'N',1024,ier)
      CALL hcdir('//PAWC',' ')
      CALL hcdir('//NTUPLE',' ')
      CALL
     &    hbookn(1000,'CAMAC and CALOR ntuple',NTUPLE_SIZE,
     &                  'NTUPLE',1024,labels)
      TB90L2_NTUPLE_BOOK  = .TRUE.
      STORE = .FALSE.
      RETURN
      ENTRY TB90L2_NTUPLE_STORE ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write ntuple to file to disk
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-JUN-1991   James Richardson
C-   Updated   8-JAN-1992   Chip Stewart   - added hcdir to //PAWC
C-
C----------------------------------------------------------------------
      IF( STORE ) GOTO 999
      STORE = .TRUE.
      tb90l2_ntuple_store = .true.
      CALL hcdir('//PAWC',' ')
      CALL hcdir('//NTUPLE',' ')
      CALL hrout(0,icycle,' ')
      CALL hrend('NTUPLE')
      close(rzunit)
      CALL rlunit(me,rzunit,ier)
      CALL intmsg(' Ntuple stored in '//ntuple_file)
      CALL hdelet(1000)                 ! dele ntuple for next run
  999 RETURN
 1000 FORMAT ('usr$out:run',i7.7,'.ntup')
      END
