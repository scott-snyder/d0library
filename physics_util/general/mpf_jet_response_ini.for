      FUNCTION mpf_jet_response_ini()

C----------------------------------------------------------------------
C-   Purpose and Methods :  book histograms and ntuples, open their output
C-       files and set directories for MPF_JET_RESPONSE package
C-
C-   Controls:  MPF_JET_RESPONSE_RCP
C-
C-   Created   Mar-25-1994   Bob Kehoe
C-   Updated   Apr-20-1995   Chip Stewart, Bob Kehoe -- CWNize
C-   Updated   Oct-18-1995   Bob Kehoe  -- change mpf include for IBM's
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:MPF_JETRES.INC'
      INTEGER k,ier,nstring,dpho_mode
      LOGICAL mpf_jet_response_ini,flag,mpf_jet_response_fin

C----------------------------------------------------------------------
      nevt = 0
      npass = 0

C-        OBTAIN HBOOK DIRECTORY, FILE NAME, AND NTUPLE TITLE
      CALL inrcp('MPF_JET_RESPONSE_RCP',ier)
      IF (ier.NE.0) CALL errmsg('inrcp failed','mpf_jet_response_ini',
     &    'error in inrcp','F')
      CALL ezpick('MPF_JET_RESPONSE_RCP')
      CALL ez_get_chars('ntup_file',nstring,ntup_file,ier)
      IF (ier.NE.0) CALL errmsg('ez_get_chars error',
     &        'mpf_jet_response_ini','ntup_file','F')
      CALL ez_get_chars('title',nstring,title,ier)
      IF (ier.NE.0) CALL errmsg('ez_get_chars error',
     &        'mpf_jet_response_ini','title','F')
      CALL ez_get_chars('HBOOK_DIRECTORY',nstring,tdir,ier)
      IF (ier.NE.0) CALL errmsg('ez_get_chars error',
     &        'mpf_jet_response_ini','tdir','F')
      if (ier.eq.0) call ezget('dpho_mode',dpho_mode,ier)
      CALL ezrset

C-        SET NEW DIRECTORY IN PAWC AND BOOK DIAGNOSTIC AND ANALYSIS PLOTS
      filedir = '//'//tdir
      pawcdir = '//PAWC/'//tdir
      CALL gtunit(iuser,unit,ier)
      IF (ier.NE.0) CALL errmsg('gtunit failed',
     &    'mpf_jet_response_ini',' ','W')
      CALL hropen(unit,tdir,ntup_file,'N',1024,ier)
      IF (ier.NE.0) CALL errmsg('hropen failed',
     &    'mpf_jet_response_ini',' ','W')
      CALL hmdir(pawcdir,'S')
      CALL hcdir(filedir,'S')
C      CALL dhshow
C-
      CALL hbnt(nt_id,title,' ')
      CALL hbname(nt_id,'event',runnum,mpf_event_chform)
      CALL hbname(nt_id,'global',et_prtn,mpf_global_chform)
      CALL hbname(nt_id,'em_vec',n_emc,mpf_emvector_chform)
      CALL hbname(nt_id,'em_iso',nele2,mpf_emiso_chform)
      CALL hbname(nt_id,'em_qual',nele3,mpf_emquality_chform)
      CALL hbname(nt_id,'jet_algo',algo,mpf_jetalgo_chform)
      CALL hbname(nt_id,'jetinfo7',nj7,mpf_jetinfo7)
      CALL hbname(nt_id,'jetinfo5',nj5,mpf_jetinfo5)
      CALL hbname(nt_id,'jetinfo3',nj3,mpf_jetinfo3)
      CALL hbname(nt_id,'jetinfon',njn,mpf_jetinfon)
      CALL hbname(nt_id,'jetinfo1',nj1,mpf_jetinfo10)
      CALL hbname(nt_id,'jetinfot',njt,mpf_jetinfot)
      CALL hbook1(100,'parton et',100,0.,100.,0.)
      CALL hbook1(101,'photon et',100,0.,100.,0.)
      CALL hbook1(102,'jet et',100,0.,100.,0.)
      CALL hbook1(103,'kt',100,0.,100.,0.)
      CALL hbook1(104,'soft et',100,0.,100.,0.)
      CALL hbook1(105,'missing et',100,0.,100.,0.)
      CALL hbook1(111,'photon phi',100,0.,6.4,0.)
      CALL hbook1(112,'jet phi',100,0.,6.4,0.)
      CALL hbook1(113,'kt phi',100,0.,6.4,0.)
      CALL hbook1(114,'soft et phi',100,0.,6.4,0.)
      CALL hbook1(115,'missing et phi',100,0.,6.4,0.)
      CALL hbook1(120,'delta phi (photon - jet)',100,0.,3.2,0.)
      CALL hbook1(121,'jet parton energy - eprime',100,-30.,30.,0.)
      CALL hbook1(122,'jet eta',100,-4.,4.,0.)
      CALL hbprof(202,'mpf_old vs et jet ',nbin_et,0.,300.,-1.5,1.5,' ')
      CALL hbprof(203,'mpf_old vs et gamma ',nbin_et,0.,300.,-1.5,
     &        1.5,' ')
      CALL hbprof(205,'mpf vs et gamma ',nbin_et,0.,300.,-1.5,1.5,' ')
      CALL hbook1(210,' missing Etx ',100,-50.,50.,0.)
      CALL hbook1(211,' missing Ety ',100,-50.,50.,0.)
      CALL hbprof(212,'Rj vs et jet ',nbin_et,0.,300.,0.0,2.0,' ')
      CALL hbprof(213,'Rj vs et gamma ',nbin_et,0.,300.,0.0,2.0,' ')
      CALL hbook1(215,' Delta phi (jet-photon) ',31,0.,3.1416,0.)
      CALL hbook1(300,' pass rates ',15,0.,15.,0.)
      CALL hbprof(500,'Recoil Response vs E?T! of Photon',nbin_et,0.,
     &      500.,-5.0,5.0,' ')
      CALL hbprof(501,'Recoil Response vs Photon E?T!',nbin_et,0.,
     &      float(nbin_et),-5.0,5.0,' ')
      DO k = 1,num_algo
        CALL hbprof(510+k,'Jet Response vs Photon E?T!',
     &      nbin_et,0.,float(nbin_et),-5.0,5.0,' ')
        CALL hbprof(560+k,'E?T! of Jet vs Photon E?T!',
     &      nbin_et,0.,float(nbin_et),0.,900.0,' ')
        CALL hbprof(520+k,'Jet Response vs Eprime',nmax_e,
     &      0.,float(2*nmax_e),-5.0,5.0,' ')
        CALL hbprof(570+k,'E of Jet vs Eprime',nmax_e,0.,
     &      float(2*nmax_e),0.,900.0,' ')
        CALL hbprof(470+k,'E?T! of Photon vs Eprime',
     &      nmax_e,0.,float(2*nmax_e),0.,900.0,' ')
        CALL hbprof(530+k,'EC Jet Response vs Eprime',nmax_e,
     &      0.,float(2*nmax_e),-5.0,5.0,' ')
        CALL hbprof(580+k,'E of EC Jet vs Eprime',nmax_e,0.,
     &      float(2*nmax_e),0.,900.0,' ')
        CALL hbprof(480+k,'E?T! of Photon vs Eprime (EC Jets)',
     &      nmax_e,0.,float(2*nmax_e),0.,900.0,' ')
      ENDDO

      if (dpho_mode.eq.4) then
        call mpf_sim
        PRINT *,'    ...finishing simulator run'
        flag = mpf_jet_response_fin()
      endif
      CALL hcdir('//PAWC',' ')
      mpf_jet_response_ini = .true.

  999 RETURN
      END
