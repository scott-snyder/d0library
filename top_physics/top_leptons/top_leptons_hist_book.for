      SUBROUTINE TOP_LEPTONS_HIST_BOOK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOP_LEPTONS Histogram Booking
C-
C-   Inputs  : 
C-             ELECTRON_HIST = true/false for electron diagnostic plots
C-             MUON_HIST     = true/false for muon diagnostic plots
C-             PHOTON_HIST   = true/false for electron diagnostic plots
C-             DIMUON_HIST   = true/false for dimuon diagnostic plots
C-             MUJET_HIST    = true/false for muon-jet diagnostic plots
C- 
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created  31-JUL-1992   Stephen J. Wimpenny
C-   Modified  3-Sep-1992   Electron diagnostic plots added and general
C-                          monitoring improved
C-   Modified 14-Sep-1992   diagnostics switched from RCP file
C-   Modified 12-Oct-1992   Vertex,trigger and muon isolation info added
C-   Modified 14-Dec-1992   Lepton-Jet Isolation plots added
C-   Modified 28-Dec-1992   Electron/Photon hists updated for new cuts
C-   Modified 31-Dec-1992   Muon plots for dE/dx/Radiation monitoring added
C-   Modified 28-Jan-1993   Muon-Jet diagnostics added
C-   Modified 17-Feb-1993   Mu-e histogram binning changed
C-   Modified  4-Mar-1993   New histograms added
C-   Modified 21-Apr-1993   Updates for RECO 11 + new mujet code
C-   Modified  6-May-1993   Main Ring timing + 3-body Mt added
C-   Updated  17-MAY-1993   Brajesh C Choudhary   Wgamma related histograms 
C-   Modified 15-Jul-1993   Mu-jet topological plots added
C-   Modified  4-Sep-1993   More event shape plots added
C-   Modifed  16-Sep-1993   More dimuon and jet plots added
C-   Modified  1-Apr-1994   1NN and hit cell muon mip plots added
C-   Modified 17-May-1994   Muon Monitoring Updated
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL ELECTRON_HIST,MUON_HIST,PHOTON_HIST
      LOGICAL DIMUON_HIST,MUJET_HIST
C
      INTEGER IER,IOFF_EL,IOFF_PH,IOFF_MU,IOFF_JT,IOFF_NU,IOFF_PHYS
      INTEGER IOFF_MJ
C
      DATA IOFF_EL,IOFF_PH,IOFF_MU,IOFF_JT/ 1000,2000,3000,4000/
      DATA IOFF_NU,IOFF_PHYS,IOFF_MJ/ 5000,6000,7000/
C
C *** Read Control Parameters from RCP file
C
      IER=0
C
C *** Get all latest parameter/Options Values
C
      CALL EZPICK('TOP_LEPTONS_RCP')
      CALL EZGET('PLOT_ELECTRON_HIST',ELECTRON_HIST,IER)
      IF(IER.EQ.0) CALL EZGET('PLOT_PHOTON_HIST',PHOTON_HIST,IER)
      IF(IER.EQ.0) CALL EZGET('PLOT_MUON_HIST',MUON_HIST,IER)
      IF(IER.EQ.0) CALL EZGET('PLOT_DIMUON_HIST',DIMUON_HIST,IER)
      IF(IER.EQ.0) CALL EZGET('PLOT_MUJET_HIST',MUJET_HIST,IER)
      IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_HIST_BOOK',' ','F')
      CALL EZRSET
C
C *** Define HBook Directory Path
C
      CALL DHDIR('TOP_LEPTONS_RCP','HBOOK_DIRECTORY',IER,' ')
C
C ---------------------------------------------------------------------
C
C *** General Event Monitoring Plots
C
      CALL HBOOK1(1,' Level 1 Trigger bits set $',
     1 33,0.,33.,0.)
      CALL HBOOK1(2,' Level 2 Filter bits set $',
     1 60,0.,60.,0.)   
      CALL HBOOK1(3,' Main Ring Event Timing (TIME29) $',
     1 25,0.,5.,0.)
      CALL HBOOK1(4,' TIME29 - Quad 1 w/MR $',25,0.,5.,0.)
      CALL HBOOK1(5,' TIME29 - Quad 2 w/MR $',25,0.,5.,0.)
      CALL HBOOK1(6,' TIME29 - Quad 3 w/MR $',25,0.,5.,0.)
      CALL HBOOK1(7,' TIME29 - Quad 4 w/MR $',25,0.,5.,0.)
      CALL HBOOK1(8,' TIME29 - EF w/MR $',25,0.,5.,0.)
      CALL HBOOK1(9,' TIME29 - no main ring $',25,0.,5.,0.)
C
      CALL HBOOK1(50,' Muon phi - no main ring $',
     1 72,0.,360.,0.) 
      CALL HBOOK1(55,' Muon phi - with main ring $',
     1 72,0.,360.,0.) 
      CALL HBOOK1(58,' Muon phi - with main ring TIME29 LT 1 sec$',
     1 72,0.,360.,0.) 
      CALL HBOOK1(59,' Muon phi - with main ring TIME29 GT 1 sec$',
     1 72,0.,360.,0.) 
C
      CALL HBOOK1(10,' Number of electrons per event$',
     1 5,0.,5.,0.)
      CALL HBOOK1(11,' Number of muons per event$',
     1 5,0.,5.,0.)
      CALL HBOOK1(12,' Number of photons per event$',
     1 5,0.,5.,0.)
      CALL HBOOK1(13,' Number of jets per event$',
     1 10,0.,10.,0.)
C
      CALL HBOOK1(20,' Number of primary vertices per event $',
     1 5,0.,5.,0.)
      CALL HBOOK1(21,' Number of secondary vertices per event $',
     1 5,0.,5.,0.)
      CALL HBOOK1(22,' Primary Vertex x position $',
     1 20,-1.,1.,0.)
      CALL HBOOK1(23,' Primary Vertex y position $',
     1 20,-1.,1.,0.)
      CALL HBOOK1(24,' Primary Vertex z position $',
     1 25,-100.,100.,0.)
      CALL HBOOK1(25,' dZ for 2 Vertex events $',
     1 25,-100.,100.,0.)
C
      CALL HBOOK1(30,' Number of Ztraks per event $',
     1 50,0.,100.,0.)
      CALL HBOOK1(31,' Ztrak multiplicty for 1st vertex $',
     1 50,0.,100.,0.)
      CALL HBOOK1(32,' Ztrak multiplicty for additional vertices $',
     1 50,0.,100.,0.)
C
      CALL HBOOK1(100,' Electron Et $',
     1 60,0.,120.,0.)
      CALL HBOOK1(101,' Electron eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(102,' Electron phi $',
     1 72,0.,360.,0.)
      CALL HBOOK1(106,' Electron detector eta $',
     1 70,-3.5,3.5,0.)
C
      CALL HBOOK1(110,' Muon Pt $',
     1 60,0.,120.,0.)
      CALL HBOOK1(111,' Muon eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(112,' Muon phi $',
     1 72,0.,360.,0.) 
      CALL HBOOK1(116,' Muon detector eta $',
     1 70,-3.5,3.5,0.)
C
      CALL HBOOK1(120,' Photon Et $',
     1 60,0.,120.,0.)
      CALL HBOOK1(121,' Photon eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(122,' Photon phi $',
     1 72,0.,360.,0.)
      CALL HBOOK1(126,' Photon detector eta $',
     1 70,-3.5,3.5,0.)
C
      CALL HBOOK1(130,' Jet Et $',
     1 50,0.,100.,0.)
      CALL HBOOK1(131,' Jet eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(132,' Jet phi $',
     1 72,0.,360.,0.)
      CALL HBOOK1(136,' Jet detector eta $',
     1 70,-3.5,3.5,0.)
C
      CALL HBOOK1(140,' Missing Et (PNUT1)$',
     1 50,0.,100.,0.)
      CALL HBOOK1(145,' Missing Et (PNUT2)$',
     1 50,0.,100.,0.)                   
      CALL HBOOK1(147,' Phi Missing Et (PNUT2)$',
     1 50,0.,360.,0.)
      CALL HBOOK1(150,' Missing Et (PNUT3)$',
     1 50,0.,100.,0.)
      CALL HBOOK1(152,' Phi Missing Et (PNUT3)$',
     1 50,0.,360.,0.)
      CALL HBOOK1(155,' Missing Et (PNUT4)$',
     1 50,0.,100.,0.)
      CALL HBOOK1(157,' Phi Missing Et (PNUT4)$',
     1 50,0.,360.,0.)
C
      CALL HBOOK1(180,' HT $',50,0.,200.,0.)
C
C *** recoil plots
C
      CALL HBOOK1(210,' Phi of recoil(jet) system $',
     1 60,0.,360.,0.)
      CALL HBOOK1(211,' dPhi (recoil system ,Etmiss) $',
     1 60,0.,180.,0.)
      CALL HBOOK1(212,' dPhi (recoil system ,Etmiss corr) $',
     1 60,0.,180.,0.)
      CALL HBOOK1(213,' Pt of recoil(jet) system $',
     1 60,0.,180.,0.)
      CALL HBOOK2(214,' Pt of recoil(jet) system vs Etmiss(2) $',
     1 50,0.,100.,50,0.,100.,0.)
      CALL HBOOK2(215,' Pt of recoil(jet) system vs Etmiss(3) $',
     1 50,0.,100.,50,0.,100.,0.)

C
C *** Physics Monitoring Plots
C
      CALL HBOOK1(IOFF_PHYS+10,' Mass (e-mu) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+11,' Mass (e-e) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+12,' Mass (mu-mu) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+14,' Mass (gamma-mu) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+15,' Mass (gamma-e) $',
     1 100,0.,200.,0.)
C
      CALL HBOOK1(IOFF_PHYS+20,' dPhi (e-mu) $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_PHYS+21,' dPhi (e-e)  $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_PHYS+22,' dPhi (mu-mu) $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_PHYS+24,' dPhi (gamma-mu) $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_PHYS+25,' dPhi (gamma-e) $',
     1 50,0.,180.,0.)
C
      CALL HBOOK2(IOFF_PHYS+30,' Etmiss vs dPhi (e-mu) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+31,' Etmiss vs dPhi (e-e) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+32,' Etmiss vs dPhi (mu-mu) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+34,' Etmiss vs dPhi (gamma-mu) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+35,' Etmiss vs dPhi (gamma-e) $',
     1 50,0.,100.,36,0.,180.,0.)
C
      CALL HBOOK2(IOFF_PHYS+40,' Pt(e) vs Pt(mu) $',
     1 60,0.,120.,60,0.,120.,0.)
      CALL HBOOK2(IOFF_PHYS+1040,' Pt(e) vs -1/Pt(mu) $',
     1 105,0.,210.,110,-0.085,0.,0.)
      CALL HBOOK2(IOFF_PHYS+41,' Pt(e1) vs Pt(e2) $',
     1 60,0.,120.,60,0.,120.,0.)
      CALL HBOOK2(IOFF_PHYS+42,' Pt(mu1) vs Pt(mu2) $',
     1 60,0.,120.,60,0.,120.,0.)
      CALL HBOOK2(IOFF_PHYS+44,' Pt(gamma) vs Pt(mu) $',
     1 60,0.,120.,60,0.,120.,0.)
      CALL HBOOK2(IOFF_PHYS+45,' Pt(gamma) vs Pt(e) $',
     1 60,0.,120.,60,0.,120.,0.)
C
      CALL HBOOK1(IOFF_PHYS+50,' Coplanarity (e-mu) $',
     1 20,0.,1.,0.)
      CALL HBOOK1(IOFF_PHYS+54,' Coplanarity (gamma-mu) $',
     1 20,0.,1.,0.)
C
      CALL HBOOK1(IOFF_PHYS+60,' dR (e-mu) $',
     1 20,0.,1.0,0.)
      CALL HBOOK1(IOFF_PHYS+64,' dR (gamma-mu) $',
     1 20,0.,1.0,0.)
C
      CALL HBOOK2(IOFF_PHYS+80,' Coplanarity vs dPhi e-mu $',
     1 20,0.,1.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+84,' Coplanarity vs dPhi gamma-mu $',
     1 20,0.,1.,36,0.,180.,0.)
C
      CALL HBOOK2(IOFF_PHYS+90,' Mass vs dPhi e-mu $',
     1 100,0.,300.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+92,' Mass vs dPhi mu1-mu2 $',
     1 100,0.,300.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+94,' Mass vs dPhi gamma-mu $',
     1 100,0.,300.,36,0.,180.,0.)
C
      CALL HBOOK2(IOFF_PHYS+110,' dRmin(mu-jet) vs Njets $',
     1 30,0.,1.5,8,0.,8.,0.)
      CALL HBOOK2(IOFF_PHYS+111,' dRmin(e-jet) vs Njets $',
     1 30,0.,1.5,8,0.,8.,0.)
      CALL HBOOK2(IOFF_PHYS+112,' dRmin(gamma-jet) vs Njets $',
     1 30,0.,1.5,8,0.,8.,0.)
C
      CALL HBOOK2(IOFF_PHYS+120,' Ptmin(mu,e) vs dPhi(mu-e) $',
     1 50,0.,100.,60,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+121,' Ptmin(e1,e2) vs dPhi(e1-e2) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+122,' Ptmin(mu1,mu2) vs dPhi(mu1-mu2) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+124,' Ptmin(mu,gamma) vs dPhi(mu-gamma) $',
     1 50,0.,100.,60,0.,180.,0.)
      CALL HBOOK2(IOFF_PHYS+125,' Ptmin(e,gamma) vs dPhi(e-gamma) $',
     1 50,0.,100.,60,0.,180.,0.)
C
      CALL HBOOK2(IOFF_PHYS+130,' Mass vs Etmiss mu-e $',
     1 100,0.,300.,50,0.,100.,0.)
      CALL HBOOK2(IOFF_PHYS+132,' Mass vs Etmiss mu1-mu2 $',
     1 100,0.,300.,50,0.,100.,0.)
      CALL HBOOK2(IOFF_PHYS+134,' Mass vs Etmiss mu-gam $',
     1 100,0.,300.,50,0.,100.,0.)
C
      CALL HBOOK1(IOFF_PHYS+140,' Mt (mu-e-Etmiss) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+141,' Mt (mu-gam-Etmiss) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+142,' Mt (e-e-Etmiss) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+143,' Mt (e-gam-Etmiss) $',
     1 100,0.,200.,0.)
C
C *** Wgamma related histograms.
C
      CALL HBOOK2(IOFF_PHYS+210,' Mt(mu-gam-Etmiss) vs dR(gam-mu) $',   
     1 100,0.,300.,25,0.,5.,0.)
      CALL HBOOK2(IOFF_PHYS+211,' Mt(mu-gam-Etmiss) vs Mt(mu-Etmiss) $',
     1  100,0.,300.,75,0.,150.,0.)
      CALL HBOOK2(IOFF_PHYS+212,' Mass(gamma-mu) vs dR(gam-mu) $',   
     1 75,0.,150.,25,0.,5.,0.)
C
      CALL HBOOK1(IOFF_PHYS+510,' Mt (e-Etmiss) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+511,' Mt (gamma-Etmiss) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_PHYS+512,' Mt (mu-Etmiss) $',
     1 100,0.,200.,0.)
C
C ***
C *** Electron Technical Quality Plots
C ***
C
      IF(.NOT.ELECTRON_HIST) GO TO 10
C
      CALL HBOOK1(IOFF_EL+10,
     1 ' Electron : No of ZTRAKS in cluster road $',
     2 10,0.,10.,0.)
      CALL HBOOK1(IOFF_EL+20,' Electron em fraction $',
     1 15,0.85,1.,0.)
      CALL HBOOK1(IOFF_EL+21,' Electron isolation $',
     1 25,0.,0.25,0.)
      CALL HBOOK1(IOFF_EL+30,' Electron CDC dE/dx $',
     1 50,0.,10.,0.)
      CALL HBOOK1(IOFF_EL+31,' Electron FDC dE/dx $',
     1 50,0.,10.,0.)
      CALL HBOOK1(IOFF_EL+32,' Electron VTX dE/dx $',
     1 50,0.,10.,0.)
      CALL HBOOK1(IOFF_EL+33,' Electron TRD signal $',
     1 50,0.,50.,0.)
      CALL HBOOK1(IOFF_EL+40,
     1 ' Electron HMatrix Chisq eta lt 2.5 $',
     2 50,0.,200.,0.)
      CALL HBOOK1(IOFF_EL+41,
     1 ' Electron HMatrix Chisq eta gt 2.5 $',
     2 50,0.,200.,0.)
      CALL HBOOK1(IOFF_EL+65,' Electron min dR to nearside jet $',
     1 30,0.,1.5,0.)
      CALL HBOOK1(IOFF_EL+67,' Electron max dPhi to awayside jet $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_EL+68,
     1 ' Elec max dPhi to awayside jet HChisq>100$',
     2 50,0.,180.,0.)
      CALL HBOOK1(IOFF_EL+69,
     1 ' Elec max dPhi to awayside jet HChisq<10 $',
     2 50,0.,180.,0.)
      CALL HBOOK1(IOFF_EL+70,' Electron Et of nearside jet $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_EL+71,' Electron Et of awayside Jet $',
     1 50,0.,100.,0.)
C
C ***
C *** Muon Technical Quality Plots
C ***
C
   10 IF(.NOT.MUON_HIST) GO TO 20
C
      CALL HBOOK1(IOFF_MU+10,' Muon Quality (IFW4) $',
     1 10,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+11,' Muon Bit Pattern - IFW2 $',
     1 25,-1.,24.,0.)
      CALL HBOOK1(IOFF_MU+12,' Muon Bit Pattern - IFW3 $',
     1 24,0.,24.,0.)
      CALL HBOOK1(IOFF_MU+13,' Muon Layer Flag (IFW1) $',
     1 20,0.,20.,0.)
C
      CALL HBOOK1(IOFF_MU+15,' Muon Quadrant No. $',
     1 20,0.,20.,0.)  
      CALL HBOOK1(IOFF_MU+16,' Integ B.dl used in p calculation $',
     1 20,0.,2.,0.)
      CALL HBOOK1(IOFF_MU+17,' Bend view quality of fit (cm,rms) $',
     1 20,0.,5.,0.)
      CALL HBOOK1(IOFF_MU+18,' Non-bend view quality of fit (cm,rms) $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+19,' Muon Global Trackfit Chisq $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MU+20,' Muon 3-D impact parameter wrt vertex$',
     1 60,0.,30.,0.)
      CALL HBOOK1(IOFF_MU+23,' Muon RZ impact parameter $',
     1 50,-50.,50.,0.)
      CALL HBOOK1(IOFF_MU+24,' Muon XY impact parameter $',
     1 50,-50.,50.,0.)
      CALL HBOOK1(IOFF_MU+25,' Floating T0 offset (nsec) $',
     1 100,-250.,250.,0.)
      CALL HBOOK1(IOFF_MU+26,' dPhi(max) muon Etmiss(3) $',
     1 36,0.,180.,0.)
      CALL HBOOK2(IOFF_MU+27,' dPhi(max) muon Etmiss(3) vs Etmiss(3) $',
     1 36,0.,180.,50,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MU+30,' Muon no associated ZTRAKS $',
     1 10,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+32,' Muon min Angle muon-ZTRAK$',
     1 30,0.,30.,0.)
      CALL HBOOK1(IOFF_MU+33,' Muon min dPhi muon-ZTRAK $',
     1 50,0.,.5,0.)
      CALL HBOOK1(IOFF_MU+34,' Muon min dTheta muon-ZTRAK $',
     1 50,0.,.5,0.)
      CALL HBOOK1(IOFF_MU+35,' min dR to closest ZTRAK $',
     1 20,0.,2.,0.)
C
      CALL HBOOK1(IOFF_MU+40,' Muon Expected Eloss in Calorimeter $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+41,' Muon Energy in hit cells $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+42,' Muon Energy in hit cells + 1NN $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+43,' Muon Energy in hit cells + 2NN $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+44,' Muon Energy from MTCA fit $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+46,' Oppsite energy in hit cells + 2NN $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+47,' Muon Expected Eloss in Iron $',
     1 20,0.,10.,0.)
C
      CALL HBOOK1(IOFF_MU+50,' Muon no WAMUS hits on track $',
     1 20,0.,20.,0.)        
      CALL HBOOK1(IOFF_MU+51,' Muon no SAMUS hits on track $',
     1 15,0.,15.,0.) 
      CALL HBOOK2(IOFF_MU+53,' Total hits on track vs eta $',
     1 25,0.,25.,70,-3.5,3.5,0.)
C
      CALL HBOOK1(IOFF_MU+65,' Muon min dR to nearside jet $',
     1 30,0.,1.5,0.)
      CALL HBOOK1(IOFF_MU+67,' Muon max dPhi to awayside jet $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_MU+70,' Muon Et of nearside jet $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MU+71,' Muon Et of awayside Jet $',
     1 50,0.,100.,0.)
C
C *** Radiation / dE/dx monitoring
C
      CALL HBOOK1(IOFF_MU+80,' Muon - E(0.4)-E(0.2) $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+81,' Muon - E(0.6)-E(0.2) $',
     1 20,0.,10.,0.)
C
      CALL HBOOK1(IOFF_MU+83,
     &  ' Muon - E(0.4)-E(0.2) CF eta < 0.5 $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+84,
     &  ' Muon - E(0.4)-E(0.2) CF 0.5 < eta < 1.0 $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+85,
     &  ' Muon - E(0.4)-E(0.2) EF $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+86,
     &  ' Muon - Et(0.4)-Et(0.2) CF eta < 0.5 $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+87,
     &  ' Muon - Et(0.4)-Et(0.2) CF 0.5 < eta < 1.0 $',
     1 20,0.,10.,0.)
      CALL HBOOK1(IOFF_MU+88,
     &  ' Muon - Et(0.4)-Et(0.2) EF $',
     1 20,0.,10.,0.)
C
   20 IF(.NOT.DIMUON_HIST) GO TO 25
C
C *** Dimuon study plots
C
      CALL HBOOK2(IOFF_MU+100,' Pt(mu1) vs Pt(mu2) all pairs $',
     1 50,0.,100.,50,0.,100.,0.)
      CALL HBOOK2(IOFF_MU+101,' Pt(mu1) vs Pt(mu2) +- only $',
     1 50,0.,100.,50,0.,100.,0.)
      CALL HBOOK2(IOFF_MU+102,' Pt(mu1) vs Pt(mu2) ++,-- only $',
     1 50,0.,100.,50,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MU+110,' dPhi (mu1mu2) $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_MU+111,' dPhi (+-) $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_MU+112,' dPhi (++,--) $',
     1 50,0.,180.,0.)
C
      CALL HBOOK2(IOFF_MU+120,' Etmiss vs dPhi (mu1mu2) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_MU+121,' Etmiss vs dPhi (+-) $',
     1 50,0.,100.,36,0.,180.,0.)
      CALL HBOOK2(IOFF_MU+122,' Etmiss vs dPhi (++,--) $',
     1 50,0.,100.,36,0.,180.,0.)
C
      CALL HBOOK1(IOFF_MU+130,' Mass (mu1mu2) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_MU+131,' Mass (+-) $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_MU+132,' Mass (++,--) $',
     1 100,0.,200.,0.)
C
      CALL HBOOK1(IOFF_MU+140,' Mass (mu1mu2) dPhi.lt.45 $',
     1 100,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MU+150,' Mass (mu1mu2) dPhi 45-135 $',
     1 100,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MU+160,' Mass (mu1mu2) dPhi.gt.135 $',
     1 100,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MU+170,' Pt (mumu) - from leptons $',
     1 100,0.,100.,0.)
      CALL HBOOK1(IOFF_MU+171,' Pt (mumu) - from leptons (+-) $',
     1 100,0.,100.,0.)
      CALL HBOOK1(IOFF_MU+172,' Pt (mumu) - from leptons (++,--) $',
     1 100,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MU+180,' Pt (mumu) - from Calorimeter $',
     1 100,0.,100.,0.)
      CALL HBOOK1(IOFF_MU+181,' Pt (mumu) - from Calo (+-) $',
     1 100,0.,100.,0.)
      CALL HBOOK1(IOFF_MU+182,' Pt (mumu) - from Calo (++,--) $',
     1 100,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MU+210,' Phi(mumu) $',
     1 60,0.,360.,0.)
      CALL HBOOK1(IOFF_MU+211,' dPhi(mumu) - Etmiss(2) $',
     1 60,0.,180.,0.)
      CALL HBOOK1(IOFF_MU+212,' dPhi(mumu) - Etmiss(3) $',
     1 60,0.,180.,0.)
C
   25 CONTINUE
      IF(.NOT.PHOTON_HIST) GO TO 30   
C
C ***
C *** Photon Technical Quality Plots
C ***
C
      CALL HBOOK1(IOFF_PH+20,' Photon em fraction $',
     1 15,0.85,1.,0.)
      CALL HBOOK1(IOFF_PH+21,' Photon Isolation $',
     1 25,0.,0.25,0.)
      CALL HBOOK1(IOFF_PH+40,
     1 ' Photon HMatrix Chisq eta lt 2.5 $',
     2 50,0.,200.,0.)
      CALL HBOOK1(IOFF_PH+41,
     1 ' Photon HMatrix Chisq eta gt 2.5 $',
     2 50,0.,200.,0.)
      CALL HBOOK1(IOFF_PH+65,' Photon min dR to nearside jet $',
     1 30,0.,1.5,0.)
      CALL HBOOK1(IOFF_PH+67,' Photon max dPhi to awayside jet $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_PH+68,
     1 ' Phot max dPhi to awayside jet HChisq>100$',
     2 50,0.,180.,0.)
      CALL HBOOK1(IOFF_PH+69,
     1 ' Phot max dPhi to awayside jet HChisq<10 $',
     2 50,0.,180.,0.)
      CALL HBOOK1(IOFF_PH+70,' Photon Et of nearside jet $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_PH+71,' Photon Et of awayside Jet $',
     1 50,0.,100.,0.)
   30 CONTINUE
C
C ***
C *** Lepton+Jets Physics Plots
C ***
C
      IF(.NOT.MUJET_HIST) GO TO 40
C
      CALL HBOOK1(IOFF_MJ+110,' Leading Muon Pt $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+111,' Leading Muon eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(IOFF_MJ+112,' Leading Muon phi $',
     1 72,0.,360.,0.)
C
      CALL HBOOK1(IOFF_MJ+120,' Second Muon Pt $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+121,' Second Muon eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(IOFF_MJ+122,' Second Muon phi $',
     1 72,0.,360.,0.)
C
      CALL HBOOK1(IOFF_MJ+130,' First Jet Et $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+131,' First Jet eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(IOFF_MJ+132,' First Jet phi $',
     1 72,0.,360.,0.)
C
      CALL HBOOK1(IOFF_MJ+140,' Second Jet Et $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+141,' Second Jet eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(IOFF_MJ+142,' Second Jet phi $',
     1 72,0.,360.,0.)
C
      CALL HBOOK1(IOFF_MJ+150,' Third Jet Et $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+151,' Third Jet eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(IOFF_MJ+152,' Third Jet phi $',
     1 72,0.,360.,0.)
C
      CALL HBOOK1(IOFF_MJ+160,' Fourth Jet Et $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+161,' Fourth Jet eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(IOFF_MJ+162,' Fourth Jet phi $',
     1 72,0.,360.,0.)
C
      CALL HBOOK1(IOFF_MJ+170,' Fifth Jet Et $',
     1 50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+171,' Fifth Jet eta $',
     1 70,-3.5,3.5,0.)
      CALL HBOOK1(IOFF_MJ+172,' Fifth Jet phi $',
     1 72,0.,360.,0.)
C
      CALL HBOOK1(IOFF_MJ+210,' Transverse Mass (mu1-nu) $',
     1 100,0.,200.,0.)
      CALL HBOOK2(IOFF_MJ+220,' ETMiss vs Pt(mu1) $',
     1 50,0.,100.,50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+221,' dPhi (mu1-ETmiss) $',
     1 50,0.,180.,0.)
      CALL HBOOK2(IOFF_MJ+230,' HT vs Pt(mu1) $',
     1 50,0.,100.,50,0.,400.,0.)
      CALL HBOOK2(IOFF_MJ+231,' ETMiss vs HT $',
     1 50,0.,400.,50,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MJ+240,' dPhi (mu1-Nearest Jet) $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_MJ+241,' dR (mu1-Nearest Jet) $',
     1 25,0.,2.5,0.)
      CALL HBOOK1(IOFF_MJ+250,' dPhi (mu1-mu2) $',
     1 50,0.,180.,0.)
      CALL HBOOK2(IOFF_MJ+251,' Pt(mu1) vs Pt(mu2) $',
     1 50,0.,100.,50,0.,100.,0.)
      CALL HBOOK1(IOFF_MJ+252,' Mass (mu1-mu2) $',
     1 100,0.,200.,0.)
C
      CALL HBOOK1(IOFF_MJ+260,' dPhi (mu2-Nearest Jet) $',
     1 50,0.,180.,0.)
      CALL HBOOK1(IOFF_MJ+261,' dR (mu2-Nearest Jet) $',
     1 25,0.,2.5,0.)
      CALL HBOOK2(IOFF_MJ+262,' Pt(mu2) vs Et Near Jet $',
     1 50,0.,100.,50,0.,100.,0.)
C
      CALL HBOOK1(IOFF_MJ+270,' Mjj Leading 2 jets $',
     1 100,0.,200.,0.)
      CALL HBOOK1(IOFF_MJ+271,' Mjj all 2 jet combinations $',
     1 100,0.,200.,0.)
C
C *** Topological quantities
C
      CALL HBOOK1(IOFF_MJ+310,' SPHERICITY $',50,0.,1.,0.)
      CALL HBOOK1(IOFF_MJ+311,' PLANARITY $',50,0.,1.,0.)
      CALL HBOOK2(IOFF_MJ+312,' SPHERICITY vs PLANARITY $',
     1 50,0.,1.,50,0.,1.,0.)
C
      CALL HBOOK1(IOFF_MJ+320,' G_SPHERICITY $',50,0.,1.,0.)
      CALL HBOOK1(IOFF_MJ+321,' G_PLANARITY $',50,0.,1.,0.)
      CALL HBOOK2(IOFF_MJ+322,' G_SPHERICITY vs G_PLANARITY $',
     1 50,0.,1.,50,0.,1.,0.)
      CALL HBOOK2(IOFF_MJ+323,' GY vs GS $',
     1 50,0.,1.,50,0.,1.,0.)
C
      CALL HBOOK1(IOFF_MJ+330,' EMAXSH $',50,0.,1.,0.)
      CALL HBOOK1(IOFF_MJ+331,' ETMAXSH $',50,0.,1.,0.)
      CALL HBOOK2(IOFF_MJ+332,' EMAXSH vs ETMAXSH $',
     1 50,0.,1.,50,0.,1.,0.)
C
      CALL HBOOK1(IOFF_MJ+340,' Fourjet HT/H $',50,0.,1.,0.)
C

   40 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
