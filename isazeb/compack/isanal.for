      LOGICAL FUNCTION ISANAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Sample analysis subroutine for ISAJET events in
C-       D0 Zebra format. Calls GTISxx utilities to fetch information
C-       from any bank. User should tailor it to his analysis.
C-       If speed is a concern it is faster to access the Zebra banks
C-       directly than using the GTISxx utilities.
C-
C-    IF not generated previously it will generate trivial
C-    calorimeter data (no depth, no shower generation) by:         
C-
C-     CALL ISMEAR  (introduces resolution smearing)
C-    
C-     then finds jets with a simplified version of the UA1 jet  
C-     algorithm with jet radius rjet and minimum scalar transverse
C-     energy ejcut                                                
C-            (RJET=1., EJCUT=5. for UA1)                               
C-     need to supply also ECCUT= min. ET for cell to be added to jet 
C-     and  ETSTOP= min. ET in a cell to start a jet:              
C-                                                                 
C-     CALL ISZJET(RJET,EJCUT,ECCUT,ETSTOP,NJMAX,NCJET)            
C-                                                                 
C-    ENTRY ISASTA: give status report
C-
C-    ENTRY ISADIA: user dialog to change defaults in ISANAL
C-
C-   Created   6-NOV-1988   Serban D. Protopopescu
C-   Updated  18-MAY-1994   Serban Protopopescu  simplified
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NJMAX
      PARAMETER (NJMAX=50)    ! maximum number of jets allowed
      INTEGER NCJET           ! no. of jets
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      CHARACTER*64 MESG
      INTEGER ID,IDV
      INTEGER LISAC,LISCL,LISAE,LISP1,LISV1,LISJT,LISAQ
      INTEGER GZISAC,LGEAN,LPJET,NPJETS
      INTEGER NANAL,IDABS,NCELLS,NCELLE,NCELLH,NJETS
      REAL AN,ETMAX,PTSQ,PTSQMX,PT,P(4),PTMAX,WEIGHT
      REAL AEM,BEM,CEM,AHAD,BHAD,CHAD,R1,R2
      REAL ET,ETE,ETH,EEM,EHAD,EJT,ABSETA,ETA,PHI,TH
      REAL RJET,EJCUT,ECCUT,ETSTOP,X,Y,Z,CHARGE,MASS
      REAL SNTH,CSTH,SNPHI,CSPHI,ETEMX,ETHMX,MISEX,MISEY,MISET
      LOGICAL FIRST,ISASTA,ISADIA,SMEAR,YES
C----------------------------------------------------------------------
C   
C          INITIALIZE   
C
      DATA NANAL/0/
      DATA FIRST/.TRUE./
      DATA SMEAR/.TRUE./
      IF(FIRST) THEN
C   
C          SET UP HISTOGRAMS    
C   
        CALL HCDIR('//PAWC',' ')    ! go to top directory
        CALL HMDIR('ISANALYZE','S')    ! create ISANAL directory
        CALL HBOOK1(1,'Charged lep Pt$',50,1.0,51.,0.)
        CALL HBOOK1(2,'Charged Particles Max. Pt$',50,0.,100.,0.)
        CALL HBOOK1(3,'ET MAX PJET$',40,0.,200.,0.)
        CALL HBOOK1(4,'ET MAX CELL$',50,0.,100.,0.)
        CALL HBOOK1(5,'ET EM MAX CELL$',50,0.,100.,0.)
        CALL HBOOK1(6,'ET HAD MAX CELL$',50,0.,100.,0.)
        CALL HBOOK1(7,'Missing ET $',50,0.,100.,0.)
        CALL HBOOK1(8,'ET MAX JET$',50,0.,200.,0.)
        CALL HBOOK1(9,'Charged Particles eta$',60,-3.0,3.0,0.)
        FIRST=.FALSE.
      ENDIF
C
C                ANALYZE EVENTS
C
      IF(LHEAD.NE.0) THEN
        CALL HCDIR('//PAWC/ISANALYZE',' ')  ! go to ISANALYZE directory
        NANAL=NANAL+1
        ISANAL=.TRUE.
        LISAE=LQ(LHEAD-IZISAE)
        IF(LISAE.EQ.0) CALL d0_ABORT(' Not ISAJET data')
        WEIGHT=Q(LISAE+12)*1000.      ! in nanobarn
        NPJETS=IQ(LISAE+7)
C
C        Loop over vertices and particles
C        and find particle with highest PT
C
        PTSQMX=0
        LISV1=0
C
  100   CALL GTISV1(LISV1,LISV1,IDV,P,X,Y,Z) ! loop over vertices
        IF(LISV1.GT.0) THEN 
          LISP1=LISV1-IZISP1
C
  200     CALL GTISP1(LISP1,LISP1,ID,P,PHI,TH,ETA) ! loop over particles
          IF(LISP1.GT.0) THEN 
            PTSQ=P(1)**2+P(2)**2
            IF(CHARGE(ID).NE.0) THEN
              CALL HFILL(9,ETA,0.,WEIGHT)
              IF(PTSQ.GT.PTSQMX) PTSQMX=PTSQ
            ENDIF
            IDABS=IABS(ID)
            IF(IDABS.EQ.12.OR.IDABS.EQ.14) THEN ! check for muons and electrons
              PT=SQRT(PTSQ)
              CALL HFILL(1,PT,0.,WEIGHT)
            ENDIF
C
            GOTO 200
          ENDIF
C
          GOTO 100
        ENDIF
        PTMAX=SQRT(PTSQMX)
        CALL HFILL(2,PTMAX,0.,WEIGHT) ! charged particle maximum momentum
C
C         find PJET (jets made from partons) with max. PT
        LPJET=0
  300   CALL GTPJET(LPJET,LPJET,ID,P,MASS,PHI,TH,ETA) ! loop over PJET
        IF(LPJET.GT.0) THEN 
          PTSQ=P(1)**2+P(2)**2
          IF(PTSQ.GT.PTSQMX) THEN
            PTSQMX=PTSQ
          ENDIF
C
          GOTO 300
        ENDIF
        PTMAX=SQRT(PTSQMX)
        CALL HFILL(3,PTMAX,0.,WEIGHT)
C   
C          Analyze pseudo-calorimeter data.
C
        IF ( SMEAR ) THEN
          CALL ISMEAR  ! generate smeared pseudo calorimeter data
        ELSE
          CALL ISACFL  ! generate unsmeared   "     "         "
        ENDIF
C
        MISEX=0
        MISEY=0
        ETMAX=0                                                         
        ETEMX=0
        ETHMX=0
        NCELLS=0
        NCELLE=0
        NCELLH=0
        LISCL=0
  400   CALL GTISCL(LISCL,LISCL,EEM,EHAD,SNTH,CSTH,CSPHI,SNPHI,
     &      PHI,ETA)               !  Loop over all non-zero cells
        IF(LISCL.GT.0.) THEN  
          ETE=EEM*SNTH
          ETH=EHAD*SNTH
          IF((ETE+ETH).GT.ETMAX) ETMAX=ETE+ETH
          IF(ETE.GT.ETEMX) ETEMX=ETE
          IF(ETH.GT.ETHMX) ETHMX=ETH
          NCELLS=NCELLS+1
          IF(EEM.GT.0.) NCELLE=NCELLE+1
          IF(EHAD.GT.0) NCELLH=NCELLH+1
C           calculate missing Et components
          MISEX=MISEX+(EEM+EHAD)*SNTH*CSPHI
          MISEY=MISEY+(EEM+EHAD)*SNTH*SNPHI
          GOTO 400
        ENDIF
        CALL HFILL(4,ETMAX,0.,WEIGHT)
        CALL HFILL(5,ETEMX,0.,WEIGHT)
        CALL HFILL(6,ETHMX,0.,WEIGHT)
        MISET=SQRT(MISEX**2+MISEY**2)
        CALL HFILL(7,MISET,0.,WEIGHT)
C
C           find jets from pseudo-calorimeter data
C
        DATA RJET,EJCUT,ECCUT,ETSTOP/0.7,5.0,.5,1.0/
        CALL ISZJET(RJET,EJCUT,ECCUT,ETSTOP,NJMAX,NCJET)
C
        AN=NCJET
        LISAE=LQ(LHEAD-IZISAE)
        CALL HFILL(2,AN,0.,WEIGHT)
C
C       Find reconstructed jet with maximum ET
C
        EJT=1.
        ETMAX=0.
        LISJT=0
        NJETS=0
  500   CALL GTISJT(LISJT,LISJT,EJT,P,MASS,PHI,TH,ETA) 
        IF(LISJT.NE.0) THEN
          NJETS=NJETS+1
          IF(EJT.GT.ETMAX) THEN
            ETMAX=EJT
          ENDIF
          GOTO 500
        ENDIF
        CALL HFILL(8,ETMAX,0.,WEIGHT)
      ELSE
        ISANAL=.FALSE.
      ENDIF
C
  999 RETURN
C
      ENTRY ISASTA()
C
      ISASTA=.TRUE.
      WRITE(MESG,1000) NANAL
 1000 FORMAT(I5,' events analyzed by ISANAL')
      CALL INTMSG(MESG)
      RETURN
C
      ENTRY ISADIA()
C
      ISADIA=.TRUE.
      SMEAR=.TRUE.
      CALL GETPAR1l(' Smear pseudo-calorimeter data? [Y] >',
     &  'L',SMEAR)
      YES=.FALSE.
      CALL GETPAR1l(' Change defaults for jet finding? [N] >',
     &  'L',YES)
      IF(YES) THEN
        CALL GETPAR(1,' RJET [0.7]>','R',RJET)
        CALL GETPAR(-1,' EJCUT [5.0]>','R',EJCUT)
        CALL GETPAR(-1,' ECCUT [0.5]>','R',ECCUT)
        CALL GETPAR(-1,' ETSTOP [1.0]>','R',ETSTOP)
      ENDIF
      RETURN
      END
