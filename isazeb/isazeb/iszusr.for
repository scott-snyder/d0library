      SUBROUTINE ISZUSR(PRUNIT)
C------------------------------------------------------------------------
C-                                                                      -
C-    Example of a user provided subroutine                             -
C-    to histogram ISAJET events with ZEBRA format                      -
C-    It uses ISAZEB utility subroutines to create                      -
C-    trivial calorimeter data (no depth, no shower generation)         -
C-                                                                      -
C-         If calorimeter data was not generated with ISARUN:           -
C-
C-     CALL ISACFL  (unsmeared data)
C-     CALL ISMEAR  (smeared data)                                      -
C-
C-                                                                      -
C-          then to find jets with a simplified version of the UA1 jet  -
C-          algorithm with jet radius rjet and minimum scalar transverse-
C-          energy ejcut                                                -
C-            (RJET=1., EJCUT=5. for UA1)                               -
C-       need to supply also ECCUT= min. ET for cell to be added to jet -
C-          and  ETSTOP= min. ET in a cell to start a jet:              -
C-                                                                      -
C-     CALL ISZJET(RJET,EJCUT,ECCUT,ETSTOP,NJMAX,NCJET)                 -
C-                                                                      -
C-                                                                      -
C-    INPUT:                                                            -
C-    PRUNIT = unit number for listing                                  -
C-                                                                      -
C-                 SDP May, 1986                                        -
C-                                                                      -
C------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NJMAX
      PARAMETER (NJMAX=50)    ! maximum number of jets allowed
      INTEGER NCJET           ! no. of jets
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C          HBOOK and other variables.   
C         NWMEMO = number of words for histograms
      INTEGER NWMEMO
      PARAMETER (NWMEMO=10000)  
      COMMON/PAWC/HMEMOR(NWMEMO)    
      REAL HMEMOR
C
      INTEGER PRUNIT,I,IJTMX,ICLMX,IPTMX,NCLS,ID,IDV
      INTEGER LISAC,LISCL,LISAE,LISP1,LISV1,LISJT,LISJS,LISAL
      REAL AN,ETMAX,PTSQ,PTSQMX,PX,PY,PTMAX,WEIGHT,ISA_WEIGHT
      REAL P(4),CSTH,SNTH,ETA,THETA,PHI,SNPHI,CSPHI,MASS
      REAL ET,EEM,EHAD,EJT,ABSETA,X,Y,Z
      REAL RJET,EJCUT,ECCUT,ETSTOP
      LOGICAL FIRST
C   
C          INITIALIZE   
C
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
        CALL HLIMIT(-NWMEMO)   
        CALL HOUTPU(PRUNIT)    
        CALL HERMES(PRUNIT)    
C   
C          SET UP HISTOGRAMS    
C   
        CALL HBOOK1(1,'ET MAX CELL$',100,0.,100.,0.)
        CALL HBOOK1(2,'NO. JETS$',20,0.,20.,0.)
        CALL HBOOK1(3,'ET MAX JET$',100,0.,100.,0.)
        CALL HBOOK1(4,'PT MAX PART$',100,0.,100.,0.)
        CALL HBOOK1(5,'PT MAX LEPTON$',100,0.,100.,0.)
        FIRST=.FALSE.
      ENDIF
C
C                ANALYZE EVENTS
C
      WEIGHT=ISA_WEIGHT()
C
C        Loop over vertices and particles
C        and find particle with highest PT
C
      PTSQMX=0
      LISV1=0
C
  100 CALL GTISV1(LISV1,LISV1,IDV,P,X,Y,Z) ! loop over vertices
      IF(LISV1.GT.0) THEN 
        LISP1=LISV1-IZISP1
C
  200   CALL GTISP1(LISP1,LISP1,ID,P,PHI,THETA,ETA) ! loop over particles
        IF(LISP1.GT.0) THEN
          PTSQ=P(1)**2+P(2)**2
          IF(PTSQ.GT.PTSQMX) PTSQMX=PTSQ
          GOTO 200
        ENDIF
C
        GOTO 100
      ENDIF
      PTMAX=SQRT(PTSQMX)
      CALL HFILL(4,PTMAX,0.,WEIGHT)
C
C       find lepton with highest Et
C
      PTSQMX=0
      LISAL=0
  300 CALL GTISAL(LISAL,LISAL,ID,P,PHI,THETA,ETA) 
      IF(LISAL.NE.0) THEN
        PTSQ=P(1)**2+P(2)**2
        IF(PTSQ.GT.PTSQMX) PTSQMX=PTSQ
        GOTO 300
      ENDIF
      PTMAX=SQRT(PTSQMX)
      CALL HFILL(5,PTMAX,0.,WEIGHT)
C   
C          find highest et cell 
C
      ETMAX=0                                                         
C
C       generate smeared calorimeter data
C       will not generate new banks if they already exist
      CALL ISMEAR              ! call ISACFL instead for unsmeared data
C
      LISCL=0
  400 CALL GTISCL(LISCL,LISCL,EEM,EHAD,SNTH,CSTH,CSPHI,SNPHI,
     &  PHI,ETA)
      IF(LISCL.GT.0.) THEN
        ET=(EEM+EHAD)*SNTH
        IF(ET.GT.ETMAX) ETMAX=ET
        GOTO 400
      ENDIF
      CALL HFILL(1,ETMAX,0.,WEIGHT)
C
C           find jets
C
      DATA RJET,EJCUT,ECCUT,ETSTOP/0.7,5.0,.5,1.0/
      CALL ISZJET(RJET,EJCUT,ECCUT,ETSTOP,NJMAX,NCJET)
C
      AN=NCJET
      CALL HFILL(2,AN,0.,WEIGHT)
C
C       Find jet with maximum ET
C
      EJT=1.
      ETMAX=0.
      LISJT=0
  500 CALL GTISJT(LISJT,LISJT,EJT,P,MASS,PHI,THETA,ETA) 
      IF(LISJT.NE.0) THEN
        IF(EJT.GT.ETMAX) ETMAX=EJT
        GOTO 500
      ENDIF
      CALL HFILL(3,ETMAX,0.,WEIGHT)
      RETURN    
C
      ENTRY ISZUSD    ! entry point for end-of-job
      CALL HLDIR('//PAWC','T')
      CALL HPDIR('//PAWC','T')
      RETURN
      END   
