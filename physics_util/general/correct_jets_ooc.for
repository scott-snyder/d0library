      SUBROUTINE CORRECT_JETS_OOC( E, ET, ETA, CONE_USED, CORR_FACTOR,
     &  IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return Out-of-cone correction factors
C-
C-   Inputs  :
C-            E     [R] -Energy of jet
C-            ET    [R] -ET of jet
C-            ETA   [R] -eta of jet
C-        CONE_USED [R] -Jet algorithm (.3,.5,.7,1.0,-1=NN)
C-
C-   Outputs :
C-
C-    CORR_FACTOR   [R] -Correction factor
C-            IER   [I] -Error code 0=ok -1=unknown algorithm
C-   Controls:
C-
C-   Created  14-JUL-1995   Richard V. Astur
C-   Updated  Oct-12-1995   Bob Kehoe  -- invert NN loss correction
C-   Updated  Oct-25-1995   Bob Kehoe  -- replace JINT with INT for IBM
C-                                        compatibility, add error entry pt.
C-                                        (CAFIX5.0)
C-   Updated  Feb-7-1997    Daniel Elvira -- implement a showering correction 
C-                          based on the subtraction of the physics out-of-cone
C-                          (particle level Herwig) from the total out-of-cone
C-                          measured in the data (CAFIX5.1).
C-                                            
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL E,ET,ETA,ETAL,ETAH,CONE_USED,CORR_FACTOR,error(2,2),
     &  err_ooc(2,2)
      REAL CORR_ETA_HIGH, CORR_ETA_LOW, ERR_ETA_HIGH_PLUS,
     &  ERR_ETA_HIGH_MIN, ERR_ETA_LOW_PLUS, ERR_ETA_LOW_MIN,
     &  FAC_ETA_HIGH_PLUS, FAC_ETA_HIGH_MIN, FAC_ETA_LOW_PLUS,
     &  FAC_ETA_LOW_MIN
      REAL ERR_FACTOR_PLUS, ERR_FACTOR_MIN
      REAL OA0(7,4),OA1(7,4),OA2(7,4),ER0P(7,4),ER1P(7,4),ER2P(7,4),
     &  ER0M(7,4),ER1M(7,4),ER2M(7,4),E0(7),E1(7)
      INTEGER I,J,JH,JL
      INTEGER OFUN(7,4)
      INTEGER IER,LRCP
      LOGICAL FIRST,OK
C----------------------------------------------------------------------
      DATA first/.true./
      call vzero(error,2*2)
      CORR_FACTOR = 1.0                     ! No correction
C
C-----------------------------------------------------------------------
C Read function and error parameters from QCD_JET_CORRECTION.RCP
C-----------------------------------------------------------------------
C
      IF (first) THEN
        CALL ezloc('QCD_JET_CORRECTION_RCP',lrcp)
        ok = lrcp .GT. 0
        IF (.NOT. ok) THEN
          CALL inrcp('QCD_JET_CORRECTION_RCP',ier)
          IF (ier.EQ.0) CALL ezpick('QCD_JET_CORRECTION_RCP')
          IF (ier.EQ.0) CALL ezerr(ier)
          IF(ier.NE.0) THEN
            CALL errmsg('RCP not found','CORRECT_JETS_OOC',
     &        'QCD_JET_CORRECTION_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF                 ! *** read in RCP parameters ***
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr(ier)
        IF (ier.EQ.0) THEN
          CALL ezget('OA0',oa0,ier)
          CALL ezget('OA1',oa1,ier)
          CALL ezget('OA2',oa2,ier)
          CALL ezget_i('OFUN',ofun,ier)
          CALL ezget('er0p',er0p,ier)
          CALL ezget('er0m',er0m,ier)
          CALL ezget('er1p',er1p,ier)
          CALL ezget('er1m',er1m,ier)
          CALL ezget('er2p',er2p,ier)
          CALL ezget('er2m',er2m,ier)
          CALL ezget('e0',e0,ier)
          CALL ezget('e1',e1,ier)
          CALL ezrset
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'correct_jets_ooc','NO RCP file to work with','F')
        ENDIF
C-------------------------------------------------------------------------
C Go from percentual errors to relative errors
C-------------------------------------------------------------------------
        DO j=1,7
          DO i=1,4
            er0p(j,i)=er0p(j,i)/100.
            er0m(j,i)=er0m(j,i)/100.
            er1p(j,i)=er1p(j,i)/100.
            er1m(j,i)=er1m(j,i)/100.
            er2p(j,i)=er2p(j,i)/100.
            er2m(j,i)=er2m(j,i)/100.
          ENDDO
        ENDDO
C
        first = .false.
      ENDIF
C-------------------------------------------------------------------------
C Choose the index associated to one particular cone size.
C Return ier=-1 if the cone size requested is not supported
C-------------------------------------------------------------------------
      IER   = 0                             ! OK
C
      IF (cone_used.eq.1.) THEN
        i = 1
      ELSE 
        IF (cone_used.eq.0.7) THEN
          i = 2
        ELSE
          IF (cone_used.eq.0.5) THEN
            i = 3
          ELSE
            IF (cone_used.eq.0.3) THEN
              i = 4
            ELSE
              ier = -1
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C--------------------------------------------------------------------------
C Select index associated to a particular pseudo-rapidity bin
C--------------------------------------------------------------------------
      jl = 1
      jh = 1
      etal = 0.2
      etah = 0.2
C
      IF (abs(eta).gt.0.2) THEN
        jh = 2
        etah = 0.6
      ENDIF
      IF (abs(eta).gt.0.6) THEN
        jl = 2
        jh = 3
        etal = 0.6
        etah = 1.
      ENDIF
      IF (abs(eta).gt.1.) THEN
        jl = 3
        jh = 4
        etal = 1.
        etah = 1.4
      ENDIF
      IF (abs(eta).gt.1.4) THEN
        jl = 4
        jh = 5
        etal = 1.4
        etah = 1.8
      ENDIF
      IF (abs(eta).gt.1.8) THEN
        jl = 5
        jh = 6
        etal = 1.8
        etah = 2.2
      ENDIF
      IF (abs(eta).gt.2.2) THEN
        jl = 6
        jh = 7
        etal = 2.2
        etah = 2.7
      ENDIF
      IF (abs(eta).gt.2.7) THEN
        jl = 7
        etal = 2.7
      ENDIF
C
      IF ((ofun(jh,i).ne.1).and.(ofun(jh,i).ne.2)) THEN
        CALL errmsg('No function specified',
     &   'correct_jets_ooc','ofun must be 1 or 2','F')
      ENDIF
      IF ((ofun(jl,i).ne.1).and.(ofun(jl,i).ne.2)) THEN
        CALL errmsg('No function specified',
     &   'correct_jets_ooc','ofun must be 1 or 2','F')
      ENDIF
C--------------------------------------------------------------------------
C calculate slope for energy dependence of errors (interpolating the parameters
C given for two extreme energies).
C--------------------------------------------------------------------------
      fac_eta_high_plus = 
     &  (er1p(jh,i)-er0p(jh,i))*(e-e0(jh))/(e1(jh)-e0(jh))
     &  +1.+er0p(jh,i)
      fac_eta_high_min = 
     &  (er1m(jh,i)-er0m(jh,i))*(e-e0(jh))/(e1(jh)-e0(jh))
     &  +1.+er0m(jh,i)
      fac_eta_low_plus = 
     &  (er1p(jl,i)-er0p(jl,i))*(e-e0(jl))/(e1(jl)-e0(jl))
     &  +1.+er0p(jl,i)
      fac_eta_low_min = 
     &  (er1m(jl,i)-er0m(jl,i))*(e-e0(jl))/(e1(jl)-e0(jl))
     &  +1.+er0m(jl,i)
C
C---------------------------------------------------------------------------
C Select function (1 for a+b*logE and 2 for a+b*x)
C Calculate OOC correction for the two eta bins closest to the jet eta
C and the relative errors 
C---------------------------------------------------------------------------
C
      IF (ofun(jh,i).eq.1) THEN
        corr_eta_high = min(oa0(jh,i) + oa1(jh,i) * log (E) , oa2(jh,i))
        err_eta_high_plus = min((oa0(jh,i)+oa1(jh,i)*log(E))
     &    *fac_eta_high_plus,oa2(jh,i)*(er2p(jh,i)+1.))
        err_eta_high_min = min((oa0(jh,i)+oa1(jh,i)*log(E))
     &    /fac_eta_high_min,oa2(jh,i)/(er2m(jh,i)+1.))
      ENDIF
C
      IF (ofun(jl,i).eq.1) THEN
        corr_eta_low = min(oa0(jl,i) + oa1(jl,i) * log (E) , oa2(jl,i))
        err_eta_low_plus = min((oa0(jl,i)+oa1(jl,i)*log(E))
     &    *fac_eta_low_plus,oa2(jl,i)*(er2p(jl,i)+1.))
        err_eta_low_min = min((oa0(jl,i)+oa1(jl,i)*log(E))
     &    /fac_eta_low_min,oa2(jl,i)/(er2m(jl,i)+1.))
      ENDIF
C
      IF (ofun(jh,i).eq.2) THEN
        corr_eta_high = min(oa0(jh,i) + oa1(jh,i) * E , oa2(jh,i))
        err_eta_high_plus = min((oa0(jh,i)+oa1(jh,i)*E)
     &    *fac_eta_high_plus,oa2(jh,i)*(er2p(jh,i)+1.))
        err_eta_high_min = min((oa0(jh,i)+oa1(jh,i)*E)
     &    /fac_eta_high_min,oa2(jh,i)/(er2m(jh,i)+1.))
      ENDIF
C
      IF (ofun(jl,i).eq.2) THEN
        corr_eta_low = min(oa0(jl,i) + oa1(jl,i) * E , oa2(jl,i))
        err_eta_low_plus = min((oa0(jl,i)+oa1(jl,i)*E)
     &    *fac_eta_low_plus,oa2(jl,i)*(er2p(jl,i)+1.))
        err_eta_low_min = min((oa0(jl,i)+oa1(jl,i)*E)
     &    /fac_eta_low_min,oa2(jl,i)/(er2m(jl,i)+1.))
      ENDIF
C
      err_eta_high_plus = err_eta_high_plus/corr_eta_high - 1.
      err_eta_high_min = 1. - err_eta_high_min/corr_eta_high
      err_eta_low_plus = err_eta_low_plus/corr_eta_low - 1.
      err_eta_low_min = 1. - err_eta_low_min/corr_eta_low
C      
C--------------------------------------------------------------------------
C Interpolate ooc correction and errors between two eta regions 
C--------------------------------------------------------------------------
      IF (etah.eq.etal) THEN
        corr_factor = corr_eta_low
        err_factor_plus = err_eta_low_plus
        err_factor_min = err_eta_low_min
      ELSE
        corr_factor = corr_eta_low + (abs(eta)-etal) *
     &   (corr_eta_high - corr_eta_low)/(etah-etal) 
        err_factor_plus = err_eta_low_plus + (abs(eta)-etal) *
     &   (err_eta_high_plus - err_eta_low_plus)/(etah-etal)
        err_factor_min = err_eta_low_min + (abs(eta)-etal) *
     &   (err_eta_high_min - err_eta_low_min)/(etah-etal)
      ENDIF
C
      corr_factor = 1. / corr_factor
C
      error(2,2) = err_factor_min
      error(1,2) = err_factor_plus
C
  999 RETURN

      ENTRY algo_loss_errors(err_ooc)
C------------------------------------------------------------------------
C-    Purpose:  output value of errors 
C-
C-    outputs:
C-          err_ooc  R  = array of statistical and systematic errors
C------------------------------------------------------------------------
      call ucopy(error,err_ooc,2*2)
      RETURN

      END
