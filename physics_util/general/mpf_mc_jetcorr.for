      SUBROUTINE mpf_mc_jetcorr(old_jet_energy,old_jet_et, algor,
     &  response,error)

C----------------------------------------------------------------------
C-   Purpose and Methods : to return value of response and its error for jets
C-        of different algorithms for MC.
C-
C-   Inputs  :
C-              old_jet_energy  R   -- uncorrected jet energy
C-              old_jet_et      R   -- uncorrected jet et
C-              algor           I   -- jet reco algorithm
C-
C-   Outputs :
C-              response        R   -- mc jet response correction
C-              error           R   -- estimated error**
C-
C-      ** Note: errors are from statistical errors, fit parameter
C-          errors and estimates of systematic errors.  these systematic errors
C-          include estimates of effects of topological differences for sample
C-          used vs. normal direct photon MC events.  also, a systematic error
C-          for 1.0 is included due to incomplete response curves and fits.
C-
C-   Created  Jun-22-1994  Bob Kehoe
C----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER npar,nalgo
      PARAMETER (npar = 2)
      PARAMETER (nalgo = 5)
      INTEGER algor
      REAL response,old_jet_energy,error
      REAL old_jet_et
C-        *** note: algorithms numbered accordingly -- 1 = NN, 2 = 0.3, 3 = 0.5,
C-        *** 4 = 0.7, and 5 = 1.0
      REAL rlow(npar,nalgo),rmed(npar,nalgo),rhi(npar,nalgo)
      REAL low_med(nalgo),med_hi(nalgo)
      DATA rlow/2.245,      -0.1032,
     &          1.829,      -0.07005,
     &          1.686,      -0.05731,
     &          1.467,      -0.03948,
     &          1.467,      -0.03948/
      DATA rmed/0.916,      -0.001224,
     &          0.8984,     -0.0007325,
     &          0.8851,     -0.0003159,
     &          0.8687,      0.00004896,
     &          0.8815,     -0.0001806/
      DATA rhi/ 0.8699,     -0.00001294,
     &          0.8716,     -0.000005994,
     &          0.8783,     -0.000053,
     &          0.8687,      0.00004896,
     &          0.8815,     -0.0001806/
      DATA low_med/13.03,  13.42, 14.05, 15.15,  14.9/
      DATA med_hi/ 40.0,  40.0, 35.0, 30.0,  30.0/

C----------------------------------------------------------------------
      response = 0.
      error = 0.

C-        *** determine response and base level of error bars ***
      IF (old_jet_et.LT.low_med(algor)) THEN
        response = rlow(1,algor) + rlow(2,algor)*old_jet_et
        error = 0.05
        IF (response.GT.1.2) THEN
          response = 1.2
          error = 0.10
        ELSEIF (old_jet_et.LT.10.) THEN
          error = 0.10
        ENDIF
      ELSEIF (old_jet_energy.LE.med_hi(algor)) THEN
        response = rmed(1,algor) + rmed(2,algor)*old_jet_energy
        error = 0.03
      ELSEIF (old_jet_energy.GT.med_hi(algor)) THEN
        response = rhi(1,algor) + rhi(2,algor)*old_jet_energy
        error = 0.04
      ENDIF

C-        *** additional errors in poorly fit regions ***
      IF (old_jet_energy.GT.200.)  THEN
        error = sqrt(error**2. + 0.1**2.)
      ELSEIF (old_jet_energy.GT.90.) THEN
        error = sqrt(error**2. + 0.03**2.)
      ELSEIF (old_jet_energy.LT.20.) THEN
        error = sqrt(error**2. + 0.05**2.)
      ENDIF

      IF (algor.EQ.5) error = sqrt(error**2. + 0.04**2.)

  999 RETURN
      END
