C----------------------------------------------------------------------
C
C-   Created  12-SEP-1995  Jaehoon Yu for Diffractive W studies.
C-   Updated  02-MAR-1996  Andrew G. Brandt  added ELC4 common
C
C----------------------------------------------------------------------
      INTEGER MAX_ELC
      PARAMETER (MAX_ELC=3)
C
C
C ****  Electron general and kinematic information
C
C
C ****  General electron info.
C
      COMMON /QCD_ELC1/
     &        NELEC, NELC_MAX, 
     &        ELC_COR_FACTOR(MAX_ELC),
     &        ELC_ET(MAX_ELC), ELC_E(MAX_ELC),
     &        ELC_EZ(MAX_ELC), ELC_EX(MAX_ELC), ELC_EY(MAX_ELC),
     &        ELC_ETA(MAX_ELC),ELC_PHI(MAX_ELC)
C
C
C ****  General electron info.
C
      REAL    ELC_COR_FACTOR         
      INTEGER NELEC,NELC_MAX
      REAL    ELC_ET, ELC_E,ELC_EZ, ELC_EX, ELC_EY,
     &        ELC_ETA,ELC_PHI
      CHARACTER*225 QCD_ELC1_TAG    
C
      DATA QCD_ELC1_TAG/
     &       'NELEC[0,3],NELC_MAX:I,ELC_COR(NELEC):R,
     &        ELC_ET(NELEC):R, ELC_E(NELEC):R,
     &        ELC_EZ(NELEC):R, ELC_EX(NELEC):R, ELC_EY(NELEC):R,
     &        ELC_ETA(NELEC):R,ELC_PHI(NELEC):R'/
C
C ****  Electron quality parameters based on Calorimeter info.
C
      COMMON /QCD_ELC2/    
     &        ELC_EM_FRAC(MAX_ELC), ELC_HMATRIX_CHISQ(MAX_ELC), 
     &        ELC_CAL_ETA(MAX_ELC), ELC_DISPERSION(MAX_ELC), 
     &        ELC_E_FISO1(MAX_ELC),  ELC_E_FISO2(MAX_ELC),
     &        ELC_ET_FISO1(MAX_ELC), ELC_ET_FISO2(MAX_ELC),
     &        ELC_E_ISO1(MAX_ELC), ELC_E_ISO2(MAX_ELC),
     &        ELC_ET_ISO1(MAX_ELC), ELC_ET_ISO2(MAX_ELC)
C
      INTEGER ELC_CAL_ETA
      REAL    ELC_EM_FRAC, ELC_HMATRIX_CHISQ, 
     &        ELC_DISPERSION, 
C
C ****  Isolation fractions (iso1 is iso cone of 0.4, iso2 is with 0.6)
C
     &        ELC_E_FISO1,  ELC_E_FISO2,
     &        ELC_ET_FISO1, ELC_ET_FISO2,
C
C ****  Isolation energy (or Et) in the annular region (DR=0.4-0.2)
C
     &        ELC_E_ISO1, ELC_E_ISO2,
     &        ELC_ET_ISO1, ELC_ET_ISO2
C
      CHARACTER*350 QCD_ELC2_TAG      
C
      DATA  QCD_ELC2_TAG/    
     &       'ELC_EM_FRAC(NELEC):R, ELC_HMTR_CHSQ(NELEC):R, 
     &        ELC_CAL_ETA(NELEC):I, ELC_DISPERSION(NELEC):R, 
     &        ELC_E_FISO1(NELEC):R,  ELC_E_FISO2(NELEC):R,
     &        ELC_ET_FISO1(NELEC):R, ELC_ET_FISO2(NELEC):R,
     &        ELC_E_ISO1(NELEC):R, ELC_E_ISO2(NELEC):R,
     &        ELC_ET_ISO1(NELEC):R, ELC_ET_ISO2(NELEC):R'/
C
C ****  Electron quality parameters based on tracking info.
C
      COMMON /QCD_ELC3/
     &        ELC_NCELLS(MAX_ELC), ELC_NCELLS_THRESHOLD(MAX_ELC),
     &        ELC_DEDX(MAX_ELC), ELC_VTX_MIP(MAX_ELC),
     &        ELC_TRK_SIG(MAX_ELC), ELC_IMPACT_XY(MAX_ELC),
     &        ELC_IMPACT_Z(MAX_ELC),
     &        ELC_ETOT_LIKE(MAX_ELC), ELC_TOT_LEFF(MAX_ELC),
     &        ELC_ELIKE_5V(MAX_ELC), ELC_ELIKE_4V(MAX_ELC),
     &        ELC_ELIKE_2V(MAX_ELC)
C
      INTEGER ELC_NCELLS, ELC_NCELLS_THRESHOLD 
      REAL    ELC_DEDX, ELC_VTX_MIP,ELC_TRK_SIG, 
     &        ELC_IMPACT_XY,ELC_IMPACT_Z,
C
C ****  TRD info.
C
     &        ELC_ETOT_LIKE, ELC_TOT_LEFF, ELC_ELIKE_5V,
     &        ELC_ELIKE_4V, ELC_ELIKE_2V
      CHARACTER*350 QCD_ELC3_TAG          
C
      DATA QCD_ELC3_TAG/
     &       'ELC_NCELLS(NELEC):I, ELC_NCELLS_THR(NELEC):I,
     &        ELC_DEDX(NELEC):R, ELC_VTX_MIP(NELEC):R,
     &        ELC_TRK_SIG(NELEC):R, ELC_IMPACT_XY(NELEC):R,
     &        ELC_IMPACT_Z(NELEC):R,
     &        ELC_ETOT_LIKE(NELEC):R, ELC_TOT_LEFF(NELEC):R,
     &        ELC_ELIKE_5V(NELEC):R, ELC_ELIKE_4V(NELEC):R,
     &        ELC_ELIKE_2V(NELEC):R'/ 
C
C Cosmic rejection info
C
      COMMON /QCD_ELC4/
     &        ELC_EMV_PRBXY(MAX_ELC), ELC_EMV_PRBRZ(MAX_ELC),
     &        ELC_EMV_PRB(MAX_ELC), ELC_NCLOUD(MAX_ELC)

      REAL ELC_EMV_PRBXY, ELC_EMV_PRBRZ, ELC_EMV_PRB
      INTEGER ELC_NCLOUD

      CHARACTER*255 QCD_ELC4_TAG        
      DATA QCD_ELC4_TAG/
     &       'ELC_EMV_PRBXY(NELEC):R, ELC_EMV_PRBRZ(NELEC):R,
     &        ELC_EMV_PRB(NELEC):R, ELC_NCLOUD(NELEC):I'/      
