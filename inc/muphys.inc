C
C- S. Abachi
C
      REAL DECAL,DEMUO,DEEM,DEHD,DEDED,SIGDEC,SIGDEM,DR
      REAL MSACA,MSAMU,TRADLCA,TRADLMU,CRADL,TDIS
      REAL MUTHE,MUETA,MUPHI,ECONE(10),AGMUCD,ECONE_EM(10)
      INTEGER   MFLG,IVRT,NMTR
      COMMON /MUPHYS/ DECAL,DEMUO,DEEM,DEHD,DEDED,SIGDEC,SIGDEM,DR,
     &     MSACA,MSAMU,TRADLCA,TRADLMU,CRADL,TDIS,
     &     MUTHE,MUETA,MUPHI,ECONE,MFLG,IVRT,NMTR,AGMUCD,ECONE_EM
C
C-             DECAL    : Energy-loss in calorimeter (GeV)
C-             DEMUO    : Energy-loss in Muon system (GeV)
C-             DEEM     : Mean energy-loss in EM (GeV)
C-             DEHD     : Mean energy-loss in FH & CH (GeV)
C-             DEDED    : Mean energy-loss in dead material(GeV)
C-             SIGDEC   : Error in calculated DEDX in GeV
C-             SIGDEM   : Error in measured DEDX in GeV
C-             DR       : Uncertainty radious at muon detector (cm)
C-             MSACA    : RMS multiple scattering angle in calorimeter(rad)
C-             MSAMU    : RMS multiple scattering angle in muon system(rad)
C-             TRADLCA  : Total number of radiation lengths in calorimeter.
C-             TRADLMU  : Total number of radiation lengths in muon system.
C-             CRADL    : Compound radiation length encountered by muon.
C-             TDIS     : Total distance traveled by muon.
C-             MUTHE    : muon theta
C-             MUETA    : muon eta
C-             MUPHI    : muon phi
C-             ECONE(10): observed energy in various cone sizes
C-             MFLG     : 1= lookup table used, 2= GEANT used, 3= other
C-             IVRT     : 0=A layer used, 1=Vertex point used,2=somewher else
C-             NMTR     : Number of CD tracks associated with muon track
C-             AGMUCD   : Angle between muon and closest CD track (degrees)
C-          ECONE_EM(10): observed energy in various cone sizes
