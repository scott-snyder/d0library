C----------------------------------------------------------------------
C- MTC_MPVCOM.INC part of the MTC (Muon Tracking in the Calorimeter) package
C-
C-   Created   8-JUL-1993   Elizabeth Gallas
C-
C- This block stores the MPV and RMS of the energy distribution 
C- of MIPs through each calorimeter cell type, layer, eta.
C- MPV (mip response most probable value) has dependence on
C-      1. calorimeter section (ical=1:10),
C-      2. calorimeter section sublayer (isub=1,isublayer(ical))
C-      3. cal sec sublayer eta region (iseta=1,22)
C-         this is NOT regular cal ieta -- see MTC_CALSECTS.INC
C- Routine MTC_FILL_MPV.FOR uses the input ADC* arrays and fills
C- the GEV* arrays for use by the MTC program.
C----------------------------------------------------------------------
      COMMON /MTC_MPVCOM/ ADCMPV(10,5,22), GEVMPV(10,5,22),
     &                    ADCMPV_W(10,5,22), GEVMPV_W(10,5,22),
     &                    GEV_MPV_ICD(768), GEV_MPV_ICD_W(768),
     &                    
     &                    ADCMPV3(10,5,22), GEVMPV3(10,5,22),
     &                    ADCMPV_W3(10,5,22), GEVMPV_W3(10,5,22),
     &                    GEV_MPV_ICD3(768), GEV_MPV_ICD_W3(768)

      REAL ADCMPV, GEVMPV, ADCMPV_W, GEVMPV_W,
     &     GEV_MPV_ICD, GEV_MPV_ICD_W
      REAL ADCMPV3, GEVMPV3, ADCMPV_W3, GEVMPV_W3,
     &     GEV_MPV_ICD3, GEV_MPV_ICD_W3
C----------------------------------------------------------------------
C- ADCMPV(10,5,22) == most prob value in adc counts
C- ADCMPV_W(10,5,22) == width of distribution in adc counts
C-
C- GeVMPV(10,5,22) == most prob value in GeV except for ICD and
C- GeVMPV_W(10,5,22) == width of dist in GeV except for ICD
C-      from ADCMPV(,,) and ADCMPV_W(,,)
C-      for all calorimeter sections except the ICD,
C-      where MPV varies tile to tile
C- GeV_mpv_ICD(), GeV_mpv_ICD_w() - contains tile to tile MVP and width
C- 
C- The constants with a "3" are for 3x3 cell per layer towers, the 
C- constants w/o a "3" are for the single (1x1) cell per layer towers.
C----------------------------------------------------------------------
