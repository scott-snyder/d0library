      SUBROUTINE DBCLB_PATH(CALTYPE,DECT,PATH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return DBL3 Path name for DECT/CALTYPE
C-
C-   Inputs  : CALTYPE = Calibration type = Peds,Pulser/Gains, Times
C-             DECT    = Detector Type
C-   Outputs : PATH    = Path Name
C-   Controls: none
C-
C-   Created  18-NOV-1989   S. Abachi, Jan Guida
C-   modified    Feb-1992   J.Green   add WAMUS drift nonlinearity, and SAMUS
C-   Updated  31-AUG-1992   Srini Rajagopalan, Add CD logical strcutures 
C-   Updated  26-OCT-1992   Haowei Xu Add Level 0
C-   Updated     Oct-93     J.Green   add WAMUS scintillator
C-   Updated   1-FEB-1995   Lewis Taylor Goss   add TRD Uranium Noise
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) DECT,PATH
      CHARACTER*17  CALTYPE
      CHARACTER*15  TOPLEV
      CHARACTER*4   BANK
C
C----------------------------------------------------------------------
C
      IF (CALTYPE(1:8).EQ.'PEDESTAL') THEN
        IF(DECT.EQ.'VTX') THEN
          BANK = 'VPCH'
        ELSE IF (DECT.EQ.'CDC') THEN
          BANK = 'DPCH'
        ELSE IF (DECT.EQ.'FDC') THEN
          BANK = 'FPCH'
        ELSE IF (DECT.EQ.'TRD') THEN
          BANK = 'TPCH'
        ELSE IF (DECT.EQ.'CAL') THEN
          BANK = 'CPDH'
        ELSE IF (DECT.EQ.'MUO') THEN
          BANK = 'MPED'
        ELSE IF (DECT.EQ.'LV0') THEN
          BANK = 'LPCH'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        IF(CALTYPE(17:17).EQ.'H') BANK = 'CLBH'
        PATH = TOPLEV//'PEDS/'//BANK
      ELSE IF (CALTYPE(1:6).EQ.'PULSER'.OR.CALTYPE(1:5).EQ.'GAINS') THEN
        IF(DECT.EQ.'VTX') THEN
          BANK = 'VGCH'
        ELSE IF (DECT.EQ.'CDC') THEN
          BANK = 'DGCH'
        ELSE IF (DECT.EQ.'FDC') THEN
          BANK = 'FGCH'
        ELSE IF (DECT.EQ.'TRD') THEN
          BANK = 'TGCH'
        ELSE IF (DECT.EQ.'CAL') THEN
          BANK = 'CGNH'
        ELSE IF (DECT.EQ.'MUO') THEN
          BANK = 'MGAN'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        IF(CALTYPE(17:17).EQ.'H') BANK = 'CLBH'
        PATH = TOPLEV//'GAINS/'//BANK
      ELSE IF (CALTYPE(1:5).EQ.'TIMES') THEN
        IF(DECT.EQ.'VTX') THEN
          BANK = 'VTCH'
        ELSE IF (DECT.EQ.'CDC') THEN
          BANK = 'DTCH'
        ELSE IF (DECT.EQ.'FDC') THEN
          BANK = 'FTCH'
        ELSE IF (DECT.EQ.'MUO') THEN
          BANK = 'MTIM'
        ELSE IF (DECT.EQ.'SAM') THEN
          BANK = 'SELC'
        ELSE IF (DECT.EQ.'LV0') THEN
          BANK = 'LTCH'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        IF(CALTYPE(17:17).EQ.'H') BANK = 'CLBH'
        PATH = TOPLEV//'TIMES/'//BANK
      ELSE IF (CALTYPE(1:6).EQ.'DTIMES') THEN
        IF (DECT.EQ.'MUO') THEN
          BANK = 'MDTM'
        ELSEIF (DECT.EQ.'LV0') THEN
          BANK = 'LDCH'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        IF(CALTYPE(17:17).EQ.'H') BANK = 'CLBH'
        PATH = TOPLEV//'DTIME/'//BANK
      ELSE IF (CALTYPE(1:5).EQ.'DRIFT') THEN
        IF (DECT.EQ.'MUO') THEN
          BANK = 'MDFT'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        IF(CALTYPE(17:17).EQ.'H') BANK = 'CLBH'
        PATH = TOPLEV//'DRIFT/'//BANK
      ELSE IF (CALTYPE(1:7).EQ.'MINTIME') THEN  
        IF (DECT.EQ.'SAM') THEN
          BANK = 'SMNT'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        IF(CALTYPE(17:17).EQ.'H') BANK = 'CLBH'
        PATH = TOPLEV//'MINTM/'//BANK
      ELSE IF (CALTYPE(1:13).EQ.'PROC_PEDESTAL') THEN
        IF(DECT.EQ.'VTX') THEN
          BANK = 'VPDH'
        ELSE IF (DECT.EQ.'CDC') THEN
          BANK = 'DPDH'
        ELSE IF (DECT.EQ.'FDC') THEN
          BANK = 'FPDH'
        ELSE IF (DECT.EQ.'TRD') THEN
          BANK = 'TPDH'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        PATH = TOPLEV//'PEDS/'//BANK
      ELSE IF (CALTYPE(1:10).EQ.'PROC_GAINS') THEN
        IF(DECT.EQ.'VTX') THEN
          BANK = 'VGNH'
        ELSE IF (DECT.EQ.'CDC') THEN
          BANK = 'DGNH'
        ELSE IF (DECT.EQ.'FDC') THEN
          BANK = 'FGNH'
        ELSE IF (DECT.EQ.'TRD') THEN
          BANK = 'TGAI'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        PATH = TOPLEV//'GAINS/'//BANK
      ELSE IF (CALTYPE(1:10).EQ.'PROC_TIMES') THEN
        IF(DECT.EQ.'VTX') THEN
          BANK = 'VTMH'
        ELSE IF (DECT.EQ.'CDC') THEN
          BANK = 'DTMH'
        ELSE IF (DECT.EQ.'FDC') THEN
          BANK = 'FTMH'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        PATH = TOPLEV//'TIMES/'//BANK
      ELSE IF (CALTYPE(1:6).EQ.'SCINTS') THEN
        IF (DECT.EQ.'MUO') THEN
          BANK = 'MSTC'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        IF(CALTYPE(17:17).EQ.'H') BANK = 'CLBH'
        PATH = TOPLEV//'SCINT/'//BANK
      ELSE IF (CALTYPE(1:8).EQ.'TRD_URAN') THEN
        IF (DECT.EQ.'TRD') THEN
          BANK = 'TURA'
        ENDIF
        TOPLEV = '//D0STP/'//BANK(1:1)//'CALIB/'
        PATH = TOPLEV//'TRDUR/'//BANK
      ENDIF
C
  999 RETURN
      END
