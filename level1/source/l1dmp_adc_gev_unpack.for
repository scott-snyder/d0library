      SUBROUTINE L1DMP_ADC_GEV_UNPACK(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the contents of the ADC bytes, expressed in
C-                              GeV.
C-
C-   Inputs  : LUN      The unit number to write to.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
C
      INTEGER LUN
C
      INTEGER ETA, PHI, INGROUP
      INTEGER ISTAT
      INTEGER ADC_PERLINE
      PARAMETER (ADC_PERLINE = 16)
C
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'Trigger Tower ADC Values in GeV'
      WRITE (LUN,*) '==============================='
      WRITE (LUN,*)
      WRITE (LUN,*) 'Note: The following values have been ' 
     &           // 'corrected for zero input energy offset.'
      WRITE (LUN,*)
C
  410 FORMAT ( SP, ' ETA=', I3, S, '   HD', 16(' ', F6.2))
  420 FORMAT ( ' PHI=', I2, ':', I2, ' EM', 16(' ', F6.2))
      DO ETA = ETA_MAX,ETA_MIN,-1
        WRITE (LUN,*)
        DO PHI = PHI_MIN, PHI_MAX, ADC_PERLINE
          WRITE (LUN,410,IOSTAT=ISTAT) -ETA, 
     &        ((FADC_BYTE(NEG_ETA, ETA, PHI+INGROUP-1, HD_TOWER)
     &             - ADC_ZERESP(NEG_ETA, ETA, PHI+INGROUP-1, HD_TOWER))
     &           * GLOBAL_ADC_SCALE, 
     &         INGROUP = 1, ADC_PERLINE)
          WRITE (LUN,420,IOSTAT=ISTAT) PHI, PHI+ADC_PERLINE-1,
     &        ((FADC_BYTE(NEG_ETA, ETA, PHI+INGROUP-1, EM_TOWER)
     &             - ADC_ZERESP(NEG_ETA, ETA, PHI+INGROUP-1, EM_TOWER))
     &           * GLOBAL_ADC_SCALE,
     &        INGROUP = 1, ADC_PERLINE)
        END DO
      END DO
C
      DO ETA = ETA_MIN,ETA_MAX
        WRITE (LUN,*)
        DO PHI = PHI_MIN, PHI_MAX, ADC_PERLINE
          WRITE (LUN,410,IOSTAT=ISTAT) ETA, 
     &        ((FADC_BYTE(POS_ETA, ETA, PHI+INGROUP-1, HD_TOWER)
     &             - ADC_ZERESP(POS_ETA, ETA, PHI+INGROUP-1, HD_TOWER))
     &           * GLOBAL_ADC_SCALE, 
     &         INGROUP = 1, ADC_PERLINE)
          WRITE (LUN,420,IOSTAT=ISTAT) PHI, PHI+ADC_PERLINE-1,
     &        ((FADC_BYTE(POS_ETA, ETA, PHI+INGROUP-1, EM_TOWER)
     &             - ADC_ZERESP(POS_ETA, ETA, PHI+INGROUP-1, EM_TOWER))
     &           * GLOBAL_ADC_SCALE,
     &         INGROUP = 1, ADC_PERLINE)
        END DO
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
