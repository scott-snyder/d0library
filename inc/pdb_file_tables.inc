      INTEGER    NTABLES
      PARAMETER (NTABLES = 13)
      CHARACTER*16 TABLE_NAME(NTABLES)
      DATA TABLE_NAME /'RAW_FILE','STA', 'STRM_STA', 'FILT_STA',
     &                            'DST', 'STRM_DST', 'FILT_DST',
     &                            'MSTA','STRM_MSTA','FILT_MSTA',
     &                            'MDST','STRM_MDST','FILT_MDST'/
      LOGICAL IN_OUT_TABLES_OK(NTABLES,NTABLES)
C ****  Rows    are IN_TABs
C ****  Columns are OUT_TABs
      DATA IN_OUT_TABLES_OK /
     &  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .TRUE., .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.TRUE., .TRUE., .FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.FALSE.,.TRUE., .TRUE., .FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.TRUE., .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.FALSE.,.TRUE., .FALSE.,.TRUE., .TRUE., .FALSE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.FALSE.,.FALSE.,.TRUE., .FALSE.,.TRUE., .TRUE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.TRUE., .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.TRUE. ,.TRUE., .FALSE.,.FALSE.,.TRUE. ,.FALSE.,
     &          .TRUE. ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.FALSE.,.FALSE.,.TRUE. ,.FALSE.,.FALSE.,.FALSE.,
     &          .FALSE.,.TRUE., .FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., .FALSE.,.FALSE.,
     &          .TRUE. ,.FALSE.,.FALSE.,.TRUE., .FALSE.,.FALSE.,
     &  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., .FALSE.,
     &          .FALSE.,.TRUE., .FALSE.,.TRUE., .FALSE.,.FALSE.,
     &  .FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE.,
     &          .FALSE.,.FALSE.,.TRUE. ,.FALSE.,.TRUE., .FALSE./
