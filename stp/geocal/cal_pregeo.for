      SUBROUTINE CAL_PREGEO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw calorimeter geometrical data
C-                         into GEANT geometrical parameters. Output
C-                         results in SRCP files. These files can be
C-                         converted into ZEBRA banks by running
C-                         CAL_POSTGEO. Data are input on stream LUNRAW.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-OCT-1988   Harrison B. Prosper, Elliott Treadwell
C-   Updated  10-DEC-1988   Stuart Fuess  CC_GEOM uses LUNSET also
C-   Updated  12-JAN-1989   Chip Stewart   LV0 ROUTINES
C-   Updated  15-JAN-1989   Harrison B. Prosper
C-                          Use logical names rather than special unit
C-                          numbers.
C-   Updated  16-JAN-1989   Chip Stewart   ICD ROUTINES
C-   Updated  19-JAN-1989   Harrison B. Prosper
C-                          SRCP_ECAL.DAT etc. are only created if
C-                          appropriate switches are set.
C-                          Add call to DO_REST (LUNSET).
C-   Updated  10-MAR-1989   A.M.Jonckheere
C-                          Add READONLY to OPENs
C-   Updated   8-DEC-1989   Harrison B. Prosper   
C-      Make compatible with new RCP
C-   Updated  13-DEC-1990   Chip Stewart   REPLACE SRCP_CONTROL with CAWSTP_RCP
C-   Updated  11-Feb-1992   Herbert Greenlee
C-      Replaced OPEN statements with calls to D0OPEN.
C-      Changed nonstandard CLOSE keyword DISPOSE to STATUS.
C-      Changed the names of pre-EZASIZ RCP files to avoid name conflict
C-        because of lack of versions on UNIX.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DO_CRYOSTAT,DO_CENTRAL,DO_ENDCAP,DO_LEVEL_ZERO,DO_ICD
      LOGICAL DO_PREGEO
      LOGICAL DO_REST,LUNEND_OPEN,LUNSET_OPEN,LUNCEN_OPEN,LUNLV0_OPEN
      LOGICAL OK
      INTEGER    I,II,JJ,NN,ERROR,NVALS,NPARS
      INTEGER    LUNLST,LUNRAW,LUNSET,LUNCEN,LUNEND,LUNLV0
      INTEGER    LUNICD,LUNOUT
      PARAMETER( LUNLST = 10 )
      PARAMETER( LUNRAW = 12 )
      PARAMETER( LUNEND = 20 ) ! Hard coded in EC routines
      PARAMETER( LUNCEN = 22 )
      PARAMETER( LUNSET = 24 ) ! Hard coded in EC routines
      PARAMETER( LUNLV0 = 26 )
      PARAMETER( LUNOUT = 28 )
      CHARACTER*48 REMARK
      CHARACTER*80 RECORD
C----------------------------------------------------------------------
C
C ****  Read control file
C
      CALL INRCP ('CAWSTP_RCP',ERROR)
      IF(ERROR.NE.0) THEN
        CALL ERRMSG('NO CAWSTP_RCP FILE','CAL_PREGEO','THUD','F')
      END IF
      CALL EZPICK('CAWSTP_RCP')
C
C ****  Read switches from CONTROL file
C
      CALL EZGET_l  ('PREGEO',DO_PREGEO,ERROR)
      IF ( .NOT. DO_PREGEO ) GOTO 999        ! Exit if PREGEO switch not set
C
      CALL EZGET  ('CRYOSTAT',DO_CRYOSTAT,ERROR)
      CALL EZGET  ('CENTRAL',DO_CENTRAL,ERROR)
      CALL EZGET  ('ENDCAP',DO_ENDCAP,ERROR)
      CALL EZGET  ('LV0',DO_LEVEL_ZERO,ERROR)
      CALL EZGET  ('ICD',DO_ICD,ERROR)
C
C ****  OPEN output files
C
      LUNCEN_OPEN = DO_CRYOSTAT .OR. DO_CENTRAL
      IF ( LUNCEN_OPEN ) THEN
        CALL D0OPEN (LUNCEN,'SRCP_UCAL0','OFL',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_UCAL','F')
      ENDIF
C
      LUNEND_OPEN = DO_CRYOSTAT .OR. DO_ENDCAP
      IF ( LUNEND_OPEN ) THEN
        CALL D0OPEN (LUNEND,'SRCP_ECAL0','OFL',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_ECAL','F')
      ENDIF
C
      LUNSET_OPEN = DO_CRYOSTAT .OR. DO_CENTRAL .OR.
     &              DO_ENDCAP   .OR. DO_ICD
      DO_REST = LUNSET_OPEN
      IF ( LUNSET_OPEN )THEN
        CALL D0OPEN (LUNSET,'SRCP_REST0','OFL',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_REST','F')
      ENDIF
C
      LUNLV0_OPEN = DO_LEVEL_ZERO
      IF ( LUNLV0_OPEN )THEN
        CALL D0OPEN (LUNLV0,'SRCP_LV00','OFL',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_LV0','F')
      ENDIF
C
C **********************************************************
C ****  Convert raw data into GEANT geometry parameters
C **********************************************************
C
C ****  Write Mixtures etc. to SRCP_REST
C
      IF ( DO_REST ) THEN
        REMARK = 'REST; Mixtures Dead material etc.'
        WRITE (6,100) REMARK
C
        CALL D0OPEN (LUNRAW,'SRCP_RAW_REST','I',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_RAW_REST','F')
        CALL EZREAD (LUNRAW,'REST',8,0,0) ! Read in REST raw data
        CLOSE(UNIT=LUNRAW)
C
        CALL DO_MISC (LUNSET)
      ENDIF
C
C ****  DO CRYOSTAT
C
      IF ( DO_CRYOSTAT ) THEN
        REMARK = 'CRYOSTAT GEOMETRY'
        WRITE (6,100) REMARK
C
        CALL D0OPEN (LUNRAW,'SRCP_RAW_CRY','I',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_RAW_CRY','F')
        CALL EZREAD (LUNRAW,'CRYOSTAT',8,0,0) ! Read in Cryostat raw data
        CLOSE(UNIT=LUNRAW)
C
        CALL CRYCEN (LUNCEN) ! Do Central cryostat geometry
        CALL CRYEND (LUNEND) ! Do Endcap cryostat geometry
        CALL CRYSET (LUNSET) ! Do Cryostat set definitions
      ENDIF
C
C
C ****  DO CENTRAL CALORIMETER
C
      IF ( DO_CENTRAL ) THEN
        REMARK = 'CENTRAL CALORIMETER'
        WRITE (6,100) REMARK
C
        CALL D0OPEN (LUNRAW,'SRCP_RAW_CC','I',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_RAW_CC','F')
        CALL EZREAD (LUNRAW,'CENTRAL',8,0,0) ! Read in Cryostat raw data
        CLOSE(UNIT=LUNRAW)
C
        CALL CC_GEOM (LUNCEN,LUNSET)
      ENDIF
C
C
C ****  DO ENDCAP CALORIMETER
C
      IF ( DO_ENDCAP ) THEN
C
        CALL D0OPEN (LUNRAW,'SRCP_RAW_EC','I',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_RAW_EC','F')
        CALL EZREAD (LUNRAW,'ENDCAP',8,0,0) ! Read in Endcap raw data
        CLOSE(UNIT=LUNRAW)
C
        CALL EZGET_i  ('PRE_PREGEO',II,ERROR)
C
        IF ( ERROR .NE. 0 ) THEN
          REMARK = 'ENDCAP CALORIMETER'
          WRITE (6,100) REMARK
C
          CALL DO_EM_GEOM         !DO EM geometry
          CALL DO_IH_GEOM         !DO IH geometry
          CALL DO_MH_GEOM         !DO MH geometry
          CALL DO_OH_GEOM         !DO OH geometry
          CALL EC_ROT_MATRICES    !Put out Rotation matrices
          CALL EC_MIXTURES        !Put out mixtures
          CALL EC_IUSETS          !Put out detector sets
        ELSE
C
C reduce IH-CONSTANTS.
C
          REMARK = 'ENDCAP PRE-PREGEO'
          WRITE (6,100) REMARK
C
          CALL IH_PRE_PRE        !DO RAW IH
        ENDIF
      ENDIF
C
C
C ****  DO ICD
C
      LUNICD = LUNEND  !   SAME UNIT NUMBER AS SRCP_ECAL
C
      IF ( DO_ICD ) THEN
        REMARK = 'INTER CRYOSTAT DETECTOR GEOMETRY'
        WRITE (6,100) REMARK
C
        CALL D0OPEN (LUNRAW,'SRCP_RAW_ICD','I',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_RAW_ICD','F')
        CALL EZREAD (LUNRAW,'ICD',8,0,0) ! Read in ICD raw data
        CLOSE(UNIT=LUNRAW)
C
        CALL MAKE_ICD(LUNICD,LUNSET) ! MAKE ICD SRCP FILE
      ENDIF
C
C
C ****  DO LV0
C
      IF ( DO_LEVEL_ZERO ) THEN
        REMARK = 'LEVEL ZERO GEOMETRY'
        WRITE (6,100) REMARK
C
        CALL D0OPEN (LUNRAW,'SRCP_RAW_LV0','I',OK)
        IF(.NOT.OK)CALL ERRMSG('CAL_PREGEO','CAL_PREGEO',
     &    'Can not open SRCP_RAW_LV0','F')
        CALL EZREAD (LUNRAW,'LV0',8,0,0) ! Read in Cryostat raw data
        CLOSE(UNIT=LUNRAW)
C
        CALL MAKE_LV0(LUNLV0) ! MAKE LV0 SRCP FILE
      ENDIF
C
C
C ****  ADD \SIZE RECORD AND CLOSE OUTPUT FILES
C
      WRITE(6,*)
     & ' Counting number of values and parameters for each file'
      IF ( LUNCEN_OPEN ) THEN
C
        CALL EZASIZ (LUNCEN,LUNOUT,'SRCP_UCAL',NVALS,NPARS)
        WRITE(6,*) ' SRCP_UCAL  \SIZE ',NVALS,NPARS
        CLOSE(UNIT=LUNCEN,STATUS='DELETE')
C
      ENDIF
C
      IF ( LUNEND_OPEN )  THEN
C
        CALL EZASIZ (LUNEND,LUNOUT,'SRCP_ECAL',NVALS,NPARS)
        WRITE(6,*) ' SRCP_ECAL  \SIZE ',NVALS,NPARS
        CLOSE(UNIT=LUNEND,STATUS='DELETE')
C
      ENDIF
C
      IF ( LUNSET_OPEN ) THEN
C
        CALL EZASIZ (LUNSET,LUNOUT,'SRCP_REST',NVALS,NPARS)
        WRITE(6,*) ' SRCP_REST  \SIZE ',NVALS,NPARS
        CLOSE(UNIT=LUNSET,STATUS='DELETE')
C
      ENDIF
C
      IF ( LUNLV0_OPEN ) THEN
C
        CALL EZASIZ (LUNLV0,LUNOUT,'SRCP_LV0',NVALS,NPARS)
        WRITE(6,*) ' SRCP_LV0   \SIZE ',NVALS,NPARS
        CLOSE(UNIT=LUNLV0,STATUS='DELETE')
C
      ENDIF
C
  100 FORMAT(/,'  *** DO ',A48,/)
  110 FORMAT(A80)
  999 RETURN
      END
