      INTEGER FUNCTION tb90l2_get_module(layer,eta,phi)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines which module corresponds to the
C-                         coordinates by reading in the file
C-              d0$cms:[stp.tb90l2]tb90l2_physics_sort.dat.
C-              On first call builds a lookup table.
C-
C-   Returned value  :
C-      NOTTB90L2    -  indicies do not correspond to physical cell
C-      TB90L2EM     -  in the EM
C-      TB90L2FH     -  in FH
C-      TB90L2CH     -  in CH
C-      TB90L2OH     -  in OH
C-      TB90L2MH     -  in MH
C-      TB90L2ICD    -  ICD
C-      TB90L2CCMG   -  in CCMG
C-      TB90L2ECMG   -  in ECMG
C-      TB90L2ICDMG  -  in ICDMG
C-   Inputs  :
C-      layer        -  layer #
C-      eta,phi      -  eta,phi coordinate
C-   Outputs :
C-   Controls:
C-
C-   Created  11-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER layer, eta, phi
C      INCLUDE 'sys$library:foriosdef.for'
      INTEGER IOS_ENDFILERR 
      PARAMETER (IOS_ENDFILERR = 33) !   ENDFILE error 
      INCLUDE 'd0$params:tb90l2_modules.def'
      LOGICAL first_call
      INTEGER tb90l2_modules               ! (layer,eta,phi)
     &  (LAYER_MIN:LAYER_MAX,ETA_MIN:ETA_MAX,PHI_MIN:PHI_MAX)
      INTEGER UNIT                      ! unit # for file
      LOGICAL in_file                   ! true until end of file is reached
      INTEGER f_layer,f_eta,f_phi       ! (layer,eta,phi) read from file
      CHARACTER*2 f_module              ! module read from file
      INTEGER iostat                    ! holds status of read.
      LOGICAL OK                        ! error code
      INTEGER user,IER                  ! id for gtunit
      INTEGER i,j,k                     ! counter
      DATA first_call/ .true. /
      DATA user /138/
      SAVE first_call,tb90l2_modules
C----------------------------------------------------------------------
      IF (first_call) THEN
        first_call = .false.
C
C ****  initialize array to NOTTB90L2
C
        DO i = LAYER_MIN , LAYER_MAX
          DO j = ETA_MIN , ETA_MAX
            DO k = PHI_MIN , PHI_MAX
              tb90l2_modules(i,j,k) = NOTTB90L2
            ENDDO
          ENDDO
        ENDDO
C
C ****  Open file, read in data and stuff into tb90l2_modules
C
        CALL gtunit(user,unit,ier)
        IF ( ier .NE. 0 ) THEN
          CALL errmsg('GTUNIT','TB90L2_GET-MODULE','Bad unit number',
     &      'F')
          RETURN
        ENDIF
        CALL d0open(unit,'TB90L2_PHYSICS_SORT_DAT','IF',OK)
        IF ( .NOT. OK ) THEN
          CALL errmsg('D0OPEN','TB90L2_GET-MODULE','File not opened',
     &      'F')
          RETURN
        ENDIF
        in_file = .TRUE.
        i = 0
        DO WHILE ( in_file )                  ! set up infinite loop to read
          ! to end of file
C
C ****  The file was written with 64 data line + 2 header lines per page.
C ****  Count number of entries and if divisiable by 64 the trash next 2 lines.
C
          IF (MOD(I,64).EQ.0) read (UNIT,100)
          READ(UNIT,101,err=5100,iostat=iostat)
     &      f_module,f_eta,f_phi,f_layer
          IF ( f_module .EQ. 'EM' ) THEN
            tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2EM
          ELSEIF ( f_module .EQ. 'FH' ) THEN
            tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2FH
          ELSEIF ( f_module .EQ. 'CH' ) THEN
            tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2CH
          ELSEIF ( f_module .EQ. 'OH' ) THEN
            IF ( f_layer .EQ. 10 ) THEN
              tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2ECMG
            ELSE
              tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2OH
            ENDIF
          ELSEIF ( f_module .EQ. 'MH' ) THEN
            IF ( f_layer .EQ. 10 ) THEN
              tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2ECMG
            ELSE
              tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2MH
            ENDIF
          ELSEIF ( f_module .EQ. 'IC' ) THEN
            tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2ICD
          ELSEIF ( f_module .EQ. 'IM' ) THEN
            tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2ICDMG
          ELSEIF ( f_module .EQ. 'MG' ) THEN
            tb90l2_modules(f_layer,f_eta,f_phi) = TB90L2CCMG
          ELSE
            CALL errmsg('TB90L2_GET_MODULE','TB90L2_CALOR_HIST',
     &      'Bad module address','E')
          ENDIF
C
C ****  detect end of file here.
C
 5100     IF ( iostat .NE. 0) THEN
            IF ( (iostat .EQ. IOS_ENDFILERR) .OR. (iostat .LT. 0) )
     &         THEN                                       ! end of file
              in_file = .FALSE.                           ! reached
            ELSE
              CALL errmsg('TB90L2_GET_MODULE','TB90L2_CALOR_HIST',
     &          'Bad data file','E')
              in_file = .FALSE.
            ENDIF
          ENDIF
          i = i + 1
        ENDDO
        CALL rlunit(user,unit,ier)
      ENDIF
      tb90l2_get_module = tb90l2_modules(layer,eta,phi)
      RETURN
C
C ****  Come here if the file is not found
C
C
C ****  format statements
C ****  100     -       trash headers
C ****  101     -       read data
C
  100 FORMAT (/)
  101 FORMAT(66x,a2,14x,3i6)            ! don't read in quotes around mods.
      END
