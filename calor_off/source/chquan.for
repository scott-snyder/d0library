      SUBROUTINE CHQUAN(IVIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up H matrix quantities event by event
C-                         Both Dead material and Live material
C-                         Energies
C-
C-   Inputs  :
C-             IVIS = 0  Do dead material energies, impact points
C-             and all visibles.
C-             IVIS = 1  Do only LONGITUDINAL MATRIX visible quantities.
C-             IVIS = 2  Do only FULL MATRIX visible quantities.
C-             IVIS = 3  Do both Longitudinal and Full matrix visibles.
C-   Outputs :
C-   Controls:
C-
C-   Created   4-JUN-1989   Rajendran Raja
C-   Modified 9-FEB-1990 N.A. Graf  Changed structure of longitudinal
C-                                  vector.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:DEAD_MATERIALS.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CHMATR.INC'
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INTEGER NCACH,PCATE,NRCATE,I
      INTEGER PCAEH,IETA,IPHI,ILYR,DELETA,DELPHI,NRCAEH
      INTEGER NDATA,NCELL,NREP,POINTER,IADDR,JBIT,JBYT,IENFL,GZCAEP
      INTEGER IDEPTH
      INTEGER IX,IET,IPH
      INTEGER IVIS
      LOGICAL FULL_MATRIX,LONG_MATRIX,INVISIBLES
C----------------------------------------------------------------------
      IX(IET,IPH,ILYR) = 1 + (IET-NETLO) + (IPH-NPHLO)*NETTOT +
     &  (ILYR-1)*NETTOT*NPHTOT                ! INDEX STATEMENT FUNCTION.
C
      IF(IVIS.EQ.0)THEN
        INVISIBLES = .TRUE.
        LONG_MATRIX = .TRUE.
        FULL_MATRIX = .TRUE.
      ELSEIF ( IVIS.EQ.1 ) THEN
        INVISIBLES = .FALSE.
        LONG_MATRIX = .TRUE.
        FULL_MATRIX = .FALSE.
      ELSEIF ( IVIS.EQ.2 ) THEN
        INVISIBLES = .FALSE.
        LONG_MATRIX = .FALSE.
        FULL_MATRIX = .TRUE.
      ELSEIF ( IVIS.EQ.3 ) THEN
        INVISIBLES = .FALSE.
        LONG_MATRIX = .TRUE.
        FULL_MATRIX = .TRUE.
      ENDIF
C
      IF ( FULL_MATRIX ) THEN
        DO 50 I = 1 , NDIMH
          QUAN(I) = 0.0                   ! zero this every event.
   50   CONTINUE
      ENDIF
C
      IF ( LONG_MATRIX ) THEN
        DO 60 I = 1 , NDIML
          QUANTL(I) = 0.0                  ! Longitudinal profile quantities
   60   CONTINUE
      ENDIF
C
      LCACH = LQ(LCACL-IZCACH)          ! belongs to present CACL
      NCACH = IQ(LCACH+2)               ! Number of cells
      PCATE = IQ(LCACH+NCACH+3)         ! Pointer to CATE for maximum Cell
      NRCATE = IQ(LCATE+2)
      PCATE = LCATE + NRCATE*(PCATE-1)  ! True pointer
      ETAC = IQ(PCATE+12)
      PHIC = IQ(PCATE+13)               ! Eta and phi of maximum energy cell
C

      NRCAEH = IQ(LCAEH+2)              ! repetition number
C
      IF ( LONG_MATRIX ) THEN
        DO 100 I = 1,NCACH
          PCAEH = IQ(LCACH+I+2)
          PCAEH = LCAEH + NRCAEH*(PCAEH-1)
          IETA = IQ(PCAEH+12)
          IPHI = IQ(PCAEH+13)
          ILYR = IQ(PCAEH+14)
          IDEPTH = 0
C
C:::      EM LAYERS
C
          IF(ILYR.LT.LYEM3A)IDEPTH = ILYR  
          IF(ILYR.GE.LYEM3A .AND. ILYR.LE.LYEM3D)IDEPTH = 3
          IF(ILYR.GT.LYEM3D .AND. ILYR.LE.MXLYEM)IDEPTH = 4  
C
C:::      FH
C
          IF(ILYR.EQ.MNLYFH)IDEPTH = 5    ! FINE HADRONIC PUNCH THROUGH
          IF ( ILYR .GE. MNLYEM .AND. ILYR. LE.MXLYEM ) THEN
            QUANTL(IDEPTH) = QUANTL(IDEPTH) + Q(PCAEH+7)
          ELSEIF(ILYR.EQ.MNLYFH)THEN
            QUANTL(IDEPTH) = QUANTL(IDEPTH) + Q(PCAEH+7)
          ENDIF
  100   CONTINUE
      ENDIF
C
      IF ( FULL_MATRIX ) THEN
        DO 200 I = 1,NCACH
          PCAEH = IQ(LCACH+I+2)
          PCAEH = LCAEH + NRCAEH*(PCAEH-1)
          IETA = IQ(PCAEH+12)
          IPHI = IQ(PCAEH+13)
          ILYR = IQ(PCAEH+14)
          CALL CNEIGH1(ETAC,PHIC,IETA,IPHI,DELETA,DELPHI)
          IF ( ILYR .GE. MNLYEM .AND. ILYR. LE.MXLYEM ) THEN
            QUAN(IX(DELETA,DELPHI,ILYR)) =
     &    QUAN(IX(DELETA,DELPHI,ILYR))+ Q(PCAEH+7)  ! EM ENERGY
          ELSEIF(ILYR.EQ.MNLYFH)THEN
            QUAN(IX(DELETA,DELPHI,8)) =
     &    QUAN(IX(DELETA,DELPHI,8))+ Q(PCAEH+7)  ! FH ENERGY
          ENDIF
  200   CONTINUE
      ENDIF
C
C ****  NOW TO ACCUMULATE DEAD MATERIAL ENERGIES
C
      IF(INVISIBLES) THEN
C
        CALL PATHST('GEAN')               ! Set path to Gean
        LCAEP = GZCAEP()                ! Get link to CAEP bank
        LCAEP = LQ(LCAEP)               ! Need to get second CAEP bank
        IF ( LCAEP.LE.0 ) GOTO 888      ! Error - No data
        NDATA = IQ(LCAEP-1)             ! Number of data words
        NCELL = IQ(LCAEP+3)             ! Number of cells with data
        NREP = IQ(LCAEP+2)              ! Repetition number
        DO 300 I = 1, NCELL             ! Loop over cells
          POINTER = NREP*(I-1)+LCAEP    ! Pointer
          IADDR = IQ(POINTER+4)         ! Packed Addr in Physics Indices
          IETA = JBYT(IADDR,25,8)
          IF(IETA.GE.128)IETA = IETA -256 ! 2'S COMPLEMENT
          IPHI = JBYT(IADDR,17,8)
          ILYR = JBYT(IADDR,9,8)
          IENFL = JBIT(IADDR,6)           !Flag to see if Energy in Gev.
          IF(IENFL.NE.0)THEN
            CALL ERRMSG('CALORIMETER','CHMAXC',
     &      'DEAD MATERIAL ENERGY NOT IN GEV ','W')
          ENDIF
          IF(ILYR.GE.DEADLO.AND.ILYR.LE.DEADHI)THEN
            CALL CNEIGH1(ETAC,PHIC,IETA,IPHI,DELETA,DELPHI)
            IF(ILYR.EQ.CCRACK.OR.ILYR.EQ.ECRACK)THEN
              QUAN(IX(DELETA,DELPHI,9)) =
     &      QUAN(IX(DELETA,DELPHI,9)) + Q(POINTER+5) ! CRACK ENERGY
              QUANTL(6) = QUANTL(6) + Q(POINTER+5)
            ELSE
              QUAN(IX(DELETA,DELPHI,10)) =
     &      QUAN(IX(DELETA,DELPHI,10)) + Q(POINTER+5) ! REST OF DEAD ENERGY
              QUANTL(7) = QUANTL(7) + Q(POINTER+5)
            ENDIF
          ENDIF
  300 CONTINUE
  888 CONTINUE
      CALL PATHRS                       ! Reset to default path
      LCAEP = GZCAEP()                  ! Resetting to true CAEP
C
C ****  NOW TO WORK OUT EM3 IMPACT POINT.
C
      CALL CAL_EM_IMPACT

      DO 890 I = 1 , NPOSN
        QUAN(IPOSN+I-1) = DEL_IMPACT(I) ! FILLING IN POSITION INFO.
  890 CONTINUE
C
      ENDIF
  999 RETURN
      END
