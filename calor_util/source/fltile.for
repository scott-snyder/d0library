      SUBROUTINE FLTILE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill the TILE bank
C-
C-   Returned value  : TRUE if completed successfully
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   ENTRY GTTILE( LTILE, ITILE_ETA, ITILE_PHI, EMF, E, ETA_CATE,
C    &  PHI_CATE, IER )
C-   Inputs  :          LTILE     [I]     Pointer to TILE bank, if
C-                                        LTILE = 0, we find it ourselves
C-                      ITILE_ETA [I]     Tile eta index from 1 to 10
C-                      ITILE_PHI [I]     Tile phi index from 1 to 8
C-   Outputs :
C-                      EMF       [R]     Em fraction of this tile
C-                      E         [R]     Energy of this tile
C-                      ETA_CATE  [R]     Average IETA index of this tile
C-                      PHI_CATE  [R]     Average IPHI index of this tile
C-                      IER       [I]     Error code 0 = OK, -1 = No TILE bank
C-
C-
C-   Created  13-JAN-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE   'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE   'D0$INC:ZEBCOM.INC'
      INCLUDE   'D0$INC:PTCATE.INC'
      INTEGER    NTILE_ETA, NTILE_PHI          ! # of tiles in eta,phi
      PARAMETER( NTILE_ETA = 10)
      PARAMETER( NTILE_PHI = 8 )
      INTEGER    NTOW_ETA, NTOW_PHI
      PARAMETER( NTOW_ETA  = 8 )
      PARAMETER( NTOW_PHI  = 8 )
      INTEGER    NWORDS
      PARAMETER( NWORDS    = 1 )              ! # of words per tile
      REAL       SMALL
      PARAMETER( SMALL     = .01 )
      INTEGER    IEMF, IEMF_MAX, ISCALE, IENERGY, IENERGY_MAX
      INTEGER    IETA_TOWER, IPHI_TOWER, IETA_MAX_TOWER, IPHI_MAX_TOWER
      REAL       EMF, ENERGY, ENERGY_OFF, ENERGY_SCALE_1, ENERGY_MAX
      REAL       ETA_TOWER, PHI_TOWER
      PARAMETER( IEMF_MAX         = '7F'X )
      PARAMETER( IENERGY_MAX      = 'FFF'X )
      PARAMETER( IETA_MAX_TOWER      = '3F'X )
      PARAMETER( IPHI_MAX_TOWER      = '3F'X )
      PARAMETER( ENERGY_OFF       = 15.0   )
      PARAMETER( ENERGY_SCALE_1   = 100.   )
      PARAMETER( ENERGY_MAX       = 1000. )
      INTEGER    GZCATE
      EXTERNAL   GZCATE
      INTEGER    LTILE, IETA, IPHI, IP, IE, IP2, ITYPE, PACKWORD
      REAL       TIE, TIP, TET, TEH, TEM, TEA
      INTEGER    LCATE, NR_CATE, POINT, IETA_BASE, IPHI_BASE
      REAL       ETA_BASE, PHI_BASE
      PARAMETER( ETA_BASE = 0.0 )             ! Reference point
      PARAMETER( PHI_BASE = 0.0 )
C----------------------------------------------------------------------
C:  ENTRY GTTILE
C
      INTEGER   LTILE2, ITILE_ETA1, ITILE_PHI1, IER, LTILE1
      REAL      EMF1, ENERGY1, ETA_CATE1, PHI_CATE1
      INTEGER   ITILE_ETA4, ITILE_PHI4, IER4
      REAL      EMF4, ENERGY4, ETA_CATE4, PHI_CATE4
      REAL      ETA_CATE, PHI_CATE
      INTEGER   GZTILE, JBIT, JBYT
      EXTERNAL  GZTILE, JBIT, JBYT
C----------------------------------------------------------------------
C   Bit description of packed word
C                                                                   LSB
C   Electromagnetic Fraction - EMF - Range from 0. to 1.
C         Accuracy desired ~ 1% (0-127)  --> 7 bits  = 7F
C
C   Energy of tile           - ENERGY - two ranges (low/hi=SET) bit 1   = 1
C     Low range 0. to 100 GeV / Accuracy .020 GeV / 0 to 4000 / 12 bits = FFF
C     High range 100. to 1000./ Accuracy .20  GeV / 0 to 4000 / 12 bits
C
C   Average center in eta    - ETA_TOWER
C     Range from 0. to 7.     / Accuracy .1      / 0 to  63   /  6 bits = 3F
C
C   Average center in phi    - PHI_TOWER
C     Range from 0. to 7.     / Accuracy .1      / 0 to  63   /  6 bits = 3F
C                                                                     MSB
C                                                             ---------
C                                                              32 BITS
C----------------------------------------------------------------------
C
C:  Book TILE bank
C
      CALL BKTILE( NTILE_ETA, NTILE_PHI, NWORDS, LTILE )
      IF ( LTILE .LE. 0 ) GOTO 900            ! Error - couldnt book TILE
C
C:  Get pointer to CATE bank
C
      LCATE   = GZCATE()
      IF ( LCATE .LE. 0 ) GOTO 900            ! Error - no CATE bank
      NR_CATE = IQ( LCATE + 2 )
C
C:  Loop over tiles in the same order as the bank is laid out and fill it
C:  as we go. Start with ring of eta which are the smallest in value.
C
      DO IETA = 1, NTILE_ETA
        DO IPHI = 1, NTILE_PHI
C
C:  For each tile, get the following information: Hadronic Energy, Electro-
C:  magnetic Energy, Energy weighted eta, Energy weighted phi, Scalar ET,
C:  Vector Et,
          TIE   = 0.0
          TIP   = 0.0
          TET   = 0.0
          TEH   = 0.0
          TEM   = 0.0
          TEA   = 0.0

C
C:  Loop over all the CATE towers in this tile
C
          IPHI_BASE   = 0 + ( IPHI - 1 ) * NTOW_PHI
          IETA_BASE   = 1 + ( IETA - 1 ) * NTOW_ETA
          DO IP2 = IPHI_BASE, IPHI_BASE + NTOW_PHI - 1
            DO IE = IETA_BASE, IETA_BASE + NTOW_ETA - 1
              DO ITYPE =1, 2
                IP    = MOD( ( IP2 + 64 ) - 1, 64 ) + 1
                IF ( PTCATE( IE, IP, ITYPE ) .GT. 0 ) THEN
                  POINT = LCATE + ( PTCATE(IE,IP,ITYPE) - 1)*NR_CATE
C
                  TEA   = TEA + ABS( Q( POINT + 7 ) )
                  TIE   = TIE + ( IE - IETA_BASE ) * ABS(Q( POINT + 7 ))
                  TIP   = TIP + ( IP2 - IPHI_BASE  ) * ABS(Q( POINT + 7
     &              ))
                  TET   = TET + Q( POINT + 8)

                  IF ( ITYPE .EQ. 1 ) THEN
                    TEM   = TEM + Q( POINT + 7)
                  ELSE
                    TEH   = TEH + Q( POINT + 7)
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDDO                     ! End of loop over towers in tile
C
C:  Calculate total quantities
C

C:  EMFRACTION
          EMF     = 0.0
          IF ( ABS( TEM ) + ABS( TEH ) .GT. SMALL ) THEN
            EMF   = ABS( TEM )/( ABS( TEM ) + ABS( TEH ) )
          ENDIF

          IEMF    = ( MAX( MIN( EMF, 1. ), 0. ) ) * IEMF_MAX


C:  ENERGY
          ENERGY  = TEM + TEH + ENERGY_OFF

          IF ( ENERGY .GE. ENERGY_SCALE_1 ) THEN
            ISCALE  = 1
            IENERGY = IENERGY_MAX * (( ENERGY - ENERGY_SCALE_1 )/(
     &          ENERGY_MAX - ENERGY_SCALE_1) )
          ELSE
            ISCALE  = 0
            IENERGY = IENERGY_MAX* (ENERGY/ENERGY_SCALE_1)
          ENDIF

C:  ITOWER_ETA, ITOWER_PHI


          IF ( TEA .GT. SMALL ) THEN
            ETA_TOWER   = TIE/TEA
            PHI_TOWER   = TIP/TEA
          ELSE
            ETA_TOWER = 0.0
            PHI_TOWER = 0.0
          ENDIF

          IETA_TOWER  = IETA_MAX_TOWER * ( ETA_TOWER/FLOAT(
     &        NTOW_ETA -1 ) )
          IPHI_TOWER  = IPHI_MAX_TOWER * ( PHI_TOWER/FLOAT(
     &        NTOW_PHI -1 ) )

C
C: Fill zebra word
C
          IENERGY   = MAX( 0, MIN( IENERGY,  IENERGY_MAX ) )
          IEMF      = MAX( 0, MIN( IEMF,     IEMF_MAX    ) )
          IETA_TOWER= MAX( 0, MIN( IETA_TOWER, IETA_MAX_TOWER ) )
          IPHI_TOWER= MAX( 0, MIN( IPHI_TOWER, IPHI_MAX_TOWER ) )
C
          PACKWORD  = 0
C
          CALL SBYT( IEMF, PACKWORD, 1, 7 )
          IF ( ISCALE .EQ. 1 ) CALL SBIT1( PACKWORD,8)
          CALL SBYT( IENERGY, PACKWORD, 9, 12 )
          CALL SBYT( IETA_TOWER, PACKWORD, 21, 6 )
          CALL SBYT( IPHI_TOWER, PACKWORD, 27, 6 )

          POINT   = 4 + IPHI + (IETA - 1 )* NTILE_PHI * NWORDS
          IQ( LTILE + POINT ) = PACKWORD
C
C: DEBUG
C
c          PHI_CATE = IPHI_BASE + PHI_TOWER
c          ETA_CATE = IETA_BASE + ETA_TOWER
c          ENERGY   = ENERGY - 15.0
c          CALL GTTILE1( 0, IETA, IPHI, EMF4, ENERGY4, ETA_CATE4,
c     &      PHI_CATE4, IER4 )
c          IF ( ABS( EMF-EMF4) .GT. .02 ) TYPE *, IQ(LHEAD+9),EMF,EMF4
c          IF ( ABS( ENERGY-ENERGY4) .GT. .3 ) TYPE *, IQ(LHEAD+9),
c     &      ENERGY,
cc     &      ENERGY4
c          IF ( ABS( ETA_CATE-ETA_CATE4) .GT. .2 ) TYPE *, IQ(LHEAD+9),
c     &      ETA_CATE,ETA_CATE4
c          IF ( ABS( PHI_CATE-PHI_CATE4) .GT. .2 ) TYPE *, IQ(LHEAD+9),
c
c     &      PHI_CATE,PHI_CATE4
        ENDDO
      ENDDO                     ! Endo of loop over tiles

  999 RETURN
  900 CONTINUE                  ! Error
      CALL ERRMSG('Error in FLTILE','FLTILE','TILE bank not completed',
     &  'W')
      RETURN


C------------------------------------------------------------------
C ENTRY GTTILE : Unpack tiles from TILE bank
C------------------------------------------------------------------
      ENTRY GTTILE( LTILE1, ITILE_ETA1, ITILE_PHI1, EMF1, ENERGY1,
     &  ETA_CATE1,
     &  PHI_CATE1, IER )

      IER     = 0
      LTILE2  = LTILE1
C
C:  Check for TILE bank
C
      IF ( LTILE2 .LE. 0 ) LTILE2 = GZTILE()
      IF ( LTILE2 .LE. 0 ) THEN
        IER = -1
        GOTO 998
      ENDIF
C
C:  Are these eta,phi tile indices legal?
C
      IF ( ( ITILE_PHI1 .LT. 1 .OR. ITILE_PHI1 .GT. NTILE_PHI ) .OR.
     &     ( ITILE_ETA1 .LT. 1 .OR. ITILE_ETA1 .GT. NTILE_ETA ) ) THEN
        IER = -2
        GOTO 998
      ENDIF
C
C:  Find out which word we want
C
      POINT     = ITILE_PHI1 + ( ITILE_ETA1 - 1 )* NTILE_PHI * NWORDS +
     &  4
      PACKWORD  = IQ( LTILE2 + POINT )
      IEMF      = JBYT( PACKWORD, 1, 7 )
      IENERGY   = JBYT( PACKWORD, 9, 12)
      IETA_TOWER= JBYT( PACKWORD, 21, 6)
      IPHI_TOWER= JBYT( PACKWORD, 27, 6)
      ISCALE    = JBIT( PACKWORD, 8 )
C
C: Do conversions
C
      EMF1      = FLOAT( IEMF )/FLOAT( IEMF_MAX )
      IF ( ISCALE .EQ. 1 ) THEN
        ENERGY1 = (ENERGY_MAX - ENERGY_SCALE_1 )* FLOAT( IENERGY )
     &    /FLOAT( IENERGY_MAX )
        ENERGY1 = ENERGY1 + ENERGY_SCALE_1
      ELSE
        ENERGY1 = ENERGY_SCALE_1 * FLOAT( IENERGY )/FLOAT( IENERGY_MAX )
      ENDIF
      ENERGY1   = ENERGY1 - ENERGY_OFF
      ETA_CATE1 = FLOAT( (NTOW_ETA-1) * IETA_TOWER )/FLOAT(
     &  IETA_MAX_TOWER )
      PHI_CATE1 = FLOAT( (NTOW_PHI-1) * IPHI_TOWER )/FLOAT(
     &  IPHI_MAX_TOWER )
C
C:  Convert this to average CATE tower number
C
      ETA_CATE1 = ETA_CATE1 + 1 + ( ITILE_ETA1 - 1 ) * NTOW_ETA
      PHI_CATE1 = PHI_CATE1 + 0 + ( ITILE_PHI1 - 1 ) * NTOW_PHI
      IF ( PHI_CATE1 .LT. 0. ) PHI_CATE1 = PHI_CATE1 + 64.

  998 RETURN
      END

