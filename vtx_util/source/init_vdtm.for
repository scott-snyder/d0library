      SUBROUTINE INIT_VDTM(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Change the time banks for use with real data.
C-                         This involves adding several words per channel in
C-                         the VTMW banks and also hanging banks containing the
C-                         time to distance conversion table (VDTM) from VTMW.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: Returns OK = .TRUE. if everything works
C-
C-   Created   7-FEB-1991   Peter Grudberg from Ed's INITIALIZE_VTX
C-   Updated   9-Mar-1992   Herbert Greenlee
C-       Changed OPEN to D0OPEN.
C-   Updated   4-JUN-1992   Peter Grudberg  Read ZEBRAized DTM map 
C-   Updated   3-AUG-1992   Peter M. Grudberg  Dont overwrite tzeros;
C-                              handle xchange dtm files 
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
C
      LOGICAL OK
C
      REAL X, Y, TZERO, TSIGMA
      INTEGER NUM_CATEG(0:2), CATEG(0:31,0:2), IER, IUSER
      INTEGER LUN, LVTMW, GZVTMW, LAYER, SECTOR, WIRE, CAT
      INTEGER ITEMS, NWIRE, SIZE, POINT, LENGTH, ILEN, IVERS
      INTEGER NWIR, NSEC, NWORDS, LVDTM, OFFSET, VERSION
      PARAMETER ( VERSION = 1 )         ! version # of VTMW bank
      CHARACTER*60 DTM_FILE
      CHARACTER*80 TXT
      CHARACTER*7 NAME, XCHOP
      LOGICAL OPENED
      INTEGER TRULEN
C
      DATA IUSER / 793 /
C----------------------------------------------------------------------
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGET('NUM_CATEG', NUM_CATEG, IER)
      CALL EZGET('CATEG', CATEG, IER)
      CALL EZGET('DEF_TZERO', TZERO, IER)
      CALL EZGET('DEF_TSIGMA', TSIGMA, IER)
C
      CALL GTUNIT(IUSER, LUN, IER)
C
      OK = .TRUE.
      DO LAYER = 0, 2
        LVTMW = GZVTMW(LAYER)
        IF ( LVTMW .GT. 0 ) THEN
          IVERS = IBITS(IC(LVTMW),13,5)
          IF ( IVERS .LT. 1 ) THEN
            CALL MZDROP(IXSTP, LVTMW, ' ')
            CALL BKVTMW(LAYER, VERSION, LVTMW)
C
C ****  Fill VTMW with some default numbers
C
            NWORDS = IC(LVTMW + 3)
            NWIR = IC(LVTMW + 4)
            NSEC = IC(LVTMW + 5)
            OFFSET = NWIR*NWORDS*NSEC + 6
            DO SECTOR = 0, NSEC - 1
              IC(LVTMW + OFFSET + SECTOR) = CATEG(SECTOR,LAYER)
              DO WIRE = 0, 7
                POINT = LVTMW + 5 + (NWIR*SECTOR + WIRE) * NWORDS
                C(POINT + 1) = TZERO        ! Tzero for wire end 0
                C(POINT + 2) = TSIGMA       ! Tzero sigma for end 0
                C(POINT + 3) = TZERO        ! Tzero for wire end 1
                C(POINT + 4) = TSIGMA       ! Tzero sigma for end 1
                C(POINT + 5) = 1.           ! time - distance scale factor
              ENDDO                         ! loop over wire
            ENDDO                           ! loop over sector
          ELSE
            NWORDS = IC(LVTMW + 3)
            NWIR = IC(LVTMW + 4)
            NSEC = IC(LVTMW + 5)
            OFFSET = NWIR*NWORDS*NSEC + 6
            DO SECTOR = 0, NSEC - 1
              IC(LVTMW + OFFSET + SECTOR) = CATEG(SECTOR,LAYER)
            ENDDO                           ! loop over sector
          ENDIF
        ENDIF
C
C ****  Now read in DTM info for this layer (allow for different VDTM banks for
C ****  different sectors within the layer)
C
        DO CAT = 0, NUM_CATEG(LAYER) - 1
          WRITE(NAME,'(A,I1,A,I2.2)') 'DTM', LAYER, '_', CAT
          CALL EZGETS(NAME, 1, DTM_FILE, LENGTH, IER)
          CALL D0OPEN(LUN,DTM_FILE,'IU',OPENED)
          IF(.NOT.OPENED)GO TO 100
          WRITE(TXT,'(A,2I3)')
     &      'Using '//DTM_FILE(1:LENGTH)//' for layer,category
     &      ',LAYER,CAT
          LENGTH = TRULEN(TXT)
          CALL ERRMSG('Nonstandard VTX DTM',
     &                'INIT_VDTM',
     &                TXT(1:LENGTH),
     &                'I')
          CALL XZRECL(ILEN,XCHOP)
          CALL FZFILE(LUN,ILEN,XCHOP)
          LVTMW = GZVTMW(LAYER)
          CALL FZIN(LUN,IDVSTP,LVTMW,-(IZVDTM+CAT),' ',0,0)
          CALL FZENDI(LUN,'QT')
          CLOSE(LUN)
        ENDDO                           ! loop over category
      ENDDO                             ! loop over layer
      CALL RLUNIT(IUSER,LUN,IER)
      CALL EZRSET
  999 RETURN
  100 CONTINUE
      OK = .FALSE.
      CALL ERRMSG('Error opening file','INIT_VDTM',
     &          'DTM file not found','W')
      CALL EZRSET
      GO TO 999
      END
