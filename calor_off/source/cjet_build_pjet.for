      LOGICAL FUNCTION CJET_BUILD_PJET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add PJET banks to existing ISAJET data.
C-                         If PJET exists, a new PJTHD bank is created
C-                         with the new set of PJET banks below.
C-                         Note: This version of the routine does not
C-                         update the reference links which point to ISAQ.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-JAN-1990   Chip Stewart
C-   Updated  19-JAN-1990   Harrison B. Prosper
C-      Protect link LISAE
C-   Updated  23-MAR-1990   Boaz Klima
C-      Changes made for a separated PJET package
C-   Updated   8-APR-1991   Scott Snyder
C-      Comment out EZPICK call, as the bank seems not to be used here.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER
      LOGICAL EZERR
      INTEGER LTEMP,LPJHD,LOLD,LISAE,LISAQ
      INTEGER GZISAE,GZPJHD
      INTEGER IZTEMP,IFLAG,ND,NS,NBANK,NTEMP,NISAE,ILINK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZISAQ.LINK'
      INCLUDE 'D0$LINKS:IZPJHD.LINK'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      CJET_BUILD_PJET = .TRUE.
      IF (FIRST) THEN
        CALL INZLNK                       ! Initialize ZLINKA
C
C$$$        CALL EZPICK('PJET_RCP')       ! SELECT PJET RCP BANK
C$$$        IF (EZERR(IER)) THEN
C$$$          CALL ERRMSG('PJET','PJET',
C$$$     &      'PJET RCP bank not found in PJET.','W')
C$$$        ENDIF
        FIRST = .FALSE.
      ENDIF
C
C ****  Check if PJET exists; check number of links in ISAE
C
      LISAE = GZISAE ()
      IF ( LISAE .LE. 0 ) THEN
        CALL ERRMSG ('PJET-bank','BUILD_PJET',
     &    'No ISAE bank','W')
        GOTO 999
      ENDIF
C
      ND = IQ(LISAE-1)                  ! Number of ISAE data words
      NS = IQ(LISAE-2)                  ! Number of ISAE structural links
C
      IF ( NS .GE. IZPJHD ) GOTO 998    ! Build PJET banks anyway
      LPJHD = GZPJHD ()
      IF ( LPJHD .GT. 0 )   GOTO 999
C
C ****  Reserve links in ZLINKA and book temporary
C ****  stand-alone bank 'TEMP'
C
      CALL GSLINK('TEMP',NTEMP)         ! Temporary header for OLD ISAE
      CALL GSLINK('BANK',NBANK)         ! Banks beneath old ISAE
      CALL GSLINK('ISAE',NISAE)         ! New ISAE
C
      CALL MZBOOK (IXMAIN,LSLINK(NTEMP),0,2,'TEMP',1,1,1,2,0)
C
C ****  ZSHUNT ISAE structure over to LTEMP
C
      IZTEMP = 1                        ! link under TEMP to hang old ISAE
      IFLAG  = 1                        ! Whole structure shunted
      LISAE  = GZISAE ()                ! Address of old ISAE
      CALL ZSHUNT( IXCOM, LISAE, LSLINK(NTEMP), -IZTEMP, IFLAG)
C
C ****  BOOK NEW ISAE - with more links and copy old ISAE data to new ISAE
C
      CALL BKISAE ( LSLINK(NISAE) )
C
C ****  Copy data from old ISAE to new one
C
      LOLD = LQ (LSLINK(NTEMP) - IZTEMP )   ! GO DOWN TO OLD ISAE BANK
      CALL UCOPY ( IQ(LOLD+1), IQ(LSLINK(NISAE)+1), ND)
C
C ****  Loop over links in old ISAE bank and shunt structures
C ****  to new ISAE.
C
      DO ILINK = 1, NS
        LOLD = LQ (LSLINK(NTEMP) - IZTEMP )    ! GO DOWN TO OLD ISAE BANK
        LSLINK(NBANK) = LQ (LOLD  - ILINK )    ! GO DOWN TO STRUCTURE
C                                              ! Beneath ISAE
C
        IF( ILINK .NE. IZISAQ .AND. LSLINK(NBANK).GT.0 ) THEN
C
C ****  SHUNT THIS STRUCTURE TO NEW ISAE
C
          CALL ZSHUNT(IXCOM,LSLINK(NBANK),LSLINK(NISAE),-ILINK,IFLAG)
C
        ELSEIF ( LSLINK(NBANK).GT.0 ) THEN
C
C ****  Build up new ISAQ banks (with extra reference link for PJET)
C ****  beneath new ISAE. Copy old ISAQ banks over one at a time.
C ****  Note: ILINK (is now) = IZISAQ and LSLINK(NBANK) (is now)
C ****  address of first OLD ISAQ bank.
C
          ND = IQ( LSLINK(NBANK) - 1)   ! Number of data words in old ISAQ
    5     CONTINUE
          CALL BKISAQ( LISAQ )          ! Book NEW ISAQ bank
          CALL UCOPY (IQ(LSLINK(NBANK)+1), IQ(LISAQ+1), ND)
          LQ(LISAQ-1) = LQ(LSLINK(NBANK)-1)     ! ISAJ reference link
C
          LSLINK(NBANK)= LQ(LSLINK(NBANK))      ! GO TO next OLD ISAQ BANK
          IF (LSLINK(NBANK).GT.0) GOTO 5
        END IF
      END DO
C
C ****  Drop temporary bank and unreserve links in ZLINKA
C
      CALL MZDROP (IXMAIN,LSLINK(NTEMP),' ')
      CALL RSLINK('TEMP',NTEMP)
      CALL RSLINK('ISAE',NISAE)
      CALL RSLINK('BANK',NBANK)
C
C ****  DO PJET CALCULATIONS - IF  NO RCP FILE THEN
C ****  USE DEFAULTS SET IN PJPGET
C
  998 CONTINUE
      CALL PJET_RCP('PJET_RCP')
      CALL PJETFL
  999 RETURN
      END
