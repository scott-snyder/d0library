      SUBROUTINE CRACK_CORR_CASH(LELPH,ECRACK,IER)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Estimate the energy loss at the module crack
C                         for a given electron(photon) using the CASH bank. 
C                         CASH bank should be already filled. Correct only
C                         eta < 1.1
C
C   Inputs  :   LELPH     [I]  PELC(or PPHO)  bank address
C   Outputs :   ECRACK    [R]  ENERGY correction.
C               IER       [I]  0 is OK.
C   Controls: None
C
C-     08-SEP-1992   W.G.D.Dharmaratna. The correction (ECRACK) has to
C                     to be added to the energy measured.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  I,J
      INTEGER  IND_PHI,IELC,LELPH
      REAL   TWOPI,ENERGY,ETA,PHI,PHIC_OFF,EM1_ARC
      INTEGER ILYR,ICHAN,IER,IPHI_EL(2),IPHI_ER(2),IETA,IPHI

      INTEGER NLYR,LCASH,LCACL,NCELL,PAKADD_WORD
      REAL ELIVE(2),CRACK_CORR,E_LYR(64,-12:12,11)
      REAL    ECOR,ECRACK
C&IF VAXVMS
      BYTE PAKADD_BYTE(4)
      EQUIVALENCE (PAKADD_WORD,PAKADD_BYTE)
C&ENDIF
      CHARACTER MSG*40
C
      DATA TWOPI/6.2831853/

C      -----------------------------------------------------------------
C
C
      IER    = 0.0
      ECRACK = 0.0
      CALL VZERO_i(IPHI_EL(1),2)
      CALL VZERO_i(IPHI_ER(1),2)
      CALL VZERO(ELIVE(1),2)

      LCACL = LQ(LELPH-2)
      IF (LCACL .LE. 0) THEN
        WRITE(MSG,1003) LCACL
        CALL ERRMSG('NO CACL BANK','CRACK_CORR_CASH',MSG,'W')
        IER = -1
        GOTO 999
      ENDIF
      PHI = Q(LCACL+12)      
      ETA = Q(LCACL+13)     
      IF (ABS(ETA).GT.1.1) GOTO 999  ! No correction
C
C**** check the crack position
      EM1_ARC = TWOPI/32.
      IND_PHI = PHI / EM1_ARC
      PHIC_OFF = PHI -IND_PHI*EM1_ARC
      IF (PHIC_OFF .GE. 0.5*EM1_ARC) THEN  ! next crack is closer
        IND_PHI = IND_PHI + 1
      ENDIF
      IF (IND_PHI .EQ. 0) IND_PHI = 32
      DO I = 1,2
        IPHI_EL(I) = 2*IND_PHI+1-I      ! two cells from each side
        IPHI_ER(I) = 2*IND_PHI+I
        IF( IPHI_ER(I).GT.64) IPHI_ER(I) = IPHI_ER(I) - 64
      ENDDO
C
      LCASH = LQ(LCACL-2)
      IF (LCASH .LE. 0) THEN
        WRITE(MSG,1003) LCASH
        CALL ERRMSG('NO CASH BANK','CRACK_CORR_CASH',MSG,'W')
        IER = -2
        GOTO 999
      ENDIF

      NCELL = IQ(LCASH+2)
      CALL VZERO(E_LYR(1,-12,1),100)
      DO J=1,2*NCELL-1,2
        PAKADD_WORD = IQ(LCASH+2+J)
        ENERGY      = Q(LCASH+3+J)
C&IF VAXVMS
        IETA = PAKADD_BYTE(4)
        IPHI = PAKADD_BYTE(3)
        ILYR = PAKADD_BYTE(2)
C&ENDIF
        IF((ILYR.GE.8.AND.ILYR.LE.10).OR.ILYR.GT.11) GOTO 300
        IF(ABS(IETA).GT.12) GOTO 300
        IF (IPHI.EQ.IPHI_EL(1).OR.IPHI.EQ.IPHI_EL(2)) THEN
          ELIVE(1) = ELIVE(1) + ENERGY
          E_LYR(IPHI,IETA,ILYR) = ENERGY
        ELSEIF(IPHI.EQ.IPHI_ER(1).OR.IPHI.EQ.IPHI_ER(2)) THEN
          ELIVE(2) = ELIVE(2) + ENERGY
          E_LYR(IPHI,IETA,ILYR) = ENERGY
        ENDIF
  300 ENDDO

      ECRACK=CRACK_CORR(ELIVE,IER)
      IF (IER .NE. 0) THEN
        ECRACK = 0.0
        CALL ERRMSG('ERROR FROM CRACK_CORR','CRACK_CORR_CASH',
     &      ' NO FURTHER CRACK CORRECTION DONE','W')
        GOTO 999
      ENDIF

C................................................................
 1003 FORMAT(' ADDRESS = ',I8)
  999 CONTINUE
      RETURN
      END
