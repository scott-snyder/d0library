      SUBROUTINE GTCPD1 (BANK,ADC,HEAD,VAL,ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return contents of bank CPD1, CPD8, CGN1 or
C-                         CGN8 depending upon the entry point. The
C-                         entry point names are:
C-                         
C-                         GTCPD1, GTCPD8, GTCGN1 or GTCGN8
C-
C-   Inputs  : BANK     Name of STP support bank (STPO,STPC,STPN)
C-             ADC      ADC card number
C-
C-   Outputs : HEAD(*) Contents of header bank  (30 full-words)
C-             VAL(*)  Values & sigmas          (768 full-words)
C-             ERR     Error code
C-                      0 .... OK
C-                     -1 .... Invalid support bank name
C-                     -2 .... STPx bank does not exist
C-                     -3 .... Calibration banks do not found
C-                     -4 .... Calibration bank for specified ADC not found
C-
C-   Controls: None
C-
C-   Created   6-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C ****  Arguments
C
      CHARACTER*(*)     BANK
      INTEGER           ADC                       ! ADC card number
      INTEGER           HEAD(*)
      REAL              VAL(*)
      INTEGER           ERR                       ! Error return code
      CHARACTER*4 STBANK
C
      INTEGER LBANK,LINK,LZFIND,IZLINK,I,J
C
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CACBNK.DEF'    ! Contains bank information
C----------------------------------------------------------------------
C
C ****  CPD1
C
      IZLINK = IZCPD1
      GOTO 50
C
C ****  CPD8
C
      ENTRY GTCPD8 (BANK,ADC,HEAD,VAL,ERR)
      IZLINK = IZCPD8
      GOTO 50
C
C ****  CGN1
C
      ENTRY GTCGN1 (BANK,ADC,HEAD,VAL,ERR)
      IZLINK = IZCGN1
      GOTO 50
C
C ****  CGN8
C
      ENTRY GTCGN8 (BANK,ADC,HEAD,VAL,ERR)
      IZLINK = IZCGN8
C
   50 CONTINUE
      ERR = 0
C
C ****  Get STPx bank address
C
      CALL UPCASE (BANK,STBANK)
      IF ( STBANK .EQ. 'STPO' ) THEN
        LBANK = LC(LSTPH-IZSTPO)
      ELSEIF ( STBANK .EQ. 'STPC' ) THEN
        LBANK = LC(LSTPH-IZSTPC)
      ELSEIF ( STBANK .EQ. 'STPN' ) THEN
        LBANK = LC(LSTPH-IZSTPN)
      ELSE
        ERR =-1
        GOTO 999
      ENDIF
C
      IF ( LBANK .LE. 0 ) THEN          ! Check if STPx bank exists
        ERR =-2
      ELSE
C
        LBANK = LC(LBANK-IZLINK)
        IF ( LBANK .LE. 0 ) THEN        ! Check if CPDx bank exists
          ERR =-3
        ELSE
C
          LINK  = LZFIND (IDVSTP,LBANK,ADC,CPADC)  ! Find bank for given ADC
          IF ( LINK .LE. 0 ) THEN
            ERR=-4
          ELSE
C
            CALL UCOPY (IC(LINK),HEAD,CNHEAD)   ! Get header words
            LINK = LINK + CNHEAD + 1
            CALL UCOPY (IC(LINK),VAL,CNDATA)    ! Get values and sigmas
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
