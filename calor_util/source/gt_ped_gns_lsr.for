      SUBROUTINE GT_PED_GNS_LSR(TASK,ISCL,CRATE,CARD,HEAD,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the values and sigmas for PED/GAINS
C-                          from the CPDH and CGLZ banks (ICD channels)
C-
C-   Inputs  : TASK = 1,2 peds, 3 gains, 10 corrected gains
C-             ISCL - 0 = x8, 1 = x1
C-             CRATE - ADC crate number
C-             CARD  - ADC card number
C-   Outputs : HEAD(30) - contents of header bank
C-             VAL(16) - mean,sigma of each channel
C-   Controls: none
C-
C-   Created  22-APR-1994   Jan Guida
C-   Updated   2-NOV-1994   Jan Guida  Add corrected gains bank 
C-   Updated  14-MAR-1995   Jan Guida  Fix unpacking of cal ped bank
C-                                      (electronics channels addressing) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,HEAD(*),LINK
      INTEGER CRATE,CARD
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCGLZ.LINK'
      INCLUDE 'D0$LINKS:IZCLZ1.LINK'
      INCLUDE 'D0$LINKS:IZCLZ8.LINK'
      INCLUDE 'D0$LINKS:IZCLZC.LINK'
C
      REAL VAL(*)
      CHARACTER*80 STRING
      INTEGER GZCGLZ,GZCLZ8,GZCLZ1,LCGLZ
      INTEGER LENGTH,LINKH,LZFIND,ISCL
      INTEGER IADC,IBLS,ITWR,IDEP,LBLS,ICH,CHAN,IX
C
      DATA LENGTH/18/
C----------------------------------------------------------------------
      LINK = 0
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   !Finds Bank with Crate
        IF (LCPDH.LE.0) THEN
          WRITE(STRING,10)CRATE
   10     FORMAT(' ERROR in GT_PED_GNS_LSR: ',
     &      'no pedestal bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
        IF ( ISCL.EQ.0 ) THEN     !X8 gains
          LINKH = LC(LCPDH-IZCPD8)
        ELSEIF ( ISCL.EQ.1 ) THEN !X1 gains
          LINKH = LC(LCPDH-IZCPD1)
        ENDIF
        LINK  = LZFIND(IDVSTP,LINKH,0,11)   !Finds bank with first card
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   !Finds Bank with Crate
        IF (LCGNH.LE.0) THEN
          WRITE(STRING,11)CRATE
   11     FORMAT(' ERROR in GT_PED_GNS_LSR: ',
     &      'no gains bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
        LCGLZ = LC(LCGNH-IZCGLZ)
        IF (LCGLZ.LE.0) THEN
          WRITE(STRING,12)CRATE
   12     FORMAT(' ERROR in GT_PED_GNS_LSR: ',
     &      'no laser bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
        IF (TASK.LT.10) THEN      ! Normal gains
          IF ( ISCL.EQ.0 ) THEN     !X8 gains
            LINK = LC(LCGLZ-IZCLZ8)
          ELSEIF ( ISCL.EQ.1 ) THEN !X1 gains
            LINK = LC(LCGLZ-IZCLZ1)
          ENDIF
        ELSE                        ! Corrected gains
          LINK = LC(LCGLZ-IZCLZC)
        ENDIF
      ENDIF
C
      IF (LINK.GT.0) THEN
        CALL UCOPY_i(IC(LINK),HEAD,NHEAD)
        IF (TASK.LT.3) THEN         ! peds => get ICD chans from CPD8/1 banks
          ICH = 1
          ITWR = 2
          DO LBLS = 0,7
            IDEP = MOD(LBLS+CARD*8,12)
            IADC = CARD - (-1 + ISIGN(1,IDEP-LBLS-1))/2
            IX = MOD(IADC,3)
            IBLS = IX*(IX+1) + 1
            CHAN = IBLS*48+ITWR*12+IDEP !ADC channel number - electronics units
            LINK  = LZFIND(IDVSTP,LINKH,CARD,11)   !Finds Bank with Card
            IF ( LINK.GT.0 ) THEN
              VAL(ICH) = C(LINK+NHEAD+2*CHAN+1)
              VAL(ICH+1) = C(LINK+NHEAD+2*CHAN+2)
              ICH = ICH+2
            ENDIF
          ENDDO
        ELSE                          ! GAINS
          CALL UCOPY(C(LINK+NHEAD+CARD*LENGTH+1),VAL,LENGTH)
        ENDIF
      ELSE
        HEAD(1)=-1
      ENDIF
  999 RETURN
      END
