      SUBROUTINE GT_PED_GNS_TRGR(TASK,CRATE,CARD,HEAD,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the values and sigmas for PED/GAINS
C-                          from the CPTR, CGTR banks  (cal trigger channels)
C-
C-   Inputs  : TASK = 1,2 peds, 3 gains
C-             CRATE - ADC crate number
C-             CARD  - ADC card number
C-   Outputs : HEAD(30) - contents of header bank
C-             VAL(128) - mean,sigma of each channel
C-   Controls: none
C-
C-   Created  22-APR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,HEAD(*)
      INTEGER CRATE,CARD
C
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
C
      REAL VAL(*)
      CHARACTER*80 STRING
      INTEGER LENGTH,LZFIND,LINK
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPTR.LINK'
      INCLUDE 'D0$LINKS:IZCGTR.LINK'
      DATA LENGTH/128/
C----------------------------------------------------------------------
      LINK = 0
      IF ( TASK.LT.3 ) THEN      !Pedestals
        LCPDH = LZFIND(IDVSTP,LCPDH,CRATE,9)   ! Finds bank with correct crate
        IF (LCPDH.LE.0) THEN
          WRITE(STRING,10) CRATE
   10     FORMAT(' ERROR in GT_PED_GNS_TRGR:  no ped bank for crate',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
        LINK = LC(LCPDH-IZCPTR)
        IF (LINK.EQ.0) THEN
          WRITE(STRING,11)CRATE
   11     FORMAT(' ERROR in GT_PED_GNS_TRGR: ',
     &      'no trigger pedestal bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
      ELSE                       !Gains
        LCGNH = LZFIND(IDVSTP,LCGNH,CRATE,9)   ! Finds bank with correct crate
        IF (LCGNH.LE.0) THEN
          WRITE(STRING,12) CRATE
   12     FORMAT(' ERROR in GT_PED_GNS_TRGR:  ',
     &      'no gains bank for crate',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
        LINK = LC(LCGNH-IZCGTR)
        IF (LINK.EQ.0) THEN
          WRITE(STRING,13)CRATE
   13     FORMAT(' ERROR in GT_PED_GNS_TRGR: ',
     &      'no trigger gain bank for crate ',I2)
          CALL INTMSG(STRING)
          HEAD(1)=-1
          GO TO 999
        ENDIF
      ENDIF
C
      IF (LINK.GT.0) THEN
        CALL UCOPY(IC(LINK),HEAD,NHEAD)
        CALL UCOPY(C(LINK+NHEAD+CARD*LENGTH+1),VAL,LENGTH)
      ELSE
        HEAD(1)=-1
      ENDIF
  999 RETURN
      END
