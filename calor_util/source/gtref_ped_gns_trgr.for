      SUBROUTINE GTREF_PED_GNS_TRGR(TASK,CRATE,CARD,HEAD,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the values and sigmas for PED/GAINS
C-                          from the CPTR, CGTR banks  (cal trigger channels)
C-                         STORED IN THE REFERENCE BANKS
C-   Inputs  : TASK = 1,2 peds, 3 gains
C-             CRATE - ADC crate number
C-             CARD  - ADC card number
C-   Outputs : HEAD(30) - contents of header bank
C-             VAL(128) - mean,sigma of each channel
C-   Controls: none
C-
C-   Created  24-JUL-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,CARD,HEAD(*),LNKOLD,CRATE
      INTEGER KSCAL,GZSCAL
C
      REAL VAL(*)
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
C----------------------------------------------------------------------
      KSCAL = GZSCAL('STPO')
      IF ( KSCAL.NE.0 ) THEN
C
C SET UP LCPDH OR LCGNH FOR REF.
C
        IF ( TASK.LT.3 ) THEN  !Reference Pedestals
          LNKOLD=LCPDH
          LCPDH=LC(KSCAL-IZCPDH)
        ELSE                   !Reference Gains
          LNKOLD=LCGNH
          LCGNH=LC(KSCAL-IZCGNH)
        ENDIF
C
      ELSE
        HEAD(1)=-1
        GO TO 999
      ENDIF
C
      CALL GT_PED_GNS_TRGR(TASK,CRATE,CARD,HEAD,VAL)
C
      IF ( TASK.LT.3 ) THEN  !Reference Pedestals
        LCPDH=LNKOLD
      ELSE                   !Reference Gains
        LCGNH=LNKOLD
      ENDIF
C
  999 RETURN
      END
