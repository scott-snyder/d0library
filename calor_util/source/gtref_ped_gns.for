      SUBROUTINE GTREF_PED_GNS(TASK,IGN,CRATE,CARD,HEAD,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Values and Sigmas for PED/GAINS
C-                         STORED IN THE REFERENCE BANKS
C-   Inputs  : TASK =1,2 PEDS, =3 Gains
C-             CARD = ADC card number
C-             CARD = ADC card number
C-             IGN = 0 X8 Gain. =1 X1 Gain
C
C-   Outputs : HEAD(30) Contents of header bank
C-             VAL(768) Value,Sigma of channels
C-
C-   Created  5-JAN-88   Rajendran Raja
C-   Updated  16-NOV-1990   Jan Guida  Added CRATE argument, and ability 
C-                                      to do multiple crates 
C-   Updated   7-JUN-1991   Jan Guida  Pedestals:  TASK = 1 or 2 (add 2)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TASK,IGN,CARD,HEAD(*),LNKOLD,CRATE
      INTEGER KSTPO,KSCAL
C
      REAL VAL(*)
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCPDH.LINK'
      INCLUDE 'D0$LINKS:IZCGNH.LINK'
C----------------------------------------------------------------------
      KSTPO=LC(LSTPH-IZSTPO)
      IF ( KSTPO.NE.0 ) THEN
        KSCAL = LC(KSTPO-IZSCAL)
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
          RETURN
        ENDIF
      ELSE
        HEAD(1)=-1
        RETURN
      ENDIF
C
      CALL GT_PED_GNS(TASK,IGN,CRATE,CARD,HEAD,VAL)
C
      IF ( TASK.LT.3 ) THEN  !Reference Pedestals
        LCPDH=LNKOLD
      ELSE                   !Reference Gains
        LCGNH=LNKOLD
      ENDIF
C
  999 RETURN
      END
