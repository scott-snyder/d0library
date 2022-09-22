      SUBROUTINE CL2_CADTFL(DO_GNSCOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank CADT for
C-      CL2_xxxx unpacking: add the pointers to the CAGS bank
C-
C-
C-   Inputs  : CADT and CAGS banks
C-   Outputs : revised CADT bank
C-   Controls: DO_GNSCOR     = .TRUE. if want DBL3 electronic gain correction 
C-
C-   Created 26-APR-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DO_GNSCOR
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INTEGER L2CADT
      INTEGER PAKADR
      INTEGER SCALE,INDEX,INDMAX
      INTEGER JCRATE,CRATEID
      INTEGER IETA,IPHI,ILYR
C&IF VAXVMS,VAXELN,LINUX
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C&ELSE
C&      EXTERNAL MVBITS
C&ENDIF
      LOGICAL CEXIST
      INTEGER INDCAGS
      REAL CL2_CAGS                     ! function to get CAGS value
      REAL CAGS,CAGS1,CAGS8             ! sin * adctogev * electr gain corr
      REAL BIG                          ! extreme value of above
      REAL STEPLN                       ! ln of step factor in table
C----------------------------------------------------------------------
C
C ****  Loop over CAD addresses to find and deduce nearest CAGS entry
C
      CALL CL2_INI                      ! get links to CADT and CAGS banks
      SCALE = 0
C
C...get info needed to calculate which CAGS table entry is closest
      BIG= C(L2CAGS+11)
      STEPLN = C(L2CAGS+14)
      DO CRATEID = 1,12                 ! Level 2 numbering
        L2CADT = LL2CADT(CRATEID)
        JCRATE = IC(L2CADT+2)            ! hardware crate number
        INDMAX = IC(L2CADT-1) - 3
        DO INDEX = 0,INDMAX
          PAKADR = IC(L2CADT + INDEX + 3)
C&IF VAXVMS,VAXELN,LINUX
          IETA = BYTES(4)
          IPHI = BYTES(3)
          ILYR = BYTES(2)
C&ENDIF
          IF(CEXIST(IETA,IPHI,ILYR)) THEN
C          IF(IETA.NE.0) THEN            ! assume CADTFL called CEXIST
C
C...CAGS is factor to multiply ADC counts by to get ETNOM in GEV
C             SCALE    = 0 ==> X8; = 1 ==> X1
            SCALE = 1
            CAGS1 =CL2_CAGS(JCRATE,INDEX,SCALE,IETA,IPHI,ILYR,DO_GNSCOR)
            SCALE = 0
            CAGS8 =CL2_CAGS(JCRATE,INDEX,SCALE,IETA,IPHI,ILYR,DO_GNSCOR)
            CAGS = SQRT(CAGS1*CAGS8)    ! compromise
            IF (CAGS.LE.0) CAGS = MAX(CAGS1,CAGS8)
            IF (CAGS.LE.0) THEN
              IC(L2CADT+INDEX+3) = 0     ! turn off channel if both bad
              CALL ERRMSG('CALORIMETER','CL2_CADTFL',
     &          'NO GAIN AVAILABLE; CHANNEL KILLED','W')
            ELSE
              INDCAGS = .5 + LOG(BIG/CAGS)/STEPLN
C
C...now store pointer as 8 bit UNSIGNED integer
              CALL MVBITS(INDCAGS,0,8,IC(L2CADT+INDEX+3),0)
            ENDIF
          ELSE
            IC(L2CADT+INDEX+3) = 0   ! Kill nonexistent if CADTFL didn't
          ENDIF
        ENDDO
      ENDDO
  999 RETURN
      END
