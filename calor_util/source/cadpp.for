      SUBROUTINE CADPP(IDET,ICRATE,IADC,IBLS,IROTOW,IDEPTH
     &,IBOX,IPPAT,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find the pulser configuration that will
C-                              pulse a given ADC specified channel
C-
C-   Inputs  : IDET    detector config as defined in CAL_PULSE_LIST.PARAMS
C-             ICRATE  adc crate
C-             IADC    adc board
C-             IBLS    bls board
C-             IROTOW  read out tower in the BLS board
C-             IDEPTH  depth in a read out tower
C-   Outputs : IBOX  pulser number [also preamp box number] 0 to 11
C-             IPPAT pulser pattern number  0 to 31
C-   Controls: ICOND from the return of a call to CADPR
C-
C-   Created   7-DEC-1988   James Kourlas  NYUHEP::KOURLAS
C-   Modified 16-JAN-1990   James Kourlas
C-   Updated  14-FEB-1992   Joan Guida  for ICD 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDET, ICRATE, IADC, IBLS, IROTOW, IDEPTH  ! ADC SCHEME
      INTEGER IBOX, IPPAT                               ! PULSER DESCRIPTION
      INTEGER ICOND, ICONDR
      INTEGER JPRBRD, JROTOW, JDEPTH                    ! PREAMP SCHEME
C----------------------------------------------------------------------
C
C     Get the PREAMP description [ i.e. ADC SCHEME -> PREAMP SCHEME ]
C
      CALL CADPR(IDET,ICRATE,IADC,IBLS,IROTOW,IDEPTH
     &,IBOX,JPRBRD,JROTOW,JDEPTH,ICOND)
C
C     Note: IBOX is both the PREAMP box number and the PULSER number.
C     Find the pulser pattern if the channel is valid:
C
      IF( ICOND .EQ. 0 ) CALL CPRPP(JPRBRD,JROTOW,JDEPTH,IPPAT,ICONDR)
      IF(ICOND .EQ. 2) CALL CPRPP_ICD(JPRBRD,JROTOW,JDEPTH,IPPAT,ICONDR)
C
  999 RETURN
      END
