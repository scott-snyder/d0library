      SUBROUTINE CADPP_LV0(IDET,ICRATE,IADC,IBLS,IROTOW,IDEPTH
     &,IBOX,IPPAT,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find the pulser configuration that will
C-                              pulse a Level-0 channel (with laser)
C-                              (using CALIB addressing convention)
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
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDET, ICRATE, IADC, IBLS, IROTOW, IDEPTH  ! ADC SCHEME
      INTEGER IBOX, IPPAT                               ! PULSER DESCRIPTION
      INTEGER ICOND, ICONDR
      INTEGER JPRBRD, JROTOW, JDEPTH                    ! PREAMP SCHEME
C----------------------------------------------------------------------
      logical lfirst
      data lfirst/.true./

      if (lfirst) then
      call intmsg(' DUMMY VERSION OF CADPP_LV0 CALLED')
      lfirst = .false.
      endif

  999 RETURN
      END
