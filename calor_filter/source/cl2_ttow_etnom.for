      SUBROUTINE CL2_TTOW_ETNOM(TTETA,TTPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert 1 trigger tower's worth of calorimeter
C-      data to nominal ET
C-
C-      WARNING: this is not strictly an analog of CL2_ROTOW_ETNOM, since it
C-              ALWAYS does the conversion, even if it's been done before
C-      It does conversion over a "normal" trigger tower's worth of address
C-              space, 64 channels
C-
C-   Inputs  : TTETA,TTPHI      the location of the trigger tower
C-   Outputs : CAEP bank (under FILT) ,PTCAEP2
C-   Controls:
C-
C-   Created   9-NOV-1990   James T. Linnemann
C-   Updated   1-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TTETA,TTPHI               ! input coords
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$INC:TTPH.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ADC_LOCATION.INC'
      INCLUDE 'D0$INC:CL2CRATE.INC'
      INTEGER NCH1,NCH2                 ! max channels in CAD1 and CAD2
      INTEGER IER                       ! error code from building pointers
      INTEGER LENTT
      PARAMETER( LENTT = 64*4  )        ! size in 16-bit address space
C                                       ! of a Trigger Tower
      INTEGER START,PAST                ! address limits of channels sought
      INTEGER LO,HI                     ! POINTERS to channels sought
      INTEGER IADC,CRATEID,NCAD         ! adc,crate, and cad bank
      INTEGER L2CAD                      ! link to CAD bank
      INTEGER CALVSN                    ! version number of data
      INTEGER ISHFT
      CHARACTER*4 ERRCODE
      SAVE IER
C
C.....temporary
      SAVE NCH1,NCH2
C------------------------------------------------------------------------
C
C...check if pointers to CADn are valid for current event
      IF (CL2CAD_EVT.NE.IQ(LHEAD+7)) THEN
C
C...build pointers to the beginning of each adc
        CALL VZERO(CRATE_CAD,NCALADC)   ! nothing found yet
        CALL CL2_FIND_ADCS(1,NCH1,L2CAD1,CALVSN,IER)
        NCH2 = 0
        IF(IER.EQ.0) CALL CL2_FIND_ADCS(2,NCH2,L2CAD2,CALVSN,IER)
        IF (IER.NE.0) THEN
          WRITE(ERRCODE,'(I4)')IER
C          CALL ERRMSG('CAL_DATA','CL2_TTOW_ETNOM',
C     &' Cal Data Format error'//ERRCODE//' found by CL2_FIND_ADCS','W')
        ELSE
          CALL BKCAEP(NCH1+NCH2,L2CAEP)
          IQ(L2CAEP+1) = 3002           ! level 2 version number
        ENDIF
        CL2CAD_EVT = IQ(LHEAD+7)
      ENDIF
      IF (IER.EQ.0) THEN
C
C...get hex address of channels corresponding to the requested trigger tower
        START = TTPH(TTETA,TTPHI,4)       ! first valid 16-bit address of TT
        IADC = ISHFT(START,-11)           ! pick off ADC number from address
C
C...first hex address outside TT
        PAST = START + LENTT              ! for most trigger towers
        IF (IABS(TTETA).GE.MNCTTE)  PAST = START + LENTT/4
        IF (IABS(TTETA).EQ.NETAL11) PAST = START + LENTT ! do all MG/ICDs
        START = ISHFT(START,16)           ! convert to 32 bits for search
        PAST = ISHFT(PAST,16)             ! convert to 32 bits for search
C
C...look up the pointer to the ADC's data
        CRATEID = L2CRATE(TTPH(TTETA,TTPHI,3)) ! change to crate in bank
        NCAD = CRATE_CAD(CRATEID)
        IF (NCAD.NE.0) THEN               ! be sure the crate was found
          IF (NCAD.EQ.1) L2CAD = L2CAD1
          IF (NCAD.EQ.2) L2CAD = L2CAD2
          CALL CL2_FIND_TTOW(IQ(L2CAD+1),ADC_POINT(IADC,CRATEID),
     &          START,PAST,LO,HI)
C...
C...and convert the channels from PH to ETNOM
          IF (LO.LE.HI) CALL CL2_MAKE_ETNOM(CRATEID,L2CAD+LO,L2CAD+HI)
          IF (IQ(L2CAEP+3).GT.NCH1+NCH2) THEN
            CALL ERRMSG('CAL_DATA','CL2_TTOW_ETNOM',
     &          ' NONZCH .GT. NCH1 + NCH2','E')
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
