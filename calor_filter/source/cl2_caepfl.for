      SUBROUTINE CL2_CAEPFL(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book and fill CAEP bank starting from CAD1 and CAD2 USING CL2 UNPACK
C-   Inputs  : CAD1,CAD2,CADT,CAGS
C-      assumes full FILT path exists
C-   Outputs : CAEP and optionally partial PNUT under FILT, PTCAEP2 array;
C-      OK = .TRUE. if no errors
C-   Controls:
C-             CL2CAEP_EVT (shared with CL2_ROTOW_ETNOM and CL2_ICDMG_ETNOM)
C-                    says which event PTCAEP2 and TTDONE are valid for
C-             THIS_EVT (private) whether this routine called this event
C-
C-      all PTCAEP2 pointers are valid after this routine is called
C-
C-   Created  17-MAY-1991   James T. Linnemann
C-   Updated  17-SEP-1991   James T. Linnemann  patch pointer management
C-   Updated  17-FEB-1992   James T. Linnemann  AGAIN! implicitly assumed called
C-                                                first
C-   Updated  10-SEP-1992   James T. Linnemann  remove bad channels from PNUT
C-                              (makes PNUT verion 2 if correcting)
C-   Updated  18-DEC-1992   William Cobau  - Use GLOB bank to store E_TOTAL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK                        ! .TRUE. if no error found
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:TTDONE.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ADC_LOCATION.INC'
      INTEGER I
      INTEGER NCH1,NCH2                 ! max channels in CAD1 and CAD2
      INTEGER IER                       ! error code from building pointers
      INTEGER LO,HI                     ! POINTERS to channels sought
      INTEGER IADC,CRATEID,NCAD         ! adc,crate, and cad bank
      INTEGER L2CAD                      ! link to CAD bank
      INTEGER CALVSN                    ! version number of data
      CHARACTER*4 ERRCODE
      INTEGER NOT
      LOGICAL DO_PNUT,DO_CAEP
      INTEGER GZGLOB,GZPNUT,THIS_EVT
      LOGICAL SOMEBODY_STARTED,DONE_HERE
      SAVE NCH1,NCH2
      SAVE THIS_EVT
      DATA THIS_EVT/-999/
C----------------------------------------------------------------------
      OK = .FALSE.
C
C...restart record keeping if it's a new event
      SOMEBODY_STARTED = (CL2CAEP_EVT.EQ.IQ(LHEAD+7)) !either here or ROTOW
      DONE_HERE = SOMEBODY_STARTED.AND.(THIS_EVT.EQ.IQ(LHEAD+7)) !here?
      IF (DONE_HERE)  THEN
        OK = (IQ(L2CAEP+3).GT.0)  !ok as long as the previous bank was made
        GOTO 999     ! return
      ELSEIF (SOMEBODY_STARTED) THEN
        CALL MZDROP(IXCOM,L2CAEP,'L')       ! drop partial CAEP from CL2_ROTOW
        L2CAEP = 0
      ENDIF
      CALL VZERO(TTDONE,(2*NETAL11+1))    ! forget they were done
      TTDONE(0) = NOT(TTDONE(0))          ! no need to do eta = 0
      CALL CL2_VZERO_PTCAEP2              ! and reset pointers
C
C...build pointers to the beginning of each adc
      CALL VZERO(CRATE_CAD,NCALADC)   ! nothing found yet
      CALL CL2_FIND_ADCS(1,NCH1,L2CAD1,CALVSN,IER)
      NCH2 = 0
      IF (IER.EQ.0) CALL CL2_FIND_ADCS(2,NCH2,L2CAD2,CALVSN,IER)
      IF (IER.NE.0) THEN
        WRITE(ERRCODE,'(I4)')IER
C        CALL ERRMSG('CAL_DATA','CL2_CAEPFL',
C     & 'Cal Data Format error'//ERRCODE//' found by CL2_FIND_ADCS','W')
      ELSE
        CALL BKCAEP(NCH1+NCH2,L2CAEP)
        IQ(L2CAEP+1) = 3002           ! level 2 version number
        CALL BKPNUT(1)
        L2PNUT = GZPNUT(1)
        IQ(L2PNUT+1) = 3002           ! level 2 version number
        Q(L2PNUT+3) = 0                  ! since not preset
        Q(L2PNUT+4) = 0
        Q(L2PNUT+14) = 0
C- See GLOB exist, if no Book GLOB to hold E_TOTAL 
        L2GLOB = GZGLOB()
        IF ( L2GLOB.EQ.0 ) THEN
          CALL BKGLOB(l2GLOB)
          L2GLOB = GZGLOB()
        ENDIF
        IQ(L2GLOB+1) = 3001              ! level 2 version number
        Q(L2GLOB+2) = 0                 ! zero E_TOTAL
        DO CRATEID = 1,12               ! look over crates
          NCAD = CRATE_CAD(CRATEID)
          IF (NCAD.NE.0) THEN           ! be sure the crate was found
            IF (NCAD.EQ.1) L2CAD = L2CAD1
            IF (NCAD.EQ.2) L2CAD = L2CAD2
            DO IADC = 0,NADCC - 1                ! loop over ADCs in crate
              LO = L2CAD + ADC_POINT(IADC,CRATEID) + 1    ! 1st data word of ADC
              HI = LO + IQ(LO - 1) - 1        ! # chan in ADC is 0th word of ADC
C...
C...and convert the channels from PH to ETNOM
              IF( LO.LE.HI ) THEN
                CALL CL2_MAKE_ETNOM(CRATEID,LO,HI)
              ENDIF
              IF (IQ(L2CAEP+3).GT.NCH1+NCH2) THEN
                CALL ERRMSG('CAL_DATA','CL2_CAEPFL',
     &            ' NONZCH .GT. NCH1 + NCH2','E')
                GO TO 999
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        OK = (IQ(L2CAEP+3).GT.0)  !ok as long as the bank had data
        DO I = -NETAL11,NETAL11
          TTDONE(I) = NOT(0)                  ! all towers marked done
        ENDDO
        CL2CAEP_EVT = IQ(LHEAD+7)             ! flag that somebody started evt
        THIS_EVT    = CL2CAEP_EVT             ! flag that it was done here
      ENDIF
C
C...remove bad channels from PNUT
      DO_CAEP = .FALSE.       !channel is left in CAEP and PTCAEP2
      DO_PNUT = .TRUE.        !subtract out of PNUT
      CALL CL2_BAD_CELL_KILL(DO_CAEP,DO_PNUT) !makes a pass 2 bank
  999 CONTINUE
      IF ( .NOT.OK ) THEN
        L2PNUT = GZPNUT(1)
        IF ( L2PNUT.GT.0 ) THEN
          CALL MZDROP(IXCOM,L2PNUT,'L')       ! drop pnut if not ok
          L2PNUT = 0
        ENDIF
      ENDIF
      RETURN
      END
