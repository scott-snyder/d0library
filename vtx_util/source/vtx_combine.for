      SUBROUTINE VTX_COMBINE(LAYER,SECTOR,WIRE,END,MATCHED,
     &  TIME,PEAK,STAT,IRAW,VSECHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Combine pulse data from two ends of a VTX wire into a
C-                         single hit.  Convert time to distance, do charge
C-                         division, and calculate area.  Results go into array
C-                         VSECHIT (and eventually into VSEC)
C-
C-   Inputs  : LAYER, SECTOR, WIRE: location in VTX
C-             END: if matched = end with larger area; if unmatched = end with
C-                    data
C-             MATCHED (L) = .TRUE. if handling two VWDA hits matched in time
C-             TIME(0:1) (F) : Drift time for each end (NOT Tzero-subtracted)
C-             PEAK(0:1) (F) : Peak height for each end
C-             STAT(0:1) (I) : Stat bits from VCHT (in VWDA status word posn):
C-                             leading(2) and trailing(3) overlap (VCHT vers=0)
C-                             saturation(0)                      (VCHT vers=1)
C-             IRAW(0:1) (B) : four bins of raw data for each end, packed 8
C-                             bits/bin (data is raw - no ped subtraction or
C-                             linearization)
C-   Outputs : VSECHIT(10) (F) : Processed hit data
C-
C-   Created   4-NOV-1993   Peter Grudberg
C-   Updated   2-DEC-1993   Liang-Ping Chen, Al CLark fill byte 1 of STATUS
C-                          word [VSECHIT(10), with STAT, IRAW ] as well
C-   Updated  18-FEB-1994   Ed Oltman  trim drift to insure identical results
C-                          for RAW and STA processing; Include VCHT version
C-                          dependance
C-   Updated  14-JUL-1994   Liang-ping Chen use trim drift for MC as well
C-   Updated  30-AUG-1994   Liang-ping Chen Replace 1-wd UCOPY calls with EQUIVALEN 
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:VCHT.PARAMS'
C
      REAL TIME(0:1), PEAK(0:1)
      INTEGER LAYER, SECTOR, WIRE, END, IRAW(0:1)
      LOGICAL MATCHED
      REAL VSECHIT(10)
C
      REAL GAIN(0:1,0:7), PED(0:1,0:7), TZERO(0:1,0:7)
      REAL STAGGER(0:7), STAGGR(0:7)
      REAL VSECTIME, SLOPE, DPLS, DMIN, EPLS, EMIN, FACTOR
      REAL TIME_CUT(2,0:7,0:31,0:2), TPLS, TMIN
      REAL TZRO, RWTIME, XDRIFT(2), XERROR, TIMERR
      REAL ZCOORD, ZERROR, HALF_LENGTH(0:2)
      REAL AREA(0:1),TMPTIME
      INTEGER LAYSAV, SECSAV, BANKID, NWCHAN, NCHAN, NWSEC
      INTEGER IW, IE, PT, LVGNL, GZVGNL, LVPDL, GZVPDL
      INTEGER HITID, STATUS, IVERS,VCHT_VERS,LVCHT,GZVCHT
      REAL    R_HITID, R_STATUS
      EQUIVALENCE ( HITID,R_HITID), (STATUS,R_STATUS)
      INTEGER PLS_CATH_BIT, MIN_CATH_BIT, IBIN, NBINS
      INTEGER OFFS, BEGIN_SUM, END_SUM, PEAK_POS, IFADC
      INTEGER TABLE(0:255), IPEAKVAL, FIRST_BIN, BASELINE
      INTEGER NEWFLG, IVERSION, LGEAN, LHSTR, ND, MCVERS
      INTEGER LVRFT, GZVRFT, GZVGEH, IL, IS, MXSEC(0:2), IER
      INTEGER N_BEFORE, N_AFTER, BBP, BAP
      INTEGER LVTMW, GZVTMW
      INTEGER IEND, OFFSET,IBYTE,SWDA(0:1),STAT(0:1)
      INTEGER MAXCNT, IPED, ITIME
      REAL    BILIPT,VSECT0,VSECT1
      BYTE BVERSION(4)
      EQUIVALENCE ( BVERSION, IVERSION )
      LOGICAL RADIAL, USE_PEAK_FOR_AREA, SUBTRACT_FIRST_BIN
      LOGICAL FIRST, MCDATA
      PARAMETER ( PLS_CATH_BIT = 3 )
      PARAMETER ( MIN_CATH_BIT = 4 )
      DATA FIRST / .TRUE. /
      DATA SLOPE  / 0. /
      DATA RADIAL / .TRUE. /
      DATA LAYSAV, SECSAV / -1, -1 /
      DATA MXSEC / 15, 31, 31 /
      DATA TIMERR / 4. /  ! The infamous 4. from VWDA
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Get geometry stuff
C
        LVRFT = GZVRFT()
        DO IW = 0, 7
          STAGGER(IW) = C(LVRFT+31+IW)
        ENDDO
        LVGEH = GZVGEH()
        DO IL = 0, 2
          HALF_LENGTH(IL) = C(LVGEH + IC(LVRFT+IL*7+4))
        ENDDO
C
C ****  MC data?
C
        MCDATA = .FALSE.
        IF ( IQ(LHEAD+1) .GT. 1000 ) MCDATA = .TRUE.
C
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET( 'MAXCNT', MAXCNT, IER )
        CALL EZGET( 'BILIPT', BILIPT, IER )
        CALL EZGET('USE_PEAK_FOR_AREA',USE_PEAK_FOR_AREA,IER)
        IF ( .NOT. USE_PEAK_FOR_AREA ) THEN
          CALL EZGET('SUBTRACT_FIRST_BIN',SUBTRACT_FIRST_BIN,IER)
          CALL EZGET('AREA_BINS_BEFORE_PEAK',BBP,IER)
          CALL EZGET('AREA_BINS_AFTER_PEAK',BAP,IER)
          CALL EZGET('BINS_BEFORE_PEAK',N_BEFORE,IER)
          N_AFTER = 3 - N_BEFORE
          PEAK_POS = N_BEFORE + 1
          IF ( BBP .GT. N_BEFORE ) THEN
            CALL ERRMSG('Data not available','VTX_COMBINE',
     &        'Bins_before_peak too large, truncating','W')
          ENDIF
          BEGIN_SUM = MAX(PEAK_POS-N_BEFORE,PEAK_POS-BBP)
          IF ( BAP .GT. N_AFTER ) THEN
            CALL ERRMSG('Data not available','VTX_COMBINE',
     &        'Bins_after_peak too large, truncating','W')
          ENDIF
          END_SUM = MIN(PEAK_POS+N_AFTER,PEAK_POS+BAP)
          NBINS = END_SUM - BEGIN_SUM + 1
        ENDIF
C
C ****  Define NEWFLG for MC data
C
        IF ( MCDATA ) THEN
          CALL EZGET('MC_VERSION',MCVERS,IER)
          IF (MCVERS.LE.0) THEN ! use flag word in GEAN HSTR bank
            IF (LHEADR.GT.0) THEN
              LGEAN = LQ(LHEADR-IZGEAN)
            ELSE
              LGEAN = 0
            ENDIF
            IF (LGEAN.GT.0) THEN
              LHSTR=LQ(LGEAN-IZHSTR)
            ELSE
              LHSTR=0
            ENDIF
C
            IF (LHSTR.GT.0) THEN
              ND=IQ(LHSTR-1)        ! CHECK IF HSTR CONTAINS VERSION INFORMATION
              IF (ND.GE.21) THEN
                IVERSION = IQ(LHSTR+21)         ! READ VERSION NUMBER
                NEWFLG = BVERSION(BYTE3)        ! VTX is in third byte
                IF ( NEWFLG.NE.1 ) NEWFLG = 0
              ELSE
                NEWFLG = 0
              ENDIF
            ELSE
C
C  Else use word in EVENT_HEAD bank.
C
              IVERSION = IQ(LHEAD+13)         ! READ VERSION NUMBER
              NEWFLG = BVERSION(BYTE3)        ! VTX is in third byte
              IF ( NEWFLG.NE.1 ) NEWFLG = 0
            ENDIF
          ELSE
            IF (MCVERS.EQ.1) NEWFLG = 0 ! old MC
            IF (MCVERS.EQ.2) NEWFLG = 1 ! new MC
          ENDIF
C
C ****  For MCDATA, must have version 0 of VTMW bank.  Check the layer 0 bank
C
          LVTMW = GZVTMW(0)
          IVERS = IBITS(IC(LVTMW),13,5)
          IF ( IVERS .NE. 0 ) THEN
            CALL ERRMSG('Wrong bank version','VTX_COMBINE',
     &        'VTMW version 0 needed for MC data','F')
            GO TO 999
          ENDIF
        ELSE  ! for real data:
C
C ****  Fill TIME_CUT array with the times that form the border between the
C ****  "good" region and the cathode region
C
          DO IL = 0, 2
            DO IS = 0, MXSEC(IL)
              DO IW = 0, 7
                CALL VDTMAX(IW,IS,IL,TPLS,TMIN,DPLS,DMIN)
                TIME_CUT(1,IW,IS,IL) = TPLS
                TIME_CUT(2,IW,IS,IL) = TMIN
              ENDDO
            ENDDO
          ENDDO
C
C ****  Must have VTMW version 1
          LVTMW = GZVTMW(0)
          IVERS = IBITS(IC(LVTMW),13,5)
          IF ( IVERS .NE. 1 ) THEN
            CALL ERRMSG('Wrong bank version','VTX_COMBINE',
     &        'VTMW version 1 needed for real data','F')
            GO TO 999
          ENDIF
C
        ENDIF
C
        CALL EZRSET
C
      ENDIF
C
      VCHT_VERS = 0
      LVCHT = GZVCHT()
      IF (LVCHT .GT. 0) VCHT_VERS = IQ(LVCHT+1)
      IF ( LAYER.NE.LAYSAV .OR. SECTOR.NE.SECSAV ) THEN
        LAYSAV = LAYER
        SECSAV = SECTOR
        BANKID = LAYER * 2**9 + SECTOR * 2**4
C
C ****  Fill local GAIN, PED and TZERO arrays for the new sector
C
        LVGNL = GZVGNL(LAYER)
        NWCHAN = IC(LVGNL+3)
        NCHAN = IC(LVGNL+4)
        NWSEC = NWCHAN * NCHAN
        PT = LVGNL + 5 + 3*41 + SECTOR*NWSEC
        DO IW = 0, 7
          DO IE = 0, 1
            GAIN(IE,IW) = C(PT+1)
            PT = PT + NWCHAN
          ENDDO
        ENDDO
C
        IF (VCHT_VERS .EQ. 0) THEN
          LVPDL = GZVPDL(LAYER)
          NWCHAN = IC(LVPDL+3)
          NCHAN = IC(LVPDL+4)
          NWSEC = NWCHAN * NCHAN
          PT = LVPDL + 5 + SECTOR*NWSEC
          DO IW = 0, 7
            DO IE = 0, 1
              PED(IE,IW) = C(PT+1)
              PT = PT + NWCHAN
            ENDDO
          ENDDO
        ENDIF
C
        IF ( .NOT. MCDATA ) THEN
          LVTMW = GZVTMW(LAYER)
          NWCHAN = IC(LVTMW+3)
          NCHAN = IC(LVTMW+4)   ! Here, 1 chan is one wire
          NWSEC = NWCHAN * NCHAN
          PT = LVTMW + 5 + SECTOR*NWSEC
          DO IW = 0, 7
            TZERO(0,IW) = C(PT+1)
            TZERO(1,IW) = C(PT+3)
            PT = PT + NWCHAN
          ENDDO
        ELSE       ! MCDATA, versio 0 of VTMW
          LVTMW = GZVTMW(LAYER)
          DO IW = 0, 7
            TZERO(0, IW )= C(LVTMW + (SECTOR * IC(LVTMW+4) + IW) *
     &                   IC(LVTMW+3) + 5 +1)
            TZERO(1,IW)=  TZERO(0,IW)
          ENDDO
        ENDIF
C
C ****  Fill STAGGR array
C
        FACTOR = 1.
        IF ( MOD(SECTOR,2) .EQ. 1 ) FACTOR = -1.
        DO IW = 0, 7
          STAGGR(IW) = FACTOR * STAGGER(IW)
        ENDDO
C
      ENDIF
C
      HITID = BANKID + 2*WIRE + END
      VSECHIT(1)=R_HITID
C
C ****  Z position and error
C
      IF ( MATCHED ) THEN
        CALL VTQDIV(LAYER,SECTOR,WIRE,PEAK(0),PEAK(1),ZCOORD,ZERROR)
      ELSE
        ZCOORD = HALF_LENGTH(LAYER)
        IF ( END .EQ. 0 ) ZCOORD = - ZCOORD
        ZERROR = HALF_LENGTH(LAYER)
      ENDIF
      VSECHIT(4) = ZCOORD
      VSECHIT(6) = ZERROR
C
C ****  Drift time
C
      IF ( MATCHED ) THEN
        RWTIME = ( TIME(0) + TIME(1) ) / 2.
        TZRO   = ( TZERO(0,WIRE) + TZERO(1,WIRE) ) / 2.
        VSECT0 = TIME(0) - TZERO(0,WIRE) + TIME_OFF
        ITIME  = MAX0(0,MIN0(2**NBITTIME-1,NINT(VSECT0/TIME_LC)))
        VSECT0 = ITIME*TIME_LC - TIME_OFF
        VSECT1 = TIME(1) - TZERO(1,WIRE) + TIME_OFF
        ITIME  = MAX0(0,MIN0(2**NBITTIME-1,NINT(VSECT1/TIME_LC)))
        VSECT1 = ITIME*TIME_LC - TIME_OFF
        VSECTIME=(VSECT0+VSECT1)/2.
        STATUS = 3
      ELSE
        RWTIME = TIME(END)
        TZRO   = TZERO(END,WIRE)
        VSECTIME=RWTIME - TZRO + TIME_OFF
        ITIME  = MAX0(0,MIN0(2**NBITTIME-1,NINT(VSECTIME/TIME_LC)))
        VSECTIME=ITIME*TIME_LC - TIME_OFF
        STATUS = 2**END
      ENDIF
C
C ****  Drift position and error
C
      IF ( MCDATA ) THEN

        TMPTIME=VSECTIME + TZRO

        CALL VTGETX(LAYER, SECTOR, WIRE, TMPTIME, TIMERR, ZCOORD, 
     &    NEWFLG, VSECTIME, XDRIFT, XERROR)

        VSECHIT(2) = XDRIFT(1)  ! + phi drift (stagger subtracted already)
        VSECHIT(3) = XDRIFT(2)  ! - phi drift
        VSECHIT(5) = XERROR
      ELSE
        CALL VTX_DRIFT(WIRE,SECTOR,LAYER,VSECTIME,SLOPE,RADIAL,
     &    DPLS,DMIN,EPLS,EMIN)
        VSECHIT(2) = DPLS + STAGGR(WIRE)   ! + phi drift position
        VSECHIT(3) = DMIN + STAGGR(WIRE)   ! - phi drift position
        VSECHIT(5) = MAX(EPLS,EMIN)   ! drift error
        IF ( VSECTIME .GT. TIME_CUT(1,WIRE,SECTOR,LAYER) ) THEN
          STATUS = IBSET(STATUS,PLS_CATH_BIT)
        ENDIF
        IF ( VSECTIME .GT. TIME_CUT(2,WIRE,SECTOR,LAYER) ) THEN
          STATUS = IBSET(STATUS,MIN_CATH_BIT)
        ENDIF
      ENDIF
      VSECHIT(9) = VSECTIME
C
C     fill byte 1 of the VWDA STATUS word ( for VSEC)
C
      DO IEND=0,1
        IF (VCHT_VERS .EQ. 0) THEN
C
C ****  This section for either VCHT(vers=0) or RAW data --> STAT(IEND) contains
C ****  VWDA status word overlap bits (bits 2,3) --> need to compute saturation
C ****  and bilinear bits (0,1)
C
          IPED = NINT(PED(IEND,WIRE) )
          SWDA(IEND) = 0
          OFFSET=0
          DO IBYTE=1,4
            IF (IBITS(IRAW(IEND), OFFSET, 8)-IPED
     &                            .GE.INT(BILIPT)) THEN
              SWDA(IEND)= IBSET(SWDA(IEND),1)        ! peak in bilinear region
              IF (IBITS(IRAW(IEND), OFFSET, 8) .GE. MAXCNT) THEN
                SWDA(IEND)= IBSET(SWDA(IEND),0)      ! Saturation
              ENDIF
            ENDIF
            OFFSET=OFFSET+8
          ENDDO
          CALL MVBITS(STAT(IEND), 2, 2, SWDA(IEND),2)
        ELSE
C
C ****  This section is for VCHT version=1 -->  STAT(IEND) contians VWDA
C ****  saturation bit (bit 0)  Cannot compute overlap or bilinear bits
C
          SWDA(IEND) = STAT(IEND)
        ENDIF
      ENDDO
      CALL MVBITS(SWDA(0), 0, 4, STATUS, 8)
      CALL MVBITS(SWDA(1), 0, 4, STATUS,12)
C
      VSECHIT(10)=R_STATUS
C
C ****  Ionization and error
C
      IF ( USE_PEAK_FOR_AREA .OR. VCHT_VERS .EQ. 1) THEN
        AREA(0) = PEAK(0) / GAIN(0,WIRE)
        AREA(1) = PEAK(1) / GAIN(1,WIRE)
      ELSE    ! Use raw data to compute area
        DO IE = 0, 1
          AREA(IE) = 0.
          IF ( IRAW(IE) .NE. 0 ) THEN
            OFFS = 32 - 8*BEGIN_SUM   ! Find starting raw data byte
            DO IBIN = BEGIN_SUM, END_SUM
              IFADC = IBITS(IRAW(IE),OFFS,8)
              IFADC = MAX(IFADC-NINT(PED(IE,WIRE)),0)
              IFADC = TABLE(IFADC)
              AREA(IE) = AREA(IE) + FLOAT(IFADC)
              OFFS = OFFS - 8   ! go to next raw data byte
            ENDDO
            IF ( SUBTRACT_FIRST_BIN ) THEN
              OFFS = 32 - 8*PEAK_POS
              IPEAKVAL = IBITS(IRAW(IE),OFFS,8)
              IPEAKVAL = MAX(IFADC-NINT(PED(IE,WIRE)),0)
              IPEAKVAL = TABLE(IFADC)
              FIRST_BIN = IPEAKVAL - INT(PEAK(IE))
              BASELINE = FIRST_BIN*NBINS
              AREA(IE) = MAX(AREA(IE)-FLOAT(BASELINE),0.)
            ENDIF
          ENDIF
          AREA(IE) = AREA(IE) / GAIN(IE,WIRE)
        ENDDO
      ENDIF
      VSECHIT(7) = AREA(0) + AREA(1)
      VSECHIT(8) = SQRT(VSECHIT(7))   ! For now, AREAERR = SQRT(AREA)
C
  999 RETURN
      END
