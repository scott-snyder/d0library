      SUBROUTINE VCOMBN( LAYER, SECTOR, WIRE, HIT1,
     &                   VTDAT1, HIT2, VTDAT2, VTHIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Combine data from two hits (from each end
C-   of wire) into a single combined hit, with the z coordinate given
C-   by charge division.
C-      This version uses only the (-z) hit (end=0) to determine the
C-   drift time, peak height and pulse width of the hit.  The pulse
C-   area returned is the sum of the areas of the component hits.
C-
C-   Inputs  : HIT1: id of (-z) hit
C-             VTDAT1: data for (-z) hits
C-             HIT2: id of (+z) hit
C-             VTDAT2: data for (+z) hit
C-             LAYER,SECTOR,WIRE: location in VTX
C-
C-   Outputs : VTHIT(NWVSEC): data for combined hit
C-
C-   Controls:
C-
C-   Created   1-FEB-1989   Peter Grudberg (from Chris Klopfenstein)
C-   Modified 28-JUN-1989   P. G. - redefined MXHTOT and fixed bug
C-   Modified 07-NOV-1989   P.G. - changed call to VTGETX (T0 subtraction)
C-   Updated   7-APR-1991   Peter Grudberg  Handle real data
C-   Updated  14-MAR-1992   Peter M. Grudberg  Add stagger to real data
C-   Updated   9-APR-1992   Peter M. Grudberg  Handle old RCP file
C-   Updated   5-JUN-1992   Peter Grudberg  Update status word,
C-                            time-to-distance,  and drift error
C-   Updated  11-SEP-1992   Alexandre Zinchenko - define NEWFLG to select
C-                          correct version of MC
C-   Updated  30-OCT-1992   Alexandre Zinchenko - add code to handle
C-                          "MC_VERSION" from RCP-file
C-   Updated  09-DEC-1992   Alexandre Zinchenko - check EVENT_HEAD for
C-                          MC version
C-   Updated  30-Sep-1993   Ed Oltman Change calling parameters to VTQDIV
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER MXHTOT, NWVSEC, NWVWDA, GZVRFT
      PARAMETER ( MXHTOT = 50 )         ! Max hits/wire-end
      PARAMETER ( NWVSEC = 10 )
      PARAMETER ( NWVWDA = 8 )
C
      REAL VTHIT(NWVSEC)
      INTEGER HIT1, HIT2
      REAL VTDAT1(NWVWDA,MXHTOT),VTDAT2(NWVWDA,MXHTOT)
C
      INTEGER STATUS, STAT1, STAT2, STATOR
      INTEGER LVRFT, LAYER, SECTOR, WIRE, END
      INTEGER LVTMW, GZVTMW, PTR, IVERS
C
      REAL RWTIME, TIME, TIMERR, AREA1, AREA2
      REAL XDRIFT(2), XERROR, ZCOORD, ZERROR
      REAL WIRLEN, TZERO
      REAL DPLS, DMIN, EPLS, EMIN, SLOPE
      LOGICAL MCDATA
C
      INTEGER MXSEC(0:2), LAY, SEC, WIR
      DATA MXSEC / 15, 31, 31 /
      REAL TIME_CUT(2,0:7,0:31,0:2), TPLS, TMIN
      REAL STAGGER, STAGGR(0:7), WIRLNGTH(0:2), DRIFT_ERROR, DEF_ERROR
      PARAMETER ( DEF_ERROR = .01 )       ! 100 micron default error
      INTEGER IWIR, ILAY, IER
      INTEGER PLS_CATH_BIT, MIN_CATH_BIT
      INTEGER NEWFLG, IVERSION, LGEAN, LHSTR, ND, MCVERS
      BYTE BVERSION(4)
      EQUIVALENCE (BVERSION, IVERSION)
      PARAMETER ( PLS_CATH_BIT = 3 )
      PARAMETER ( MIN_CATH_BIT = 4 )
      LOGICAL FIRST, RADIAL
      real     peak1,peak2
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LVRFT = GZVRFT()
        DO IWIR = 0, 7
          STAGGR(IWIR) = C(LVRFT+31+IWIR)
        ENDDO
        DO ILAY = 0, 2
          WIRLNGTH(ILAY) = 2. * C(LVGEH + IC(LVRFT+ILAY*7+4))
        ENDDO
        MCDATA = .FALSE.
        IF ( IQ(LHEAD + 1) .GT. 1000 ) MCDATA = .TRUE.
C
C ****  Define NEWFLG for MC data
C
        IF ( MCDATA ) THEN
          CALL EZPICK('VTRAKS_RCP')
          CALL EZGET('MC_VERSION',MCVERS,IER)
          CALL EZRSET
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
        ELSE
C
C ****  Fill TIME_CUT array with the times that form the border between the
C ****  "good" region and the cathode region
C
          DO LAY = 0, 2
            DO SEC = 0, MXSEC(LAY)
              DO WIR = 0, 7
                CALL VDTMAX(WIR,SEC,LAY,TPLS,TMIN,DPLS,DMIN)
                TIME_CUT(1,WIR,SEC,LAY) = TPLS
                TIME_CUT(2,WIR,SEC,LAY) = TMIN
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        RADIAL = .TRUE.
        SLOPE = 0.
      ENDIF
C
C ****  Monte carlo or real?
C
      IF ( IQ(LHEAD + 1) .GT. 1000 ) THEN
        MCDATA = .TRUE.
      ELSE
        MCDATA = .FALSE.
        STAGGER = STAGGR(WIRE)
        IF (MOD(SECTOR,2) .EQ. 1 ) STAGGER = - STAGGER
      ENDIF
      WIRLEN = WIRLNGTH(LAYER)        ! in cm
C  Get status word for both hits and combine
      STATUS = 0
      IF ( HIT1 .GT. 0 ) THEN
        CALL UCOPY(VTDAT1(8,HIT1),STAT1,1)
      ELSE
        STAT1 = 0
      ENDIF
      IF ( HIT2 .GT. 0 ) THEN
        CALL UCOPY(VTDAT2(8,HIT2),STAT2,1)
      ELSE
        STAT2 = 0
      ENDIF
C  Move low nibble of STAT1, STAT2 into byte 1 of STATUS
      CALL MVBITS( STAT1, 0, 4, STATUS, 8 )
      CALL MVBITS( STAT2, 0, 4, STATUS, 12 )
C  Store hit numbers for end 0 and 1 in byte 2 and 3
      CALL MVBITS( HIT1, 0, 8, STATUS, 16 )
      CALL MVBITS( HIT2, 0, 8, STATUS, 24 )
C  Now set the lowest two bits: set bit 0 for (-z) hit present,
C  bit 1 for (+z) hit present
      IF ( HIT1 .GT. 0 ) STATUS = IBSET( STATUS, 0 )
      IF ( HIT2 .GT. 0 ) STATUS = IBSET( STATUS, 1 )
C  Now handle the three cases separately: (-z) hit only, (+z) hit only,
C  both (-z) and (+z) hits
      IF ( HIT2 .EQ. 0 ) THEN           ! (-z) hit only
        END = 0
C  Channel number:
        CALL UCOPY(VTDAT1(1,HIT1),VTHIT(1),1)  ! -z channel number
C  Only one hit, so set z to the hit end and zerror=wirlen/2
        VTHIT(4) = - WIRLEN / 2.
        VTHIT(6) = WIRLEN / 2.
C  Calculate drift distance and error from drift time
        RWTIME = VTDAT1( 2, HIT1 )
        TIMERR = VTDAT1( 6, HIT1 )
        LVTMW = GZVTMW(LAYER)
        IF ( MCDATA ) THEN
C
C ****  Must have version 0 of VTMW bank
C
          IVERS = IBITS(IC(LVTMW),13,5) ! Get version number from status
                                        ! word
          IF ( IVERS .NE. 0 ) THEN
            CALL ERRMSG('Wrong bank version', 'VCOMBN',
     &                  'Using wrong version of VTMW', 'F')
            GO TO 999
          ENDIF
          CALL VTGETX( LAYER, SECTOR, WIRE, RWTIME, TIMERR,
     &                 ZCOORD, NEWFLG, TIME, XDRIFT, XERROR )
          VTHIT(2) = XDRIFT(1)
          VTHIT(3) = XDRIFT(2)
          VTHIT(5) = XERROR
        ELSE                            ! real data
C
C ****  Find T0 and subtract it from RWTIME.  This must use version 1 of the
C ****  VTMW bank.
C
          IVERS = IBITS(IC(LVTMW),13,5) ! Get version number from status
                                        ! word
          IF ( IVERS .NE. 1 ) THEN
            CALL ERRMSG('Wrong bank version', 'VCOMBN',
     &                  'Using wrong version of VTMW', 'F')
            GO TO 999
          ENDIF
          PTR = LVTMW + (SECTOR*IC(LVTMW+4)+WIRE)*IC(LVTMW+3) + 6
          TZERO = C(PTR+2*END)
          TIME = RWTIME - TZERO
C
C ****  Convert time --> distance using VDTM banks
C
          CALL VTX_DRIFT(WIRE,SECTOR,LAYER,TIME,SLOPE,RADIAL,
     &      DPLS,DMIN,EPLS,EMIN)
          VTHIT(2) = DPLS + STAGGER           ! x, assuming phi > 0
          VTHIT(3) = DMIN + STAGGER           ! x, assuming phi < 0
          VTHIT(5) = MAX(EPLS,EMIN)
          IF ( TIME .GT. TIME_CUT(1,WIRE,SECTOR,LAYER) ) THEN
            STATUS = IBSET(STATUS,PLS_CATH_BIT)
          ENDIF
          IF ( TIME .GT. TIME_CUT(2,WIRE,SECTOR,LAYER) ) THEN
            STATUS = IBSET(STATUS,MIN_CATH_BIT)
          ENDIF
        ENDIF
C  Load drift time from (-z) hit
        VTHIT(9) = TIME
C  Pulse area from (-z) only
        VTHIT(7) = VTDAT1( 3, HIT1 )
        VTHIT(8) = VTDAT1( 7, HIT1 )    ! pulse area error
C  Load status word
        CALL UCOPY(STATUS,VTHIT(10),1)  ! status word for combined hit
C  Now for (+z) hit only
      ELSEIF ( HIT1 .EQ. 0 ) THEN
        END = 1
C  Channel number = (+z) wire channel number
        CALL UCOPY(VTDAT2(1,HIT2),VTHIT(1),1)   ! +z channel number
C  One hit; z=(+z) end, zerror=wirlen/2
        VTHIT(4) = WIRLEN / 2.
        VTHIT(6) = WIRLEN / 2.
C  Drift distance:
        RWTIME = VTDAT2( 2, HIT2 )
        TIMERR = VTDAT2( 6, HIT2 )
        LVTMW = GZVTMW(LAYER)
        IF ( MCDATA ) THEN
C
C ****  Must have version 0 of VTMW bank
C
          IVERS = IBITS(IC(LVTMW),13,5) ! Get version number from status
                                        ! word
          IF ( IVERS .NE. 0 ) THEN
            CALL ERRMSG('Wrong bank version', 'VCOMBN',
     &                  'Using wrong version of VTMW', 'F')
            GO TO 999
          ENDIF
          CALL VTGETX( LAYER, SECTOR, WIRE, RWTIME, TIMERR,
     &                 ZCOORD, NEWFLG, TIME, XDRIFT, XERROR )
          VTHIT(2) = XDRIFT(1)
          VTHIT(3) = XDRIFT(2)
          VTHIT(5) = XERROR
        ELSE                            ! real data
C
C ****  Find T0 and subtract it from RWTIME.  This must use version 1 of the
C ****  VTMW bank.
C
          IVERS = IBITS(IC(LVTMW),13,5) ! Get version number from status
                                        ! word
          IF ( IVERS .NE. 1 ) THEN
            CALL ERRMSG('Wrong bank version', 'VCOMBN',
     &                  'Using wrong version of VTMW', 'F')
            GO TO 999
          ENDIF
          PTR = LVTMW + (SECTOR*IC(LVTMW+4)+WIRE)*IC(LVTMW+3) + 6
          TZERO = C(PTR+2*END)
          TIME = RWTIME - TZERO
C
C ****  Convert time --> distance using VDTM banks
C
          CALL VTX_DRIFT(WIRE,SECTOR,LAYER,TIME,SLOPE,RADIAL,
     &      DPLS,DMIN,EPLS,EMIN)
          VTHIT(2) = DPLS + STAGGER           ! x, assuming phi > 0
          VTHIT(3) = DMIN + STAGGER           ! x, assuming phi < 0
          VTHIT(5) = MAX(EPLS,EMIN)
          IF ( TIME .GT. TIME_CUT(1,WIRE,SECTOR,LAYER) ) THEN
            STATUS = IBSET(STATUS,PLS_CATH_BIT)
          ENDIF
          IF ( TIME .GT. TIME_CUT(2,WIRE,SECTOR,LAYER) ) THEN
            STATUS = IBSET(STATUS,MIN_CATH_BIT)
          ENDIF
        ENDIF
        VTHIT(9) = TIME
        VTHIT(7) = VTDAT2( 3, HIT2 )    ! pulse area from (+z) hit
        VTHIT(8) = VTDAT2( 7, HIT2 )    ! pulse area error
        CALL UCOPY(STATUS,VTHIT(10),1)  ! status word for comb. hit
C  Now for both ends hit
      ELSE
        AREA1 = VTDAT1( 3, HIT1 )
        AREA2 = VTDAT2( 3, HIT2 )
        IF ( AREA1 .GE. AREA2 ) THEN
          CALL UCOPY(VTDAT1(1,HIT1),VTHIT(1),1) ! -z channel number
        ELSE
          CALL UCOPY(VTDAT2(1,HIT2),VTHIT(1),1) ! +z channel number
        ENDIF
C  Calculate z from charge division
c        CALL VTQDIV( WIRLEN, AREA1, AREA2, ZCOORD, ZERROR )
        PEAK1 = VTDAT1( 5,HIT1)
        PEAK2 = VTDAT2( 5,HIT2)
        CALL VTQDIV(LAYER,SECTOR,WIRE,PEAK1,PEAK2,ZCOORD,ZERROR)
        VTHIT(4) = ZCOORD               ! z from charge division
        VTHIT(6) = ZERROR               ! error in z
C  Calculate drift distance; use the average of the two drift times
C  Note: z-dependent correction possible
        RWTIME = (VTDAT1(2,HIT1)+VTDAT2(2,HIT2))/2.
        TIMERR = VTDAT1( 6, HIT1 )
        LVTMW = GZVTMW(LAYER)
        IF ( MCDATA ) THEN
C
C ****  Must have version 0 of VTMW bank
C
          IVERS = IBITS(IC(LVTMW),13,5) ! Get version number from status
                                        ! word
          IF ( IVERS .NE. 0 ) THEN
            CALL ERRMSG('Wrong bank version', 'VCOMBN',
     &                  'Using wrong version of VTMW', 'F')
            GO TO 999
          ENDIF
          CALL VTGETX( LAYER, SECTOR, WIRE, RWTIME, TIMERR,
     &                 ZCOORD, NEWFLG, TIME, XDRIFT, XERROR )
          VTHIT(2) = XDRIFT(1)
          VTHIT(3) = XDRIFT(2)
          VTHIT(5) = XERROR
        ELSE                            ! real data
C
C ****  Find T0 and subtract it from RWTIME.  This must use version 1 of the
C ****  VTMW bank.
C
          IVERS = IBITS(IC(LVTMW),13,5) ! Get version number from status
                                        ! word
          IF ( IVERS .NE. 1 ) THEN
            CALL ERRMSG('Wrong bank version', 'VCOMBN',
     &                  'Using wrong version of VTMW', 'F')
            GO TO 999
          ENDIF
          PTR = LVTMW + (SECTOR*IC(LVTMW+4)+WIRE)*IC(LVTMW+3) + 6
          TZERO = (C(PTR)+C(PTR+2))/2.  ! Average the two ends
          TIME = RWTIME - TZERO
C
C ****  Convert time --> distance using VDTM banks
C
          CALL VTX_DRIFT(WIRE,SECTOR,LAYER,TIME,SLOPE,RADIAL,
     &      DPLS,DMIN,EPLS,EMIN)
          VTHIT(2) = DPLS + STAGGER           ! x, assuming phi > 0
          VTHIT(3) = DMIN + STAGGER           ! x, assuming phi < 0
          VTHIT(5) = MAX(EPLS,EMIN)
          IF ( TIME .GT. TIME_CUT(1,WIRE,SECTOR,LAYER) ) THEN
            STATUS = IBSET(STATUS,PLS_CATH_BIT)
          ENDIF
          IF ( TIME .GT. TIME_CUT(2,WIRE,SECTOR,LAYER) ) THEN
            STATUS = IBSET(STATUS,MIN_CATH_BIT)
          ENDIF
        ENDIF
        VTHIT(9) = TIME
C  Pulse area = sum of (-z) and (+z) pulse areas
        VTHIT(7) = VTDAT1( 3, HIT1 ) + VTDAT2( 3, HIT2 )
        VTHIT(8) = SQRT( VTDAT1(7,HIT1)**2
     &                  +VTDAT2(7,HIT2)**2 )   ! area error
        CALL UCOPY(STATUS,VTHIT(10),1)  ! status word for comb. hit
      ENDIF
  999 RETURN
      END
