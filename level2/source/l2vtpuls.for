      SUBROUTINE L2VTPULS(LABEL, EXPDAT, MAXPUL,
     &  NPULSE, L2TIME, L2AREA, RAW, STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : search for hits in an FADC channel [LEVEL2 version]
C-
C-   Inputs  : LABEL: logical channel number: bit-packed location of 
C-                                            VTX layer, sector, wire
C-             EXPDAT     : unpacked raw data
C-             MAXPUL     : maximum number of hits allowed
C-
C-   Outputs : NPULSE     : number of pulses found
C-             integer L2TIME(MAXPUL)  : NINT( raw time/least_count )
C-             integer L2AREA(MAXPUL): BiLinear(PeakH-Ped)-BiLinear(FrontP-Ped)
C-             integer RAW(MAXPUL)   : packed 4 buckets of raw FADC data 
C-             integer STATUS(MAXPUL): status bits for each hit,
C-                       bit 0: 1 if saturated, 0 otherwise.
C-                       bit 1: 1 if peak in bilinear region
C-                       bit 2: 1 if there is overlap on leading edge
C-                       bit 3: 1 if there is overlap on trailing edge
C-                       bits 4-31: unused (yet)
C-             The above returned values extracted from VTPULS'
C-             HITLST(LPULSE,*)= content of each hit as described in the doc.
C-
C-   Created                Domenico Pizzuto
C-   Updated  29-APR-1994   Liang-ping Chen  for Level 2, EXPDAT is filled.
C-                                           Delete reference to ZDEXPD to
C-                                           eliminate link problem in level 2
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER MAXPUL, LPULSE, LFADC
      PARAMETER ( LPULSE = 8 )
      PARAMETER ( LFADC = 512 )
      INTEGER NATUR,NPULSE,ISUML,J
      INTEGER I,ILAST,IFIRST,ISUMF,S,INIT, IPEV
      INTEGER IPREAD, IFBIN, LMAX
      INTEGER EXPDAT(*)
      INTEGER EXPDATR(2*LFADC),RAWW,RAWB,BBP,POS
      INTEGER IOFS, LABEL, IFLAG, IAREA, IPED
      INTEGER KPVPDL
      INTEGER PULTH1(2), PULTH2(2), PULTH3(2), PULMAX(2)
      INTEGER LAYER, SECTOR, WIRE, END
      INTEGER GZL2VPDL, DCDTYP
      INTEGER IER
      INTEGER TABLE(0:255), MAXCNT
      INTEGER ITIME
      INTEGER L2TIME(*), L2AREA(*),RAW(*), STATUS(*)
      INTEGER HITID, MXHTOT
      PARAMETER (MXHTOT=500)
      REAL     HITLST(8,MXHTOT)
      INTEGER IHITLST(8,MXHTOT)
      EQUIVALENCE (HITLST,IHITLST)
C
C      REAL  HITLST(LPULSE,*), PULWEI(2)
      REAL  PULWEI(2)
      REAL  B(LFADC), COUNT, AREA, FLAG
      REAL  NBPBIN, SUM, SUMX, COEFF(50,2), COEF2(50,2)
      REAL  THR1, THR2, THR3, FPED, FRQNCY
      REAL BILIRT, BILIPT
      REAL TIME, LEAST_COUNT

      LOGICAL BILFLG, TBLFLG, NOIPED, USE_DEF, OVRLAP

      EQUIVALENCE ( FLAG, IFLAG )
      DATA INIT /0/
      DATA LEAST_COUNT / 0.25 /   ! least count value of time in VCHT (ns)
C----------------------------------------------------------------------
      IF (EXPDAT(1).EQ.0) THEN 
        NPULSE=0
        GOTO 999
      ENDIF
C 
      IF (INIT.EQ.0) THEN
        INIT = 1
C
C **** Get parameters from RCP
C
        CALL EZPICK( 'L2CDHT_RCP' )
        CALL EZGET( 'TABLE', TABLE, IER )
        CALL EZRSET
C
        CALL EZPICK( 'VTRAKS_RCP' )
        CALL EZGET( 'PULTH1', PULTH1, IER )
        CALL EZGET( 'PULTH2', PULTH2, IER )
        CALL EZGET( 'PULTH3', PULTH3, IER )
        CALL EZGET( 'PULMAX', PULMAX, IER )
        CALL EZGET( 'PULWEI', PULWEI, IER )
        CALL EZGET( 'FRQNCY', FRQNCY, IER )
        CALL EZGET( 'BILIRT', BILIRT, IER )
        CALL EZGET( 'BILIPT', BILIPT, IER )
        CALL EZGET( 'MAXCNT', MAXCNT, IER )
        CALL EZGET('USE_DEFAULT', USE_DEF, IER)
        CALL EZGET('BINS_BEFORE_PEAK',BBP,IER)
        IF ( USE_DEF ) THEN
          IF ( IQ(LHEAD+1) .GT. 1000 ) THEN
            TBLFLG = .FALSE.            ! MC DATA
            BILFLG = .TRUE.
          ELSE
            TBLFLG = .TRUE.             ! REAL (COSMIC) DATA
            BILFLG = .FALSE.
          ENDIF
        ELSE
          CALL EZGET( 'TBLFLG', TBLFLG, IER )
          CALL EZGET( 'BILFLG', BILFLG, IER )
        ENDIF
        CALL EZRSET
        IF ( FRQNCY .NE. 0 ) THEN
          NBPBIN = 1000. / FRQNCY         ! bin width in ns
        ELSE
          NBPBIN = 1000. / 106.
        ENDIF
        DO 7 J = 1, 2
          COEFF(1,J) = 1.
          COEF2(1,J) = 1.*COEFF(1,J)
          DO 4 I = 2, 50
            COEFF(I,J) = COEFF(I-1,J) * PULWEI(J)
            COEF2(I,J) = COEFF(I,J) * FLOAT(I)
    4     CONTINUE
    7   CONTINUE
      ENDIF
C
C
      NPULSE=0
      NOIPED = .FALSE.
C
      DCDTYP = 0
C
      IF( EXPDAT(1) .EQ. 0 ) GOTO 990
C
      NATUR = 1                       ! wire channel
      LAYER  = IBITS( LABEL, 9, 3 )
      SECTOR = IBITS( LABEL, 4, 5 )
      WIRE   = IBITS( LABEL, 1, 3 )
      END    = IBITS( LABEL, 0, 1 )
      KPVPDL = GZL2VPDL(LAYER)
      KPVPDL = KPVPDL + ( SECTOR*IC(KPVPDL+4) + 2*WIRE + END )
     &         * IC( KPVPDL+3 ) + 5
      FPED  = C( KPVPDL+1 )            ! Pedestal
      IPED = NINT( FPED )
C
      THR1 = FLOAT( PULTH1(NATUR) )
      THR2 = FLOAT( PULTH2(NATUR) )
      THR3 = FLOAT( PULTH3(NATUR) )
      IPREAD = 1
  100 IF( EXPDAT(IPREAD) .EQ. 0 ) GOTO 990
      CALL UCOPY(EXPDAT(IPREAD),EXPDATR(IPREAD),EXPDAT(IPREAD)+2)
      IFBIN = EXPDAT(IPREAD+1)
      LMAX  = EXPDAT(IPREAD  )
      IPEV  = IPREAD + 1
      IPREAD = IPREAD + LMAX + 2
      OVRLAP = .FALSE.
C
C ****  compute first difference
C
      B(1) = 0.
      IF ( BILFLG .OR. TBLFLG ) THEN
        NOIPED = .TRUE.
        EXPDAT(IPEV+1) = EXPDAT(IPEV+1) - IPED
        IF (EXPDAT(IPEV+1) .GT. 0) THEN
          IF ( TBLFLG ) THEN
            EXPDAT(IPEV+1) = TABLE(EXPDAT(IPEV+1))
          ELSE
            CALL ZBICVT(BILIPT, BILIRT, EXPDAT(IPEV+1))
          ENDIF
        ENDIF
        DO 15 I = 2, LMAX
          EXPDAT(IPEV+I) = EXPDAT(IPEV+I) - IPED
          IF (EXPDAT(IPEV+I) .GT. 0) THEN
            IF ( TBLFLG ) THEN
              EXPDAT(IPEV+I) = TABLE(EXPDAT(IPEV+I))
            ELSE
              CALL ZBICVT(BILIPT, BILIRT, EXPDAT(IPEV+I))
            ENDIF
          ENDIF
          B(I)=FLOAT( EXPDAT(IPEV+I) - EXPDAT(IPEV+I-1) )
   15   CONTINUE
      ELSE
        DO 5 I = 2, LMAX
          B(I)=FLOAT( EXPDAT(IPEV+I) - EXPDAT(IPEV+I-1) )
    5   CONTINUE
      ENDIF
C
C ****  search for three successive bins above threshold
C ****  or two successive bins above threshold with sum above threshold
C
      IFIRST=0
      ILAST=1
      ISUMF=0
      ISUML=0
   10 COUNT=0
      I = ILAST + 1
   11 IF( I .GT. LMAX-1 ) GOTO 100
      IF( B(I) .GE. THR1 ) THEN
        IF( B(I-1) .GE. THR1 ) THEN
          IF ((NOIPED .AND. EXPDAT(IPEV+I) .GE. 0) .OR.
     &       ((.NOT. NOIPED) .AND. EXPDAT(IPEV+I) .GE. IPED)) THEN
            IF ( B(I+1).GE.THR1 .OR. B(I)+B(I-1).GE.THR2 ) THEN
              IFIRST = I - 2
              IF( B(IFIRST) .LE. 0 ) IFIRST = I - 1
              ISUMF = I - 2
              GOTO 30
            ELSE
              I = I + 3
            ENDIF
          ELSE
            I = I + 1
          ENDIF
        ELSE
          I = I + 1
        ENDIF
      ELSE
        I = I + 2
      ENDIF
      GO TO 11
C
C ****   found pulse - find where 1st difference returns to zero
C
   30 ILAST=IFIRST
      ISUML=ISUMF
      IOFS  = 1
      SUM=0.
      SUMX=0.
      DO 40 I = IFIRST, LMAX
        IF(B(I).LE.0.)THEN
          ILAST=I
          ISUML=I
          GO TO 50
        ENDIF
        SUM  = SUM  + B(I) * COEFF(IOFS,NATUR)
        SUMX = SUMX + B(I) * COEF2(IOFS,NATUR)
        IOFS = IOFS + 1
   40 CONTINUE
      ILAST=LMAX
C
C ****  require pulse height exceeding threshold
C
   50 IF(EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1).LE.THR3)GO TO 10
C
C ****  calculate integral of pulse by truncating if a second pulse arrives
C ****  immediately, or if 3 consecutive differences are less than leading
C ****  edge threshold, or truncate if these conditions are not met after
C ****  mxtail number of bins after the pulse peak.
C
      IFLAG = 0.
      S=ISUML
   60 S=S+1
      COUNT=COUNT+1
      IF ((S+2).GE.LMAX.OR.(COUNT+3).GE.PULMAX(NATUR)) THEN
        ISUML = MIN(S+2, LMAX)
        IF ( OVRLAP ) IFLAG = IBSET(IFLAG,2)  ! Set lead overlap bit
        OVRLAP = .FALSE.
        GOTO 70
      END IF
      IF (B(S).GE.THR1 .AND. B(S+1).GE.THR1 .AND.
     +   (B(S+2).GE.THR1 .OR. B(S)+B(S+1).GE.THR2)) THEN
        IF ( OVRLAP ) IFLAG = IBSET(IFLAG,2)  ! Set lead overlap bit
        IFLAG = IBSET(IFLAG,3)               ! set trail overlap flag
        OVRLAP = .TRUE.
        ISUML=S-1
        GOTO 70
      END IF
      IF (-B(S).LE.THR1.AND.-B(S+1).LE.THR1 .AND.-B(S+2).LE.THR1) THEN
        ISUML=S
        IF (B(S+1).LE.0) THEN
          IF (B(S+2).LT.0) THEN
            ISUML = S+2
          ELSE
            ISUML= S+1
          ENDIF
        ENDIF
        IF ( OVRLAP ) IFLAG = IBSET(IFLAG,2)  ! Set lead overlap bit
        OVRLAP = .FALSE.
        GOTO 70
      END IF
      GOTO 60
C
C ****  compute the area & Correct it.  Set bits for saturation and bilinear
C ****  region.  Note that MAXCNT is no longer bilinear-converted.
C
   70 IAREA = 0

      DO 80 I=ISUMF,ISUML
        IAREA = IAREA + EXPDAT(IPEV+I)
   80 CONTINUE
      IF( EXPDATR(IPEV+ILAST-1) .GE. MAXCNT ) IFLAG = IBSET(IFLAG,0)
      IF( EXPDATR(IPEV+ILAST-1)-IPED .GE. INT(BILIPT) ) IFLAG =
     &  IBSET(IFLAG,1)
      IF ( NOIPED ) THEN
        AREA = FLOAT( IAREA )
      ELSE
        AREA = FLOAT( IAREA ) - (ISUML-ISUMF+1) * FPED
      ENDIF
C
      IF ( AREA .LT. THR3 ) GOTO 10

C
C ****  Use peak height for area
C
      AREA = EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1 )
C
C
C ****  STORE 4 RAW DATA BINS FOR USE IN VCHT BANK..
C
      POS = 24
      RAWW=  0
      DO I = 0,3
        IF ( (ILAST-1-BBP+I .GE. IFIRST) .AND.
     &       (ILAST-1-BBP+I .LE. LMAX)) THEN
          RAWB = EXPDATR(IPEV+ILAST-1-BBP+I)
        ELSE
          RAWB = 0
        ENDIF
        CALL MVBITS(RAWB,0,8,RAWW,POS)
        POS = POS - 8
      ENDDO
C
C ****  record pulse parameters
C
      NPULSE=NPULSE+1
      CALL UCOPY(LABEL,HITLST(1,NPULSE),1)
      TIME = NBPBIN*(SUMX/SUM + IFIRST-1 + IFBIN - .5)  ! SHIFT BIN CENTER
      ITIME = NINT(TIME/LEAST_COUNT)
      HITLST(2,NPULSE) = FLOAT(ITIME) * LEAST_COUNT
      HITLST(3,NPULSE) = AREA
      HITLST(4,NPULSE) = FLOAT((ILAST-IFIRST))*NBPBIN
      HITLST(5,NPULSE) = EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1 )
      CALL UCOPY(RAWW,HITLST(6,NPULSE),1)
      HITLST(7,NPULSE) = SQRT(ABS(AREA))    ! pulse area error
      HITLST(8,NPULSE) = FLAG
      IF( NPULSE .LT. MAXPUL ) GOTO 10
C
C ****  End of work. All peaks found ( ? )
C
  990 CONTINUE
C
      DO HITID=1, NPULSE 
C                   convert real to integer            
        L2TIME(HITID) = NINT(HITLST(2,HITID)/LEAST_COUNT)
        L2AREA(HITID) = INT(HITLST(3,HITID))
C                   copy bytes into integer 
        RAW(HITID)    = IHITLST(6,HITID)
        STATUS(HITID) = IHITLST(8,HITID)
      ENDDO
C
  999 RETURN
      END
