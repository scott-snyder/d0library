      SUBROUTINE L2CDPULS(channel_id, depth, data, maxhit,
     &  npulse, L2time, L2area, L2peak, L2status)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find hits on one CDC channel, or
C-                         use in level-2.
C-
C-   Inputs  : integer channel_id : logical channel number
C-             integer depth : depth of FADC data
C-             integer data(depth) : One channel's FADC data
C-   Outputs : integer maxhit : maximum number of hits allowed
C-             integer npulse : number of hits found
C-             integer time(maxhit) : drift times of hits, in
C-                                    units 1/128 ns
C-             integer area(maxhit) : pulse area of hits
C-             integer peak(maxhit) : peak height of hits
C-             integer status(maxhit) : status byte for each hit,
C-                     bit 0 - saturation flag
C-                     bit 1 - overlap flag
C-                     bits 2/3 - unused as yet
C-                     bits 4-7 - pulse width in units of 4 FADC bins
C-                                (range 0 - 59 + overflow)
C-   Controls: none.
C-
C-   Created  11-DEC-1992   Chris Klopfenstein
C-   (adapted from CDPULS: Qizhong Li-Demarteau, O. Callot, D. Pizzuto)
C-   Modified 19-May-93 CK - Use Dan Claes routine to find CDC pedestal
C-   bank in L2 STP file.
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:ZebStp.inc'
      include 'd0$inc:cdpara.inc'
      integer gzdpdh, gzl2dpdh
      integer channel_id, depth, maxhit
      integer data(depth)
C      integer L2time(maxhit), L2area(maxhit)
C      integer L2peak(maxhit), L2status(maxhit)
C  mod for compatability with DSECHT
      integer mxhtot
      parameter (mxhtot=500)
      integer L2time(mxhtot), L2area(mxhtot)
      integer L2peak(mxhtot), L2status(mxhtot)
      integer expdat(2*lfadc), table(0:255)
      real b(lfadc), count, area
      integer npulse, iarea, iped, natur
      integer jdpdl
      integer layer, sector, wire
      integer ipread, ifbin, lmax, ipev
      integer ifirst, ilast, isumf, isuml, iofs
      integer s, iflag
      real fped, thr1, thr2, thr3
      integer pulth1(2), pulth2(2), pulth3(2), pulmax(2)
      real pulwei(2)
      real ffrqcy, bilirt, bilipt
      real ftime
      integer tscale, tmax, armax
      integer width, widmax
      integer err, maxpul, maxcnt
      logical bilflg, sbtrct, tblflg
      real nbpbin, sum, sumx, coeff(50,2), coef2(50,2)
      logical EZError
      integer i, ind, j
      logical first, inL2
      data first /.true./
C      parameter (tmax = 4096)
C      parameter (tscale = 64)
C----------------------------------------------------------------------
      if (first) then
        first = .false.
        CALL EZPICK('L2CDHT_RCP')
C temp fix
C        CALL EZPICK('DTRAKS_RCP')
        IF (EZERROR(ERR)) THEN
          CALL ERRMSG('L2CDHT','L2CDPULS', 
     &      'Unable to find bank L2CDHT','W')
          GOTO 999
        ENDIF
        call EZGET('INL2', INL2, err)
        CALL EZGET('FFRQCY',FFRQCY,ERR)
        CALL EZGET('BILFLG',BILFLG,ERR)
        CALL EZGET('BILIRT',BILIRT,ERR)
        CALL EZGET('BILIPT',BILIPT,ERR)
        CALL EZGET('MAXCNT',MAXCNT,ERR)
        CALL EZGET('SBTRCT',SBTRCT,ERR)
        CALL EZGET('PULTH1(1)',PULTH1(1),ERR)
        CALL EZGET('PULTH2(1)',PULTH2(1),ERR)
        CALL EZGET('PULTH3(1)',PULTH3(1),ERR)
        CALL EZGET('PULMAX(1)',PULMAX(1),ERR)
        CALL EZGET('PULWEI(1)',PULWEI(1),ERR)
        CALL EZGET('MAXPUL',MAXPUL,ERR)
        if (err .ne. 0) maxpul = maxhit
        CALL EZGET('TBLFLG',TBLFLG,ERR)
        CALL EZGET('TABLE(1)',TABLE(0),ERR)
        CALL EZGET('TSCALE',TSCALE,ERR)
        CALL EZGET('TMAX',TMAX,ERR)
        CALL EZGET('ARMAX',ARMAX,ERR)
        CALL EZGET('WIDMAX',WIDMAX,ERR)
        CALL EZRSET
        IF (BILFLG) CALL ZBICVT(BILIPT,BILIRT,MAXCNT)
        IF (TBLFLG) MAXCNT = TABLE(MAXCNT)
        DO 7 J = 1, 2
          COEFF(1,J) = 1.
          COEF2(1,J) = 1.*COEFF(1,J)
          DO 4 I = 2, 50
            COEFF(I,J) = COEFF(I-1,J) * PULWEI(J)
            COEF2(I,J) = COEFF(I,J) * FLOAT(I)
    4     CONTINUE
    7   CONTINUE
      endif
C
C  quit if empty channel
C
      if (data(1) .le. 0) goto 999
      npulse = 0
C
C  unpack layer, sector, wire from channel id, and fetch pedestal
C
      layer = IBITS(channel_id, 9, 2)
      sector = IBITS(channel_id, 4, 5)
      wire = IBITS(channel_id, 0, 4)
      if (ldpdh .le. 0) then
        if (INL2) then
          ldpdh = gzl2dpdh()
        else
          ldpdh = gzdpdh()
        endif
      endif
      JDPDL = LC( LDPDH - (LAYER+1) )
      JDPDL = JDPDL + ( SECTOR*IC(JDPDL+4)+WIRE ) *IC(JDPDL+3) + 4
      FPED  = C( JDPDL+1 )              ! Pedestal
      IPED = NINT( FPED )
C
C  check whether sense wire or delay line, set pulse-finding
c  thresholds accordingly
C
      IF ( WIRE .GE. NBSENS ) THEN
        NATUR = 2
      ELSE
        NATUR = 1
      ENDIF
      THR1 = FLOAT( PULTH1(NATUR) )
      THR2 = FLOAT( PULTH2(NATUR) )
      THR3 = FLOAT( PULTH3(NATUR) )
      IPREAD = 1
C
C  loop over clusters
C
  100 IF( data(ipread) .EQ. 0 ) GOTO 999
      IFBIN = data(ipread+1)
      LMAX  = data(ipread)
      IPEV  = IPREAD + 1
      IPREAD = IPREAD + LMAX + 2
      IF (FFRQCY .NE. 0) THEN
        NBPBIN = 1000. / FFRQCY
      ELSE
        NBPBIN = 1000. / 106.
      ENDIF
C
C ****  compute first difference
C
      B(1) = 0.
      IF (BILFLG .OR. TBLFLG) THEN
C        NOIPED = .TRUE.
C subtract pedestal
        data(ipev+1) = data(ipev+1) - iped
        IF (data(ipev+1) .GT. 0) THEN
C  bilinear conversion 
          IF (TBLFLG) THEN
            EXPDAT(IPEV+1) = Table(data(ipev+1))
          ELSE
            expdat(ipev+1) = data(ipev+1)
            CALL ZBICVT(BILIPT,BILIRT,EXPDAT(IPEV+1))
          ENDIF
        else
          expdat(ipev+1) = data(ipev+1)
        ENDIF
        DO 15 ind = 2, LMAX
C subtract pedestal
          data(ipev+ind) = data(ipev+ind) - iped
        IF (data(ipev+ind) .GT. 0) THEN
C  bilinear conversion 
          IF (TBLFLG) THEN
            expdat(ipev+ind) = Table(data(ipev+ind))
          ELSE
            expdat(ipev+ind) = data(ipev+ind)
            CALL ZBICVT(BILIPT,BILIRT,EXPDAT(IPEV+ind))
          ENDIF
        else
          expdat(ipev+ind) = data(ipev+ind)
        ENDIF
        B(ind)=FLOAT(EXPDAT(IPEV+ind) - EXPDAT(IPEV+ind-1))
   15   CONTINUE
      ELSE
        expdat(ipev + 1) = data(ipev + 1)
        DO 5 ind = 2, LMAX
          expdat(ipev+ind) = data(ipev+ind)
          B(ind )= FLOAT(data(IPEV+ind) - data(IPEV+ind-1))
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
          IF (EXPDAT(IPEV+I) .GE. 0) then
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
   50 IF ((EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1)) .LE.
     &  THR3)GO TO 10
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
        ISUML=MIN(S+2, LMAX)
        GOTO 70
      END IF
      IF (B(S).GE.THR1 .AND. B(S+1).GE.THR1 .AND.
     +   (B(S+2).GE.THR1 .OR. B(S)+B(S+1).GE.THR2)) THEN
        IFLAG = IBSET(IFLAG,1)               ! set overlap flag
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
        GOTO 70
      END IF
      GOTO 60
C
C ****  computes the area
C
   70 IAREA = 0
      DO 80 I=ISUMF,ISUML
        IF (EXPDAT(IPEV+I) .GE. MAXCNT) IFLAG = IBSET(IFLAG, 0 )
        IAREA = IAREA + EXPDAT(IPEV+I)
   80 CONTINUE
      AREA = FLOAT(IAREA)
      IF ( AREA .LT. THR3 ) GOTO 10
C
C ****  record pulse parameters
C
      NPULSE=NPULSE+1
C      HITLST(1,NPULSE) = FLABEL
C SHIFT BIN CENTER
C      HITLST(2,NPULSE) = NBPBIN*(SUMX/SUM + IFIRST-1 + IFBIN - .5)
C      HITLST(3,NPULSE) = AREA
C      HITLST(4,NPULSE) = FLOAT((ILAST-IFIRST))*NBPBIN
C      IF (SBTRCT) THEN
C        HITLST(5,NPULSE) = EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1)
C      ELSE
C        HITLST(5,NPULSE) = EXPDAT(IPEV+ILAST-1)
C      ENDIF
C      IF (WIRE .GE. NBSENS .AND. WIRE .LE. MXFADC) THEN
C        HITLST(6,NPULSE) = 1.             ! time error for delay line
C      ELSE
C        HITLST(6,NPULSE) = 4.                 ! drift time error
C      ENDIF
C      HITLST(7,NPULSE) = SQRT(ABS(AREA))    ! pulse area error
C      HITLST(8,NPULSE) = FLAG
C
      ftime = NBPBIN*(SUMX/SUM + IFIRST-1 + IFBIN - .5)
      L2time(npulse) = min(int(ftime * tscale), tmax*tscale - 1)
      L2area(npulse) = min(int(area), armax)
      if (sbtrct) then
        L2peak(npulse) = data(ipev+ilast-1) - data(ipev+ifirst-1)
      else
        L2peak(npulse) = data(ipev+ilast-1)
      endif
      if (L2peak(npulse) .lt. 0) L2peak(npulse) = 0
      L2status(npulse) = iflag
      width = min((ilast - ifirst)/4, widmax)
      L2status(npulse) = L2status(npulse) + ISHFT(width, 4)
C
C      if (npulse .lt. maxpul) goto 10
C  mod to comply with DSECHT
      if (npulse .lt. maxhit) goto 10
C
  999 RETURN
      END
