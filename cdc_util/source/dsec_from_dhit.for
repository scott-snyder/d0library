      SUBROUTINE DSEC_FROM_DHIT(lay, sec, nhits, nhits1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build DSEC bank (CDC hits for one
C-                         sector) from DHIT bank (CDC compressed
C-                         hit bank). 
C-
C-   Inputs  : integer lay = layer number
C-             integer sec = sector number
C-   Outputs : DSEC bank
C-             NHITS       = total number of hits (since information is 
C-                           lost on whether delay line hits were 
C-                           registered on both or only one end this is 
C-                           approximated by counting each delay line 
C-                           hit only once.
C-
C-             NHITS1      = number of sense wire hits
C-   Controls: none.
C-
C-   Created  13-NOV-1993   Chris Klopfenstein
C-                          modelled on ZFDSEC (Q. Li et al.)
C-   Updated   7-APR-1994   Norman A. Graf Fixed calculation of number
C-                          of sense wire and total hits (NHITS and NHITS1)
C-   Updated   8-APR-1994   Qizhong Li-Demarteau  changed pulse area to
C-                                ionisation of hit (MIP) to avoid reading
C-                                database when reconstructed from STA
C-   Updated  11-APR-1994   Srini Rajagopalan  Fix bug, status word wrongly
C-                          in (LDSEC+8) instead of (LDSEC+9)
C-   Updated  10-FEB-1995   Norman A. Graf   Fix delay line z if
C-                                           not already done in DHIT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZebStp.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
C
      INTEGER KPDSEC, IPDSEC
      INTEGER LDTVA,IPVA,ERR, GZDTVA
      INTEGER I, LAY, SEC, NDHIT, NDSEC, LHIT
      integer point, hit, pulse
      INTEGER LDGNL, LDTMW, IPGN, IPTM, IER
      integer GZDSEC
      REAL TIME, GAIN, VELO, SOFF, VFACTR, GFACTR
      real terrsw, terrdl
      REAL VELP, VELM
      LOGICAL FIRST, EZERROR, MCDATA
      LOGICAL VPLVMI,SWDERR,D0CFUN
      integer nhits, nhits1, wire, status
      integer mxhtot
      parameter (mxhtot = 500)
      real hitlist(6, mxhtot), fwire, fstatus
      equivalence (wire, fwire)
      equivalence (status, fstatus)
      integer npulse(0:mxsens)
      REAL    MIP
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C ****  Corrections...
C
      LOGICAL FIXZ
      INTEGER POLYORDER,II
      REAL POLYCOEFF(10),ZOLD,DZ
      INTEGER LDHIT,GZDHIT,LCDCH,GZCDCH
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','ZFDSEC',
     &    'Unable to find bank DTRAKS_RCP','W')
        ENDIF
        CALL EZGET('VFACTR',VFACTR,IER)
        IF (IER .NE. 0) VFACTR = 1.0
        CALL EZGET('GFACTR',GFACTR,IER)
        IF (IER .NE. 0) GFACTR = 1.0
        IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
        IF (.NOT. MCDATA) THEN
          CALL EZGET('VPLVMI',VPLVMI,ERR)
          CALL EZGET('SWDERR',SWDERR,ERR)
          CALL EZGET('D0CFUN',D0CFUN,ERR)            
        ENDIF
        call EZGET('TERRSW', TERRSW, ERR)
        if (err .ne. 0) terrsw = 12.0
        call EZGET('TERRDL', TERRDL, ERR)
        if (err .ne. 0) terrdl = 2.2
C
        CALL EZGET('FIXZ',FIXZ,IER)
        CALL EZGET('POLYORDER',POLYORDER,IER)
        CALL EZGET('POLYCOEFF',POLYCOEFF,IER)
C
        CALL EZRSET
      ENDIF
C
C  if DSEC bank exists for this sector, assume it's already 
C  been filled (and exit).
C
      KPDSEC = GZDSEC(sec, lay)
      if (KPDSEC .gt. 0) goto 999
C
C  Fill array with info for all hits in this sector - from DHIT,
C  book DSEC large enough to accomodate hits
C
      LDHIT = GZDHIT()
      LCDCH = GZCDCH()
      call fill_dhitlist(lay, sec, mxhtot, nhits1, hitlist)
      call BKDSEC(lay, sec, nhits1, KPDSEC)
      call VZERO(npulse, nbsens)
      nhits = nhits1
      do hit = 1, nhits1
        fwire = hitlist(1, hit)
        npulse(wire) = npulse(wire) + 1
        if (hitlist(4, hit) .ne. 0.0) nhits = nhits + 1 !hit has z position
      enddo
C
C ****  Fill DSEC bank from DHIT array, with time => cm and FADC => M.I.P.
C ****  conversion .
C
      NDSEC = IQ( KPDSEC + 2 )                      ! number of sense wires
      LHIT  = IQ( KPDSEC + 3 )                      ! number of words/hit
      LDGNL = LC( LDGNH - (LAY+1) )
      LDTMW = LC( LDTMH - (LAY+1) )
      IF (.NOT. MCDATA) LDTVA = GZDTVA(LAY)
      IPDSEC= 2*NDSEC + 3
      hit = 0
      DO 10 WIRE = 0, NDSEC-1
        IQ (KPDSEC+4+WIRE) = npulse(wire)
        IF (npulse(wire) .NE. 0) THEN
          IQ(KPDSEC+4+NDSEC+WIRE) = IPDSEC      ! Insert pointers in DSEC
          SOFF = C(LC(LDGEH-3) + 26 + WIRE)
          IPGN = LDGNL + (SEC*IC(LDGNL+4)+WIRE)*IC(LDGNL+3) + 4
          GAIN = C(IPGN + 1) * GFACTR
          IPTM = LDTMW + (SEC*IC(LDTMW+4)+WIRE)*IC(LDTMW+3) + 4
C          TZER = C(IPTM + 1)
          VELO = C(IPTM + 2) * VFACTR
          IF (.NOT. MCDATA) THEN
            IPVA = LDTVA+IC (LDTVA+2)*(IC (LDTVA+1)*SEC+WIRE)+2
            VELP = C (IPVA+1)                       ! + side SW velocity
            VELM = C (IPVA+2)                       ! - side SW velocity
          ENDIF
          DO 30 pulse = 1, npulse(wire)
            hit = hit + 1
            POINT = KPDSEC + IPDSEC
            IQ(POINT + 1) = IQ(KPDSEC - 5) + wire
            time = hitlist(2, hit)               ! note t-zero already sub.
            IF (VPLVMI .AND. (.NOT.MCDATA)) THEN
              Q(POINT+2) = SOFF + VELP * TIME         ! + side
              Q(POINT+3) = SOFF - VELM * TIME         ! - side
            ELSE
              Q(POINT+2) = SOFF + VELO * TIME
              Q(POINT+3) = SOFF - VELO * TIME
            END IF
            IF (.NOT. MCDATA .AND. D0CFUN) 
     &          CALL DNLCOR(WIRE, LAY, Q(POINT+2), Q(POINT+3))
            IF (.NOT. MCDATA .AND. SWDERR) THEN
              CALL DXYERR(WIRE, Q(POINT+2), Q(POINT+5))
            ELSE
              Q(POINT+5) = VELO * terrsw
            END IF
            if ((wire .eq. 0) .or. (wire .eq. 6)) then
              q(point + 4) = hitlist(4, hit)     ! z position
              q(point + 6) = hitlist(5, hit)     ! error on z
C
C ****  Fix delay line nonlinearity
C       from Taka Yasuda using muons
C
C       dz = (-0.149+-0.013) + (0.0129+-0.004)*z
C       Note that since CDC determines coordinate system, there is NO offset
C
              IF(FIXZ) THEN
C
C ****  need to check if correction has already been applied from RAW
C
                IF(IBITS(IQ(LDHIT),0,1) .EQ. 0) THEN
                  ZOLD = HITLIST(4, HIT)     ! Z POSITION
                  DZ = 0
                  IF(ZOLD.NE.0) THEN
                    DO II = 1,POLYORDER
                      DZ = DZ + POLYCOEFF(II)*ZOLD**(II-1)
                    ENDDO
                  ENDIF
C
                  Q(POINT + 4) = Q(POINT + 4) + DZ
C
C ****  Set status bit in CDCH to indicate correction has been applied
C ****  to DHIT (i.e. from STA)
C
                  IQ(LCDCH) = IBSET(IQ(LCDCH),1)
                ENDIF
              ENDIF
            else
              q(point + 4) = 0.
              Q(POINT+6) = 9999.
            endif
            MIP = hitlist(3, hit)
            Q(POINT+7) = MIP
            IF (GAIN .GT. 0.0) THEN
              Q(POINT+8) = GAIN * SQRT(ABS(MIP/GAIN))   ! error on MIP
            ENDIF
            fstatus = hitlist(6,hit)
            iq(point + 9) = status
            iq(point + 10) = 0                 ! no pointers to DCDA
            iq(point + 11) = 0
            iq(point + 12) = 0
            IPDSEC = IPDSEC + LHIT
   30     CONTINUE
        ELSE
          IQ(KPDSEC+4+NDSEC+WIRE) = 0
        ENDIF
   10 CONTINUE
      IQ (KPDSEC+1)=NHITS1            ! Insert Total # Of Hits In Sector
  999 RETURN
      END
