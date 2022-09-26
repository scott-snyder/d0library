      SUBROUTINE DSECHT(LAY,SEC,NHITS,NHITS1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : hitfinding in a sector
C-                         Routine based on old version of CDHITS
C-
C-   Inputs  :   layer, sector
C-   Outputs : NHITS  (number of hits in this sector)
C-             NHITS1 (number of hits on sense wires in this sector)
C-             bank DSEC and DCDA are filled
C-   Controls:
C-
C-   Created  15-MAR-1989   Qizhong Li-Demarteau
C-   Updated  03-MAY-1989   Qizhong Li-Demarteau  choice of subtract T_triger
C-                                                or T_offset
C-   Updated  06-APR-1990   Qizhong Li-Demarteau  fill hist for trigger signal
C-   Updated  08-NOV-1990   Qizhong Li-Demarteau  do not book bank for empty
C-                                                sectors
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  28-AUG-1991   Qizhong Li-Demarteau  added an output argument
C-                                                NHITS1
C-   Updated   2-APR-1993   Qizhong Li-Demarteau  made TRGOFF run-dependent
C-   Modified July 1993 C. Klopfenstein - use level2 hits if CDD2 was
C-   dropped. If raw data exists use it directly.
C-   Modified November 1993 C. Klopfenstein - use DHIT bank if no CDD2
C-                          bank (to run from compressed STA's)
C-                          Order of preference is (1) CDD2 (raw)
C-                                                 (2) DHIT (compressed hits)
C-                                                 (3) CDH2 (L2 hits)
C-                          Also - add Paul Rubinov's pulse area correction
C-                          using pressure in DBMON
C-   Updated   5-JAN-1994   Qizhong Li-Demarteau   fixed crash when L2 hits is
C-                                                 used
C-   Updated   4-MAR-1994   Qizhong Li-Demarteau  moved DBL3 database reading
C-                                   to this routine and skip DBL3 reading if 
C-                                   reconstructed from DHIT on STA
C-
C-   Updated   8-MAR-1994   C. Klopfenstein - fix bug in applying pulse
C-                          area correction.
C-
C-   Updated  15-MAR-1994   C. Klopfenstein - change logic for reconstructing
C-                          from DHIT: (1) use DHIT if it exists and is 
C-                          usable (3 wd/hit), else (2) use CDD2 if it exists,
C-                          drop DHIT if present, else (3) use L2 hits if
C-                          neither CDD2 nor (usable) DHIT exist.
C-   Updated  10-SEP-1994   Qizhong Li-Demarteau  fixed DHIT checking problem
C-                                                for the road tracking
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:CDCCOS.INC'
      INTEGER LAY, SEC
      INTEGER I, NHITS, NHITS1, IP, LHIT, LSTADC, ERR
      INTEGER NPULSE(0:11), NEVOLD, RUNOLD
      INTEGER KPDSEC, KPDCDA, BANKID
      INTEGER TRGLAY, TRGSEC, TRGWIR, RUNTYP, HISTON(10)
      INTEGER IER
      INTEGER MXHTOT
      PARAMETER( MXHTOT= 500 )
      INTEGER RUN, ID, III, NSIZE
      PARAMETER( NSIZE = 10 )
      INTEGER TRUNNO(NSIZE)
      INTEGER MAX_CDCRT
      LOGICAL TRGFLG, DONE
      LOGICAL EZERROR, BYPASS_DBL3_ERROR
      LOGICAL PD_INI, TM_INI, GN_INI, IOK, MCDATA
      LOGICAL USECDD2
      REAL    GLBLT0(NSIZE)
      REAL HITLST(LPULSE,MXHTOT), TRTIME, TRGOFF
      real area, fstatus
      integer status
      equivalence (status, fstatus)
      logical corrected
      integer gzcdd2, gzcdh2
      integer gzdhit, ldhit, length_dhit, min_len_dhit
      parameter (min_len_dhit = 3)    ! min dhit words/hit for use in RECO
      integer mask4
      parameter (mask4 = 15)
      integer label
      real flabel
      equivalence (label, flabel)
C
      DATA  NEVOLD / -1 /, RUNOLD/ -1 /
C----------------------------------------------------------------------
C
      IF (LHEAD .EQ. 0) GOTO 999
      IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNOLD) THEN
        RUNOLD = RUN
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DSECHT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_l('TRGFLG',TRGFLG,ERR)
        CALL EZGET_i('TRGLAY',TRGLAY,ERR)
        CALL EZGET_i('TRGSEC',TRGSEC,ERR)
        CALL EZGET_i('TRGWIR',TRGWIR,ERR)
        CALL EZGET('TRGOFF',TRGOFF,ERR)
        CALL EZGET_i('RUNTYP',RUNTYP,ERR)
        CALL EZGET_iarr('TRUNNO(1)',TRUNNO(1),ERR)
        CALL EZGET('GLBLT0(1)',GLBLT0(1),ERR)
        CALL EZGET_iarr('HISTON(1)',HISTON(1),ERR)
        CALL EZGET_i('MAX_CDCRT',MAX_CDCRT,IER)
        CALL EZGET_l('PD_INI',PD_INI,IER)
        CALL EZGET_l('TM_INI',TM_INI,IER)
        CALL EZGET_l('GN_INI',GN_INI,IER)
        CALL EZGET_l('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZRSET
        IF (.NOT.TRGFLG .AND. TRUNNO(1) .NE. 0) THEN
          DO 150 III = NSIZE, 1, -1
            IF (TRUNNO(III) .NE. 0) THEN
              IF (IQ(LHEAD+6) .GE. TRUNNO(III)) THEN
                TRGOFF = GLBLT0(III)
                GOTO 160
              ENDIF
            ENDIF
  150     CONTINUE
  160     CONTINUE
        ENDIF
C
C     add reading DBL3 database here - skip if we are reconstructing
C     from DHIT
C
        ldhit = gzdhit()
        if (ldhit .gt. 0) length_dhit = IQ(ldhit + 3)
        IF ((GZCDD2().LE.0) .AND. (GZDHIT() .GT. 0) .and.
     &      (length_dhit .ge. min_len_dhit)) GOTO 111
        IF (.NOT. MCDATA) THEN
          CALL DDBINI(RUNOLD,MAX_CDCRT,PD_INI,TM_INI,GN_INI,IOK)
          IF (.NOT. IOK) THEN
            IF (BYPASS_DBL3_ERROR) THEN
              CALL INTMSG('DTRAKS:'//
     &          'Error in updating STP banks from DBL3')
            ELSE
              CALL ERRMSG('DTRAKS','DSECHT',
     &        'error in updating STP banks from DBL3','F')
              GOTO 999
            ENDIF
          ENDIF
        ENDIF
  111 CONTINUE
      ENDIF
C
      LSTADC = IQ(LCDCH+5) - 1
C
C ****  IF D0 Hall trigger not synchronous, substract trigger time
C            (only calculate trigger time once per event)
C
      IF ( IQ( LHEAD+9 ) .NE. NEVOLD ) THEN
        NEVOLD = IQ( LHEAD+9 )
        LCDD2 = GZCDD2()
        LDHIT = GZDHIT()
        IF (LDHIT .LE. 0 .AND. LCDD2 .GT. 0) USECDD2 = .TRUE.
        IF (TRGFLG) THEN
          LAYER  = TRGLAY
          SECTOR = TRGSEC
          WIRE   = TRGWIR
          CALL CDPULS( NPULSE(0), HITLST(1,1), MXHTOT )
          IF ( NPULSE(0) .NE. 0  ) THEN
            TRTIME = HITLST(2,1) - TRGOFF      ! default time...
          ELSE
            TRTIME = 0.
          ENDIF
        ENDIF
C
        IF (HISTON(9) .NE. 0) THEN
          CALL HF1(1991,HITLST(2,1),1.)
          CALL HF1(1992,HITLST(4,1),1.)
          CALL HF1(1993,HITLST(5,1),1.)
        ENDIF
      ENDIF
C
C
      NHITS  = 0
      NHITS1 = 0
      LAYER = LAY
      SECTOR = SEC
C  check for DHIT bank - use it if it exists and has right number of
C  words/hit
      IF (USECDD2) GOTO 500
      ldhit = gzdhit()
      if (ldhit .gt. 0) then         ! reconstruct from DHIT
        length_dhit = IQ(ldhit + 3)     ! check for usable version of DHIT
        if (length_dhit .ge. min_len_dhit) then
          call dsec_from_dhit(lay, sec, nhits, nhits1)
          call dhtchk(lay, sec, 1, done)
          goto 999
        endif
      endif
C  use raw data CDD2 if it exists (and DHIT was not used).
C  if a DHIT bank exists, drop it here.
 500  if (LCDD2 .gt. 0) then
        if (ldhit .gt. 0) call MZDROP(ixcom, ldhit, ' ')
        DO 40 WIRE = 0, LSTADC
          NPULSE(WIRE) = 0
          IF( NHITS .EQ. MXHTOT ) GOTO 40
C
C ****  This builds the Pulse list in HITLST array ( with a maximum number
C ****  for the whole sector )
C
          CALL CDPULS (NPULSE(WIRE) , HITLST(1,NHITS+1), MXHTOT-NHITS)
          NHITS = NHITS + NPULSE (WIRE)
          IF (WIRE .LE. NBSENS-1) NHITS1 = NHITS1 + NPULSE(WIRE)
   40   CONTINUE
C reconstruct from L2 hits if neither DHIT nor CDD2 present.
      else if (gzcdh2() .gt. 0) then    ! reconstruct from L2 hits
        call Fill_CDC_L2hitlist(lay, sec, mxhtot, nhits,
     &                          npulse(0), hitlst)
        nhits1 = nhits
        do wire = nbsens, lstadc
          nhits1 = nhits1 - npulse(wire)
        enddo
      endif
C
      CALL DHTCHK(LAYER,SECTOR,1,DONE)
      IF( NHITS .EQ. 0 ) GOTO 999
C
      CALL BKDSEC( LAYER, SECTOR, NHITS1, KPDSEC )
      CALL BKDCDA( LAYER, SECTOR, NHITS , KPDCDA )
C
C ****  correct for trigger time
C
      IF (TRGFLG) THEN
        IF ( TRTIME .NE. 0. ) THEN
          DO 50 I = 1 , NHITS
            HITLST(2,I) = HITLST(2,I) - TRTIME
   50     CONTINUE
        ENDIF
      ELSE
        IF ( TRGOFF .NE. 0. ) THEN
          DO 51 I = 1 , NHITS
C  subtract trgoff if working directly from raw data. For
C  level2 hits it's already been done in fill_hitlist.
            if (gzcdd2() .gt. 0)
     &        HITLST(2,I) = HITLST(2,I) - TRGOFF
   51      CONTINUE
         ENDIF
       ENDIF
C
C  correct the area using DBMON info
C
       do i = 1, nhits
         area = hitlst(3, i)
         flabel = hitlst(1,i)
         wire = IAND(label, mask4)
         call DAREA_CORRECT(area, corrected)
         if (corrected) then
           fstatus = hitlst(8, i)
           status = IBSET(status, 2)
           hitlst(8, i) = fstatus
           hitlst(3, i) = area
         endif
       enddo
C
      CALL ZFDCDA( HITLST, NPULSE )
      CALL ZFDSEC
C
C ****  Fill the delay line information
C
      CALL CDGETZ
      IF( RUNTYP .NE. 0 ) THEN
C
C ****  Sum for mean Z ( = trigger time in cosmic set-up )
C
        IP = KPDSEC + 3 + 2* IQ(KPDSEC+2)
        LHIT = IQ( KPDSEC+3 )
        DO 60 I = 1, NHITS1
          IF( Q( IP + 6 ) .LT. 10. ) THEN
            NBZMES = NBZMES + 1
            ZMEAN  = ZMEAN  + Q( IP + 4 )
          ENDIF
          IP = IP + LHIT
   60   CONTINUE
      ENDIF
C
C ****  Debug the resulting banks if requested
C
      IF ( DBGFLG .AND. LVLDBG(4).GT.0 ) THEN
        BANKID = ( 32 * LAYER + SECTOR ) * 16
        CALL PRDCDA( LUNDBG, KPDCDA, BANKID, 'SINGLE', LVLDBG(4))
        CALL PRDSEC( LUNDBG, KPDSEC, BANKID, 'SINGLE', LVLDBG(4))
      ENDIF
C
  999 RETURN
      END
