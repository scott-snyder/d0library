      SUBROUTINE CDPULS ( NPULSE, HITLST, MAXPUL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : search hits in a FADC
C-
C-   Inputs  : LAYER, SECTOR, WIRE = wire coordinates...
C-             MAXPUL     = Maximum number of pulse
C-   Outputs : NPULSE     = # of found pulses
C-             HITLST(LPULSE,*)= content of each hit as described in the doc.
C-
C-   Created                Domenico Pizzuto
C-   Updated  17-JUN-1987   Olivier Callot
C-   Updated  26-JUN-1987   Olivier Callot  decode unpacked datas
C-   Updated  17-JUL-1987   Olivier Callot  change args, add errors
C-   Updated  17-MAR-1989   Qizhong Li-Demarteau   improve calculation
C-                                                 for pulse area
C-   Updated  20-APR-1989   Qizhong Li-Demarteau   use SRCP 
C-   Updated  03-MAY-1989   Qizhong Li-Demarteau   add bilinear conversion
C-   Updated  15-JUN-1989   Qizhong Li-Demarteau   NBPBIN changed according
C-                                          to the freqrency of FADC system
C-   Updated  22-JUN-1989   Qizhong Li-Demarteau   check FADC bad bins
C-   Updated  20-JUL-1989   Qizhong Li-Demarteau   correct delta T for DL 
C-   Updated  10-APR-1990   Qizhong Li-Demarteau   modify calculation for
C-                                                 pulse height
C-   Updated  21-JUN-1990   Qizhong Li-Demarteau  added a choice to use table
C-                                           instead of bilinear conversion
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  26-DEC-1992   Qizhong Li-Demarteau  change time errors to be
C-                                                RCP parameters 
C-   Updated  24-MAY-1993   Paul Rubinov   added pulse hight correction
C-                                         according to pressure in DBMON 
C-   Updated November 93 C. Klopfenstein - remove pulse area correction
C-                       (move this to DSECHT), and fix handling of pulses 
C-                       where leading edge runs over end of cluster.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
C
      INTEGER MAXPUL, ERR, MAXCNT
      INTEGER NATUR,NPULSE,ISUML,J, NCALL, RUNNO
      INTEGER I,ILAST,IFIRST,ISUMF,S,INIT, IPEV
      INTEGER EXPDAT(2*LFADC), IPREAD, IFBIN, LMAX , HEIGHT(0:255)
      INTEGER IOFS, LABEL, IFLAG, IAREA, IPED, JDPDL
      INTEGER PULTH1(2), PULTH2(2), PULTH3(2), PULMAX(2), CDSURV
      INTEGER TABLE(0:255)
      REAL    PULWEI(2)
      REAL  HITLST(LPULSE,*)
      REAL  B(LFADC),COUNT, FLABEL, AREA, FLAG
      REAL  NBPBIN,SUM,SUMX, COEFF(50,2), COEF2(50,2)
      REAL  THR1, THR2, THR3, ERRB, FPED, FFRQCY, BILIRT, BILIPT
      REAL    TERRSW, TERRDL
      INTEGER IER
      LOGICAL EZERROR
      LOGICAL BILFLG, BINCHK, NOIPED, SBTRCT, TBLFLG
      LOGICAL CORRECTED
      CHARACTER*8 ZFORMA
C
      EQUIVALENCE ( LABEL, FLABEL ), ( FLAG,IFLAG )
      SAVE INIT, NCALL
      DATA INIT /0/, NCALL/0/
C----------------------------------------------------------------------
      IF (INIT.EQ.0) THEN
        INIT = 1
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDPULS',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('FFRQCY',FFRQCY,ERR)
        CALL EZGET('BILFLG',BILFLG,ERR)
        CALL EZGET('BILIRT',BILIRT,ERR)
        CALL EZGET('BILIPT',BILIPT,ERR)
        CALL EZGET('MAXCNT',MAXCNT,ERR)
        CALL EZGET('SBTRCT',SBTRCT,ERR)
        CALL EZGET('CDSURV',CDSURV,ERR)
        CALL EZGET('PULTH1(1)',PULTH1(1),ERR)
        CALL EZGET('PULTH2(1)',PULTH2(1),ERR)
        CALL EZGET('PULTH3(1)',PULTH3(1),ERR)
        CALL EZGET('PULMAX(1)',PULMAX(1),ERR)
        CALL EZGET('PULWEI(1)',PULWEI(1),ERR)
        CALL EZGET('TERRSW',TERRSW,ERR)
        CALL EZGET('TERRDL',TERRDL,ERR)
        CALL EZGET('TBLFLG',TBLFLG,ERR)
        CALL EZGET('TABLE(1)',TABLE(0),ERR)
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
      ENDIF
C
      IF (CDSURV .NE. NCALL) THEN
        NCALL = CDSURV
        CALL VZERO(NEVPED,(MXFADC+1)*32*4)
        CALL VZERO(SUMPED,(MXFADC+1)*32*4)
        CALL VZERO(SM2PED,(MXFADC+1)*32*4)
      ENDIF
C
      NPULSE=0
      NOIPED = .FALSE.
C
C ****  get FADC datas after unpacking
C
      LABEL  = ( LAYER * 32 + SECTOR ) * 16 + WIRE
      CALL CDUNPK( LAYER, SECTOR, WIRE, EXPDAT )
      IF (EXPDAT(1) .EQ. 0 ) GOTO 999
C
C ****  Decides if Delay line : if WIRE is NBSENS or more
C
      IF ( WIRE .GE. NBSENS ) THEN
        NATUR = 2
      ELSE
        NATUR = 1
      ENDIF
      JDPDL = LC( LDPDH - (LAYER+1) )
      JDPDL = JDPDL + ( SECTOR*IC(JDPDL+4)+WIRE ) *IC(JDPDL+3) + 4
      FPED  = C( JDPDL+1 )              ! Pedestal
      IPED = NINT( FPED )
      THR1 = FLOAT( PULTH1(NATUR) )
      THR2 = FLOAT( PULTH2(NATUR) )
      THR3 = FLOAT( PULTH3(NATUR) )
      IPREAD = 1
      IF ( DBGFLG .AND. LVLDBG(3) .GE. 3 ) THEN
        WRITE( LUNDBG, 4000 ) LAYER, SECTOR, WIRE
 4000   FORMAT(//' Raw data for layer',I2,' sector',I3,' wire',I3/)
      ENDIF
  100 IF( EXPDAT(IPREAD) .EQ. 0 ) GOTO 999
      IFBIN = EXPDAT(IPREAD+1)
      LMAX  = EXPDAT(IPREAD  )
      IPEV  = IPREAD + 1
      IPREAD = IPREAD + LMAX + 2
      IF (FFRQCY .NE. 0) THEN
        NBPBIN = 1000. / FFRQCY
      ELSE
        NBPBIN = 1000. / 106.
      ENDIF
C
C ****  If step 1 of alignement, compute pedestal
C ****  In step 2, refine ( cut at 3 sigmas )
C
      IF ( CDSURV .GT. 0 .AND. CDSURV .LE. 2 ) THEN
        IF( WIRE .GT. MXFADC ) GOTO 777
        SUM = .5 * ( EXPDAT(IPEV+1)+EXPDAT(IPEV+2) )
        IF( CDSURV .NE. 1 .AND.
     &      ABS( SUM - FPED) .GT. 3.*C(JDPDL+2)) GOTO 777
        NEVPED(WIRE,SECTOR,LAYER) = NEVPED(WIRE,SECTOR,LAYER) + 1
        SUMPED(WIRE,SECTOR,LAYER) = SUMPED(WIRE,SECTOR,LAYER) + SUM
        SM2PED(WIRE,SECTOR,LAYER) = SM2PED(WIRE,SECTOR,LAYER) + SUM**2
  777   CONTINUE
      ENDIF
C
C ****  compute first difference
C
      B(1) = 0.
      IF (BILFLG .OR. TBLFLG) THEN
        NOIPED = .TRUE.
        EXPDAT(IPEV+1) = EXPDAT(IPEV+1) - IPED
        IF (EXPDAT(IPEV+1) .GT. 0) THEN
          IF (TBLFLG) THEN
            EXPDAT(IPEV+1) = TABLE(EXPDAT(IPEV+1))
          ELSE
            CALL ZBICVT(BILIPT,BILIRT,EXPDAT(IPEV+1))
          ENDIF
        ENDIF
        DO 15 I = 2, LMAX
          EXPDAT(IPEV+I) = EXPDAT(IPEV+I) - IPED
        IF (EXPDAT(IPEV+I) .GT. 0) THEN
          IF (TBLFLG) THEN
            EXPDAT(IPEV+I) = TABLE(EXPDAT(IPEV+I))
          ELSE
            CALL ZBICVT(BILIPT,BILIRT,EXPDAT(IPEV+I))
          ENDIF
        ENDIF
        B(I)=FLOAT( EXPDAT(IPEV+I) - EXPDAT(IPEV+I-1) )
   15   CONTINUE
        IF (DBGFLG .AND. (BINCHK .OR. LVLDBG(3) .GT. 0)) THEN
          NOIPED = .FALSE.
          DO 6 I = 1, LMAX
            EXPDAT(IPEV+I) = EXPDAT(IPEV+I) + IPED
    6     CONTINUE
        ENDIF
      ELSE
        DO 5 I = 2, LMAX
          B(I)=FLOAT( EXPDAT(IPEV+I) - EXPDAT(IPEV+I-1) )
    5   CONTINUE
      ENDIF
      IF ( DBGFLG .AND. LVLDBG(3) .GE. 3 ) THEN
        WRITE( LUNDBG, 4100 ) IFBIN, LMAX, (EXPDAT(J), J=IPEV+1,
     &    IPEV+LMAX)
 4100   FORMAT('0 ifbin=',I4,' lmax =',I4,2X,20I5/(25X,20I5))
      ENDIF
C
C  check bad bins in FADC data, if requested
C      
      IF (DBGFLG .AND. BINCHK)  
     &  CALL DBNCHK(B,LMAX,EXPDAT,IPEV,IFBIN)
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
      ISUML = LMAX
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
      IF (NOIPED) THEN
        AREA = FLOAT(IAREA)
      ELSE
        AREA = FLOAT(IAREA) - (ISUML-ISUMF+1) * FPED
      ENDIF
      IF ( AREA .LT. THR3 ) GOTO 10
      IF (CORRECTED) IFLAG = IBSET(IFLAG, 2)
C
C ****  record pulse parameters
C
      NPULSE=NPULSE+1
      HITLST(1,NPULSE) = FLABEL
      HITLST(2,NPULSE) = NBPBIN*(SUMX/SUM + IFIRST-1 + IFBIN - .5)  ! SHIFT BIN CENTER
      HITLST(3,NPULSE) = AREA
      HITLST(4,NPULSE) = FLOAT((ILAST-IFIRST))*NBPBIN
      IF (SBTRCT) THEN
        HITLST(5,NPULSE) = EXPDAT(IPEV+ILAST-1) - EXPDAT(IPEV+IFIRST-1)
      ELSE
        HITLST(5,NPULSE) = EXPDAT(IPEV+ILAST-1)
      ENDIF
      IF (WIRE .GE. NBSENS .AND. WIRE .LE. MXFADC) THEN
        HITLST(6,NPULSE) = TERRDL                 ! time error for delay line
      ELSE
        HITLST(6,NPULSE) = TERRSW                 ! drift time error
      ENDIF
      HITLST(7,NPULSE) = SQRT(ABS(AREA))    ! pulse area error
      HITLST(8,NPULSE) = FLAG
C
C ****  Debug if requested
C
      IF( DBGFLG .AND. LVLDBG(3).NE.0 ) THEN
        WRITE( LUNDBG, 1000) LAYER, SECTOR, WIRE, ISUMF+IFBIN,
     &                       ISUML+IFBIN
 1000   FORMAT('0**CDPULS**  On layer',I2,' sector',I3,' wire',I3,
     &         '   new hit within bins ',2I4/)
        WRITE( LUNDBG, 1100) (I+IFBIN,(EXPDAT(IPEV+I+J),
     &                        J=0,MIN(ISUML-I,19)), I=ISUMF,ISUML,20)
 1100   FORMAT(I5,5X,20I5)
        WRITE( LUNDBG, 1200) ZFORMA( HITLST(1,NPULSE) ),
     &                             ( HITLST(I,NPULSE),I=2,LPULSE-1),
     &                       ZFORMA( HITLST(LPULSE,NPULSE) )
 1200   FORMAT(/10X,'Pulse parameters =',A8,6F10.3,A8)
      ENDIF
      IF( NPULSE .LT. MAXPUL ) GOTO 10
C
C ****  End of work. All peaks found ( ? )
C
  999 CONTINUE
      RETURN
      END
