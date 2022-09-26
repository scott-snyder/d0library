      SUBROUTINE CDSRES( NBTWIR, LAB, RES, WZ, NDEGF )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sums the residual in the appropriate
C-                         variable(s)
C-
C-   Inputs  : NBTWIR   [I] = number of elements to process
C-             LAB(*)   [I] = encoded hit info
C-             RES(*,2) [R] = Residual of the fit in x-y and r-z
C-             WZ(*)    [R] = Z weights ( gives valid residuals )
C-             NDEGF(2) [I] = Degree of freedom of the fits
C-   Outputs : none
C-
C-   Created  15-OCT-1987   Olivier Callot
C-   Updated  30-MAR-1989   Qizhong Li-Demarteau  use SRCP and fix bugs 
C-                                                on array boundary
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
C
      INTEGER NBTWIR, LAB(*), NDEGF(2)
      INTEGER MXVAL, NCALL, NUM, NFADC, LHIT, IPPUL, NEV
      INTEGER CDSURV, ERR
      INTEGER LABEL, LAY, SEC, WIR, NUMHIT, ISIDE, KOF, LUNIT, NUMDL
      INTEGER IPDSEC, LBIN, NBIN, KKK, I, J, JJ
      INTEGER IER
      REAL    RES(NBTWIR,2), WZ(*)
      REAL    RESID, X, W, SLOPE, DTIM, ZRES, ARINV
      REAL    MXHIGH, PLHIGH
      LOGICAL FIRST
      LOGICAL EZERROR
C
      SAVE  FIRST, NCALL
      DATA  FIRST/.TRUE./, NCALL/0/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDSRES',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('CDSURV',CDSURV,ERR)
        CALL EZGET('MXHIGH',MXHIGH,ERR)
        if (err .ne. 0) mxhigh = 500.0
        CALL EZRSET
      ENDIF
C
      IF ( CDSURV .NE. NCALL ) THEN
C
C ****  Initialize every storage array for a new step in alignement
C
        NCALL = CDSURV
        CALL VZERO_i( NBRES (0,0,0), NBSENS*32*4 )
        CALL VZERO( SUMRES(0,0,0), NBSENS*32*4 )
        CALL VZERO( SUMRE2(0,0,0), NBSENS*32*4 )
        CALL VZERO_i( NZRES(1,0,0),  NBDELY*32*4 )
        CALL VZERO( SZRES(1,0,0),  NBDELY*32*4 )
        CALL VZERO( SZRE2(1,0,0),  NBDELY*32*4 )
        CALL VZERO_i( NDLEV(1,0,0),  NBDELY*32*4 )
        CALL VZERO( SDLRES(1,0,0), NBDELY*32*4 )
        CALL VZERO( SDLRE2(1,0,0), NBDELY*32*4 )
        CALL VZERO_i( NEVGAI(0,0,0), (MXFADC+1)*32*4 )
        CALL VZERO( SUMGAI(0,0,0), (MXFADC+1)*32*4 )
        CALL VZERO( SU2GAI(0,0,0), (MXFADC+1)*32*4 )
      ENDIF
      DO 1 KKK = 1, NBTWIR
        LABEL = LAB(KKK)
        RESID = RES(KKK,1)
        IF ( LABEL .NE. 0 ) THEN
          LAY  = IBITS( LABEL, 16, 2 )
          SEC  = IBITS( LABEL, 11, 5 )
          WIR  = IBITS( LABEL,  8, 3 )
          NUM  = IBITS( LABEL,  1, 7 )
          IPDSEC = LDSEC( SEC, LAY )
          IPDSEC = IQ( IPDSEC+4+WIR+IQ(IPDSEC+2) ) +
     &                      (NUM-1)*IQ(IPDSEC+3) + IPDSEC
          ISIDE= 2*IBITS( LABEL,  0, 1 )-1
          PLHIGH = Q(LDCDA( SEC, LAY ) + IQ(IPDSEC+10) + 5)
          if (PLHIGH .GT. MXHIGH) goto 1
C
C ****  Residual is counted differently depending on side ( i.e. drift
C ****  direction ). For physical alignement of the wire, don't weight by
C ****  ISIDE, but be sure to be uniform in phi. This can not work with
C ****  cosmics...
C
          NBRES( WIR,SEC,LAY) = NBRES( WIR,SEC,LAY) + 1
          SUMRES(WIR,SEC,LAY) = SUMRES(WIR,SEC,LAY) + ISIDE * RESID
          SUMRE2(WIR,SEC,LAY) = SUMRE2(WIR,SEC,LAY) + RESID **2
C
C ****  Sum values of the MIP ionisation.
C
          ARINV = 1./Q(LDCDA( SEC, LAY ) + IQ(IPDSEC+10) +3 )
          NEVGAI(WIR,SEC,LAY) = NEVGAI(WIR,SEC,LAY) + 1
          SUMGAI(WIR,SEC,LAY) = SUMGAI(WIR,SEC,LAY) + ARINV
          SU2GAI(WIR,SEC,LAY) = SU2GAI(WIR,SEC,LAY) + ARINV**2
          NUMDL = 0
          IF( WIR .EQ. 0      ) NUMDL = 1
          IF( WIR .EQ. MXSENS ) NUMDL = 2
   45     IF ( NUMDL .NE. 0 .AND. WZ(KKK) .GT. 0 ) THEN
C
C ****  Mean gain on one side of the delay line
C
            IF ( IQ( IPDSEC+11 ) .NE. 0 ) THEN
              ARINV = 1. / Q( LDCDA(SEC,LAY)+IQ(IPDSEC+11)+3 )
              JJ = MXSENS + 2*NUMDL - 1
              NEVGAI(JJ,SEC,LAY) = NEVGAI(JJ,SEC,LAY) + 1
              SUMGAI(JJ,SEC,LAY) = SUMGAI(JJ,SEC,LAY) + ARINV
              SU2GAI(JJ,SEC,LAY) = SU2GAI(JJ,SEC,LAY) + ARINV**2
            ENDIF
C
C ****  The other side
C
            IF ( IQ( IPDSEC+12 ) .NE. 0 ) THEN
              ARINV = 1. / Q( LDCDA(SEC,LAY)+IQ(IPDSEC+12)+3 )
              JJ = MXSENS + 2*NUMDL
              NEVGAI(JJ,SEC,LAY) = NEVGAI(JJ,SEC,LAY) + 1
              SUMGAI(JJ,SEC,LAY) = SUMGAI(JJ,SEC,LAY) + ARINV
              SU2GAI(JJ,SEC,LAY) = SU2GAI(JJ,SEC,LAY) + ARINV**2
            ENDIF
            IF( NDEGF(2) .GE. 2 ) THEN
              ZRES = RES(KKK,2) * FLOAT(NDEGF(2)+2) / FLOAT(NDEGF(2))
C
C ****  Residual of the Z fit.
C
              NZRES(NUMDL,SEC,LAY) = NZRES(NUMDL,SEC,LAY) + 1
              SZRES(NUMDL,SEC,LAY) = SZRES(NUMDL,SEC,LAY) + ZRES
              SZRE2(NUMDL,SEC,LAY) = SZRE2(NUMDL,SEC,LAY) + ZRES**2
            ENDIF
            IF ( NUM .LE. NHTDLY ) THEN
              IF ( DLRES( NUM, NUMDL, SEC, LAY ) .LT. 1000. ) THEN
C
C ****  Relative alignement of the two ends of the DL
C
                NDLEV (NUMDL,SEC,LAY) = NDLEV(NUMDL,SEC,LAY) + 1
                SDLRES(NUMDL,SEC,LAY) = SDLRES(NUMDL,SEC,LAY) +
     &                                  DLRES(NUM,NUMDL,SEC,LAY)
                SDLRE2(NUMDL,SEC,LAY) = SDLRE2(NUMDL,SEC,LAY) +
     &                                  DLRES(NUM,NUMDL,SEC,LAY)**2
              ENDIF
            ENDIF
          ENDIF
        ENDIF
    1 CONTINUE
  999 RETURN
      END
