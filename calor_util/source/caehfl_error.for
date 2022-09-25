      SUBROUTINE CAEHFL_ERROR(IETA,IPHI,LAYER,SCALE,E,XC,YC,ZC,SIGZV,
     &  EXYZMAT,SETSQ,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-          Compute error matrix in cell energy components Ex, Ey, Ez,
C-          taking account of error in total energy:
C-
C-            Sig(E)**2 = A * E**2 + C * B * E + C**2 * Sig(ped)**2
C-          where C is the conversion from ADC counts to total energy,
C-
C-          as well as error in (x,y,z) and z-vertex.
C-
C-
C-   Inputs  : IETA,IPHI,LAYER [I] CELL PHYSICS ADDRESS
C-             SCALE           [I] x1=1, x8=0
C-             E               [R] Cell energy from CAEP
C-             XC,YC,ZC        [R] Cell center position (cm)
C-                                 wrt interaction vertex
C-             SIGZV           [R] Estimated variance on Z of
C-                                 interaction vertex
C-   Outputs : EXYZMAT(3,3)    [R] Error matrix in Ex, Ey, Ez
C-             SETSQ           [R] Sigma**2 Et
C-             IER             [I] Error flag 0   OK
C-                                           -1   Error matrix incorrect
C-   Controls: nope
C-
C-   Created   1-MAY-1992   Harrison B. Prosper
C-   Updated  15-JUN-1992   Harrison B. Prosper
C-      Change arguments of CELLSIZE
C-   Updated  15-JUL-1992   Harrison B. Prosper
C-      Change error calculation a bit to reduce possible
C-      rounding error problems.
C-   Updated  26-AUG-1992   Harrison B. Prosper
C-      Debug printout of error contributions.
C-   Updated  28-AUG-1992   Stan Krzywdzinski
C-      Modified CGEV handling upon return from GTCGEV routine.
C-      Corrected coding of AX and AY for the VARIANCE ON Ey.
C-   Updated  18-SEP-1992   Stan Krzywdzinski
C-      Entry point GET_CAEHFL_ERROR_SUMS to optionally (RCP switch
C-      GET_SUMS) sum up energies (ESUMX,ESUMY,ESUM) and error
C-      contributions (ASUMX,BSUMX,ALFASUMX,RESTSUMX
C-                     ASUMY,BSUMY,ALFASUMY,RESTSUMY).
C-   Updated  30-SEP-1992   Stan Krzywdzinski
C-      Added collecting ASUM, BSUM and RESTSUM for Sig(E)**2
C-   Updated  30-SEP-1992   Stan Krzywdzinski
C-      Incorporated ALPHA into SIGMA_ constants.
C-      Made some variables REAL*8.
C-   Updated  24-NOV-1992   Stan Krzywdzinski
C-      Made calculation of errors and error contributions identical.
C-      The calculation itself was reformulated in terms of sines
C-      and cosines to avoid large numbers in the expressions.
C-   Updated  16-DEC-1992   Stan Krzywdzinski
C-      Warning message when number of error constants from CAHITS.RCP is
C-      not right.
C-      Replaced SIGP by SIGP*SIGP.
C-      Replaced ABS(E) by E.
C-   Updated  17-DEC-1992   Harrison B. Prosper,Stan Krzywdzinski
C-      Added collection of NCELLS, CSUM, CMIN, CMAX, PEDSUM under
C-      the GET_SUMS switch.
C-   Updated  31-MAR-1993   Stan Krzywdzinski
C-      Implemented calculation of full error matrix in Ex, Ey, Ez;
C-      replaced subroutine arguments SEX,SEY by EXYZMAT(3,3),SETSQ.
C-      Added collecting ASUMT, BSUMT, ALFASUMT, RESTSUMT for Sig(Et)**2
C-   Updated   4-APR-1993   Stan Krzywdzinski
C-      Enforced formal bounds on SINTSQ and COSTSQ.
C-   Updated  29-APR-1993   Stan Krzywdzinski
C-      Checking error matrix for correctness.
C-   Updated  14-DEC-1993   Stan Krzywdzinski
C-      Protected against cell energy E too small.
C-   Updated  26-MAR-1994   Chip Stewart  - nominal k's and sigma's
C-   Updated  29-AUG-1994   Stan Krzywdzinski
C-      Replaced CERN routines: RFACT,VZERO,UCOPY by explicit code. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,LAYER,SCALE,IER
      REAL    E,XC,YC,ZC,SIGZV,EXYZMAT(3,3),SETSQ
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      CHARACTER*80 MESSAGE
      REAL    ECELL
      REAL    SIGMA_EM(3),SIGMA_HAD(3),SIGMA_ICD(3),SIGMA_MSG(3)
      REAL    A,B,C,ALPHA,CGEV(3),EE,PED,PEDSUM,CSUM,CMIN,CMAX
      REAL    CAX,CBX,CALPHAX,RESTX
      REAL    CAY,CBY,CALPHAY,RESTY
      REAL    CAT,CBT,CALPHAT,RESTT
      REAL    DD,RR,X2,Y2,Z2,DIST
      REAL    SIGP,SIGPHI,SIGR,SIGZ
      REAL    SESQ, AUX, RAUX, AAUX
      REAL    COSX,COSY,COSXSQ,COSYSQ,SINTSQ,COSTSQ
      REAL    DR(NETAL,NLYRL),DZ(NETAL,NLYRL),DPHI(NETAL,NLYRL)
      REAL    PRINT_ERRORS_MIN
      REAL    SIGMA(NLYRL,NETAL,0:1),CGEV_K(NLYRL,NETAL)
      INTEGER NCGEV,I,J,K,JETA,EVT,MAXLINE,IPRINT,SSUNIT,LUNOUT
      INTEGER RUNNO, EVONUM, RUN, NDIM, NCELLS
      LOGICAL PRINT_ERRORS, GET_SUMS,USE_CGEV
      LOGICAL FIRST
C----------------------------------------------------------------------
      REAL    DET
C----------------------------------------------------------------------
      REAL ESUMX, ESUMY, ESUM, ASUM,  BSUM,            RESTSUM
      REAL                     ASUMX, BSUMX, ALFASUMX, RESTSUMX
      REAL                     ASUMY, BSUMY, ALFASUMY, RESTSUMY
      REAL                     ASUMT, BSUMT, ALFASUMT, RESTSUMT
      REAL E_SUMXY(2), E_SUM, ERR_SUMS(3), ERRX_SUMS(4), ERRY_SUMS(4)
      REAL ERRT_SUMS(4)
      INTEGER N_CELLS
      REAL C_SUMS(3), PED_SUM
      INTEGER IER2
      INTEGER OEVT
C----------------------------------------------------------------------
      SAVE ESUMX, ESUMY, ESUM, ASUM,  BSUM,            RESTSUM
      SAVE                     ASUMX, BSUMX, ALFASUMX, RESTSUMX
      SAVE                     ASUMY, BSUMY, ALFASUMY, RESTSUMY
      SAVE                     ASUMT, BSUMT, ALFASUMT, RESTSUMT
      SAVE NCELLS, CSUM, CMIN, CMAX, PEDSUM
      SAVE OEVT
      SAVE FIRST
C----------------------------------------------------------------------
      DATA OEVT /-1/
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      DO I=1,3
        DO J = 1,3
          EXYZMAT(I,J) = 0.
        ENDDO
      ENDDO
      SETSQ = 0.
      IER = 0
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
C
C ****  Get constants for error calculation
C
        SIGMA_EM(1) = 0.
        SIGMA_EM(2) = 0.
        SIGMA_EM(3) = 0.
        CALL EZGETA('SIGMA_EM',0,0,0,NDIM,IER)
        IF ( (NDIM .NE. 3) .OR. (IER .NE. 0) ) THEN
          CALL ERRMSG('No error constants','CAEHFL_ERROR',
     &      ' Obsolete CAHITS.RCP','W')
        ELSE
          CALL EZGET_rarr('SIGMA_EM', SIGMA_EM, IER)
        ENDIF
C
        SIGMA_HAD(1) = 0.
        SIGMA_HAD(2) = 0.
        SIGMA_HAD(3) = 0.
        CALL EZGETA('SIGMA_HAD',0,0,0,NDIM,IER)
        IF ( (NDIM .NE. 3) .OR. (IER .NE. 0) ) THEN
          CALL ERRMSG('No error constants','CAEHFL_ERROR',
     &      ' Obsolete CAHITS.RCP','W')
        ELSE
          CALL EZGET_rarr('SIGMA_HAD',SIGMA_HAD,IER)
        ENDIF
C
        SIGMA_ICD(1) = 0.
        SIGMA_ICD(2) = 0.
        SIGMA_ICD(3) = 0.
        CALL EZGETA('SIGMA_ICD',0,0,0,NDIM,IER)
        IF ( (NDIM .NE. 3) .OR. (IER .NE. 0) ) THEN
          CALL ERRMSG('No error constants','CAEHFL_ERROR',
     &      ' Obsolete CAHITS.RCP','W')
        ELSE
          CALL EZGET_rarr('SIGMA_ICD',SIGMA_ICD,IER)
        ENDIF
C
        SIGMA_MSG(1) = 0.
        SIGMA_MSG(2) = 0.
        SIGMA_MSG(3) = 0.
        CALL EZGETA('SIGMA_MSG',0,0,0,NDIM,IER)
        IF ( (NDIM .NE. 3) .OR. (IER .NE. 0) ) THEN
          CALL ERRMSG('No error constants','CAEHFL_ERROR',
     &      ' Obsolete CAHITS.RCP','W')
        ELSE
          CALL EZGET_rarr('SIGMA_MSG',SIGMA_MSG,IER)
        ENDIF
        CALL EZGET_l('PRINT_ERRORS',PRINT_ERRORS,IER)
        CALL EZGET('PRINT_ERRORS_MIN',PRINT_ERRORS_MIN,IER)
        CALL EZGET_i('PRINT_ERRORS_MAXLINE',MAXLINE,IER)
        CALL EZGET_l('USE_CGEV_FOR_ERRORS',USE_CGEV,IER)
        IF (IER .NE. 0) USE_CGEV = .FALSE.
        CALL EZGET_l('GET_SUMS',GET_SUMS,IER)
        IF (IER .NE. 0) THEN
          GET_SUMS = .FALSE.
        ENDIF
        CALL EZRSET
C
C ****  GET NOMINAL SIGMA'S,K'S
C
        CALL EZLOC('CAL_MODULE_RCP',K)
        IF (K.EQ.0) THEN
          CALL INRCP('D0$CALOR_OFF:CAL_MODULE.RCP',IER)
        END IF
        CALL EZPICK('CAL_MODULE_RCP')
        CALL EZGET('SIGMA_X8',SIGMA(1,1,0),IER)
        CALL EZGET('SIGMA_X1',SIGMA(1,1,1),IER)
        CALL EZGET('CGEV_K',  CGEV_K(1,1), IER)
        CALL EZRSET
C
C ****  Get CELL dimensions in cylindrical coordinates
C
        CALL CELLSIZE(1,NLYRL,DR,DPHI,DZ,IER)
        IF (IER .EQ. 0) THEN
C
C ****    Compute SigR**2, SigPHI**2, SigZ**2
C
          DO J = 1, NLYRL
            DO I = 1, NETAL
              DR(I,J)   = DR(I,J)*DR(I,J)
              DPHI(I,J) = DPHI(I,J)*DPHI(I,J)
              DZ(I,J)   = DZ(I,J)*DZ(I,J)
            ENDDO
          ENDDO
        ELSE
          DO J = 1, NLYRL
            DO I = 1, NETAL
              DR(I,J)   = 0.
              DPHI(I,J) = 0.
              DZ(I,J)   = 0.
            ENDDO
          ENDDO
        ENDIF
C
C ****  DEBUG PRINTOUT
C
        IPRINT = 0
        LUNOUT = SSUNIT()
        IF ( PRINT_ERRORS ) THEN
          WRITE(LUNOUT,1000)
          WRITE(LUNOUT,1001)
        ENDIF
      ENDIF                           ! FIRST
C
      IF (GET_SUMS) THEN
        EVT = EVONUM()
        IF ( OEVT .NE. EVT ) THEN
          OEVT = EVT
          ESUMX    = 0.
          ESUMY    = 0.
          ESUM     = 0.
          ASUM     = 0.
          BSUM     = 0.
          RESTSUM  = 0.
          ASUMX    = 0.
          BSUMX    = 0.
          ALFASUMX = 0.
          RESTSUMX = 0.
          ASUMY    = 0.
          BSUMY    = 0.
          ALFASUMY = 0.
          RESTSUMY = 0.
          ASUMT    = 0.
          BSUMT    = 0.
          ALFASUMT = 0.
          RESTSUMT = 0.
C
          NCELLS   = 0
          CSUM     = 0.
          CMIN     = 999999.0
          CMAX     =-999999.0
          PEDSUM   = 0.
        ENDIF
      ENDIF
C
      X2  = XC*XC
      Y2  = YC*YC
      Z2  = ZC*ZC
      RR  = X2+Y2
      DD  = RR + Z2
C
      ECELL = E
      IF (ABS(ECELL) .LT. 1.E-10) THEN
        ECELL = 0.
        WRITE(MESSAGE,'(''Cell energy='',E11.4)') E
        CALL ERRMSG('E cell low','CAEHFL_ERROR',MESSAGE,'W')
      ENDIF

      EE  = ECELL*ECELL
C
      DIST=SQRT(DD)
      COSX=XC/DIST
      COSXSQ = COSX*COSX
      COSY=YC/DIST
      COSYSQ = COSY*COSY
      SINTSQ = COSXSQ+COSYSQ
      IF (SINTSQ .GT. 1.) SINTSQ = 1.
      COSTSQ = 1.-SINTSQ
      IF (COSTSQ .LT. 0.) COSTSQ = 0.
C
      IF (GET_SUMS) THEN
        ESUMX = ESUMX + ECELL*COSX
        ESUMY = ESUMY + ECELL*COSY
        ESUM  = ESUM  + ECELL
      ENDIF
C
C
      IF    (( LAYER .EQ. 8 ) .OR.
     &       ( LAYER .EQ.10 ) ) THEN
C
C ****  MASSLESS GAPS
C
        A = SIGMA_MSG(1)
        B = SIGMA_MSG(2)
        ALPHA = SIGMA_MSG(3)
      ELSEIF ( LAYER .EQ. 9 ) THEN
C
C ****  ICD
C
        A = SIGMA_ICD(1)
        B = SIGMA_ICD(2)
        ALPHA = SIGMA_ICD(3)
      ELSEIF ( LAYER .LE. 7 ) THEN
C
C ****  EM
C
        A = SIGMA_EM(1)
        B = SIGMA_EM(2)
        ALPHA = SIGMA_EM(3)
      ELSE
C
C ****  HADRONIC
C
        A = SIGMA_HAD(1)
        B = SIGMA_HAD(2)
        ALPHA = SIGMA_HAD(3)
      ENDIF
C
C ****  GET CONVERSION CONSTANT FROM ADC TO TOTAL ENERGY
C

      IF(USE_CGEV) THEN
        CGEV(1) = 0.
        CGEV(2) = 0.
        CGEV(3) = 0.
        CALL GTCGEV(LAYER,IPHI,IETA,SCALE,NCGEV,CGEV,IER)
        C    = CGEV(1)
        SIGP = CGEV(2)
        PED  = CGEV(3)
      ELSE
        C =    CGEV_K(LAYER,ABS(IETA))
        SIGP = SIGMA(LAYER,ABS(IETA),SCALE)
      END IF
C
      SESQ = EE*A + C*ECELL*B + C*C*SIGP*SIGP
C
      IF (GET_SUMS) THEN
C
C ****  GET CONTRIBUTIONS TO VARIANCE IN TOTAL ENERGY
C
        ASUM    = ASUM + EE
        BSUM    = BSUM + C*ECELL
        RESTSUM = RESTSUM + C*C*SIGP*SIGP
        PEDSUM  = PEDSUM + PED
        CSUM    = CSUM + C
        NCELLS  = NCELLS + 1
        IF ( C .LT. CMIN ) THEN
          CMIN = C
        ENDIF
        IF ( C .GT. CMAX ) THEN
          CMAX = C
        ENDIF
      ENDIF
C
C ****  GET SIG**2 PHI, R, AND Z FOR THE CELL
C
      JETA  = IABS(IETA)
C
      SIGPHI= DPHI(JETA,LAYER)
      SIGR  = DR(JETA,LAYER)
      SIGZ  = DZ(JETA,LAYER)
C
C
      AAUX = (COSTSQ*SIGR+SINTSQ*SIGZ)/RR
      RAUX = (C*C*SIGP*SIGP+EE*SINTSQ*COSTSQ*SIGZV/RR)
C
C **** VARIANCE ON Ex
C
      CAX     = EE*COSXSQ
      CBX     = C*ECELL*COSXSQ
      CALPHAX = EE*( COSYSQ*SIGPHI+COSXSQ*COSTSQ*AAUX )
      RESTX   = COSXSQ*RAUX
C
      IF (ALPHA .GT. 0.) THEN
        EXYZMAT(1,1) = CAX*A + CBX*B + CALPHAX/ALPHA + RESTX
      ELSE
        EXYZMAT(1,1) = CAX*A + CBX*B +                 RESTX
      ENDIF
C
      IF (GET_SUMS) THEN
        ASUMX    = ASUMX    + CAX
        BSUMX    = BSUMX    + CBX
        ALFASUMX = ALFASUMX + CALPHAX
        RESTSUMX = RESTSUMX + RESTX
      ENDIF
C
C ****  VARIANCE ON Ey
C
      CAY     = EE*COSYSQ
      CBY     = C*ECELL*COSYSQ
      CALPHAY = EE*( COSXSQ*SIGPHI+COSYSQ*COSTSQ*AAUX )
      RESTY   = COSYSQ*RAUX
C
      IF (ALPHA .GT. 0.) THEN
        EXYZMAT(2,2) = CAY*A + CBY*B + CALPHAY/ALPHA + RESTY
      ELSE
        EXYZMAT(2,2) = CAY*A + CBY*B +                 RESTY
      ENDIF
C
      IF (GET_SUMS) THEN
        ASUMY    = ASUMY    + CAY
        BSUMY    = BSUMY    + CBY
        ALFASUMY = ALFASUMY + CALPHAY
        RESTSUMY = RESTSUMY + RESTY
      ENDIF
C
C ****  VARIANCE ON Et, taking into account (Ex, Ey) correlation
C
      CAT     = EE*SINTSQ
      CBT     = C*ECELL*SINTSQ
      CALPHAT = EE*SINTSQ*COSTSQ*AAUX
      RESTT   = SINTSQ*RAUX
C
      IF (ALPHA .GT. 0.) THEN
        SETSQ = CAT*A + CBT*B + CALPHAT/ALPHA + RESTT
      ELSE
        SETSQ = CAT*A + CBT*B +                 RESTT
      ENDIF
C
      IF (GET_SUMS) THEN
        ASUMT    = ASUMT    + CAT
        BSUMT    = BSUMT    + CBT
        ALFASUMT = ALFASUMT + CALPHAT
        RESTSUMT = RESTSUMT + RESTT
      ENDIF
C
C
      IF (ALPHA .GT. 0.) THEN
        AUX = (COSTSQ*SIGR/ALPHA+SINTSQ*(SIGZ/ALPHA+SIGZV))/RR
      ELSE
        AUX = (                  SINTSQ*(           SIGZV))/RR
      ENDIF
C
C ****  VARIANCE ON Ez
C
      EXYZMAT(3,3) = SESQ*COSTSQ + EE*SINTSQ*SINTSQ*AUX
C
C ****  CORRELATION (Ex, Ey)
C
      IF (ALPHA .GT. 0.) THEN
        EXYZMAT(1,2) = COSX*COSY*( SESQ-EE*(SIGPHI/ALPHA-COSTSQ*AUX) )
      ELSE
        EXYZMAT(1,2) = COSX*COSY*( SESQ-EE*(            -COSTSQ*AUX) )
      ENDIF
      EXYZMAT(2,1) = EXYZMAT(1,2)
C
C ****  CORRELATION (Ex, Ez)
C
      EXYZMAT(1,3) = COSX*SQRT(COSTSQ) * ( SESQ-EE*SINTSQ*AUX )
      EXYZMAT(3,1) = EXYZMAT(1,3)
C
C ****  CORRELATION (Ey, Ez)
C
      EXYZMAT(2,3) = COSY*SQRT(COSTSQ) * ( SESQ-EE*SINTSQ*AUX )
      EXYZMAT(3,2) = EXYZMAT(2,3)
C
C ****  Determinant of the matrix
C
      DET = EXYZMAT(1,1)*EXYZMAT(2,2)*EXYZMAT(3,3)
     &  +2.*EXYZMAT(1,2)*EXYZMAT(2,3)*EXYZMAT(1,3)
     &  -   EXYZMAT(1,1)*EXYZMAT(2,3)*EXYZMAT(2,3)
     &  -   EXYZMAT(2,2)*EXYZMAT(1,3)*EXYZMAT(1,3)
     &  -   EXYZMAT(3,3)*EXYZMAT(1,2)*EXYZMAT(1,2)
      IF (DET .LE. 0.) THEN
        EXYZMAT(1,2) = 0.
        EXYZMAT(2,1) = 0.
        EXYZMAT(1,3) = 0.
        EXYZMAT(3,1) = 0.
        EXYZMAT(2,3) = 0.
        EXYZMAT(3,2) = 0.
        DET = EXYZMAT(1,1)*EXYZMAT(2,2)*EXYZMAT(3,3)
        IF (DET .LE. 0.) THEN
          DO I=1,3
            DO J = 1,3
              EXYZMAT(I,J) = 0.
            ENDDO
          ENDDO
          SETSQ = 0.
          IER = -1
        ELSE
          IF ( SINTSQ .NE. 0.) THEN
            SETSQ = (COSXSQ*EXYZMAT(1,1)+COSYSQ*EXYZMAT(2,2))/SINTSQ
          ELSE
            SETSQ = 0.
          ENDIF
        ENDIF
      ENDIF
C
C
C ****  Optional print-out
C
      IF ( PRINT_ERRORS ) THEN
        IF ( SETSQ .GE. PRINT_ERRORS_MIN ) THEN
C
          RUN = RUNNO()
          EVT = EVONUM()
          WRITE(LUNOUT,'(1X,''Run, Event '',2I10)') RUN, EVT
          IPRINT = IPRINT + 1
C
          WRITE(LUNOUT,1010) IETA,IPHI,LAYER,
     &                       XC,YC,ZC,C,A,B,ALPHA,E
          IPRINT = IPRINT + 1
C
          IF (ALPHA .GT. 0.) THEN
            WRITE(LUNOUT,1011) SQRT(CAT*A),SQRT(CBT*B),
     &                         SQRT(CALPHAT/ALPHA),SQRT(RESTT),
     &                         SQRT(SETSQ)
          ELSE
            WRITE(LUNOUT,1011) SQRT(CAT*A),SQRT(CBT*B),
     &                         0.,                 SQRT(RESTT),
     &                         SQRT(SETSQ)
          ENDIF
          IPRINT = IPRINT + 1
C
          IF ( IPRINT .GE. MAXLINE ) THEN
            IPRINT = 0
            WRITE(LUNOUT,1000)
            WRITE(LUNOUT,1001)
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
C
C----------------------------------------------------------------------
      ENTRY GET_CAEHFL_ERROR_SUMS(E_SUMXY, E_SUM,
     &                            ERR_SUMS, ERRX_SUMS, ERRY_SUMS,
     &                            ERRT_SUMS,
     &                            N_CELLS, C_SUMS, PED_SUM, IER2)
C-
C-   Purpose and Methods : Outputs total energies and sums over cells
C-                         in error formulas:
C-
C-           Sig(E)**2 =  A * ERR_SUMS(1)          +
C-                        B * ERR_SUMS(2)          +
C-                            ERR_SUMS(3)
C-
C-           Sig(Ex)**2 = A * ERRX_SUMS(1)         +
C-                        B * ERRX_SUMS(2)         +
C-                            ERRX_SUMS(3) / ALPHA +
C-                            ERRX_SUMS(4)
C-
C-           Sig(Ey)**2 = A * ERRY_SUMS(1)         +
C-                        B * ERRY_SUMS(2)         +
C-                            ERRY_SUMS(3) / ALPHA +
C-                            ERRY_SUMS(4)
C-
C-           Sig(Et)**2 = A * ERRT_SUMS(1)         +
C-                        B * ERRT_SUMS(2)         +
C-                            ERRT_SUMS(3) / ALPHA +
C-                            ERRT_SUMS(4)
C-
C-   associated with parameters A, B, ALPHA and the rest. The parameters
C-   can be determined thru fits to the test beam data.
C-
C-   Inputs  : none
C-   Outputs : E_SUMXY(2)      [R] Ex, Ey sums over all cells
C-             E_SUM           [R] Total E sum over all cells
C-             ERR_SUMS(3)     [R] Sig(E)**2  sums
C-             ERRX_SUMS(4)    [R] Sig(Ex)**2 sums
C-             ERRY_SUMS(4)    [R] Sig(Ey)**2 sums
C-             ERRT_SUMS(4)    [R] Sig(Et)**2 sums
C-             N_CELLS         [I] Total number of cells used
C-             C_SUMS(3)       [R] Sum, min, max of conversion constants
C-                                 from ADC counts to GeV
C-             PED_SUM         [R] Pedestal sum
C-             IER2            [I] Error flag 0=OK
C
      IER2 = 0
C
      IF (GET_SUMS) THEN
        E_SUMXY(1)   = ESUMX
        E_SUMXY(2)   = ESUMY
        E_SUM        = ESUM
        ERR_SUMS(1)  = ASUM
        ERR_SUMS(2)  = BSUM
        ERR_SUMS(3)  = RESTSUM
        ERRX_SUMS(1) = ASUMX
        ERRX_SUMS(2) = BSUMX
        ERRX_SUMS(3) = ALFASUMX
        ERRX_SUMS(4) = RESTSUMX
        ERRY_SUMS(1) = ASUMY
        ERRY_SUMS(2) = BSUMY
        ERRY_SUMS(3) = ALFASUMY
        ERRY_SUMS(4) = RESTSUMY
        ERRT_SUMS(1) = ASUMT
        ERRT_SUMS(2) = BSUMT
        ERRT_SUMS(3) = ALFASUMT
        ERRT_SUMS(4) = RESTSUMT
C
        N_CELLS      = NCELLS
        C_SUMS(1)    = CSUM
        C_SUMS(2)    = CMIN
        C_SUMS(3)    = CMAX
        PED_SUM      = PEDSUM
      ELSE
        IER2 = -1
        E_SUMXY(1) = 0.
        E_SUMXY(2) = 0.
        E_SUM = 0.
        ERR_SUMS(1) = 0.
        ERR_SUMS(2) = 0.
        ERR_SUMS(3) = 0.
        DO I = 1,4
          ERRX_SUMS(I) = 0.
          ERRY_SUMS(I) = 0.
          ERRT_SUMS(I) = 0.
        ENDDO        
C
        N_CELLS = 0
        C_SUMS(1) = 0.
        C_SUMS(2) = 0.
        C_SUMS(3) = 0.
        PED_SUM = 0.
      ENDIF
C
      RETURN
C----------------------------------------------------------------------
C
C ****  FORMATS
C
 1000 FORMAT(1X,' IETA',' IPHI',' ILYR',
     &       5X,'XC',5X,'YC',5X,'ZC','      C','      A','      B',
     &       '  ALPHA','  ENERGY')
 1001 FORMAT(1X,'  SIGETA','  SIGETB',' SIGETAL',' SIGETRE','   SIGET')
 1010 FORMAT(1X,3I5,3(1X,F6.1),F7.4,3(F7.2),F7.3)
 1011 FORMAT(1X,5(1X,F7.3))
C
      END
