      SUBROUTINE GET_CELL_PARAMS(IETA,IPHI,LAYER,NPAR,PAR,EXYZMAT,
     &                           SETSQ,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     A shell around CAEHFL_ERROR routine providing cell 
C-     energy, cell coordinates and dimensions, an ERROR MATRIX in
C-     cell energy components and transverse energy error.
C-
C- ==> IMPORTANT: Banks CGEV and CAEP (or CACL/CASH) must be present !!! 
C-
C-   Inputs  : IETA,IPHI,LAYER [I] Cell physics address
C-             NPAR            [I] Size of PAR vector: could be from 1 to 7
C-   Outputs : PAR(NPAR)       [R] Useful cell parameters:
C-                                  E          cell energy from CAEP/CASH
C-                                  XC,YC,ZC   cell center wrt vertex
C-                                  DR,DPHI,DZ cell dimensions
C-             EXYZMAT(3,3)    [R] Error matrix in Ex, Ey, Ez
C-             SETSQ           [R] Sigma**2 Et (correlation (Ex,Ey) taken
C-                                              care of !)
C-             STATUS          [I] Error flag:
C-                                  0 - OK
C-                                 -1 - No cell info (no CAEP or CASH)  
C-                                 -2 - Cell position not available
C-                                 -3 - Failed to get errors
C-   Controls: None
C-
C-   Created   6-APR-1993   Stan M. Krzywdzinski
C-   Modified 10-NOV-1993   Stan M. Krzywdzinski
C-     Corrected declaration of FIRST (sin detected by FLINT)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,LAYER,NPAR,STATUS
      REAL PAR(NPAR),EXYZMAT(3,3),SETSQ
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C----------------------------------------------------------------------
      INTEGER PACKED_ADDRESS
      BYTE BYTES(4)
      EQUIVALENCE (PACKED_ADDRESS,BYTES)
C----------------------------------------------------------------------
      INTEGER GZCAEP,GZCACL,GZCASH,LNCH
      INTEGER NRP,NCH,I,IIETA,IIPHI,ILAYER,INFO,SCALE,IOK,NV,IPAR
      LOGICAL CFOUND
      REAL    E,XC,YC,ZC,SIGZV
      INTEGER    NVMAX
      PARAMETER (NVMAX = 10)
      REAL    XYZV(3,NVMAX),DXYZV(3,NVMAX)
      REAL    DR(NETAL,NLYRL),DPHI(NETAL,NLYRL),DZ(NETAL,NLYRL)
      LOGICAL FIRST
C----------------------------------------------------------------------
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      CALL VZERO(PAR,NPAR)
      CALL VZERO(EXYZMAT,9)
      STATUS = 0
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL CELLSIZE(1,NLYRL,DR,DPHI,DZ,IOK)
        IF (IOK .NE. 0) THEN
          CALL VZERO(DR,  NLYRL*NETAL)
          CALL VZERO(DPHI,NLYRL*NETAL)
          CALL VZERO(DZ,  NLYRL*NETAL)
        ENDIF
      ENDIF
C
      CFOUND = .FALSE.
C
C *** First search CAEP bank for the requested cell 
C
      LCAEP = GZCAEP()
      IF (LCAEP .GT. 0) THEN
        NRP=IQ(LCAEP+2)
        NCH=IQ(LCAEP+3)
C
        DO I=1,NCH
          LNCH=LCAEP+(I-1)*NRP
          PACKED_ADDRESS = IQ(LNCH+4)
          IIETA = BYTES(BYTE4)
          IIPHI = BYTES(BYTE3)
          ILAYER= BYTES(BYTE2)
          CFOUND = (IIETA .EQ.IETA )  .AND.
     &             (IIPHI .EQ.IPHI )  .AND.
     &             (ILAYER.EQ.LAYER)
          IF ( CFOUND ) THEN
            INFO = BYTES(BYTE1)
C ***       Get scale (x8 or x1)
            IF ( BTEST(INFO,1) ) THEN
              SCALE = 1
            ELSE
              SCALE = 0
            ENDIF
C
            E    = Q(LNCH+5)
            GO TO 100
          ENDIF
        ENDDO
      ENDIF
C
C *** Next try CACL & CASH banks to get the requested cell info
C
      LCACL = GZCACL()
      DO WHILE (LCACL .GT. 0)
        LCASH = LQ(LCACL-2)
        DO WHILE (LCASH .GT. 0)
          NRP=2
          NCH=IQ(LCASH+2)
C
          DO I=1,NCH
            LNCH=LCASH+(I-1)*NRP
            PACKED_ADDRESS = IQ(LNCH+3)
            IIETA = BYTES(BYTE4)
            IIPHI = BYTES(BYTE3)
            ILAYER= BYTES(BYTE2)
            CFOUND = (IIETA .EQ.IETA )  .AND.
     &               (IIPHI .EQ.IPHI )  .AND.
     &               (ILAYER.EQ.LAYER)
            IF ( CFOUND ) THEN
              INFO = BYTES(BYTE1)
C ***         Get scale (x8 or x1)
              IF ( BTEST(INFO,1) ) THEN
                SCALE = 1
              ELSE
                SCALE = 0
              ENDIF
C
              E    = Q(LNCH+4)
              GO TO 100
            ENDIF
          ENDDO
          LCASH = LQ(LCASH)
        ENDDO
        LCACL = LQ(LCACL)
      ENDDO
C
      STATUS = -1
      CALL ERRMSG('No cell data','GET_CELL_PARAMS',
     &  ' Cell neither found in CAEP nor in CASH','W')
      GO TO 999
C
C
  100 CONTINUE
C
C *** Get position of vertices and their errors
C
      CALL CAEHFL_VERTEX(NVMAX,XYZV,DXYZV,NV,IOK)
C
C *** Get cell coordinates in detector frame
C
      CALL CELXYZ(IETA,IPHI,LAYER,XC,YC,ZC,IOK)
C
      IF(IOK.EQ.0) THEN
        IF (NV .GT. 0) THEN
C
C ***     Use 1-st vertex to offset
C
          XC = XC-XYZV(1,1)
          YC = YC-XYZV(2,1)
          ZC = ZC-XYZV(3,1)
          SIGZV = DXYZV(3,1)*DXYZV(3,1)
        ELSE
          SIGZV = 0.
        ENDIF
C
        CALL CAEHFL_ERROR(IETA,IPHI,LAYER,SCALE,E,XC,YC,ZC,SIGZV,
     &    EXYZMAT,SETSQ,IOK)
        IF (IOK .NE. 0) THEN
          STATUS = -3
          CALL ERRMSG('No cell errors','GET_CELL_PARAMS',
     &      ' CAEHFL_ERROR failed to get error matrix','W')
          GO TO 999
        ENDIF
C
        IPAR = 1
        IF (IPAR .LE. NPAR) PAR(IPAR) = E
        IPAR = 2
        IF (IPAR .LE. NPAR) PAR(IPAR) = XC
        IPAR = 3
        IF (IPAR .LE. NPAR) PAR(IPAR) = YC
        IPAR = 4
        IF (IPAR .LE. NPAR) PAR(IPAR) = ZC
        IPAR = 5
        IF (IPAR .LE. NPAR) PAR(IPAR) =   DR(IETA,LAYER)
        IPAR = 6
        IF (IPAR .LE. NPAR) PAR(IPAR) = DPHI(IETA,LAYER)
        IPAR = 7
        IF (IPAR .LE. NPAR) PAR(IPAR) =   DZ(IETA,LAYER)
      ELSE
        STATUS = -2
        CALL ERRMSG('No cell XYZ','GET_CELL_PARAMS',
     &    ' Cannot get cell position','W')
        GO TO 999
      ENDIF
C
C
  999 RETURN
      END
