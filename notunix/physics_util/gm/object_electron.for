      SUBROUTINE OBJECT_ELECTRON(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object ELECTRON.
C-
C-   Inputs  : IDX      [I]   Object Number
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C-              ARRAY(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-              ARRAY(I+1)    (SigEx)**2
C-              ARRAY(I+2)    (SigEy)**2
C-              ARRAY(I+3)    EM energy in cluster outside central tower
C-              ARRAY(I+4)    Total energy in core cone
C-              ARRAY(I+5)    Total energy in isolation cone
C-              ARRAY(I+6)    EM energy in core cone
C-              ARRAY(I+7)    EM energy in isolation cone
C-              ARRAY(I+8)    CHISQ
C-              ARRAY(I+9)    TRUNCATED CHISQ
C-              ARRAY(I+10)   Number of central tracks in cluster road
C-              ARRAY(I+11)   Distance of closest approach of central track
C-              ARRAY(I+12)   CDC MIP
C-              ARRAY(I+13)   FDC MIP
C-              ARRAY(I+14)   VTX MIP
C-              ARRAY(I+15)   TRD anode likelihood efficiency
C-              ARRAY(I+16)   Spare
C-              ARRAY(I+17)   Sig**2(Ez)
C-              ARRAY(I+18)   dExdEy
C-              ARRAY(I+19)   dExdEz
C-              ARRAY(I+20)   dEydEz
C-
C-   Controls:
C-
C-   Created   1-DEC-1992   Harrison B. Prosper
C-   Updated  21-MAY-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-    Add full error matrix
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
      INCLUDE 'D0$LINKS:IZHMTE.LINK'
C----------------------------------------------------------------------
      INTEGER STATUS, I, J, K, L, II, JJ, NN
      INTEGER NUMBER
      INTEGER LBANK, GZPELC, LH,BANKLENGTH
      INTEGER QUALITY,OK
      INTEGER LZTRK,LDTRK,LFDCT,LVTXT,LTRDT,LVERT,GZVERT
      REAL    THETA,ZVTX
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM = 9 )
      INTEGER MAXBUF
      PARAMETER( MAXBUF = MINNUM + 20 )
C----------------------------------------------------------------------
      INTEGER VERSION
      INTEGER IBUFFER(MAXBUF)
      REAL    BUFFER(MAXBUF)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
C----------------------------------------------------------------------
      SAVE NUMBER
C----------------------------------------------------------------------
C
C ****  OBJECT: ELECTRON
C
      II = MIN(IDX,NUMBER)
      NN = MIN(NMAX,MAXBUF)
      CALL VZERO(BUFFER,NN)
C
      IF ( II .GT. 0 ) THEN
        LBANK = GZPELC()
C
C ****  Get bank length
C
        IF ( LBANK .GT. 0 ) THEN
          BANKLENGTH = IQ(LBANK - 1)
        ENDIF
C
        I = 0
        DO WHILE ( I .LT. NUMBER )
          I = I + 1
          IF ( I .EQ. II ) THEN
            I = NUMBER
          ELSE
            LBANK = LQ(LBANK)
          ENDIF
        ENDDO
C
C ****  Fill fixed part
C
        CALL UCOPY(Q(LBANK+3),BUFFER(IPX),5)
        BUFFER(IETA)  = Q(LBANK+9)
        BUFFER(IPHI)  = Q(LBANK+10)
C
        LVERT=GZVERT(1)
        ZVTX=0.
        IF(LVERT.GT.0) ZVTX=Q(LVERT+5)
C
        THETA=Q(LBANK+8)
        CALL DET_ETA(ZVTX,THETA,BUFFER(IDETA))
        VERSION = IQ(LBANK+1)
        IF (VERSION.EQ.1) THEN
          IBUFFER(IQUALITY) = IQ(LBANK+20)
        ELSE IF (VERSION.EQ.2) THEN
          IBUFFER(IQUALITY) = IQ(LBANK+20)
        ELSE
          IBUFFER(IQUALITY) = IQ(LBANK+30)
        ENDIF
C
C ****  Fill rest
C
        CALL UCOPY(Q(LBANK+11),BUFFER(IX1),2) ! SIG**2
        CALL UCOPY(Q(LBANK+14),BUFFER(IX3),5) ! ISOLATION
C
        LH = LQ(LBANK-IZHMTE)
        IF ( LH .GT. 0 ) THEN
          BUFFER(IX8) = Q(LH+5)     ! CHISQ
          BUFFER(IX9) = Q(LH+7)     ! TRUNCATED CHISQ
        ENDIF
C
        BUFFER(IX10) = Q(LBANK+21)   ! NUMBER OF CD TRACKS
        BUFFER(IX11) = Q(LBANK+22)   ! DISTANCE OF CLOSEST APPROACH
C
C ****  NOW HANDLE TRACKING...
C
        DO I = 1,4
          BUFFER(IX11+I) = -1       ! Default if no information
        ENDDO
        LZTRK = LQ(LBANK-3)             ! Link to associated ZTRAK bank
        IF(LZTRK .GT. 0) THEN
          LVTXT = LQ(LZTRK-6)
          LDTRK = LQ(LZTRK-7)
          LFDCT = LQ(LZTRK-8)
          IF(LDTRK.NE.0) BUFFER(IX12) = Q(LDTRK+20) ! Ionization in chamber
          IF(LFDCT.NE.0) BUFFER(IX13) = Q(LFDCT+20)
          IF(LVTXT.NE.0) BUFFER(IX14) = Q(LVTXT+20)
          LTRDT = LQ(LZTRK-9)             ! Link to associated TRD bank
          IF(LTRDT .GT. 0) THEN
            BUFFER(IX15)  = Q(LTRDT+15)! Efficiency derived from likelihood
          ENDIF
        ENDIF
C
C ****  Add rest of error matrix
C
        IF ( BANKLENGTH .GE. 31 ) THEN
          CALL UCOPY(Q(LBANK+26),BUFFER(IX17),4)
        ENDIF
C
        CALL UCOPY(BUFFER,ARRAY,NN)
      ENDIF
      RETURN
C
      ENTRY NOBJ_ELECTRONS(NOBJS,NSIZE)
      CALL GTPELC_TOTAL(NUMBER,STATUS)
      NOBJS = NUMBER
      NSIZE = MAXBUF
C
  999 RETURN
      END
