      SUBROUTINE OBJECT_PHOTON(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object PHOTON.
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
C-              ARRAY(I+10)   Spare
C-              ARRAY(I+11)   Spare
C-              ARRAY(I+12)   Spare
C-              ARRAY(I+13)   Spare
C-              ARRAY(I+14)   Spare
C-              ARRAY(I+15)   Spare
C-              ARRAY(I+16)   Spare
C-              ARRAY(I+17)   Sig**2(Ez)
C-              ARRAY(I+18)   dExdEy
C-              ARRAY(I+19)   dExdEz
C-              ARRAY(I+20)   dEydEz
C-   Controls:
C-
C-   Created   1-DEC-1992   Harrison B. Prosper
C-   Updated  21-MAY-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
      INCLUDE 'D0$LINKS:IZHMTP.LINK'
C----------------------------------------------------------------------
      INTEGER STATUS, I, J, K, L, II, JJ, NN
      INTEGER NUMBER, BANKLENGTH
      INTEGER LBANK, GZPPHO, LH,LVERT,GZVERT
      INTEGER QUALITY,OK
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM = 9 )
      INTEGER MAXBUF
      PARAMETER( MAXBUF = MINNUM + 20 )
C----------------------------------------------------------------------
      INTEGER IBUFFER(MAXBUF)
      REAL    BUFFER(MAXBUF)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
      REAL    THETA,ZVTX
      INTEGER VERSION
C----------------------------------------------------------------------
      SAVE NUMBER
C----------------------------------------------------------------------
C
C ****  OBJECT: PHOTON
C
      II = MIN(IDX,NUMBER)
      NN = MIN(NMAX,MAXBUF)
      CALL VZERO(BUFFER,NN)
C
      IF ( II .GT. 0 ) THEN
        LBANK = GZPPHO()
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
        CALL UCOPY(Q(LBANK+3),BUFFER(IPX),5)
        BUFFER(IETA)  = Q(LBANK+9)
        BUFFER(IPHI)  = Q(LBANK+10)
C
        LVERT=GZVERT(1)
        ZVTX=0.
        IF(LVERT.GT.0) ZVTX=Q(LVERT+5)
        THETA=Q(LBANK+8)
        CALL DET_ETA(ZVTX,THETA,BUFFER(IDETA))
C
        IF (VERSION.EQ.1) THEN
          IBUFFER(IQUALITY) = IQ(LBANK+20)
        ELSE IF (VERSION.EQ.2) THEN
          IBUFFER(IQUALITY) = IQ(LBANK+23)
        ELSE
          IBUFFER(IQUALITY) = IQ(LBANK+30)
        ENDIF
C
        CALL UCOPY(Q(LBANK+11),BUFFER(IX1),2)   !SIG**2
        CALL UCOPY(Q(LBANK+14),BUFFER(IX3),5)   !ISOLATION
C
        LH = LQ(LBANK-IZHMTP)
        IF ( LH .GT. 0 ) THEN
          BUFFER(IX8) = Q(LH+5)                 !CHISQ
          BUFFER(IX9) = Q(LH+7)                 !TRUNCATED CHISQ
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
      ENTRY NOBJ_PHOTONS(NOBJS,NSIZE)
      CALL GTPPHO_TOTAL(NUMBER,STATUS)
      NOBJS = NUMBER
      NSIZE = MAXBUF
  999 RETURN
      END
