      SUBROUTINE OBJECT_PHOTON1(PHOTON_TYPE,IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns PHOTON information for Top analysis
C-                         Has to have PARTICLE_SELECT package
C-
C-   Inputs  : PHOTON_TYPE =  'GAM_TGHT' 'GAM_LSE' 'MCG_TGHT'
C-             IDX      [I]   Object Number
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
C-   Created:   4-NOV-1994   Rajendran Raja  BASED ON OBJECT_PHOTON
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PHOTON_TYPE
      INTEGER NPHOTONS,NPHOINF
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
      INCLUDE 'D0$LINKS:IZHMTP.LINK'
C----------------------------------------------------------------------
      INTEGER NN
      INTEGER LBANK, LH,BANKLENGTH
      INTEGER LVERT,GZVERT
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
C
C will allow this many max electrons of any type
C
      INTEGER MAX_PHOTONS
      PARAMETER( MAX_PHOTONS = 5 )
      INTEGER PHOTON_LINKS(MAX_PHOTONS)
C----------------------------------------------------------------------
C
C ****  OBJECT: PHOTON
C
      NN = MIN(NMAX,MAXBUF)
      CALL VZERO(BUFFER,NN)
C
      IF(NMAX .GT. MAXBUF) THEN
        CALL ERRMSG('OBJECT_PHOTON1','OBJECT_PHOTON1',
     &    'Too much info requested. Will set to MAXBUF','I')
      ENDIF
C
      CALL GTSLINK_ONE(PHOTON_TYPE,IDX,LBANK)
      BANKLENGTH = IQ(LBANK-1)
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
      VERSION = IQ(LBANK+1)
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
  999 RETURN
C
C
C**************************************************************************
C
      ENTRY NOBJ_PHOTONS1(PHOTON_TYPE,NPHOTONS,NPHOINF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used in top analysis
C-
C-   Inputs  : PHOTON_TYPE =  'GAM_TGHT' 'GAM_LSE' 'MCG_TGHT'
C-
C-   Outputs  :  NPHOTONS     Number of photons
C-               NPHOINF    Number of information per photons
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JUN-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      CALL GTSLINK(PHOTON_TYPE,MAX_PHOTONS,NPHOTONS,
     &  PHOTON_LINKS)
      NPHOINF = MAXBUF
C----------------------------------------------------------------------
      RETURN
      END
