      SUBROUTINE OBJECT_MISSING_ET(IDX,NMAX,ARRAY)
      ENTRY OBJECT_ETMISS(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object MISSING-ET
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
C-              ARRAY(I+3)    SigEt
C-              ARRAY(I+4)    EtScalar
C-              ARRAY(I+5)    (SigEz)**2
C-              ARRAY(I+6)    dExdEy
C-              ARRAY(I+7)    dExdEz
C-              ARRAY(I+8)    dEydEz
C-   Controls:
C-
C-   Created   1-DEC-1992   Harrison B. Prosper
C-   Updated  23-MAY-1993   Harrison B. Prosper
C-      Add full error matrix
C-   Updated  14-SEP-1993   Harrison B. Prosper
C-      Add check on values in bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
C----------------------------------------------------------------------
      INTEGER STATUS, I, J, K, L, II, JJ, NN, N
      INTEGER NUMBER
      INTEGER LBANK, GZPNUT
      INTEGER LVERT, GZVERT, BANKLENGTH
      REAL    ZVTX, THETA
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM  =   9 )
      INTEGER MAXBUF
      PARAMETER( MAXBUF  = MINNUM + 8 )
C----------------------------------------------------------------------
      INTEGER IBUFFER(MAXBUF)
      REAL    BUFFER(MAXBUF)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
C----------------------------------------------------------------------
      SAVE NUMBER
C----------------------------------------------------------------------
C
C ****  OBJECT: MISSING ET
C
      II = MIN(IDX,NUMBER)
      NN = MIN(NMAX,MAXBUF)
      CALL VZERO(BUFFER,NN)
C
      IF ( II .GT. 0 ) THEN
        LBANK = GZPNUT(II)
C
C ****  FIXED PART
C
        IF ( LBANK .GT. 0 ) THEN
          BANKLENGTH = IQ(LBANK-1)
C
          CALL UCOPY(Q(LBANK+3),BUFFER(IPX),5)
          BUFFER(IETA) = Q(LBANK+9)
          BUFFER(IPHI) = Q(LBANK+10)
C
          LVERT = GZVERT(1)
          ZVTX = 0.
          IF(LVERT.GT.0) ZVTX = Q(LVERT+5)
          THETA = Q(LBANK+8)
          IF ( ABS(THETA) .GT. 0.0  ) THEN
            CALL DET_ETA(ZVTX, THETA, BUFFER(IDETA))
          ELSE
            BUFFER(IDETA)=-999.0
          ENDIF
C
C ****  OTHER
C
          IF ( BANKLENGTH .GE. 18 ) THEN
            N = 8
          ELSE
            N = 4
          ENDIF
          CALL UCOPY(Q(LBANK+11),BUFFER(IX1),N)
        ENDIF
        CALL UCOPY(BUFFER,ARRAY,NN)
      ENDIF
      RETURN
C
      ENTRY NOBJ_MISSING_ET(NOBJS,NSIZE)
      ENTRY NOBJ_ETMISS(NOBJS,NSIZE)
C
      LBANK = GZPNUT(0)
      IF ( LBANK .GT. 0 ) THEN
        NUMBER= IQ(LBANK-5)
      ELSE
        NUMBER= 0
      ENDIF
C
      NOBJS = NUMBER
      NSIZE = MAXBUF
C
  999 RETURN
      END
