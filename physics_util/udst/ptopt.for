      SUBROUTINE PTOPT(PX,PY)
C----------------------------------------------------------------------
C-
C-   Created   1-APR-1995   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
      INTEGER LCAEP,NCH,NR,PWORD,IETA,IPHI,LAYER,IER,I,LPTR,GZVERT
      INTEGER LCAEQ,GZCAEQ,IENR,JBYT,ISCALE,HL,IVERS,IEMAX,LVERT,GZCAEP
      data IEMAX / z'1FFF' / ! E stored in 13 bits - assume always true!!
      REAL    XV,YV,ZV,XC,YC,ZC,E,R,PX,PY,W
      REAL OFFSET, E_SCALE_1, E_PREC_2
C
      PX=0.
      PY=0.
      ZV=0.
      LVERT = GZVERT(0)
      IF(LVERT.GT.0)THEN
        XV = Q(LVERT+3)
        YV = Q(LVERT+4)
        ZV = Q(LVERT+5)
      ENDIF
      LCAEP=GZCAEP()
      IF(LCAEP.GT.0)THEN
        NR=IQ(LCAEP+2)
        NCH=IQ(LCAEP+3)
        LPTR=LCAEP
        DO I=1,NCH
          PWORD=IQ(LPTR+4+(I-1)*NR)
          CALL CAEP_INDICES(PWORD,IETA,IPHI,LAYER)
          CALL CELXYZ(IETA,IPHI,LAYER,XC,YC,ZC,IER)
          IF(IER.EQ.0)THEN
            R=(XC-XV)**2+(YC-YV)**2+(ZC-ZV)**2
            IF(R.GT.0)THEN
              R=SQRT(R)
              E=Q(LPTR+5+(I-1)*NR) 
              IF(E.GT.0.AND.LAYER.LE.14)THEN
                IF(LAYER.LE.7)THEN
                  W=1.
                ELSEIF(LAYER.LE.10)THEN
                  W=0.1
                ELSEIF (E.LT.0.3) THEN
                  W=0.1
                ELSEIF (E.LT.1.0) THEN
                  W=0.6
                ELSE
                  W=1.
                ENDIF
                PX=PX-W*E*(XC-XV)/R
                PY=PY-W*E*(YC-YV)/R
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ELSE
        LCAEQ=GZCAEQ()         ! Handles QCD or D0 mdst
        IF (LCAEQ.LE.0) GOTO 999
        IVERS = IQ( LCAEQ + 1)        ! Version number
        HL    = IQ(LCAEQ+2)           ! Header length
        NCH   = IQ(LCAEQ-1) - HL      ! # of cells = total - #header
        IF ( IVERS .GT. 1 ) THEN
          OFFSET    = Q( LCAEQ + 4)
          E_SCALE_1 = Q( LCAEQ + 5)
          E_PREC_2  = Q( LCAEQ + 6)
        ELSE
          OFFSET    = .500
          E_SCALE_1 = 8.91
          E_PREC_2  = .05
        ENDIF
        DO I = 1, NCH
          PWORD = IQ(LCAEQ+I+HL)
          IETA = JBYT(PWORD, 1, 7) - 37
          IF (IETA.LE.0) IETA = IETA - 1
          IPHI = JBYT(PWORD, 8, 6) + 1
          LAYER = JBYT(PWORD,14, 5)
          ISCALE = JBYT(PWORD,19,1)
          IENR = JBYT(PWORD,20,13)
          IF (ISCALE.EQ.0) THEN
            E = FLOAT(IENR)*(E_SCALE_1+OFFSET)/FLOAT(IEMAX)
          ELSE
            E = E_PREC_2*IENR + E_SCALE_1 + OFFSET
          ENDIF
          E = E - OFFSET
          CALL CELXYZ(IETA,IPHI,LAYER,XC,YC,ZC,IER)
          IF(IER.EQ.0)THEN
            R=(XC-XV)**2+(YC-YV)**2+(ZC-ZV)**2
            IF(R.GT.0)THEN
              R=SQRT(R)
              IF(E.GT.0.AND.LAYER.LE.14)THEN
                IF(LAYER.LE.7)THEN
                  W=1.
                ELSEIF(LAYER.LE.10)THEN
                  W=0.1
                ELSEIF (E.LT.0.3) THEN
                  W=0.1
                ELSEIF (E.LT.1.0) THEN
                  W=0.6
                ELSE
                  W=1.
                ENDIF
                PX=PX-W*E*(XC-XV)/R
                PY=PY-W*E*(YC-YV)/R
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
