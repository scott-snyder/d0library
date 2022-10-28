      SUBROUTINE GET_TTEE_JETS(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-NOV-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VERT.INC'
      INCLUDE 'D0$INC:JET.INC'
C
      REAL    OLDE(5),OLDPHI,OLDETA,OLDEMF,EM_SCALE(3)
      REAL    NEWE(5),NEWEN,NEWET,NEWPHI,NEWETA,CORR_FAC,DE(5)
      REAL    ET,ET_CORR,FICD,THETA,ETAD,PHI,ETA,FEM
      INTEGER RVERSION,LJETS,GZJETS,ISYS
      INTEGER IER,I,J,N,JET_ALGO,NCELL
      LOGICAL DO_ZSP,DO_UND,DO_OOC
      LOGICAL MC,FOUND_MATCH,FIRST
      REAL    TEMPLATE(5,4)
      DATA    FIRST/.TRUE./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGETA_i('EM_SCALE_FACTORS',0,0,0,N,IER)
        CALL EZGETA('EM_SCALE_FACTORS',1,N,1,EM_SCALE,IER)
        CALL EZRSET
        MC = .FALSE.
        IF(LQ(LHEAD-17) .GT. 0)THEN
          MC = .TRUE.
          RVERSION = 0
        ENDIF
        DO_ZSP = .TRUE.
        DO_UND = .TRUE.
        DO_OOC = .TRUE.
        FIRST = .FALSE.
      ENDIF
C
C ** Initialize
c
      DO I = 1,MAX_JET
        DO J = 1,8
          JET(J,I) = 0.0
        ENDDO
        DO J = 1,6
          NNJET(J,I) = 0.0
        ENDDO
      ENDDO
C
      CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(IER.NE.0)CALL ERRMSG('SET_CAPH','GET_TTEE_JETS',' ','W')
C
      I = 0
      NJET = 0
      LJETS = GZJETS()
      IF(LJETS.GT.0) THEN
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS = GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS = GZJETS()
        DO WHILE (LJETS.GT.0)
          I = I+1
          IF(I .GT. MAX_JET)GOTO 100
C
          CALL UCOPY(Q(LJETS+2),OLDE,5)
          OLDPHI = Q(LJETS+8)
          OLDETA = Q(LJETS+9)
          OLDEMF = Q(LJETS+14)
          ISYS = 0
          JET_ALGO = 2
          IF(MC) THEN
            CALL MC_ET_CORR(OLDE,OLDETA,OLDPHI,OLDEMF,JET_ALGO,
     &          NEWE,NEWETA,NEWPHI,DE,FOUND_MATCH)
          ELSE
            CALL JET_CONE_CORR(LJETS,DO_ZSP,DO_UND,DO_OOC,EM_SCALE,
     &          ZVERT,ISYS,NEWEN,NEWET,NEWETA,NEWPHI,NEWE,FOUND_MATCH)
          ENDIF
C
          CORR_FAC = NEWEN/OLDE(4)
          ET       = SQRT(OLDE(1)**2+OLDE(2)**2)
          ET_CORR  = SQRT(NEWE(1)**2+NEWE(2)**2)
          NCELL    = IQ(LJETS+16)
          FICD     = Q(LJETS+17)
          THETA    = 2*ATAN(EXP(-NEWETA))
          CALL DET_ETA(ZVERT,THETA,ETAD)
C
          NJET = NJET+1
          JET(1,I) = ET
          JET(2,I) = ET_CORR
          JET(3,I) = NEWPHI
          JET(4,I) = NEWETA
          JET(5,I) = FICD
          JET(6,I) = OLDEMF
          JET(7,I) = NCELL
          JET(8,I) = ETAD
C
          LJETS = LQ(LJETS)
        ENDDO
      ENDIF
  100 CONTINUE
      NUM_JET = NJET        !Initial value of NUM_JET (number of good
      DO I = 1,NJET         !jets) and IJET the array containing the
        IJET(I) = I         !jet numbers of good jets before applying
      ENDDO                 !jet cuts to determine good jets
      DO I = NJET+1,MAX_JET
        IJET(I) = 0
      ENDDO
      CALL reset_caph
C
      CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
      I = 0
      NJETNN = 0
      LJETS = GZJETS()
      IF(LJETS.GT.0)THEN
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS = GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS = GZJETS()
        DO WHILE (LJETS.GT.0)
          I = I+1
          IF(I .GT. MAX_JET)GOTO 999
C
          ET  = SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          PHI = Q(LJETS+8)
          ETA = Q(LJETS+9)
          FEM = Q(LJETS+14)
          NCELL = IQ(LJETS+16)
          FICD  = Q(LJETS+17)
C
          NJETNN = NJETNN+1
          NNJET(1,I) = ET
          NNJET(2,I) = PHI
          NNJET(3,I) = ETA
          NNJET(4,I) = FICD
          NNJET(5,I) = FEM
          NNJET(6,I) = NCELL
C
          LJETS = LQ(LJETS)  ! pointer to next jet
        ENDDO
      ENDIF
      CALL reset_caph
C
  999 RETURN
      END
