      SUBROUTINE CAL_ECELL_LIST(VTX,ETA,PHI,NC, EEM,ETOT,ET,
     &                   NUM_CELL,LIST_ETA,LIST_PHI,LIST_LAY,DEDX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Finds the calorimeter energy contained
C-                          in a cone measured by number of cells
C-                          around the hit cells. The list of the
C-                          cells & energies in them are also returned.
C-
C-   Inputs  :   VTX(3) Event (or track) vertex point
C-               ETA    Eta of the cone axis (pseudo-rapidity)
C-               PHI    Phi of the cone axis (rad.)
C-               NC     # of cells around the hit cell (sort of cone size)
C-                       0 = consider the hit cells only
C-             *IER*    IF <0 then do not call clinpl in a second call but use
C-                        MUCACEL values instead. (**caution: For expert use
C-                        only). |IER| = 10 Then fill adjacent cells info
C-                        in MCACEL1.INC.
C-
C-   Outputs :   EEM     EM Energy in Cal.
C-               ETOT    Total energy in Cal. (EM + HAD)
C-               ET      Transverse total energy in cone.
C-          NUM_CELL     Total number of cells involved
C-       LIST_ETA(*)     list of IETA of cells     ! dimension < 10000
C-       LIST_PHI(*)     list of IPHI of cells
C-       LIST_LAY(*)     list of ILAY of cells
C-           DEDX(*)     List of dedx in each cell
C-
C-   Controls:   IER     0=OK, >1 then trouble.
C-
C-   Created  27-MAR-1992   SHAHRIAR ABACHI
C-   Modified 10-DEC-1993   SHAHRIAR ABACHI   Dimensions of arguments of call 
C-                                            to CNBORS modified to make FLINT
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NC
      REAL VTX(3),ETA,PHI,EEM,ETOT,ET
      INTEGER LIST_ETA(*),LIST_PHI(*),LIST_LAY(*)
      INTEGER IER
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MCACEL.INC'
      INCLUDE 'D0$INC:MCACEL1.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER ETAMIN,ETAMAX,PHIMIN,PHIMAX,LAYMIN,LAYMAX
      INTEGER NMAX,NMAX2,NUM_CELL,IER1
      PARAMETER (NMAX=10000)
      REAL DEDX(NMAX),ENRGT(NMAX)
      INTEGER ARGSOK,IET(4),IPH(4),ILY(4),NCMAX,IC
      INTEGER I,J,K,L,M,II,IJK,OK,NNBC,IDLY,IDET,IDPH,ICELL
      REAL THETA,DIRCOS(3)
      LOGICAL N_CELL(-NETAL:NETAL,NPHIL,NLYRL)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA NCMAX /9/
      SAVE N_CELL
C
      EEM = 0.0
      ETOT = 0.0
      ET = 0.0
      NUM_CELL = 0
      ETAMIN = NETAL
      ETAMAX = -NETAL
      PHIMIN = NPHIL
      PHIMAX = 1
      LAYMIN = NLYRL
      LAYMAX = 1
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        DO I=-NETAL,NETAL
          DO J=1,NPHIL
            DO K = 1,NLYRL
              N_CELL(I,J,K) = .FALSE.
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
      IF(IER .GE. 0) THEN
        THETA = 2.* ATAN(EXP(-ETA))
        DIRCOS(1) = SIN(THETA) * COS(PHI)
        DIRCOS(2) = SIN(THETA) * SIN(PHI)
        DIRCOS(3) = COS(THETA)
        CALL CLINPL(VTX,DIRCOS,NCLMAX,NCELL,IETAC,IPHIC,LAYERC,
     &                TLEN,ARGSOK)
        IF(ARGSOK .NE. 0) THEN
          WRITE(6,*) 'CAL_ECELL: PROBLEM IN CLINPL'
          IER = 1
          GOTO 999
        ENDIF
      ENDIF
      IER1 = IER
      IER = 0
C
      DO I=1,NCELL
        NUM_CELL = NUM_CELL + 1
        LIST_ETA(I) = IETAC(I)
        LIST_PHI(I) = IPHIC(I)
        LIST_LAY(I) = LAYERC(I)
        N_CELL(LIST_ETA(I),LIST_PHI(I),LIST_LAY(I)) = .TRUE.
        IF(LIST_ETA(I) .LT. ETAMIN) ETAMIN = LIST_ETA(I)
        IF(LIST_ETA(I) .GT. ETAMAX) ETAMAX = LIST_ETA(I)
        IF(LIST_PHI(I) .LT. PHIMIN) PHIMIN = LIST_PHI(I)
        IF(LIST_PHI(I) .GT. PHIMAX) PHIMAX = LIST_PHI(I)
        IF(LIST_LAY(I) .LT. LAYMIN) LAYMIN = LIST_LAY(I)
        IF(LIST_LAY(I) .GT. LAYMAX) LAYMAX = LIST_LAY(I)
      ENDDO
C
      IF(NC .LE. NCMAX) THEN
        DO IC=1,NC
          ICELL = 3
          DO I=1,NUM_CELL
            IDLY = -2
            DO L=1,ICELL
              IDLY = IDLY + 1
              IDET = -2
              DO J=1,ICELL
                IDET = IDET + 1
                IDPH = -2
                DO K=1,ICELL
                  IDPH = IDPH + 1
                  CALL CNBORS(LIST_ETA(I),LIST_PHI(I),LIST_LAY(I),
     &                         IDET,IDPH,IDLY,NNBC,IET,IPH,ILY,OK)
                  IF(OK .NE. 0) THEN
                    WRITE(6,*) 'CAL_ECELL: PROBLEM IN CNBORS'
                    IER = 2
                    GOTO 10
                  ENDIF
                  DO M=1,NNBC
                    IF(.NOT. N_CELL(IET(M),IPH(M),ILY(M))) THEN
                      N_CELL(IET(M),IPH(M),ILY(M)) = .TRUE.
                      NUM_CELL = NUM_CELL + 1
                      LIST_ETA(NUM_CELL) = IET(M)
                      LIST_PHI(NUM_CELL) = IPH(M)
                      LIST_LAY(NUM_CELL) = ILY(M)
                      IF(IET(M) .LT. ETAMIN) ETAMIN = IET(M)
                      IF(IET(M) .GT. ETAMAX) ETAMAX = IET(M)
                      IF(IPH(M) .LT. PHIMIN) PHIMIN = IPH(M)
                      IF(IPH(M) .GT. PHIMAX) PHIMAX = IPH(M)
                      IF(ILY(M) .LT. LAYMIN) LAYMIN = ILY(M)
                      IF(ILY(M) .GT. LAYMAX) LAYMAX = ILY(M)
                    ENDIF
                  ENDDO
   10           ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C
      ELSE
        WRITE(6,11) NCMAX
   11   FORMAT(5X,'From CAL_ECELL : Sorry, NC > '
     &      ,I4, 'is not availble at this time')
        IER = 3
      ENDIF
C
      DO I=1,NUM_CELL
        CALL CELL_ENERGY(LIST_ETA(I),LIST_PHI(I),
     &                       LIST_LAY(I),DEDX(I),ENRGT(I))
        ETOT = ETOT + DEDX(I)
        ET = ET + ENRGT(I)
        IF(IER1 .GE. 0 .AND. I .LE. NCELL) DECELL(I) = DEDX(I)
        IF(LIST_LAY(I) .LE. MXLYEM) THEN
          EEM = EEM + DEDX(I)
        ENDIF
      ENDDO
C
      IF(IABS(IER1) .EQ. 10) THEN
        K = 0
        NCELL1 = NUM_CELL - NCELL
        DO I = 1,NUM_CELL
          DO J = 1,NCELL
            IF( LIST_ETA(I) .NE. IETAC(J) .AND.
     &          LIST_PHI(I) .NE. IPHIC(J) .AND.
     &          LIST_LAY(I) .NE. LAYERC(J) ) THEN
              K = K + 1
              IETAC1(K) = LIST_ETA(I)
              IPHIC1(K) = LIST_PHI(I)
              LAYERC1(K) = LIST_LAY(I)
              DECELL1(K) = DEDX(I)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      IF(ETAMIN .GT. ETAMAX) THEN
        ETAMIN = -NETAL
        ETAMAX = NETAL
      ENDIF
      IF(PHIMIN .GT. PHIMAX) THEN
        PHIMIN = 1
        PHIMAX = NPHIL
      ENDIF
      IF(LAYMIN .GT. LAYMAX) THEN
        LAYMIN = 1
        LAYMAX = NLYRL
      ENDIF
  998 CONTINUE
      DO I=ETAMIN,ETAMAX
        DO J=PHIMIN,PHIMAX
          DO K=LAYMIN,LAYMAX
            N_CELL(I,J,K) = .FALSE.
          ENDDO
        ENDDO
      ENDDO
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
