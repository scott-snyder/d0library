      SUBROUTINE LJTOP_DECODE_MCDAT_NTUPLE(USE_EVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get MC/DATA info and fill in quans.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-APR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      INCLUDE 'D0$INC:LJTOP_HMATRIX_MCDAT.INC'
C
      REAL    JET_ET_CUT
      INTEGER IER,NELE
      INTEGER NJT
      INTEGER I
C
      REAL    MTAG(5)
      EQUIVALENCE (MTAG(1),TAG1_X)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL USE_EVENT
C----------------------------------------------------------------------
      IF ( FIRST  ) THEN
        FIRST = .FALSE.
        CALL EZPICK('LJTOP_HMATRIX_RCP')
        CALL EZGET('JET_ET_CUT',JET_ET_CUT,IER)
        CALL EZRSET
      ENDIF
C
C LOAD COMMON BLOCK.
C
      IQ(LHEAD+1) = 1005    !FAKING IT
      IQ(LHEAD+6) = RUN
      IQ(LHEAD+7) = EVENT1
      IQ(LHEAD+8) = EVENT2
      IQ(LHEAD+9) = EVENT

      NELE = 1       !ONLY ONE ELECTRON IN NTUPLE
      NJETS = NJETSN  !JNEP number of jets
      CALL UCOPY(ELE(1),P24_ELECTRON(1,1),NE)
      P2_NEUT(1) = NEUTX
      P2_NEUT(2) = NEUTY

      MET_D0 = MET
C
C ****  CORRECTED QUANTITIES
C
C
      ELECTRON = .FALSE.
      IF ( NELE.GT.0 ) THEN
        CALL UCOPY(P24_ELECTRON,P6_CLUST,6)
        ELECTRON = .TRUE.
      ENDIF
C
C ****  IMPOSE JET THRESHOLD
C
      NJT = 0
      DO I = 1 , NJETS
        IF ( JETS(5,I).GE.JET_ET_CUT ) THEN
          NJT = NJT + 1
          CALL UCOPY(JETS(1,I),P25_JETS(1,NJT),5)
          CALL ETAPHI(P25_JETS(1,NJT),P25_JETS(2,NJT),P25_JETS(3,NJT),
     &      P25_JETS(6,NJT),P25_JETS(7,NJT))
        ENDIF
      ENDDO
      NJETS=NJT
C
      NVER=1
      CALL UZERO(P9_VERTEX(1,1),1,3)  !NOMINAL VERTEX
      P9_VERTEX(1,3) = Z_VERT1
C
      IF(TIGHT_EL.EQ.1.0)THEN
        TIGHT_ELECTRON=1
        LOOSE_ELECTRON=0
      ELSE
        TIGHT_ELECTRON=0
        LOOSE_ELECTRON=1
      ENDIF
C
      RDEDX = DEDX
      ELE_HIGH=ELE_HGH
C
      CALL UCOPY(MTAG,P25_MUON_TAGGED(1,1),5)
      P25_MUON_TAGGED(24,1)=DELTA_R
      P25_MUON_TAGGED(25,1)= PTREL
      NTAG = NTAGS
C
      USE_EVENT = .TRUE.
  999 RETURN
      END
