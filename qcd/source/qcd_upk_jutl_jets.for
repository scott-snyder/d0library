      FUNCTION QCD_UPK_JUTL_JETS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack L1/L2 part of JUTL bank
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  08-JAN-1993  Andrew G. Brandt
C-   Updated  10-MAR-1994  Andrew G. Brandt add JRUN 1=1a 2=1b NL1=-1
C-                         If 1B but LT not available
C-   Updated  03-NOV-1994  Andrew G. Brandt for CW allow LJ+TT
C-   Updated  12-DEC-1995  Andrew G. Brandt Sort TT,LJ,L2
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_JUTL_JETS.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      LOGICAL QCD_UPK_JUTL_JETS
C
      INTEGER      I,J,JRUN
      INTEGER      NTTS,NLJS,NL2S,IER
      REAL         TTJET(3,MAX_TT),LJJET(3,MAX_LJ),L2JET(5,MAX_L2),CSIZ
      REAL  UN_TT_ETA(MAX_TT),UN_TT_PHI(MAX_TT),UN_TT_ET(MAX_TT)
      REAL  UN_LJ_ETA(MAX_LJ),UN_LJ_PHI(MAX_LJ),UN_LJ_ET(MAX_LJ)
      REAL  UN_L2_ETA(MAX_L2),UN_L2_PHI(MAX_L2),UN_L2_ET(MAX_L2)
      REAL  UN_L2_EMF(MAX_L2)
      INTEGER INDTT(MAX_TT),INDLJ(MAX_LJ),INDL2(MAX_L2)
C
C----------------------------------------------------------------------
      QCD_UPK_JUTL_JETS = .TRUE.
C
      JRUN=IRUN
C
C- Initialize
C
      DO J = 1,MAX_TT
        TT_ETA(J)=-999.
        TT_PHI(J)=-999.
        TT_ET(J)=-999.
      ENDDO
C
      DO J = 1,MAX_LJ
        LJ_ETA(J)=-999.
        LJ_PHI(J)=-999.
        LJ_ET(J)=-999.
      ENDDO
C
      DO J = 1,MAX_L2
        L2_ETA(J)=-999.
        L2_PHI(J)=-999.
        L2_ET(J)=-999.
        L2_EMF(J)=-999.
      ENDDO
C
C- Get L1/L2 jet information
C
      CALL GTJUTL_JETS(JRUN,NTTS,TTJET,NLJS,LJJET,NL2S,L2JET,CSIZ,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('QCD_UPK_JUTL_JETS','QCD_UPK_JUTL_JETS',
     +              'No JUTL bank found','F')
        GO TO 999
      END IF
C
C- Fill the QCD_JUTL_JETS common block with sorted quantities
C
C  Trigger towers
C
      NTT=NTTS
      DO J=1,NTT
        UN_TT_ETA(J)=TTJET(1,J)
        UN_TT_PHI(J)=TTJET(2,J)
        UN_TT_ET(J) =TTJET(3,J)
      ENDDO
C
      IF(NTT.GT.0) THEN
        CALL VZERO(INDTT,MAX_TT)
        CALL SORTZV(UN_TT_ET,INDTT,NTT,1,1,0)
      END IF
C
      DO J=1,NTT
        I=INDTT(J)
        TT_ETA(J)=UN_TT_ETA(I)
        TT_PHI(J)=UN_TT_PHI(I)
        TT_ET(J)=UN_TT_ET(I)
      ENDDO
C
C Large tiles
C
      NLJ=NLJS
      DO J=1,NLJ
        UN_LJ_ETA(J)=LJJET(1,J)
        UN_LJ_PHI(J)=LJJET(2,J)
        UN_LJ_ET(J) =LJJET(3,J)
      ENDDO
C
      IF(NLJ.GT.0) THEN
        CALL VZERO(INDLJ,MAX_LJ)
        CALL SORTZV(UN_LJ_ET,INDLJ,NLJ,1,1,0)
      END IF
C
      DO J=1,NLJ
        I=INDLJ(J)
        LJ_ETA(J)=UN_LJ_ETA(I)
        LJ_PHI(J)=UN_LJ_PHI(I)
        LJ_ET(J)=UN_LJ_ET(I)
      ENDDO
C
C L2
C
      NL2=NL2S
      DO J=1,NL2
        UN_L2_ETA(J)=L2JET(1,J)
        UN_L2_PHI(J)=L2JET(2,J)
        UN_L2_ET(J) =L2JET(3,J)
        UN_L2_EMF(J)=L2JET(5,J)
      ENDDO
C
      IF(NL2.GT.0) THEN
        CALL VZERO(INDL2,MAX_L2)
        CALL SORTZV(UN_L2_ET,INDL2,NL2,1,1,0)
      END IF
C
      DO J=1,NL2
        I=INDL2(J)
        L2_ETA(J)=UN_L2_ETA(I)
        L2_PHI(J)=UN_L2_PHI(I)
        L2_ET(J) =UN_L2_ET(I)
        L2_EMF(J)=UN_L2_EMF(I)
      ENDDO
C
      CONESZ=CSIZ
C
  999 RETURN
      END
