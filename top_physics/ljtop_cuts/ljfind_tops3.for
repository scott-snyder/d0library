      SUBROUTINE LJFIND_TOPS3(NJETS,PJETS,PWLEP,PTOP1,PTOP2,PWHAD,
     &  INDX,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     find best t tbar candidates
C-     Find first t -> W + b1  with W-> leptons
C-                    assuming b1 gives highest Et top (Etb1>ETCUTB(1))
C-     Find then W-> hadrons
C-                assuming it is made of a pair of jets with 
C-                Et> ET_CUTW with mass closest to W (removing b1)
C-     Find second t assuming remaining jet giving highest Et top 2
C-                 is b2  (Etb2>ETCUTB(2))  
C-
C-   Input:
C-   NJETS          = number of jets
C-   PJETS(5,NJETS) = jet 4-vector + jet Et
C-   PWLEP(5) =   "      of W decaying leptonically (5 has 2nd pz)
C-
C-   Outputs : 
C-   PTOP1(5) = 4-vector of top with W decaying leptonically + mass
C-   PTOP2(5) =   "          "           "      hadronically + mass
C-   PWHAD(7) =   "      of W decaying hadronically + mass (3 comb.)
C-   INDX= ordered list for PJETS, 1=B assoc. with W->leptons
C-                                 2,3= jets from W-> hadrons
C-                                 4= B assoc. with W->hadrons
C-   IOK       = -1 no acceptable solution, 0 no tops found
C-               1 only top 1 found, 2 both tops found
C-               
C-
C-   Created  21-MAY-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NJETS
      REAL    PJETS(5,NJETS),PLEP(4),PNUT(2)
      REAL    PTOP1(5),PTOP2(5),PWLEP(5),PWHAD(7)
      INTEGER INDX(4),IOK,I,J,NTRY
      REAL    ET_CUTB(2),COSWB,ETMAX,WTRY(5),ETP
      REAL    DIFF,DIFF_MIN,ET_CUTW
      LOGICAL OK
C----------------------------------------------------------------------
      IOK=0
      DO J=1,4
        INDX(J)=0
        PWHAD(J)=0
      ENDDO
      PTOP1(5)=-10.
      PTOP2(5)=-10.
      PWHAD(5)=-10.
      PWHAD(6)=-10.
      PWHAD(7)=-10.
      CALL LJTOP_GET_JCUTS(ET_CUTB,ET_CUTW)
C
C          find top 1 
      INDX(1)=0
      ETMAX=0.
      DO I=1,NJETS
        IF(PJETS(5,I).GT.ET_CUTB(1)) THEN  ! select jets with Et>Etcut
          CALL PAIR_MASS(PWLEP,PJETS(1,I),PTOP1)
          ETP=SQRT(PTOP1(1)**2+PTOP1(2)**2)
C
          IF(ETP.GT.ETMAX) THEN
            ETMAX=ETP
            INDX(1)=I
          ENDIF
C
        ENDIF
      ENDDO
      IF(INDX(1).EQ.0) GOTO 999
      CALL PAIR_MASS(PWLEP,PJETS(1,INDX(1)),PTOP1)
      IOK=1
      IF(NJETS.LT.3) GOTO 999
C
C         find a W -> hadrons 
      NTRY=0
      DIFF_MIN=100.
      DIFF=DIFF_MIN+1.
      DO I=1,NJETS-1
        DO J=I+1,NJETS
          IF(I.NE.INDX(1).AND.J.NE.INDX(1).AND.
     &      PJETS(5,J).GT.ET_CUTW) THEN
            CALL PAIR_MASS(PJETS(1,I),PJETS(1,J),WTRY)
            DIFF=ABS(WTRY(5)-80.0)
            IF(DIFF.LT.DIFF_MIN) THEN
              IF(NTRY.GT.0.AND.NTRY.LT.3) PWHAD(5+NTRY)=PWHAD(5)
              DIFF_MIN=DIFF
              CALL UCOPY(WTRY,PWHAD,5)
              INDX(2)=I
              INDX(3)=J
              NTRY=NTRY+1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C          find top 2
      IF(NJETS.LT.4.OR.NTRY.EQ.0) GOTO 999
      DO I=1,4
C        WTRY(I)=PWHAD(I)*80./PWHAD(5)
        WTRY(I)=PWHAD(I)
      ENDDO
      COSWB=-1.
      INDX(4)=0
      ETMAX=0.
      DO I=1,NJETS
        IF(I.NE.INDX(1).AND.I.NE.INDX(2).AND.
     &    I.NE.INDX(3).AND.PJETS(5,I).GT.ET_CUTB(2)) THEN
C
          CALL PAIR_MASS(WTRY,PJETS(1,I),PTOP2)
          ETP=SQRT(PTOP2(1)**2+PTOP2(2)**2)
C
          IF(ETP.GT.ETMAX) THEN
            ETMAX=ETP
            INDX(4)=I
          ENDIF
C
        ENDIF
      ENDDO
      IF(INDX(4).EQ.0) GOTO 999
      CALL PAIR_MASS(WTRY,PJETS(1,INDX(4)),PTOP2)
      IOK=2
  999 RETURN
      END
