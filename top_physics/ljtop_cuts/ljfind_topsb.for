      SUBROUTINE LJFIND_TOPSB(NJETS,PJETS,PWLEP,PTOP1,PTOP2,PWHAD,
     &  INDX,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     find best t tbar candidates
C-     assuming the b-jets have been tagged
C-     Use ISAJET information to tag
C-
C-   Input:
C-   NJETS          = number of jets
C-   PJETS(5,NJETS) = jet 4-vector + jet Et
C-   PWLEP(5) =   "      of W decaying leptonically (5 has 2nd pz)
C-
C-   Outputs : 
C-   PTOP1(5,2) = 4-vectors and mass of top with W -> l+nu (2 solutions)
C-   PTOP2(5,2) =   "          "           "     W -> hadrons
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
      REAL    PTOP1(5,2),PTOP2(5,2),PWLEP(5),PWHAD(7)
      INTEGER INDX(4),IOK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,J,NTRY,GZISAE,GZISAQ,LISAQ,ID,NBS
      REAL    PBS(5,2),ETBS(2)
      REAL    ET_CUTB(2),ETCUTB(2),COSWB,COSWJ,VDOTN,WTRY(5)
      REAL    DEL_W,DELW,DIFF,DIFF_MIN,ET_CUTW,ETCUTW,SAVEPZ
      LOGICAL OK
      DATA ET_CUTB,ET_CUTW/10.0,10.0,10.0/
      DATA DEL_W/20./
C----------------------------------------------------------------------
      IOK=0
      IF(GZISAE().LE.0) GOTO 999  ! no ISAJET information
      CALL LJTOP_GET_JCUTS(ET_CUTB,ET_CUTW)
C         partons
      NBS=0
      LISAQ=GZISAQ()
      DO WHILE (LISAQ.GT.0)    ! Loop for leptons
        ID=IABS(IQ(LISAQ+1))
C
        IF ( ID.EQ.5 ) THEN   ! b's
          NBS=NBS+1
          CALL UCOPY(Q(LISAQ+2),PBS(1,NBS),5)
        ENDIF
        LISAQ=LQ(LISAQ)
      ENDDO
      DO J=1,4
        INDX(J)=0
        PWHAD(J)=0
      ENDDO
      PTOP1(5,1)=-10.
      PTOP2(5,1)=-10.
      PWHAD(5)=-10.
      PWHAD(6)=-10.
      PWHAD(7)=-10.
C
C          find top 1 
      DO J=1,NBS
        INDX(J)=0
        COSWB=.99
        DO I=1,NJETS
          IF(PJETS(5,I).GT.ET_CUTB(1)) THEN  ! select jets with Et>Etcut
            COSWJ=ABS(VDOTN(PBS(1,J),PJETS(1,I),3))
            DIFF=ABS(PBS(4,J)-PJETS(4,I))/PBS(4,J)
            IF(COSWJ.GT.COSWB.AND.DIFF.LT..25) THEN
              COSWB=COSWJ
              INDX(J)=I
            ENDIF
          ENDIF
C
        ENDDO
      ENDDO
      IF(INDX(2).NE.0) THEN
        IF(INDX(1).EQ.0) THEN
          INDX(1)=INDX(2)
        ELSE
          INDX(4)=INDX(2)
        ENDIF
      ENDIF
      INDX(2)=0
      IF(INDX(1)+INDX(4).EQ.0) GOTO 999
      IF(INDX(1).NE.0) CALL PAIR_MASS(PWLEP,PJETS(1,INDX(1)),PTOP1(1,1))
      IF(INDX(4).NE.0) CALL PAIR_MASS(PWLEP,PJETS(1,INDX(4)),PTOP1(1,2))
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
     &      I.NE.INDX(4).AND.J.NE.INDX(4).AND.
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
      DO I=1,4
C        WTRY(I)=PWHAD(I)*80./PWHAD(5)
        WTRY(I)=PWHAD(I)
      ENDDO
      IOK=2
      IF(INDX(4).GT.0) CALL PAIR_MASS(WTRY,PJETS(1,INDX(4)),PTOP2(1,1))
      IF(INDX(1).GT.0) CALL PAIR_MASS(WTRY,PJETS(1,INDX(1)),PTOP2(1,2))
      IF(INDX(4).EQ.0) CALL UCOPY(PTOP2(1,2),PTOP1(1,1),5)
      IF(INDX(1)*INDX(4).NE.0) THEN      ! chose only one solution
        DIFF=ABS(PTOP1(5,1)-PTOP2(5,1))
        IF(ABS(PTOP1(5,2)-PTOP2(5,2)).LT.DIFF) THEN
          CALL UCOPY(PTOP1(1,2),PTOP1(1,1),5)
          CALL UCOPY(PTOP2(1,2),PTOP2(1,1),5)
          NTRY=INDX(1)
          INDX(1)=INDX(4)
          INDX(4)=NTRY
        ENDIF
      ENDIF
  999 RETURN
      END
