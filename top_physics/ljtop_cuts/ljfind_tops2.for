      SUBROUTINE LJFIND_TOPS2(PWLEP,PJETS,PTOP1,PTOP2,PWHAD,INDX,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      find TOP jets by minimizing mass difference between t and tb
C-
C-   Inputs  : 
C-        PWLEP= 4-momenta W->leptons
C-        PJETS= 4-momenta of jets
C-   Outputs : 
C-   PTOP1(5) = 4-vector of top with W decaying leptonically + mass
C-   PTOP2(5) =   "          "           "      hadronically + mass
C-   PWHAD(7) =   "      of W decaying hadronically + mass (3 comb.)
C-       INDX= ordered list for PJETS, 1=B assoc. with W->leptons
C-                                     2,3= jets from W-> hadrons
C-                                     4= B assoc. with W->hadrons
C-       OK=   false, no good combination found
C-               
C-   ENTRY TOP_CUTS2(ETCUTB,ETCUTW,DELW)  change default cuts
C-
C-   Created  21-MAY-1992   Serban D. Protopopescu
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    PWLEP(5),PJETS(5,4),PTOP1(5),PTOP2(5),PWHAD(7)
      LOGICAL OK
      INTEGER INDX(4)
      REAL    MASST1(4),MASST2(4),DM(4),PSUM(5)
      REAL    DMMIN,MW(3),MTP1,MTP2,MWH,SAVEPZ
      REAL    ET_CUTB(2),ET_CUTW
      INTEGER K1,K2,K3,I2(3),I3(3),I,NTRY
C----------------------------------------------------------------------
      OK=.FALSE.
      CALL LJTOP_GET_JCUTS(ET_CUTB,ET_CUTW)
C      
C         try 4 combinations
      DMMIN=300.
      DO I=1,4
        IF(PJETS(5,I).GT.ET_CUTB(1)) THEN  ! select jets with Et>Etcut
          CALL PAIR_MASS(PWLEP,PJETS(1,I),PTOP1)
          MASST1(I)=PTOP1(5)
          K1=1
          IF(I.LT.4) K1=I+1
          K2=K1+1
          IF(K2.GT.4) K2=1
          CALL VADD(PJETS(1,K1),PJETS(1,K2),PSUM,4)
          K3=K2+1
          IF(K3.GT.4) K3=1
          CALL PAIR_MASS(PSUM,PJETS(1,K3),PTOP2)
          MASST2(I)=PTOP2(5)
          DM(I)=ABS(MASST1(I)-MASST2(I))
          IF(MASST1(I).GT.180.OR.MASST2(I).GT.180.) DM(I)=300.
          IF(MASST1(I).LT.80.OR.MASST2(I).LT.80.) DM(I)=300.
          IF(DM(I).LT.DMMIN) THEN
            INDX(1)=I
            DMMIN=DM(I)
          ENDIF
        ENDIF
      ENDDO
      IF(DMMIN.GT.200.) RETURN
C
C       find best W combination
      I=0
      DO K1=1,3
        IF(K1.NE.INDX(1)) THEN
          DO K2=K1+1,4
            IF(K2.NE.INDX(1).AND.PJETS(5,K2).GT.ET_CUTW) THEN
              CALL PAIR_MASS(PJETS(1,K1),PJETS(1,K2),PSUM)
              I=I+1
              MW(I)=PSUM(5)
              I2(I)=K1
              I3(I)=K2
              DM(I)=ABS(MW(I)-80.)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      DMMIN=AMIN1(DM(1),DM(2),DM(3))
      DO I=1,3
        IF(DM(I).LE.DMMIN) K1=I
      ENDDO
      INDX(2)=I2(K1)
      INDX(3)=I3(K1)
      MWH=MW(K1)
      K1=5
      DO I =1,3
        IF(MW(I).NE.MWH) THEN
          K1=K1+1
          PWHAD(K1)=MW(I)
        ENDIF
      ENDDO
C
C      find last B jet
      DO I=1,4
        IF(I.NE.INDX(1).AND.I.NE.INDX(2).AND.I.NE.INDX(3)) INDX(4)=I
      ENDDO
      CALL PAIR_MASS(PWLEP,PJETS(1,INDX(1)),PTOP1)
      CALL PAIR_MASS(PJETS(1,INDX(2)),PJETS(1,INDX(3)),PWHAD)
      CALL PAIR_MASS(PWHAD,PJETS(1,INDX(4)),PTOP2)
      OK=.TRUE.
  999 RETURN
      END
