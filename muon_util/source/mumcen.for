      SUBROUTINE MUMCEN(NMOD,MACHIT,MCRS,MFINE)
C-----------------------------------------------------------------
C-
C-    Purpose and Methods : Create fine and coarse centroids for one module
C-                          Based on MUCENT, MU_WAM_MAC, MU_SAM_MAC
C-
C-    Input  :  NMOD   - Module ID
C-              MACHIT(26,4) - Array of latches in MAC format
C-
C-    Output :  MCRS   - Bit array of coarse centroids
C-              MFINE(4) - Bit array of fine centroids
C-
C-    Created :  7-JAN-94  M. Fortner
C-              12/94 MF modify EF layer logic, 2/95 Modify CFX logic
C-		6/95 MF bug fix for EFAL logic
C-
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER NMOD,MACHIT(26,4),MCRS,MFINE(4)
      INTEGER IOUT,IND,I,J,K,NCEN,NCRS,KCRS,JCRS,JFINE
      INTEGER MCENT(128),SAMHIT(104)
      LOGICAL LH1,LH2,LH3
C
      INTEGER MCFA(4,7,2),MEFA(6,7,2)
      INTEGER MSFX(3,3),MCFB(4,6,2),MCFC(4,6,2)
      INTEGER MEFL(4,3,2),MEFR(4,3,2),MEFAL(6,3,2),MEFAR(6,5,2)
      INTEGER ICHIP,MCHIP1(460),MCHIP2(460),ICENTS
      INTEGER IVERS,IER,MUDVER
      EXTERNAL MUDVER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C         Centroid pattern logic
C
      DATA MCFA/    1,4,1,3, 1,4,0,2, 1,4,2,2,
     &     1,3,1,2, 1,3,0,1, 1,3,1,1, 1,3,2,1,
     &              2,3,1,4, 1,3,2,1, 1,3,2,1,
     &    -1,3,1,2,-1,3,0,1,-1,3,1,1,-1,3,2,1/
      DATA MEFA/        1,4,1,3,1,2, 1,3,1,2,1,1, 1,4,1,2,1,1,
     &     1,4,1,3,1,1, 1,4,1,3,2,2, 1,4,0,2,0,1, 1,3,0,2,0,1,
     &                  1,4,2,2,2,1, 1,4,2,3,2,2, 1,4,2,3,2,1,
     &     1,4,1,3,0,2, 1,4,1,2,0,1, 1,4,1,3,0,1, 1,3,1,2,0,1/
      DATA MEFAL/       1,3,1,2,1,1, 1,4,1,3,1,1, 1,4,2,2,2,1, 
     &                  1,4,2,3,1,1, 1,4,2,3,2,2, 1,4,2,3,2,1/
      DATA MEFAR/       1,4,1,3,1,2, 1,3,1,2,0,1, 1,4,1,3,0,1,
     &     1,4,1,3,0,2, 1,3,0,2,0,1,
     &                  1,4,1,2,0,1, 1,4,1,2,1,1, 1,4,2,3,1,1,
     &     1,4,2,3,1,1, 1,4,2,3,1,1/
      DATA MCFB/
     &     0,1,2,3, 1,1,1,2, 1,1,1,3, 1,2,1,3, 0,1,1,2, 2,1,0,3,
     &     2,1,1,2, 1,1,2,3, 2,1,1,3, 1,2,2,3, 2,2,1,3, 2,2,1,3/
      DATA MCFC/
     &     0,1,2,3, 1,1,1,2, 1,1,1,3, 1,2,1,3, 2,1,1,2, 2,1,0,3,
     &     1,1,2,2, 1,1,2,3, 2,1,1,3, 1,2,2,3, 2,2,1,3, 2,2,1,3/
      DATA MEFL/
     &     1,1,1,2, 1,1,1,3, 1,2,1,3,
     &     2,1,1,2, 2,1,1,3, 2,2,1,3/
      DATA MEFR/
     &     1,1,1,2, 1,1,1,3, 1,2,1,3,
     &     1,1,2,3, 1,2,2,3, 1,1,2,2/
      DATA MSFX/0,1,0, 1,1,1, 0,1,1/
C
      DATA MCHIP1/9*0,4*1,0,2*1,3*0,4*1,0,2*1,3*0,4*1,0,2*1,3*0,
     &           21*0,2,2,0,2,2,0,2,23*0,2,2,0,2,2,0,2,2*0,
     &     8*11,2*0,8*11,2*0,5*11,2*0,11,2*0,8*11,2*0,8*11,2*0,
     &     11,2*0,11,6*0,8*11,12*0,11,2*0,11,6*0,8*11,2*0,
     &     8*12,2*0,8*12,2*0,5*12,2*0,12,2*0,8*12,2*0,8*12,12*0,
     &     8*12,2*0,8*12,12*0,8*12,2*0,8*12,92*0,
     &     21,22,21,0,21,22,21,3*0,21,0,21,22,21,0,21,22,2*0,
     &     21,22,21,0,21,22,21,3*0,21,0,21,22,21,0,21,22,2*0,
     &     21,22,21,0,21,22,21,3*0,21,0,21,22,21,0,21,22,3*0/
C
      DATA MCHIP2/9*0,4*1,0,2*1,3*0,4*1,0,2*1,3*0,4*1,0,2*1,3*0,
     &           21*0,4,3,0,4,3,0,3,23*0,3,4,0,3,4,0,4,2*0,
     &     8*11,2*0,8*11,2*0,5*11,2*0,11,2*0,8*11,2*0,8*11,2*0,
     &     13,2*0,14,6*0,14,13,14,13,14,13,14,13,12*0,
     &     14,2*0,13,6*0,13,14,13,14,13,14,13,14,2*0,
     &     13,13,14,14,13,13,14,14,2*0,
     &     8*12,2*0,5*12,2*0,12,2*0,8*12,2*0,
     &     14,14,13,13,14,14,13,13,12*0,
     &     14,13,14,13,14,13,14,13,2*0,14,13,14,13,14,13,14,13,12*0,
     &     13,14,13,14,13,14,13,14,2*0,13,14,13,14,13,14,13,14,92*0,
     &     21,22,21,0,21,22,21,3*0,21,0,21,22,21,0,21,22,2*0,
     &     21,22,21,0,21,22,21,3*0,21,0,21,22,21,0,21,22,2*0,
     &     21,22,21,0,21,22,21,3*0,21,0,21,22,21,0,21,22,3*0/
C
C         Select centroid logic set
C
      IF (FIRST) THEN
        CALL EZGET('CENTROID_SET',ICENTS,IER)
        FIRST = .FALSE.
      ENDIF
      ICHIP = 0
      IF (NMOD.LE.0.OR.NMOD.GT.460) RETURN
      IF (ICENTS.EQ.0) THEN
        ICHIP = MCHIP1(NMOD)
        IVERS = MUDVER(0)
        IF (IVERS.EQ.3.OR.IVERS.EQ.23) ICHIP=MCHIP2(NMOD)
      ENDIF
      IF (ICENTS.EQ.1) ICHIP=MCHIP1(NMOD)
      IF (ICENTS.EQ.2) ICHIP=MCHIP2(NMOD)
      IF (ICHIP.EQ.0) RETURN
C
C         Initialize Fine and Coarse Centroid arrays
C
      MCRS = 0
      DO I=1,4
        MFINE(I)=0
      ENDDO
      DO I=1,128
        MCENT(I)=0
      ENDDO
      NCEN = 48
C
C         WAMUS CFA centroids
C
      IF (ICHIP.EQ.1) THEN
        DO I = 1,24
          IND = (I-1)*2
          LH3 = MACHIT(I+1,4).NE.0
          DO J = 1,7
            LH1 = MACHIT(I+MCFA(1,J,1),MCFA(2,J,1)).NE.0
            LH2 = MACHIT(I+MCFA(3,J,1),MCFA(4,J,1)).NE.0
            IF (LH1.AND.LH2) MCENT(IND+1)=1
            LH1 = MOD(IND,6).NE.0
            IF (LH1.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCFA(1,J,2).GE.0) THEN
              LH1 = MACHIT(I+MCFA(1,J,2),MCFA(2,J,2)).NE.0
            ELSE
              LH1 = MACHIT(I-MCFA(1,J,2),MCFA(2,J,2)).EQ.0
            ENDIF
            LH2 = MACHIT(I+MCFA(3,J,2),MCFA(4,J,2)).NE.0
            IF (LH1.AND.LH2.AND.LH3) MCENT(IND+2)=1
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
C         WAMUS EF A-layer centroids
C
      ELSE IF (ICHIP.EQ.2) THEN
        DO I = 1,24
          IND = (I-1)*2
          DO J = 1,7
            DO K=1,2
              LH1 = MACHIT(I+MEFA(1,J,K),MEFA(2,J,K)).NE.0
              LH2 = MACHIT(I+MEFA(3,J,K),MEFA(4,J,K)).NE.0
              LH3 = MACHIT(I+MEFA(5,J,K),MEFA(6,J,K)).NE.0
              IF (LH1.AND.LH2.AND.LH3) MCENT(IND+K)=1
            ENDDO
            LH1 = MOD(IND,6).NE.0
            IF (LH1.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
C         WAMUS EF A-layer centroids (lower left to upper right)
C
      ELSE IF (ICHIP.EQ.3) THEN
        DO I = 1,24
          IND = (I-1)*2
          DO J = 1,3
            DO K=1,2
              LH1 = MACHIT(I+MEFAL(1,J,K),MEFAL(2,J,K)).NE.0
              LH2 = MACHIT(I+MEFAL(3,J,K),MEFAL(4,J,K)).NE.0
              LH3 = MACHIT(I+MEFAL(5,J,K),MEFAL(6,J,K)).NE.0
              IF (LH1.AND.LH2.AND.LH3) MCENT(IND+K)=1
            ENDDO
            LH1 = MOD(IND,6).NE.0
            IF (LH1.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
C         WAMUS EF A-layer centroids (lower right to upper left)
C
      ELSE IF (ICHIP.EQ.4) THEN
        DO I = 1,24
          IND = (I-1)*2
          DO J = 1,5
            DO K=1,2
              LH1 = MACHIT(I+MEFAR(1,J,K),MEFAR(2,J,K)).NE.0
              LH2 = MACHIT(I+MEFAR(3,J,K),MEFAR(4,J,K)).NE.0
              LH3 = MACHIT(I+MEFAR(5,J,K),MEFAR(6,J,K)).NE.0
              IF (LH1.AND.LH2.AND.LH3) MCENT(IND+K)=1
            ENDDO
            LH1 = MOD(IND,6).NE.0
            IF (LH1.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
C         WAMUS CF B-layer centroids
C
      ELSE IF (ICHIP.EQ.11) THEN
        DO I = 1,24
          IND = (I-1)*2
          DO J = 1,6
            DO K=1,2
              LH1 = MACHIT(I+MCFB(1,J,K),MCFB(2,J,K)).NE.0
              LH2 = MACHIT(I+MCFB(3,J,K),MCFB(4,J,K)).NE.0
              IF (LH1.AND.LH2) MCENT(IND+K)=1
            ENDDO
            IF (IND.NE.0.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
C         WAMUS CF C-Layer centroids
C
      ELSE IF (ICHIP.EQ.12) THEN
        DO I = 1,24
          IND = (I-1)*2
          DO J = 1,6
            DO K=1,2
              LH1 = MACHIT(I+MCFC(1,J,K),MCFC(2,J,K)).NE.0
              LH2 = MACHIT(I+MCFC(3,J,K),MCFC(4,J,K)).NE.0
              IF (LH1.AND.LH2) MCENT(IND+K)=1
            ENDDO
            IF (IND.NE.0.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
C         WAMUS EF left centroids (lower left to upper right)
C
      ELSE IF (ICHIP.EQ.13) THEN
        DO I = 1,24
          IND = (I-1)*2
          DO J = 1,3
            DO K=1,2
              LH1 = MACHIT(I+MEFL(1,J,K),MEFL(2,J,K)).NE.0
              LH2 = MACHIT(I+MEFL(3,J,K),MEFL(4,J,K)).NE.0
              IF (LH1.AND.LH2) MCENT(IND+K)=1
            ENDDO
            IF (IND.NE.0.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
C         WAMUS EF right centroids (lower right to upper left)
C
      ELSE IF (ICHIP.EQ.14) THEN
        DO I = 1,24
          IND = (I-1)*2
          DO J = 1,3
            DO K=1,2
              LH1 = MACHIT(I+MEFR(1,J,K),MEFR(2,J,K)).NE.0
              LH2 = MACHIT(I+MEFR(3,J,K),MEFR(4,J,K)).NE.0
              IF (LH1.AND.LH2) MCENT(IND+K)=1
            ENDDO
            IF (IND.NE.0.AND.MCENT(IND).EQ.1) MCENT(IND+1)=0
            IF (MCENT(IND+2).EQ.1) MCENT(IND+1)=0
          ENDDO
        ENDDO
C
      ELSE IF (ICHIP.GE.20) THEN
        DO I = 1,26
          DO J = 1,4
            SAMHIT((I-1)*4+J) = MACHIT(I,J)
          ENDDO
        ENDDO
C
C         SAMUS X/Y-plane centroids
C
        IF (ICHIP.EQ.21) THEN
          NCEN = 128
          DO I = 1,64
            KCRS = (I-1)/4
            IF (SAMHIT(I+4).NE.0) MCRS=IBSET(MCRS,KCRS)
            IND = (I-1)*2
            DO J = 1,3
              K = (J+1)/2
              LH1 = SAMHIT(I+3).EQ.MSFX(1,J)
              LH2 = SAMHIT(I+4).EQ.MSFX(2,J)
              LH3 = SAMHIT(I+5).EQ.MSFX(3,J)
              IF (K.EQ.2) LH3=LH3.AND.(SAMHIT(I+6).EQ.0)
              IF (LH1.AND.LH2.AND.LH3) MCENT(IND+K)=1
            ENDDO
          ENDDO
C
C         SAMUS U-plane centroids
C
        ELSE
          NCEN = 96
          DO I = 1,96
            KCRS = (I-1)/6
            IF (SAMHIT(I+4).NE.0) MCRS=IBSET(MCRS,KCRS)
            DO J = 1,3
              LH1 = SAMHIT(I+3).EQ.MSFX(1,J)
              LH2 = SAMHIT(I+4).EQ.MSFX(2,J)
              LH3 = SAMHIT(I+5).EQ.MSFX(3,J)
              IF (J.EQ.3) LH3=LH3.AND.(SAMHIT(I+6).EQ.0)
              IF (LH1.AND.LH2.AND.LH3) MCENT(I)=1
            ENDDO
          ENDDO
        ENDIF
C
      ENDIF
C
C         Convert centroids into fine and coarse bit arrays
C
      DO I = 0,NCEN-1
        IF (MCENT(I+1).NE.0) THEN
          IOUT = I
          IF (NMOD.LT.400) IOUT=IOUT+1
          IND = IOUT/32
          JFINE = IOUT - IND*32
          MFINE(IND+1) = IBSET(MFINE(IND+1),JFINE)
          IF (NMOD.LT.400) THEN
            NCRS = NCEN/16
            JCRS = I/NCRS
            MCRS = IBSET(MCRS,JCRS)
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
