      SUBROUTINE SPLJET(NUMJET,IJET,ETSHARE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SPLITS JETS THAT SHARE TOWERS OR SEPARATES THEM
C-   DEPENDING ON THE FRACTION OF SHARED ENERGY AND SEPARATION OF THEIR CENTERS
C-
C-   Inputs  : NUMJET THE NUMBER OF THE LAST JET FOUND
C-             IJET THE NUMBER OF THE JET THAT SHARES TOWERS WITH NUMJET
C-             ETSHARE - THE AMOUNT OF SHARED ET
C-
C-   Outputs : THE MODIFIED JETS, JPTS, JTSH BANKS
C-   Controls: VARIOUS CUTS ON PARAMETERS AND SO ON...
C-
C-      THIS SUBROUTINE FIRST CHECKS TO SEE IF THE JET NUMJET IS
C-      IDENTICAL WITH AN EXISTING JET OR IF NUMJET SHARES ALL OF
C-      ITS TOWERS WITH OTHER JETS, IF SO NUMJET IS DROPPED.
C-      ELSE
C-        NUMJET IS COMBINED WITH IJET IF IT SHARES MORE THAN
C-        THE FRACTION SPLIFR WITH IJET
C-        OTHERWISE THE JETS ARE SPLIT AND COMMON TOWERS DIVIDED UP
C-        ACCORDING TO WHICH JET CENTER IS CLOSER.
C-
C-      ENTRY SPLINI
C-        GET PARAMETERS FROM CAJETS_RCP
C-
C-   Created  27-JUL-1989   Nicholas Hadley
C-   Updated   2-OCT-1990   Chip Stewart  - MODIFIED RCP INPUT
C-   Updated  19-NOV-1991   Nick Hadley, Boaz Klima
C-     Fill JETS instead of JTSH
C-   Updated  24-MAR-1992   Nick Hadley - Swap 4 and 5 in RCP input
C-   Updated  14-APR-1994   Bob Hirosky  - STORE SHARE FRACTION
C-                                       - DO UND/ZSP CORRECTIONS FOR
C-                                         OVERLAPPING JETS
C-   Updated  19-SEP-1995   Bob Hirosky  - fill jet word 41, ICR noise
C-   Updated  18-OCT-1995   Bob Hirosky - use nicer area calc routines
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
      INCLUDE 'D0$INC:L2LINK.INC'           ! Add zebra link common
      INCLUDE 'D0$INC:CON_AREA.INC'
C
      REAL PI, TWOPI, HALFPI
      PARAMETER (PI=3.141593,TWOPI=6.283185,HALFPI=1.570796)
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$LINKS:IZJTSH.LINK'
      INTEGER IJET, NUMJET, GZJETS, I, II
      REAL ETAPHY, PHIPHY, PHIJ, ETAJ, R2TMP, TWORAD
C      INTEGER LJETS1, LJETS2
C      REAL ETJ
C      INTEGER IER, LJPTS1, LJPTS2, LJTSH1, LJTSH2
C      INTEGER MYJTSH(2), MSHARE
C      INTEGER LLLJETS,ITEMP,IFL
C      LOGICAL FIRST
      INTEGER MYJPTS(2), NSHARE
      REAL RUNIT,ETSHARE,SPLIFR,SHARFR
      REAL E(4), ETEM(2), ETJ1, ETJ2
      INTEGER LCAEH, GZCAEH, NUMADD, NUMIN2, IGOOD, ITARG, NTOW
      INTEGER NREP, POINT, NTOTOW, NEMTOW, GZCATE, LCATE
      REAL PHI, THETA, ETA
      REAL ETASUM(2), PHISUM(2),ETA2SU(2),PHI2SU(2)
      REAL    PHITMP, ETATMP, ETAWID, PHIWID
      INTEGER MAXPTS
      PARAMETER( MAXPTS = 4000 )
      INTEGER MYJET(2), KK, NUMPTS(2), KPOINT(MAXPTS,2)
      REAL PHTMP(2), RAD1, RAD2, ELEFT
C                                                         ! area calc words
      REAL SMAREA(3), SMAREA_STHW(3), SMAREA_ICR(3), SMAREA_ICR_STHW(3)
C
      INTEGER IRUNIT,ISPLIFR
      EQUIVALENCE ( ISPLIFR , SPLIFR  )
      EQUIVALENCE ( IRUNIT , RUNIT  )
      EQUIVALENCE ( L2LINK(5), MYJPTS(1) )
      EQUIVALENCE ( L2LINK(7), MYJET(1) )
C
C----------------------------------------------------------------------
      CALL MZLINT(IXCOM,'/L2LINK/',DUM,L2LINK(NLNK),DUM)
      IF(NUMJET.LE.1) THEN
        LJETS = GZJETS()
        MYJPTS(2) = LQ(LJETS-IZJPTS)
        DO I = 1, IQ(MYJPTS(2) + 2)
          IQ(MYJPTS(2)+2+I) = ABS( IQ(MYJPTS(2)+2+I) )
        ENDDO
        GOTO 999
      ENDIF
C
C               GET THE POINTER TO THE LAST JET
C
      LJETS = GZJETS()
      DO 100 I = 1 , NUMJET -1
        IF (I.EQ.IJET) THEN
          MYJET(2) = LJETS
          MYJPTS(2) = LQ(MYJET(2)-IZJPTS)
          ETAJ = Q(MYJET(2)+9)
          PHIJ = Q(MYJET(2)+8)
          ETJ2 = Q(MYJET(2)+6)
        END IF
        LJETS = LQ(LJETS)
  100 CONTINUE
      MYJET(1)= LJETS
      MYJPTS(1) = LQ(MYJET(1)-IZJPTS)
      ETAPHY = Q(MYJET(1)+9)
      PHIPHY = Q(MYJET(1)+8)
      ETJ1 = Q(MYJET(1)+6)
C
C ****  CALCULATE OVERLAPPING and NON-OVERLAPPING AREAS
C
C
      CALL SMJET_CON_AREA(CONE_USED,NUM_CONES,IJET,CONE_ARRY,
     &  SMAREA,SMAREA_STHW,SMAREA_ICR,SMAREA_ICR_STHW)
C
C               IS THE LAST JET IDENTICAL WITH IJET OR IS ALL ITS
C       ENERGY SHARED WITH EXISTING JETS IF SO, DROP IT
C

      R2TMP = (ETAPHY-ETAJ)**2 + (PHIPHY-PHIJ)**2
      ELEFT = ABS(ETJ1-ETSHARE)         ! ENERGY NOT SHARED IN NUMJET
      IF (R2TMP.LE.TWORAD .OR. ELEFT.LE.0.01) THEN !JETS ARE THE SAME
C
C       FIRST CLEAN UP THE CATE BANK
C
        ITARG = IJET + 1000*NUMJET
        LCATE = GZCATE()
        NTOTOW = MOD(IQ(LCATE+3),CATENM)
        NEMTOW = INT(IQ(LCATE+3)/CATENM)
        NREP = IQ(LCATE+2)
        DO 200 NTOW = NEMTOW+1, NTOTOW
          POINT = LCATE + NREP*(NTOW-1)
          ITARG = INT(IQ(POINT+16)/1000)
          IF(NUMJET.EQ.ITARG) IQ(POINT+16) = IQ(POINT+16)-1000*NUMJET
          IF(IQ(POINT+16).EQ.NUMJET) IQ(POINT+16) = IJET
  200   CONTINUE
C
        NUM_CONES = NUM_CONES - 1   ! DROP NEW CONE FROM OVERLAP GROUP
C
        CALL MZDROP(IXMAIN,MYJET(1),' ')       !DROP THE LAST JET BANK
        NUMJET = NUMJET - 1
        GOTO 999
      END IF
C
C       NUMJET IS NOT IDENTICAL TO IJET
C       SHOULD THE JETS BE SPLIT OR COMBINED
C
      NSHARE = 0
      SHARFR = ETSHARE/MIN(ETJ1,ETJ2)
C
C ****  SAVE SHARFR IN BOTH JETS BANKS
C
      Q(MYJET(1)+37) = SHARFR
      Q(MYJET(2)+37) = SHARFR
C
      IF (SHARFR.GE.SPLIFR) THEN        ! COMBINE THE JETS
C
C       FIRST CLEAN UP THE CATE BANK
C
        ITARG = IJET + 1000*NUMJET
        LCATE = GZCATE()
        NTOTOW = MOD(IQ(LCATE+3),CATENM)
        NEMTOW = INT(IQ(LCATE+3)/CATENM)
        NREP = IQ(LCATE+2)
        DO 220 NTOW = NEMTOW+1, NTOTOW
          POINT = LCATE + NREP*(NTOW-1)
          ITARG = INT(IQ(POINT+16)/1000)
          IF(NUMJET.EQ.ITARG) THEN
            IQ(POINT+16) = IQ(POINT+16)-1000*NUMJET
            IF(IQ(POINT+16).NE.IJET) THEN
              NSHARE = 1        ! TROUBLE MANY JETS SHARE ET
            END IF
          END IF
          IF(IQ(POINT+16).EQ.NUMJET) IQ(POINT+16) = IJET
  220   CONTINUE
C
        IF(IQ(MYJET(2)+15).EQ.2) NSHARE=3  ! FLAG A COMBINED AND SPLIT JET
C
        KK = 2              ! STORE THE COMBINED JET IN JET IJET
        DO 230 I = 2 ,11
          Q(MYJET(KK)+I)= 0.
  230   CONTINUE
        ETEM(KK) = 0.
        ETASUM(KK) = 0.
        ETA2SU(KK) = 0.
        PHISUM(KK) = 0.
        PHI2SU(KK) = 0.
C
        LCAEH = GZCAEH()
        NREP = IQ(LCAEH+2)
        DO 310 I = 1 , IQ(MYJPTS(2)+2)
          IF(IQ(MYJPTS(2)+I+2).GT.0) THEN  ! POINT NOT SHARED
            POINT = LCAEH + NREP*(IQ(MYJPTS(2)+I+2)-1)
            Q(MYJET(KK)+2) = Q(POINT+4) + Q(MYJET(KK)+2)
            Q(MYJET(KK)+3) = Q(POINT+5) + Q(MYJET(KK)+3)
            Q(MYJET(KK)+4) = Q(POINT+6) + Q(MYJET(KK)+4)
            Q(MYJET(KK)+5) = Q(POINT+7) + Q(MYJET(KK)+5)
            Q(MYJET(KK)+6) = Q(POINT+8) + Q(MYJET(KK)+6)
            DO 300 II = 1 ,4
              E(II) = Q(POINT+3+II)
  300       CONTINUE
            CALL ETOETA(E,PHITMP,THETA,ETATMP)
            IF (ABS(PHITMP-PHIJ).GT.(TWOPI-ABS(PHITMP-PHIJ))) THEN
              IF(PHIJ.LT.PHITMP) THEN
                PHITMP = PHITMP - TWOPI
              ELSE
                PHITMP = PHITMP + TWOPI
              END IF
            END IF
            ETASUM(KK) = ETATMP*Q(POINT+8) + ETASUM(KK)
            PHISUM(KK) = PHITMP*Q(POINT+8) + PHISUM(KK)
            ETA2SU(KK) = ETATMP*ETATMP*Q(POINT+8) + ETA2SU(KK)
            PHI2SU(KK) = PHITMP*PHITMP*Q(POINT+8) + PHI2SU(KK)
            Q(MYJET(KK)+10) = Q(POINT+9) + Q(MYJET(KK)+10)
            Q(MYJET(KK)+11) = Q(POINT+10) + Q(MYJET(KK)+11)
            IF(IQ(POINT+14).LE.7) ETEM(KK) = ETEM(KK)+Q(POINT+8)
          END IF
  310   CONTINUE
        NUMADD = 0
        DO 320 I = 1 , IQ(MYJPTS(1)+2)
          IF(IQ(MYJPTS(1)+I+2).GT.0) THEN  ! POINT NOT SHARED
            NUMADD = NUMADD +1
            POINT = LCAEH + NREP*(IQ(MYJPTS(1)+I+2)-1)
            Q(MYJET(KK)+2) = Q(POINT+4) + Q(MYJET(KK)+2)
            Q(MYJET(KK)+3) = Q(POINT+5) + Q(MYJET(KK)+3)
            Q(MYJET(KK)+4) = Q(POINT+6) + Q(MYJET(KK)+4)
            Q(MYJET(KK)+5) = Q(POINT+7) + Q(MYJET(KK)+5)
            Q(MYJET(KK)+6) = Q(POINT+8) + Q(MYJET(KK)+6)
            DO 315 II = 1 ,4
              E(II) = Q(POINT+3+II)
  315       CONTINUE
            CALL ETOETA(E,PHITMP,THETA,ETATMP)
            IF (ABS(PHITMP-PHIJ).GT.(TWOPI-ABS(PHITMP-PHIJ))) THEN
              IF(PHIJ.LT.PHITMP) THEN
                PHITMP = PHITMP - TWOPI
              ELSE
                PHITMP = PHITMP + TWOPI
              END IF
            END IF
            ETASUM(KK) = ETATMP*Q(POINT+8) + ETASUM(KK)
            PHISUM(KK) = PHITMP*Q(POINT+8) + PHISUM(KK)
            ETA2SU(KK) = ETATMP*ETATMP*Q(POINT+8) + ETA2SU(KK)
            PHI2SU(KK) = PHITMP*PHITMP*Q(POINT+8) + PHI2SU(KK)
            Q(MYJET(KK)+10) = Q(POINT+9) + Q(MYJET(KK)+10)
            Q(MYJET(KK)+11) = Q(POINT+10) + Q(MYJET(KK)+11)
            IF(IQ(POINT+14).LE.7) ETEM(KK) = ETEM(KK)+Q(POINT+8)
          END IF
  320   CONTINUE
        E(1) = Q(MYJET(KK)+2)
        E(2) = Q(MYJET(KK)+3)
        E(3) = Q(MYJET(KK)+4)
        E(4) = SQRT(E(1)**2+E(2)**2+E(3)**2)
        CALL ETOETA(E,PHI,THETA,ETA)
        Q(MYJET(KK)+7) = THETA
        Q(MYJET(KK)+8) = PHI
        Q(MYJET(KK)+9) = ETA
C
C          FIX THE JPTS BANK, ADD IN THE NEW POINTS
C
        CALL MZPUSH(IXMAIN,MYJPTS(2),0,NUMADD,'I')
        LJETS = GZJETS()
        DO 400 I = 1 , NUMJET -1
          IF (I.EQ.IJET) THEN
            MYJET(2) = LJETS
            MYJPTS(2) = LQ(MYJET(2)-IZJPTS)
            NUMIN2 = IQ(MYJPTS(2)+2)
          END IF
          LJETS = LQ(LJETS)
  400   CONTINUE
        MYJET(1) = LJETS
        MYJPTS(1) = LQ(MYJET(1)-IZJPTS)
        IGOOD = 0
        DO 410 I = 1, IQ(MYJPTS(1)+2)
          IF(IQ(MYJPTS(1)+I+2).GT.0) THEN  ! POINT NOT SHARED
            IGOOD = IGOOD + 1
            IQ(MYJPTS(2)+NUMIN2+IGOOD+2) = IQ(MYJPTS(1)+2+I)
          END IF
  410   CONTINUE
        IQ(MYJPTS(2)+2) = NUMIN2+ NUMADD
        Q(MYJET(KK)+14) = ETEM(KK)/Q(MYJET(KK)+6)
        IQ(MYJET(KK)+15) = 1 + 100* NSHARE    ! 1 MEANS COMBINED JET
        ETAWID = ETA2SU(KK)/Q(MYJET(KK)+6)
        PHIWID = PHI2SU(KK)/Q(MYJET(KK)+6)
        ETAWID = ETAWID-(ETASUM(KK)/Q(MYJET(KK)+6))**2
        ETAWID = SQRT(ABS(ETAWID))
        PHIWID = PHIWID-(PHISUM(KK)/Q(MYJET(KK)+6))**2
        PHIWID = SQRT(ABS(PHIWID))
        Q(MYJET(KK)+12) = ETAWID
        Q(MYJET(KK)+13) = PHIWID
C
C ****  COMBINE AREAS
C
        Q(MYJET(KK)+30) = Q(MYJET(KK)+30) + SMAREA_STHW(3)
        Q(MYJET(KK)+31) = Q(MYJET(KK)+31) + SMAREA(3)
        Q(MYJET(KK)+38) = Q(MYJET(KK)+38) + SMAREA_ICR(3)
        Q(MYJET(KK)+41) = Q(MYJET(KK)+41) + SMAREA_ICR_STHW(3)
        CONE_ARRY(NUM_CONES,3) = IJET  ! CONE NOW IN IJET
C
C               DROP THE LAST JET BANK AND ITS DEPENDENTS
C
        CALL MZDROP(IXMAIN,MYJET(1),' ')       !DROP THE LAST JET BANK
        NUMJET = NUMJET - 1
        GOTO 999
C
      ELSE                              ! KEEP THE JETS SEPARATE
C                                        THIS IS THE SECOND POSSIBILITY
C
C       FIRST CLEAN UP THE CATE BANK
C
        LCATE = GZCATE()
        NTOTOW = MOD(IQ(LCATE+3),CATENM)
        NEMTOW = INT(IQ(LCATE+3)/CATENM)
        NREP = IQ(LCATE+2)
        DO 510 NTOW = NEMTOW+1, NTOTOW
          POINT = LCATE + NREP*(NTOW-1)
          ITARG = INT(IQ(POINT+16)/1000)
          IF(NUMJET.EQ.ITARG) THEN
            ITARG = IQ(POINT+16)-1000*NUMJET
            IF(ITARG.NE.IJET) THEN
              NSHARE = 2           ! TROUBLE JETS SHARE ET
              IQ(POINT+16) = ITARG
            END IF
          END IF
          ITARG = IJET + 1000*NUMJET
          IF(IQ(POINT+16).EQ.ITARG) THEN
            DO 500 II = 1 ,4
              E(II) = Q(POINT+3+II)
  500       CONTINUE
            CALL ETOETA(E,PHITMP,THETA,ETATMP)
            PHTMP(2) = PHITMP
            IF (ABS(PHITMP-PHIJ).GT.(TWOPI-ABS(PHITMP-PHIJ))) THEN
              IF(PHIJ.LT.PHITMP) THEN
                PHTMP(2) = PHITMP - TWOPI
              ELSE
                PHTMP(2) = PHITMP + TWOPI
              END IF
            END IF
            RAD2 = (ETATMP-ETAJ)**2 + (PHTMP(2)-PHIJ)**2
            PHTMP(1) = PHITMP
            IF (ABS(PHITMP-PHIPHY).GT.(TWOPI-ABS(PHITMP-PHIPHY))) THEN
              IF(PHIPHY.LT.PHITMP) THEN
                PHTMP(1) = PHITMP - TWOPI
              ELSE
                PHTMP(1) = PHITMP + TWOPI
              END IF
            END IF
            RAD1 = (ETATMP-ETAPHY)**2 + (PHTMP(1)-PHIPHY)**2
            IQ(POINT+16) = NUMJET
            IF(RAD1.GT.RAD2)  IQ(POINT+16) = IJET
          END IF
  510   CONTINUE
C
C
C
        DO 650 KK = 1, 2
          DO 630 I = 2 ,11
            Q(MYJET(KK)+I)= 0.
  630     CONTINUE
          NUMPTS(KK) = 0
          ETEM(KK) = 0.
          ETASUM(KK) = 0.
          ETA2SU(KK) = 0.
          PHISUM(KK) = 0.
          PHI2SU(KK) = 0.
  650   CONTINUE
C
        LCAEH = GZCAEH()
        NREP = IQ(LCAEH+2)
        DO 710 I = 1 , IQ(MYJPTS(2)+2)
          POINT = LCAEH + NREP*(IQ(MYJPTS(2)+I+2)-1)
          DO 700 II = 1 ,4
            E(II) = Q(POINT+3+II)
  700     CONTINUE
          CALL ETOETA(E,PHITMP,THETA,ETATMP)
          PHTMP(2) = PHITMP
          IF (ABS(PHITMP-PHIJ).GT.(TWOPI-ABS(PHITMP-PHIJ))) THEN
            IF(PHIJ.LT.PHITMP) THEN
              PHTMP(2) = PHITMP - TWOPI
            ELSE
              PHTMP(2) = PHITMP + TWOPI
            END IF
          END IF
          RAD2 = (ETATMP-ETAJ)**2 + (PHTMP(2)-PHIJ)**2
          PHTMP(1) = PHITMP
          IF (ABS(PHITMP-PHIPHY).GT.(TWOPI-ABS(PHITMP-PHIPHY))) THEN
            IF(PHIPHY.LT.PHITMP) THEN
              PHTMP(1) = PHITMP - TWOPI
            ELSE
              PHTMP(1) = PHITMP + TWOPI
            END IF
          END IF
          RAD1 = (ETATMP-ETAPHY)**2 + (PHTMP(1)-PHIPHY)**2
          KK = 1
          IF(RAD1.GT.RAD2) KK = 2
          NUMPTS(KK) = NUMPTS(KK) + 1
          KPOINT(NUMPTS(KK),KK) = IQ(MYJPTS(2)+I+2)
          Q(MYJET(KK)+2) = Q(POINT+4) + Q(MYJET(KK)+2)
          Q(MYJET(KK)+3) = Q(POINT+5) + Q(MYJET(KK)+3)
          Q(MYJET(KK)+4) = Q(POINT+6) + Q(MYJET(KK)+4)
          Q(MYJET(KK)+5) = Q(POINT+7) + Q(MYJET(KK)+5)
          Q(MYJET(KK)+6) = Q(POINT+8) + Q(MYJET(KK)+6)
          ETASUM(KK) = ETATMP*Q(POINT+8) + ETASUM(KK)
          PHISUM(KK) = PHTMP(KK)*Q(POINT+8) + PHISUM(KK)
          ETA2SU(KK) = ETATMP*ETATMP*Q(POINT+8) + ETA2SU(KK)
          PHI2SU(KK) = PHTMP(KK)*PHTMP(KK)*Q(POINT+8) + PHI2SU(KK)
          Q(MYJET(KK)+10) = Q(POINT+9) + Q(MYJET(KK)+10)
          Q(MYJET(KK)+11) = Q(POINT+10) + Q(MYJET(KK)+11)
          IF(IQ(POINT+14).LE.7) ETEM(KK) = ETEM(KK)+Q(POINT+8)
  710   CONTINUE
        NUMADD = 0
        DO 720 I = 1 , IQ(MYJPTS(1)+2)
          IF(IQ(MYJPTS(1)+I+2).GT.0) THEN  ! POINT NOT SHARED
            NUMADD = NUMADD +1
            POINT = LCAEH + NREP*(IQ(MYJPTS(1)+I+2)-1)
            DO 715 II = 1 ,4
              E(II) = Q(POINT+3+II)
  715       CONTINUE
            CALL ETOETA(E,PHITMP,THETA,ETATMP)
            PHTMP(2) = PHITMP
            IF (ABS(PHITMP-PHIJ).GT.(TWOPI-ABS(PHITMP-PHIJ))) THEN
              IF(PHIJ.LT.PHITMP) THEN
                PHTMP(2) = PHITMP - TWOPI
              ELSE
                PHTMP(2) = PHITMP + TWOPI
              END IF
            END IF
            RAD2 = (ETATMP-ETAJ)**2 + (PHTMP(2)-PHIJ)**2
            PHTMP(1) = PHITMP
            IF (ABS(PHITMP-PHIPHY).GT.(TWOPI-ABS(PHITMP-PHIPHY))) THEN
              IF(PHIPHY.LT.PHITMP) THEN
                PHTMP(1) = PHITMP - TWOPI
              ELSE
                PHTMP(1) = PHITMP + TWOPI
              END IF
            END IF
            RAD1 = (ETATMP-ETAPHY)**2 + (PHTMP(1)-PHIPHY)**2
            KK = 1
            IF(RAD1.GT.RAD2) KK = 2
            NUMPTS(KK) = NUMPTS(KK) + 1
            KPOINT(NUMPTS(KK),KK) = IQ(MYJPTS(1)+I+2)
            Q(MYJET(KK)+2) = Q(POINT+4) + Q(MYJET(KK)+2)
            Q(MYJET(KK)+3) = Q(POINT+5) + Q(MYJET(KK)+3)
            Q(MYJET(KK)+4) = Q(POINT+6) + Q(MYJET(KK)+4)
            Q(MYJET(KK)+5) = Q(POINT+7) + Q(MYJET(KK)+5)
            Q(MYJET(KK)+6) = Q(POINT+8) + Q(MYJET(KK)+6)
            ETASUM(KK) = ETATMP*Q(POINT+8) + ETASUM(KK)
            PHISUM(KK) = PHTMP(KK)*Q(POINT+8) + PHISUM(KK)
            ETA2SU(KK) = ETATMP*ETATMP*Q(POINT+8) + ETA2SU(KK)
            PHI2SU(KK) = PHTMP(KK)*PHTMP(KK)*Q(POINT+8) + PHI2SU(KK)
            Q(MYJET(KK)+10) = Q(POINT+9) + Q(MYJET(KK)+10)
            Q(MYJET(KK)+11) = Q(POINT+10) + Q(MYJET(KK)+11)
            IF(IQ(POINT+14).LE.7) ETEM(KK) = ETEM(KK)+Q(POINT+8)
          END IF
  720   CONTINUE
C
        DO 750 KK = 1, 2
          E(1) = Q(MYJET(KK)+2)
          E(2) = Q(MYJET(KK)+3)
          E(3) = Q(MYJET(KK)+4)
          E(4) = SQRT(E(1)**2+E(2)**2+E(3)**2)
          CALL ETOETA(E,PHI,THETA,ETA)
          Q(MYJET(KK)+7) = THETA
          Q(MYJET(KK)+8) = PHI
          Q(MYJET(KK)+9) = ETA
          Q(MYJET(KK)+14) = ETEM(KK)/Q(MYJET(KK)+6)
          IQ(MYJET(KK)+15) = 2                 ! 2 MEANS SPLIT JET
          ETAWID = ETA2SU(KK)/Q(MYJET(KK)+6)
          PHIWID = PHI2SU(KK)/Q(MYJET(KK)+6)
          ETAWID = ETAWID-(ETASUM(KK)/Q(MYJET(KK)+6))**2
          ETAWID = SQRT(ABS(ETAWID))
          PHIWID = PHIWID-(PHISUM(KK)/Q(MYJET(KK)+6))**2
          PHIWID = SQRT(ABS(PHIWID))
          Q(MYJET(KK)+12) = ETAWID
          Q(MYJET(KK)+13) = PHIWID
C
C       FIX THE JPTS BANK
C
          NUMADD = NUMPTS(KK)-IQ(MYJPTS(KK)+2)
          CALL MZPUSH(IXMAIN,MYJPTS(KK),0,NUMADD,'I')
          IQ(MYJPTS(KK)+2) = NUMPTS(KK)
          DO 730 I = 1, NUMPTS(KK)
            IQ(MYJPTS(KK)+2+I) = KPOINT(I,KK)
  730     CONTINUE
C
  750   CONTINUE
C
C
C
C ****  SPLIT AREAS
C
        Q(MYJET(1)+30) = Q(MYJET(1)+30) - SMAREA_STHW(2)
        Q(MYJET(1)+31) = Q(MYJET(1)+31) - SMAREA(2)
        Q(MYJET(1)+38) = Q(MYJET(1)+38) - SMAREA_ICR(2)
        Q(MYJET(1)+41) = Q(MYJET(1)+41) - SMAREA_ICR_STHW(2)
        Q(MYJET(2)+30) = Q(MYJET(2)+30) - SMAREA_STHW(1)
        Q(MYJET(2)+31) = Q(MYJET(2)+31) - SMAREA(1)
        Q(MYJET(2)+38) = Q(MYJET(2)+38) - SMAREA_ICR(1)
        Q(MYJET(2)+41) = Q(MYJET(2)+41) - SMAREA_ICR_STHW(1)
C
      ENDIF
C
C
C---Deactivate our zebra link area
  999 DUM(1) = 0                       ! Deactivate
      RETURN
C
      ENTRY SPLINI
C
C                 get constants out of CAJETS.RCP
C
      IRUNIT  = ALG_PARAMS(5)
      ISPLIFR = ALG_PARAMS(4)
      TWORAD=RUNIT**2
C
      RETURN
      END
