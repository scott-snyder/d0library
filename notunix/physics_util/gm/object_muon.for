      SUBROUTINE OBJECT_MUON(NMU,NINFO,XMU_ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns muon information for global
C-                         monitoring package.
C-
C-   Inputs  :  NMU        Muon number
C-              NINFO      Number of information requested
C-
C-   Outputs :  XMU_ARRAY  Array of information returned. The maximum
C-                         list is : PX PY PZ P PT ETA PHI ETAD QF
C-                                   ISOL1 ISOL2 ISOL4 NCD ANGCD IMPT
C-                                   IFW4 IFW1 BDL DPHI DTHETA CAL_EN
C-                                   TRK_HITS FIT_HITS
C-   Controls:
C-
C-   Created  25-NOV-1992   SHAHRIAR ABACHI
C-   Updated  25-JAN-1993   Wyatt Merritt
C-   Updated  24-FEB-1993   K. Wyatt Merritt  Add error checks, elim
C-                          INTMSG call, fix bug in filling DET_ETA
C-   Updated  28-FEB-1993   Harrison B. Prosper
C-    Fix call to GZPMUO (add ITRAK)
C-   Updated   4-MAY-1993   Harrison B. Prosper
C-    Fix call to GZPMUO again!
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMU,NINFO
      REAL XMU_ARRAY(*)
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MAXINFO
      PARAMETER (MAXINFO=23)
      INTEGER IQMUO(MAXINFO)
      REAL QMUO(MAXINFO)
      EQUIVALENCE (QMUO,IQMUO)
      INTEGER I,LPMUO,LMUOT,GZPMUO,NINFO1,ITRAK
      INTEGER NPMUO,NMUINF
      CHARACTER*80 MSG
C
      NINFO1 = NINFO
      IF(NINFO .GT. MAXINFO) THEN
        MSG = 'NUMBER OF INFO REQUESTED IS LARGER THAN AVAILABLE.
     &    WILL BE SET TO MAXINFO'
        CALL ERRMSG('OBJECT_MUON','OBJECT_MUON',MSG,'I')
        NINFO1 = MAXINFO
      ENDIF
C
      DO I = 1 , NMU
        ITRAK = I
        LPMUO = GZPMUO(ITRAK)
      ENDDO
      IF (LPMUO .EQ. 0) THEN
        CALL ERRMSG('WRONGMUONS','OBJECT_MUON',
     &    'NO PMUO BANK PRESENT','W')
        GO TO 999
      ENDIF
      LMUOT = LQ(LPMUO-2)
      IF (LMUOT .EQ. 0) THEN
        CALL ERRMSG('BADMUONS','OBJECT_MUON',
     &    'NO MUOT BANK PRESENT','W')
        GO TO 999
      ENDIF
C
      QMUO(1) = Q(LPMUO + 10)         !px
      QMUO(2) = Q(LPMUO + 11)         !py
      QMUO(3) = Q(LPMUO + 12)         !pz
      QMUO(4) = Q(LPMUO + 13)         !p
      QMUO(5) = Q(LPMUO + 14)         !pt
      QMUO(6) = Q(LPMUO + 16)         !eta
      QMUO(7) = Q(LPMUO + 17)         !phi
      QMUO(8) = Q(LPMUO + 16)         !eta_detector (eta for now)
      IQMUO(9) = IQ(LPMUO + 44)       !quality flag (IFW2)
      QMUO(10) = Q(LPMUO + 29)        !isolation_1
      QMUO(11) = Q(LPMUO + 30)        !isolation_2
      QMUO(12) = Q(LPMUO + 31)        !isolation_4
      QMUO(13) = IQ(LPMUO + 6)        !No of CD tracks
      QMUO(14) = Q(LPMUO + 37)        !angle between muon and CD (degrees)
      QMUO(15) = Q(LPMUO + 41)        !impact parameter
      QMUO(16) = IQ(LPMUO + 9)        !quality flag 2 (IFW4)
      IQMUO(17) = IQ(LMUOT + 4)       ! IFW1
      QMUO(18) = Q(LMUOT + 22)        ! integral B.dl
      QMUO(19) = Q(LPMUO + 38)        ! ZTRAK_DPHI
      QMUO(20) = Q(LPMUO + 39)        ! ZTRAK_DTHETA
      QMUO(21) = Q(LPMUO + 34)        ! Calorimeter energy in tight cone
      QMUO(22) = Q(LPMUO + 46)        ! Hits on track
      QMUO(23) = Q(LPMUO + 47)        ! Fitted hits on track
C
      DO I=1,NINFO1
        XMU_ARRAY(I) = QMUO(I)
      ENDDO
C
      CONTINUE
C
  999 RETURN
C
C**************************************************************************
C
      ENTRY NOBJ_MUONS(NPMUO,NMUINF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used in global monitoring
C-
C-   Inputs  :
C-
C-   Outputs  :  NPMUO     Number of pmuo muons
C-               NMUINF    Number of information per muon
C-   Outputs :
C-   Controls:
C-
C-   Created  25-NOV-1992   SHAHRIAR ABACHI
C-   Modified 20-APR-1993   Stan M. Krzywdzinski Explicit sorting in
C-      decreasing order of Pt.
C-
C----------------------------------------------------------------------
      NPMUO = 0
      LPMUO = GZPMUO(0)
    1 IF(LPMUO .GT. 0) THEN
        NPMUO = NPMUO + 1
        LPMUO = LQ(LPMUO)
        GOTO 1
      ENDIF
      IF (NPMUO .GT. 1) THEN
C
C *** Sort banks in decreasing order of Pt
C
        LPMUO = GZPMUO(0)
        CALL ZSORT(IXMAIN,LPMUO,14)
        LPMUO = GZPMUO(0)
        CALL ZTOPSY(IXMAIN,LPMUO)
      ENDIF
C
C
      NMUINF = MAXINFO
C
C----------------------------------------------------------------------
      RETURN
      END
