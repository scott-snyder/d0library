      SUBROUTINE GET_ISA_ELECTRON(P4_ELEC,P4_NEUT,W_ISA,TOP_MASS,
     &  P4_CLUST,DIFF,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SET UP ELECTRON QUANTITIES FROM ISAJET
C-   INFO
C-
C-   Inputs  :ISAL BANKS
C-   Outputs :P4_ELEC,P4_NEUT = 4 VECTORS FROM ISAJET OF ELECTRON
C-            AND NEUTRINO.
C-            W_ISA = 4 VECTOR OF PARENT W.
C-            TOP_MASS = MASS OF TOP QUARK GIVING THE W.
C-            P4_CLUST = 4 VECTOR OF CLUSTER FROM CACL BANK THAT IS CLOSEST
C-            TO Isajet electron.
C-            DIFF is difference is Isajet electron 4 vector and P4_CLUST
C-            IER NON ZERO. SOMETHING WRONG WITH EVENT
C-   Controls:
C-
C-   Created   5-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER WLINK,TLINK
      EQUIVALENCE (CRFLNK(3),WLINK),(CRFLNK(4),TLINK)
      CHARACTER*32 ALG_NAME
      REAL    TOP_MASS,PT_MAX
      INTEGER J,IER,IER1,NDECAY,NCHARS
      REAL    P4_ELEC(*),P4_NEUT(*),W_ISA(*),P4_CLUST(*),DIFF
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST  ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZ_GET_CHARS('ELECTRON_ALGORITHM_NAME',NCHARS,
     &    ALG_NAME,IER)
        CALL EZGET('PT_MAX_OTHER_NEUTRINO',PT_MAX,IER)
        CALL EZRSET
      ENDIF
C
      IER = 0
      CALL SET_CAPH(ALG_NAME,0,IER)
      CALL GET_ISA_W_ENU(1,NDECAY,P4_ELEC,P4_NEUT,IER)
      IF(IER.EQ.0)THEN
        DO J = 1 ,4
          W_ISA(J) = P4_ELEC(J) + P4_NEUT(J)
        ENDDO
        WLINK = CSTLNK(LNKMX-1)           ! Link to W ISAJ
        TLINK = LQ(WLINK-1)               ! link to parent Top ISAJ
        TOP_MASS = Q(TLINK + 6)              ! Generated top mass
C
        CALL SEE_IF_OTHER_NEUTRINO(PT_MAX,IER1)
        IER = IER + IER1
C
        CALL FIND_CLOSE_CLUSTER(P4_ELEC,P4_CLUST,DIFF,IER1)
C
        IER = IER + IER1
C
      ENDIF
      CALL RESET_CAPH
  999 RETURN
      END
