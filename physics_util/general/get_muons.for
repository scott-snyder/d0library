      SUBROUTINE GET_MUONS(NMUO_MAX,ET_CUT,P9_MUON,NMUO,MUON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET MUONS FROM THE CURRENT PATH.
C-                         THE OUTPUT IS ORDERED ACCORDING TO DECREASING ET
C-                         (HIGHEST ET FIRST).
C-
C-   Inputs  : NMUO_MAX           [I]   MAXIMUM NUMBER OF MUONS TO OUTPUT
C-                                      IN P9_MUON(9,*)
C-             ET_CUT             [R]   ET CUT FOR MUONS
C-   Outputs : P9_MUON(9,*)       [R]   4-VECTOR OF MUON (PX,PY,PZ,P)
C-                                      + ET_MUON
C-                                      + THETA
C-                                      + PHI
C-                                      + ETA
C-                                      + ISOLATION PARAMETER (CONE 0.6)
C-
C-             NMUO               [I]   TOTAL NUMBER OF MUONS SURVIVING
C-                                      ET CUT
C-             MUON               [L]   TRUE if NMUO > 0
C-   Controls:
C-
C-   Created   5-JAN-1991   Rajendran Raja
C-   Updated  12-NOV-1991   Stan M. Krzywdzinski, Harrison B. Prosper
C-      Make into three tidy utilities, fix a few things.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMUO_MAX,NMUO
      REAL    ET_CUT,P9_MUON(9,*)
      LOGICAL MUON
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      INTEGER GZPMUO,LPMUO
      REAL    PT_MUO
C
C----------------------------------------------------------------------
C
      MUON = .FALSE.
      NMUO = 0
      CALL VZERO(P9_MUON,9*NMUO_MAX)
C
      LPMUO = GZPMUO(0)
      IF ( LPMUO.LE.0 ) THEN
        GOTO 999
      ENDIF
C
      PT_MUO = 0
      IF ( LPMUO.NE.0 ) THEN
        CALL ZSORT(IXMAIN,LPMUO,14)      ! SORT IN INCREASING ORDER OF ET
        LPMUO = GZPMUO(0)                ! GET IT AGAIN
        CALL ZTOPSY(IXMAIN,LPMUO)       ! HIGHEST ET FIRST
        LPMUO = GZPMUO(0)                ! GET IT AGAIN
        DO WHILE (LPMUO .NE.0)
          PT_MUO = Q(LPMUO+14)
          IF(PT_MUO.GT.ET_CUT)THEN
            NMUO = NMUO + 1
            IF (NMUO.LE.NMUO_MAX) THEN
              CALL UCOPY(Q(LPMUO+10),P9_MUON(1,NMUO),6)
              P9_MUON(7,NMUO) = Q(LPMUO+17)
              P9_MUON(8,NMUO) = Q(LPMUO+16)
              P9_MUON(9,NMUO) = Q(LPMUO+32)       ! ISOLATION PARAMETER
                                                  ! BASED ON CONE OF 0.6
            ENDIF
          ENDIF
          LPMUO = LQ(LPMUO)
        ENDDO
      ENDIF
      IF (NMUO.GT. 0) MUON = .TRUE.
C
  999 RETURN
      END
