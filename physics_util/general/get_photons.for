      SUBROUTINE GET_PHOTONS(NPHO_MAX,ET_CUT,P9_PHOTON,NPHO,PHOTON)    
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET PHOTONS FROM THE CURRENT PATH.
C-                         THE OUTPUT IS ORDERED ACCORDING TO DECREASING ET
C-                         (HIGHEST ET FIRST).
C-
C-   Inputs  : NPHO_MAX           [I]   MAXIMUM NUMBER OF PHOTONS TO OUTPUT
C-                                      IN P9_PHOTON(9,*)
C-             ET_CUT             [R]   ET CUT FOR PHOTONS
C-   Outputs : P9_PHOTON(9,*)     [R]   4-VECTOR OF PHOTON (EX,EY,EZ,E)
C-                                      + ET_PHOTON
C-                                      + THETA
C-                                      + PHI
C-                                      + ETA
C-                                      + EXTRA ENERGY IN ISOLATION CONE (.7).
C-
C-             NPHO               [I]   TOTAL NUMBER OF PHOTONS SURVIVING
C-                                      ET CUT
C-             PHOTON             [L]   TRUE if NPHO > 0
C-             
C----------------------------------------------------------------------
C-   Created   5-JAN-1991   Rajendran Raja
C-   Updated  12-NOV-1991   Stan M. Krzywdzinski, Harrison B. Prosper
C-      Make into three tidy utilities, fix a few things.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPHO_MAX,NPHO
      REAL    ET_CUT,P9_PHOTON(9,*)
      LOGICAL PHOTON
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      INTEGER GZPPHO
      REAL    ET_PHOT
C
C----------------------------------------------------------------------
C
      PHOTON = .FALSE.
      NPHO = 0
      CALL VZERO(P9_PHOTON,9*NPHO_MAX)
C
      LPPHO = GZPPHO()
      IF ( LPPHO.LE.0 ) THEN
        GOTO 999
      ENDIF
C
      ET_PHOT = 0
      IF ( LPPHO.NE.0 ) THEN
        CALL ZSORT(IXMAIN,LPPHO,7)      ! SORT IN INCREASING ORDER OF ET
        LPPHO = GZPPHO()                ! GET IT AGAIN
        CALL ZTOPSY(IXMAIN,LPPHO)       ! HIGHEST ET FIRST
        LPPHO = GZPPHO()                ! GET IT AGAIN
        DO WHILE (LPPHO.NE.0)
          ET_PHOT = Q(LPPHO+7)
          IF(ET_PHOT.GT.ET_CUT)THEN
            NPHO = NPHO + 1
            IF(NPHO.LE.NPHO_MAX) THEN
              CALL UCOPY(Q(LPPHO+3),P9_PHOTON(1,NPHO),6)
              P9_PHOTON(7,NPHO) = Q(LPPHO+10)
              P9_PHOTON(8,NPHO) = Q(LPPHO+ 9)
              P9_PHOTON(9,NPHO) = Q(LPPHO+16) - Q(LPPHO+6)     ! EXTRA ENERGY
                                                               ! IN LARGE CONE
            ENDIF
          ENDIF
          LPPHO = LQ(LPPHO)
        ENDDO
      ENDIF
      IF (NPHO.GT.0) PHOTON = .TRUE.
C
  999 RETURN
      END
