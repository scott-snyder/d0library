      SUBROUTINE GET_ELECTRONS(NELEC_MAX,ET_CUT,P12_ELECTRON,NELEC,
     &                         ELECTRON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET ELECTRONS FROM THE CURRENT PATH.
C-                         THE OUTPUT IS ORDERED ACCORDING TO DECREASING ET
C-                         (HIGHEST ET FIRST).
C-
C-   Inputs  : NELEC_MAX          [I]   MAXIMUM NUMBER OF ELECTRONS TO OUTPUT
C-                                      IN P12_ELECTRON(12,*)
C-             ET_CUT             [R]   ET CUT FOR ELECTRONS
C-   Outputs : P12_ELECTRON(12,*) [R]   4-VECTOR OF ELECTRON (EX,EY,EZ,E)
C-                                      + ET_ELECTRON
C-                                      + THETA
C-                                      + PHI
C-                                      + ETA
C-                                      + TOTAL ENERGY IN ISOLATION CONE
C-                                      + DISTANCE OF CLOSEST APPROACH OF
C-                                        CENTRAL TRACK
C-                                      + HMATRIX DIMENSION
C-                                      + CHISQUARED FOR HMATRIX
C-
C-             NELEC              [I]   TOTAL NUMBER OF ELECTRONS SURVIVING
C-                                      ET CUT
C-             ELECTRON           [L]   TRUE if NELEC > 0
C-
C----------------------------------------------------------------------
C-
C-   Created   5-JAN-1991   Rajendran Raja
C-   Updated  12-NOV-1991   Stan M. Krzywdzinski, Harrison B. Prosper
C-      Make into three tidy utilities, fix a few things.
C-   Updated  13-OCT-1992   Stan M. Krzywdzinski
C-                          Increased P10_ELECTRON(10,*) --> P12_ELECTRON(12,*)
C-                          and changed meaning of P12_ELECTRON(10,*)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NELEC_MAX,NELEC
      REAL    ET_CUT,P12_ELECTRON(12,*)
      LOGICAL ELECTRON
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZHMTE.LINK'
C----------------------------------------------------------------------
      INTEGER GZPELC
      REAL    ET_ELEC
C----------------------------------------------------------------------
      ELECTRON = .FALSE.
      NELEC = 0
      CALL VZERO(P12_ELECTRON,12*NELEC_MAX)
C
      LPELC = GZPELC()
      IF ( LPELC.LE.0 ) THEN
        GOTO 999
      ENDIF
C
      ET_ELEC = 0
      IF ( LPELC.NE.0 ) THEN
        CALL ZSORT(IXMAIN,LPELC,7)      ! SORT IN INCREASING ORDER OF ET
        LPELC = GZPELC()                ! GET IT AGAIN
        CALL ZTOPSY(IXMAIN,LPELC)       ! HIGHEST ET FIRST
        LPELC = GZPELC()                ! GET IT AGAIN
        DO WHILE (LPELC.NE.0)
          ET_ELEC = Q(LPELC+7)            ! ET OF ELECTRON
          IF(ET_ELEC.GT.ET_CUT)THEN
            NELEC = NELEC + 1
            IF(NELEC.LE.NELEC_MAX) THEN
              CALL UCOPY(Q(LPELC+3),P12_ELECTRON(1,NELEC),6)
              P12_ELECTRON( 7,NELEC) = Q(LPELC+10)
              P12_ELECTRON( 8,NELEC) = Q(LPELC+ 9)
              P12_ELECTRON( 9,NELEC) = Q(LPELC+16)
              P12_ELECTRON(10,NELEC) = Q(LPELC+22)
              LHMTE = LQ(LPELC-IZHMTE)
              IF (LHMTE .GT. 0) THEN
                P12_ELECTRON(11,NELEC) = IQ(LHMTE+3)
                P12_ELECTRON(12,NELEC) =  Q(LHMTE+5)
              ENDIF
            ENDIF
          ENDIF
          LPELC = LQ(LPELC)
        ENDDO
      ENDIF
      IF (NELEC.GT.0) ELECTRON = .TRUE.
C
  999 RETURN
      END
