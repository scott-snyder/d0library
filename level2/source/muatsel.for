      LOGICAL FUNCTION MUATSEL( ITK ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MUON Alignment Track SELection
C-
C-   Returned value  : 
C-   Inputs  : ITK MUOT track ID
C-   Outputs : 
C-   Controls: 
C-
C-   Created  12-MAY-1993   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ITK
      INTEGER  NHIT, IWAD(40)
      REAL     X(40), Y(40), Z(40)
C
      INTEGER I,K1,K2, NHITW
      INTEGER NMOD(40), NPLN(40), NWIR(40), IER
      INTEGER IMORI(40), IMOD(40), IMHIT(40), NIMOD, DUM(40)
      INTEGER MUORIENT
C----------------------------------------------------------------------
      MUATSEL = .FALSE.

C
      CALL GTMHOT( 1,ITK, NHIT, X,Y,Z,IWAD )
C
      IF ( NHIT.LT.5 ) GOTO 999       ! Nhit >= 5
C
      NHITW = 0
      NIMOD = 0
      DO I=1,NHIT
        IF ( ABS(X(I)).GT.1000.0 ) GOTO 110
        IF ( ABS(Y(I)).GT.1000.0 ) GOTO 110
        IF ( ABS(Z(I)).GT.1000.0 ) GOTO 110
        CALL MUADD( IWAD(I), NMOD(I), NPLN(I), NWIR(I), IER )
        IF ( IER.NE.0 ) GOTO 110
        NHITW = NHITW + 1
        DO K1=1,NIMOD
          IF ( NMOD(I).EQ.IMOD(K1) ) THEN
            IMHIT(K1) = IMHIT(K1) + 1
            GOTO 120
          END IF
        END DO
        NIMOD = NIMOD + 1
        IMOD(NIMOD) = NMOD(I)
        IMHIT(NIMOD) = 1
        IMORI(NIMOD) = MUORIENT(NMOD(I))
  120   CONTINUE
  110   CONTINUE
      END DO
C
      IF ( NHITW.LT.5 ) GOTO 999      ! NhitW >= 5
      IF ( NIMOD.LT.2 ) GOTO 999
C
      DO K1=1,NIMOD                   ! Scan B-C
        IF ( IMOD(K1).GE.100.AND.IMOD(k1).LT.200 ) THEN
        IF ( IMHIT(K1).GE.2 ) THEN
          DO K2=1,NIMOD
            IF ( IMOD(K2).GE.200.AND.IMOD(k1).LE.307 ) THEN
            IF ( IMORI(k1).EQ.IMORI(K2) ) THEN
              IF ( (IMHIT(k1).EQ.2.AND.IMHIT(K2).GE.3).OR.
     1             (IMHIT(k1).EQ.3.AND.IMHIT(K2).GE.2) ) THEN
                MUATSEL = .TRUE.
                GOTO 999
              END IF
            END IF
            END IF
          END DO
        END IF
        END IF  
      END DO
C
      DO K1=1,NIMOD         ! SCAN A-B and A-C
        IF ( IMOD(K1).LT.100 .AND.IMHIT(K1).GE.3 ) THEN
          DO K2=1,NIMOD
            IF ( IMOD(K2).GE.100.AND.IMOD(K2).LE.307 ) THEN
            IF ( IMHIT(K2).EQ.3 .AND.IMORI(k1).EQ.IMORI(K2) ) THEN
              MUATSEL = .TRUE.
            END IF
            END IF
          END DO
        END IF
      END DO
C
  999 RETURN
      END
