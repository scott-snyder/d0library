      REAL FUNCTION FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Provide the stagger for a specified wire.
C-
C-   Returned value  : value of stagger
C-   Inputs  : HALF,UNIT,QUAD,SECTOR,WIRE    UNIT=0 (Theta)   UNIT=1 (Phi)
C-
C-   Created  19-JAN-1990   Jeffrey Bantly
C-   Created  13-AUG-1992   Robert E. Avery    New version,
C-                          for use within either FTRAKS or D0GEANT.
C-                          Works for either version of MC geometry.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER LFDTA,GZFDTA,LFDPH,GZFDPH,GZFGEH,ICALL,IER,IWIRE
C
      REAL    STAGGER,STAGVAL(0:NBPSEN-1,0:2)
C
      LOGICAL MC_OLD
C
      SAVE    ICALL,STAGVAL
      DATA    ICALL /0/
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        ICALL=1
C
        LFGEH=GZFGEH()
        IF ( LFGEH .GT. 0 ) THEN
          MC_OLD = ( IC(LFGEH+10) .EQ. 9002 )
        ENDIF
C
        LFDTA=GZFDTA()
        IF(LFDTA.GT.0) THEN
          DO IWIRE=0,NBTSEN-1
            STAGVAL(IWIRE,0)=C(LFDTA+36+IWIRE)
          ENDDO
        ENDIF
C
        LFDPH=GZFDPH()
        IF(LFDPH.GT.0) THEN
          DO IWIRE=0,NBPSEN-1
            STAGVAL(IWIRE,1) =  C(LFDPH+54+IWIRE)
            IF ( MC_OLD ) THEN
              STAGVAL(IWIRE,2) =  C(LFDPH+54+IWIRE)
            ELSE
              STAGVAL(IWIRE,2) = -C(LFDPH+54+IWIRE)
            ENDIF
          ENDDO
        ENDIF
      END IF
C
      IF ( UNIT .EQ. 0 ) THEN
        IF ( SECTOR .GT. 2 ) THEN
          STAGGER=STAGVAL(WIRE,0)
        ELSE
          STAGGER=0.
        ENDIF
      ELSE
        STAGGER=STAGVAL(WIRE,1+HALF)
      ENDIF
C
      FSTAGR=STAGGER
C
  999 RETURN
      END
