      SUBROUTINE FILL_CAWX(LCASH,LCAWX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill CAWX bank from CASH and CAW7 banks
C-
C-   Inputs  : LCASH [I] - Link to CASH
C-   Outputs : LCAWX [I] - link to CAWX
C-
C-   Controls: none
C-
C-   Created  24-JUL-1995   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAW7.LINK'
      INCLUDE 'D0$LINKS:IZCAWX.LINK'
      INTEGER LCASH,LCAW7,LCAWX
      INTEGER NCELL_CASH,NCELL_CAW7,COUNT,I,J
      INTEGER PADRCAW7,PADRCASH
      INTEGER NCELLMAX
      PARAMETER( NCELLMAX = 400 )
      REAL    ENERGY(NCELLMAX)
      INTEGER PADR(NCELLMAX)
      LOGICAL NOT_IN_CASH
C----------------------------------------------------------------------
      
      LCAWX = 0

      IF (LCASH.LE.0) THEN
        CALL ERRMSG('NO CASH','FILL_CAWX',' ' ,'W')
        GOTO 999
      ENDIF
      LCAW7 = LQ(LCASH-IZCAW7)
      IF (LCAW7.LE.0) THEN
        CALL ERRMSG('NO CAW7','FILL_CAWX',' ' ,'W')
        GOTO 999
      ENDIF

      NCELL_CASH = IQ(LCASH+2)
      NCELL_CAW7 = IQ(LCAW7+2)

      COUNT = 0

      DO I=1,NCELL_CAW7
        PADRCAW7 = IQ(LCAW7+2*I+1)
        NOT_IN_CASH = .TRUE.
        DO J=1,NCELL_CASH
          PADRCASH = IQ(LCASH+2*J+1)
          IF (PADRCASH.EQ.PADRCAW7) NOT_IN_CASH = .FALSE.
        ENDDO
        IF (NOT_IN_CASH) THEN
          COUNT = COUNT + 1
          PADR(COUNT)   = IQ(LCAW7+2*I+1)
          ENERGY(COUNT) =  Q(LCAW7+2*I+2)
        ENDIF
      ENDDO

      CALL BKCAWX(LCASH,COUNT,LCAWX)
      
      IF (LCAWX.GT.0) THEN
        DO I=1,COUNT
          IQ(LCAWX+2*I+1) = PADR(I)
          Q (LCAWX+2*I+2) = ENERGY(I)
        ENDDO
      ENDIF

  999 RETURN
      END
