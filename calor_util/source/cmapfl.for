      SUBROUTINE CMAPFL (LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the bank CMAP. See CMAP.ZEB for a
C-   full description.
C-
C-   Inputs  : LBANK    [I]     Address of first pre-cluster bank
C-                              in linear chain.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-JAN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LBANK
C
      INTEGER LCMAP,I,J,K,L,II,JJ,IER,NS,IBANK
      INTEGER IVERS,NCLUSTERS,IREPEAT,ICLASS,INEXT
      INTEGER GZCMAP
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
C ****  Check pre-cluster bank address
C
      IF ( LBANK .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CMAPFL',
     &    'Pre-cluster bank address is ZERO','W')
        GOTO 999
      ENDIF
C
C ****  Get CMAP address
C
      LCMAP = GZCMAP()
      IF ( LCMAP .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CMAPFL',
     &    'CMAP bank address is ZERO','W')
        GOTO 999
      ENDIF
C
C ****  Get Repetition number etc. from CMAP bank
C
      CALL GTCMAP_TOTAL (IVERS,NCLUSTERS,IREPEAT,ICLASS,INEXT,IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CMAPFL',
     &    'Error in GTCMAP','W')
        GOTO 999
      ENDIF
C
      IF ( NCLUSTERS .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CMAPFL',
     &    'NCLUSTERS is ZERO in CMAP','W')
        GOTO 999
      ENDIF
C
C ****  Initialize reference links
C
      NS = IQ(LCMAP-2)                  ! Number of structural links
      IBANK = LBANK                     ! First pre-cluster bank
      JJ = 0
      DO WHILE (JJ .LT. NCLUSTERS .AND. IBANK .GT. 0)
        JJ = JJ + 1
        LQ(LCMAP-NS-JJ) = IBANK
        IBANK = LQ(IBANK)               ! Next pre-cluster bank
      ENDDO
C
C ****  Check for pre-cluster count mis-match
C
      IF ( JJ .NE. NCLUSTERS .AND. IBANK .NE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CMAPFL',
     &    'Mis-match in pre-cluster count in CMAP','W')
        GOTO 999
      ENDIF
C
C ****  Initialize CLASS, NEXT words
C
      CALL CONNECT_INIT (IQ(LCMAP+1),IREPEAT,ICLASS,INEXT,1,NCLUSTERS)
C
  999 RETURN
      END
