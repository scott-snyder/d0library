      SUBROUTINE CACL_SCAN(ETMIN,NJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Count CACL pre-clusters as JETS with Et>ETMIN.
C-   Use the Cluster map bank CMAP to map pre-clusters into jets via the
C-   CLASS and NEXT words in CMAP. Based on CACL_TO_JETS
C-
C-   Inputs  : ETMIN    [R]     Minimum JET Et
C-   Outputs : NJETS    [I]     Number of Jets found
C-   Controls: NONE
C-
C-   Created  7-JUNE-1993   Chip Stewart
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL    ETMIN
      INTEGER NJETS
C
      INTEGER IVERS,NCACL,IREPEAT,ICLASS,INEXT,LCLASS,LNEXT,ICLUST
C
      INTEGER NJET_MAX,CELLS_MAX
      PARAMETER (NJET_MAX=20)
      PARAMETER (CELLS_MAX=2500)
C
      INTEGER GZCACL,GZCMAP
C
      INTEGER ICACL,JCACL,KCACL,I,J,K,L,IER
C
      INTEGER CLASS,NEXT,CATW
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Link area
      INCLUDE 'D0$PARAMS:CAPH.DEF'
C
      INTEGER POINT, NREP, II
      REAL E(4), ET 
C
      LOGICAL LOOP
C----------------------------------------------------------------------
C
C ****  Define statement functions class,next
C
      CATW(J)  = LCMAP + (J-1)*IREPEAT
      CLASS(J) = CATW(J)+ICLASS
      NEXT(J)  = CATW(J)+INEXT
C----------------------------------------------------------------------
C
      NJETS = 0
C
C ****  GET LINK TO START OF CACL BANKs.
C
      LCACL = GZCACL()
      IF ( LCACL .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &    'NO CACL BANK','W')
        GOTO 999
      END IF
C
C ****  Get address of CMAP
C
      LCMAP = GZCMAP()
      IF ( LCMAP .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &    'NO CMAP BANK','W')
        GOTO 999
      ENDIF
C
C ****  Get number of pre-clusters etc.
C
      CALL GTCMAP_TOTAL (IVERS,NCACL,IREPEAT,ICLASS,INEXT,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &    'Problem in GTCMAP; NO JETS produced','W')
        GOTO 999
      ENDIF
C
C ****  LOOP OVER ALL CACL BANKS IN CMAP
C
      DO 100 ICACL = 1, NCACL
C
C ****  Loop over class starting at the element with next equal to
C       class.  Leave inner loop when that element is re-encountered.
C
        IF( IQ(CLASS(ICACL)) .NE. IQ(NEXT(ICACL)) ) GOTO 100
        JCACL = ICACL
C
C ****   NEW JET
C
        NJETS = NJETS + 1
        CALL VZERO(E(1),4)
C
C *************************************
C ****  LOOP OVER a class of CACL banks
C *************************************
C
        LOOP = .TRUE.
        DO WHILE ( LOOP )
C
C ****  Get address of CACL bank JCACL from CMAP reference link
C
          CALL GTCMAP (JCACL,LCLASS,LNEXT,LCACL,IER)
C
          IF ( LCACL .LE. 0 ) THEN
            CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &        'GTCMAP Returns LCACL = 0','W')
            GOTO 100
          END IF
C
C ****  ADD EX,EY,EZ,E TO BUILD JET
C
          DO 19, I = 1, 2
   19     E(I) = E(I) + Q(LCACL+ 3 + I)
          JCACL = IQ(NEXT(JCACL))
          LOOP  = IQ(NEXT(JCACL)) .NE. IQ(CLASS(JCACL))
        END DO
C
C ****  APPLY ETMIN CUT TO JET
C
        ET = SQRT( E(1)*E(1) + E(2)*E(2))
        IF ( ET.LT.ETMIN ) NJETS = NJETS - 1
  100 CONTINUE
C
  999 RETURN
      END
