C VAX/DEC CMS REPLACEMENT HISTORY, Element JUTLFL.FOR
C *5    29-MAR-1994 20:15:09 ASTUR "PROD_OMNI_FILTER"
C *4    29-MAR-1994 12:14:45 ASTUR "PROD_OMNI_FILTER"
C *3    24-FEB-1994 18:42:45 ASTUR "Needed for UDST maker "
C *2    18-NOV-1993 15:45:19 ASTUR "Richard V. Astur: Portability fixes"
C *1    13-SEP-1993 11:51:25 ASTUR "QCD Group: First Release"
C VAX/DEC CMS REPLACEMENT HISTORY, Element JUTLFL.FOR
      SUBROUTINE JUTLFL( D0MDST )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill a JUTL bank. Drop 17 words if
C-                         this is for D0 MDST.
C-
C-   Inputs  : D0MDST [L] .TRUE. if this is a D0MDST
C-   Outputs :
C-   Controls:  DOMDST controls how this bank is made
C-
C-   Created   5-DEC-1992   Andrew J. Milder
C-   Expanded 15-DEC-1992   Richard Astur
C-   Modified 7-FEB-1994    R. Astur "Update for D0 MDST"
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$LINKS:IZJTCS.LINK'
      INTEGER  GZPROC, LJTCS, NADD, LPROC, LCAPH, IPOINT, GZCAPH, LZFIDH
      EXTERNAL GZPROC, GZCAPH, LZFIDH
      INTEGER LJUTL, QCD_POINT, IER
      REAL CONEU
      LOGICAL   D0MDST                  ! Are we doing the D0 mdst or QCD?
C: QCD MASK
      INTEGER QCD_MASK, I, GZTRGR
C: GLOB
      INTEGER GZGLOB, LGLOB
C: VERT
      INTEGER NVER
      REAL ZVER(3,3)
C: L0
      INTEGER MI_FLAG               ! Multiple interaction flag
      INTEGER IFAST, LPLV0, GZPLV0
      INTEGER MI_TOOL, MULTIPLE_INTERACTION_TOOL
      EXTERNAL MULTIPLE_INTERACTION_TOOL
      LOGICAL OK, FAST_FLAG, SLOW_FLAG
      REAL FASTZ, SLOWZ
C----------------------------------------------------------------------
C
      QCD_POINT = 0
C
C: Book JUTL and hang
C
      IF ( D0MDST ) THEN
        CALL BKJUTL(LJUTL,1)                        ! Hang on -1 link
      ELSE
        CALL BKJUTL(LJUTL,3)
      ENDIF
C
C: If d0 mdst, drop words, change version number
C
      IF ( D0MDST ) THEN
        IQ( LJUTL + 1 ) = IQ( LJUTL + 1) + 1        ! Increase version number
        CALL MZPUSH( IXCOM, LJUTL, 0, -17, 'I' )    ! drop these words
      ENDIF
C
      QCD_POINT = QCD_POINT + 1           ! Version number filled by BKJUTL
C
C: QCD filter mask (now there are two of them! vers=4,5)
C
      CALL QCD_GET_MASK(  QCD_MASK )            ! Mask 1
      IQ( LJUTL + QCD_POINT + 1) = QCD_MASK
      QCD_POINT = QCD_POINT + 1
      CALL QCD_GET_MASK2( QCD_MASK )
      IQ( LJUTL + QCD_POINT + 1) = QCD_MASK     ! Mask 2
      QCD_POINT = QCD_POINT + 1

      IF ( .NOT. D0MDST ) THEN
C
C: Event quality flag from GLOB
C
C        IQ( LJUTL + QCD_POINT + 1 ) = -1   ! Not found yet
C        LGLOB = GZGLOB()
C        IF ( LGLOB .GT. 0 ) IQ( LJUTL + QCD_POINT + 1) = IQ( LGLOB + 2
C     &      )
C        QCD_POINT = QCD_POINT + 1
C
C: L0 information. If TRGR is present, we can get it all, else we cannot.
C: Preset numbers to -999.
        DO I = 1, 5
          Q( LJUTL + QCD_POINT + I ) = -999.
        ENDDO
C
C: Ideally, we get this info from PLV0 bank
C
        CALL GTPLV0_ZONLY(FASTZ, FAST_FLAG, SLOWZ, SLOW_FLAG, MI_FLAG
     &      )
        Q( LJUTL + QCD_POINT + 1 ) = SLOWZ
        IF ( SLOW_FLAG ) THEN
          Q( LJUTL + QCD_POINT + 2 ) = 1   ! 1=good 0=bad
        ELSE
          Q( LJUTL + QCD_POINT + 2 ) = 0
        ENDIF

        Q( LJUTL + QCD_POINT + 3 ) = MI_FLAG
        Q( LJUTL + QCD_POINT + 4 ) = FASTZ
        IF ( FAST_FLAG ) THEN
          Q( LJUTL + QCD_POINT + 5  )= 1
        ELSE
          Q( LJUTL + QCD_POINT + 5  )= 0
        ENDIF
C
C: If FAST_FLAG is not good, then we may not have been able to find the
C: PLV0 bank. Settle for fast z info from L0VT.
        IF ( .NOT. FAST_FLAG ) THEN
          CALL GTL0VT( FASTZ, IFAST, SLOWZ, MI_FLAG, OK )
          IF ( OK ) THEN
            IF ( MI_FLAG .GT. 0 ) THEN
              Q( LJUTL + QCD_POINT + 1 ) = SLOWZ
              Q( LJUTL + QCD_POINT + 2 ) = 1
            ENDIF
            Q( LJUTL + QCD_POINT + 3 ) = MI_FLAG
            Q( LJUTL + QCD_POINT + 4 ) = FASTZ
            IF ( IFAST .EQ. 1 ) THEN
              Q( LJUTL + QCD_POINT + 5  )= 1
            ELSE
              Q( LJUTL + QCD_POINT + 5 ) = 0
            ENDIF
          ENDIF
        ENDIF
        QCD_POINT = QCD_POINT + 5
C
C: Take the first 3 vertices. If no vertex is there, set it to -999.
C
C        CALL ZVERTE(NVER, ZVER, DZVER )
        CALL VERTEX_INFO(3, NVER, ZVER, OK )
        IF (.NOT. OK) NVER = 0
        DO I = 1, MIN( NVER, 3 )
          Q( LJUTL + QCD_POINT + 1 ) = ZVER( 1,I)
          Q( LJUTL + QCD_POINT + 2 ) = ZVER( 2,I)
          QCD_POINT = QCD_POINT + 2
        ENDDO
C: Fill the rest with -999.
        DO I = MIN( NVER, 3 ) + 1, 3
          Q( LJUTL + QCD_POINT + 1 ) = -999.
          Q( LJUTL + QCD_POINT + 2 ) = -999.
          QCD_POINT = QCD_POINT + 2
        ENDDO
C
C: Get PLV0 time sigma word
C
        LPLV0 = GZPLV0()
        IF ( LPLV0 .GT. 0 ) Q( LJUTL + QCD_POINT + 1 ) = Q( LPLV0 + 4)
        QCD_POINT = QCD_POINT + 1
C
C: Get Multiple interaction tool word
C
        Q( LJUTL + QCD_POINT + 1 ) = MULTIPLE_INTERACTION_TOOL()
        QCD_POINT = QCD_POINT + 1

C
C: The rest are spares.
C
      ENDIF
C
C: Fill end of bank with a list of L1 and L2 jets found. This routine will
C: Size the bank properly to its needs
C
      CALL QCD_FILTER_TRIGGER(LJUTL)
C
C
C: If d0 mdst, drop 6 end words, but add JTCS bank
C
      IF ( D0MDST ) THEN

        LPROC = GZPROC()
        IF ( LPROC .GT. 0 ) LCAPH = LQ( LPROC - IZCAPH )
        IF ( LCAPH .LE. 0 ) THEN
          CALL ERRMSG('No CAPH','JUTLFL','Cant find CAPH bank','W')
          RETURN
        ENDIF
        LJTCS = 0
C        LJTCS = LZFIDH(IXMAIN, 'JTCS', LHEAD )   ! Find JTCS
        DO WHILE ( LCAPH .GT. 0 .AND. LJTCS .EQ. 0 )
          LJTCS = LQ(LCAPH - IZJTCS )
          LCAPH = LQ( LCAPH )
        ENDDO
        IF ( LJTCS .GT. 0 ) THEN
          LCAPH = LQ( LJTCS + 1 )              ! Find CAPH it hangs from
          CONEU = Q( LCAPH + 6 )               ! Conesize of this JTCS
          NADD  = IQ( LJTCS - 1 ) - 1          ! # of data words in JTCS - 1
          CALL MZPUSH( IXCOM, LJUTL, 0, NADD-6, 'I' )  ! Have 6 spares
          IPOINT = IQ( LJUTL - 1) - NADD
          LCAPH  = GZCAPH()
C          LJTCS = LZFIDH(IXMAIN, 'JTCS', LHEAD )   ! Find JTCS
          DO WHILE ( LCAPH .GT. 0 .AND. LJTCS .EQ. 0 )
            LJTCS = LQ(LCAPH - IZJTCS )
            LCAPH = LQ( LCAPH )
          ENDDO
          IF ( LJTCS .LE. 0 ) THEN
            CALL ERRMSG('LOST JTCS','JUTLFL',' ','F')
            RETURN
          ENDIF
          CALL UCOPY( Q(LJTCS+2), Q(LJUTL+IPOINT+1), NADD )
          Q(LJUTL+IPOINT+1) = IQ(LJUTL+IPOINT+1)     ! Repetition number
          Q(LJUTL+IPOINT+2) = IQ(LJUTL+IPOINT+2)     ! # of jets
        ELSE
          CALL MZPUSH( IXCOM, LJUTL, 0, -6, 'I' )  ! Have 6 spares
          CALL ERRMSG('No JTCS','JUTLFL','couldnt find JTCS','W')
        ENDIF
      ENDIF
  999 RETURN
      END
