C DEC/CMS REPLACEMENT HISTORY, Element KEEP_BANKS.FOR
C *1     7-FEB-1990 23:02:34 STEWART "DROP_BANKS ASSOCIATED ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element KEEP_BANKS.FOR
      SUBROUTINE KEEP_BANKS ( IXDIV, MAXDEPTH, NBRANCH,
     &  IZLINK, NDEPTH, LSTART, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : KEEP a specified section of a ZEBRA TREE of banks,
C-   starting at the bank with address LSTART.
C-
C-   Inputs  : IXDIV    [I]     Division number (Unused as yet)
C-             MAXDEPTH [I]     Maximum number of depths in TREE
C-             NBRANCH  [I]     Number of Branches to keep
C-             IZLINK(MAXDEPTH,*) [I] link offsets of banks to keep
C-             NDEPTH(*)[I]     The number of Depths per branch
C-             LSTART   [I]     Address of bank at which to
C-                              start tree scan.
C-             IER      [I]     0 if routine successful
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  30-JAN-1990   Harrison B. Prosper, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IXDIV
      INTEGER MAXDEPTH,NBRANCH
      INTEGER IZLINK(MAXDEPTH,*)
      INTEGER NDEPTH(*)
      INTEGER LSTART
      INTEGER IER
C
      INTEGER II,JJ,KK,LL,JBASE,KBASE,NS,LBANK,IBANK,NSUP,NDROP
      LOGICAL DROP,BRANCH_END
C
      INTEGER OFFSET
      PARAMETER( OFFSET = 100 ) ! Maximum number of KEPT banks/depth
      INTEGER MXLINKS
      PARAMETER( MXLINKS= 50 )          ! Maximum number of links/bank
      INTEGER MXDROPS
      PARAMETER( MXDROPS= 50 )          ! Maximum number of links/bank
      INTEGER DLINKS(MXLINKS,MXDROPS)
      INTEGER LSUPP(2*OFFSET),KEEP(2*OFFSET,0:MXLINKS)
      CHARACTER*4 BANK(MXLINKS)
      INTEGER NDLINKS(MXDROPS)
      INTEGER I,N,DEPTH,J,K,LCHAIN
      LOGICAL FIRST
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  IF LSTART .LE. 0 USE LHEAD
C
      IF (LSTART.LE.0) LSTART = LHEAD
C
C ****  IF FIRST CALL THEN CONSTRUCT DROP_LINK LISTS - 
C
      IF (.NOT.FIRST) GOTO 899 
      FIRST = .FALSE.
C
C ****  Initialize search
C
      DEPTH = 0                         ! Depth in tree
      JBASE = 0
      KBASE = OFFSET
      LSUPP(JBASE+1) = LSTART
      NSUP  = 1                         ! Only one bank at this level initially
      NDROP = 0
C
C ****  Initialize links to be kept beneath bank at LSTART
C
      NS = IQ(LSTART-2)                 ! Get number of structural links
      IF ( NS .GT. 0 ) THEN
        DO I =  0,NS
          KEEP(JBASE+1,I) = 0
        ENDDO
        DO I =  1,NBRANCH
          KEEP(JBASE+1,IZLINK(1,I)) = 1
        ENDDO
      ELSE
        IER=-4
        GOTO 999
      ENDIF
C
C ****  Start scanning loop
C
      DO WHILE ( NSUP .GT. 0 )
        DEPTH = DEPTH + 1
C
        KK = 0                          ! Number of banks at next level down
        DO JJ = 1, NSUP                 ! LOOP over support banks
          LL = LSUPP(JBASE+JJ)          ! Get support bank address
          NS = IQ(LL-2)                 ! Get number of structural links
C
          IF ( NS .GT. 0 .AND. NS .LE. MXLINKS ) THEN
C
            DO II = 0,NS                ! LOOP over links
              LBANK = LQ(LL-II)         ! Bank address hanging from link
              IF ( LBANK .GT. 0 ) THEN
C
C ****  DROP logic here. Determine if banks beneath this link
C ****  should be dropped.
C
                CALL ZCHAIN(IXDIV,LHEAD,LBANK,
     &              MXLINKS,BANK,DLINKS(1,NDROP+1),
     &              NDLINKS(NDROP+1),IER)
C
                DROP = KEEP(JBASE+JJ,II) .LE. 0
C
                IF ( DROP ) THEN
                  NDROP = NDROP + 1
                  CALL DHTOC (4,IQ(LBANK-4),BANK(NDLINKS(NDROP+1)))
C                  WRITE(6,120) (K,BANK(K),DLINKS(K,NDROP),
C     &              K=1,NDLINKS(NDROP)),K,BANK(NDLINKS(NDROP+1))
                ELSE
                  IF ( KK .LT. OFFSET ) THEN    ! Check buffer quota
                    KK = KK + 1                 ! Bank NOT FOUND
                    LSUPP(KBASE+KK) = LBANK     ! Note bank address
C
C ****  Note the branches which pass through this bank and
C ****  remember the structural link associated with each branch.
C ****  All branches "I" which pass through current bank will have the
C ****  SAME link offset II for a given depth.
C
                    N = IQ(LBANK-2)     ! Number of structural links
                    IF ( N .GT. 0 ) THEN
                      DO I =  0,N
                        KEEP(KBASE+KK,I) = 0
                      ENDDO
C
                      BRANCH_END = .TRUE.
                      DO I =  1,NBRANCH
                        IF ( DEPTH .LT. NDEPTH(I) ) THEN
                          IF ( IZLINK(DEPTH,I) .EQ. II ) THEN
C
C ****  CANDIDATE LINK FOR KEEP - CHECK THAT LINKS TO HEAD MATCH TOO
C
                            IF (DEPTH.GT.1) THEN
                              DO J = 1, DEPTH - 1
                                IF(IZLINK(J,I).NE.DLINKS(J,NDROP+1))
     &                            GOTO 799
                              END DO
                            END IF
                            KEEP(KBASE+KK,IZLINK(DEPTH+1,I)) = 1
  799                       CONTINUE
                            BRANCH_END = .FALSE.
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDIF
C
C ****  Check if we're at the end of a branch.
C
                    IF ( BRANCH_END ) THEN
                      KK = KK - 1
                    ENDIF
C
                  ELSE
                    IER = -2                    ! Buffer full
                    GOTO 999
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
C ****  Setup for to search next level in tree
C
        NSUP = KK
        IF ( JBASE .GT. 0 ) THEN
          JBASE = 0
          KBASE = OFFSET
        ELSE
          JBASE = OFFSET
          KBASE = 0
        ENDIF
      ENDDO
C
  120 FORMAT(1X,I4,5X,A4,5X,I4)
  899 CONTINUE
      DO I = 1, NDROP
        LBANK = LCHAIN (LSTART,DLINKS(1,I),NDLINKS(I))
        CALL MZDROP(IXMAIN, LBANK, 'L') ! Bank dropped
      END DO
  999 RETURN
      END
