C DEC/CMS REPLACEMENT HISTORY, Element DROP_BANKS.FOR
C *1     7-FEB-1990 23:02:18 STEWART "DROP_BANKS ASSOCIATED ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element DROP_BANKS.FOR
      SUBROUTINE DROP_BANKS ( IXDIV, MAXDEPTH, NBRANCH,
     &  IZLINK, NDEPTH, LSTART, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DROP a specified section of a ZEBRA TREE of banks,
C-   starting at the bank with address LSTART.
C-
C-   Inputs  : IXDIV    [I]     Division number (Unused as yet)
C-             MAXDEPTH [I]     Maximum number of depths in TREE
C-             NBRANCH  [I]     Number of Branches to drop
C-             IZLINK(MAXDEPTH,*) [I] link offsets of banks to drop
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
       INTEGER II,JJ,KK,NS,LBANK,IBANK,NSUP,NDROP,MXLINKS,MXDROPS
C
       PARAMETER( MXLINKS= 50 )          ! Maximum number of links/bank
       PARAMETER( MXDROPS= 50 )          ! Maximum number of drops
       INTEGER DLINKS(MXLINKS,MXDROPS),DZERO(MXDROPS)
       CHARACTER*4 BANK(MXLINKS)
       INTEGER NDLINKS(MXDROPS)
       INTEGER I,N,DEPTH,J,K,LCHAIN,LCPATH,IDEP,LSUP,NLDROP
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
       IF (FIRST) THEN
         FIRST = .FALSE.
         NDROP = 0
         DO II = 1,  NBRANCH
           LBANK = LCHAIN(LSTART,IZLINK(1,II),NDEPTH(II))
           IF ( LBANK .GT. 0 ) THEN
   10        NDROP = NDROP + 1
             CALL ZCHAIN(IXDIV,LSTART,LBANK,
     &        MXLINKS,BANK,DLINKS(1,NDROP),NDLINKS(NDROP),IER)
             CALL DHTOC (4,IQ(LBANK-4),BANK(NDLINKS(NDROP)+1))
             WRITE(6,120) (K,BANK(K),DLINKS(K,NDROP),
     &        K=1,NDLINKS(NDROP)),K,BANK(NDLINKS(NDROP)+1)
C
C ****  check for more BANKS to DROP under other paths (linear chains...)
C ****  Look for linear chain closest to banks to be dropped
C
             DZERO(NDROP) = 0
             DO IDEP = NDLINKS(NDROP)-1, 2, -1
               LBANK = LQ(LBANK+1)
               IF (LQ(LBANK).GT.0) THEN
                 DZERO(NDROP) = IDEP    !note depth of linear chain
                 NLDROP = 1
                 LSUP = LCHAIN(LSTART,DLINKS(1,NDROP),DZERO(NDROP))
                 LSUP = LQ(LSUP)
                 DO WHILE (LSUP.GT.0)
                   LBANK=LCHAIN(LSUP,DLINKS(DZERO(NDROP)+1,NDROP),
     &               NDLINKS(NDROP)-DZERO(NDROP))
                   IF (LBANK.GT.0) NLDROP = NLDROP + 1
                   LSUP = LQ(LSUP)
                 END DO
                 GOTO 799
               END IF
             END DO
  799        CONTINUE
             IF(NLDROP.GT.0) WRITE(6,130) NLDROP,BANK(DZERO(NDROP)+1) 
           ENDIF
         ENDDO
       END IF
C
C ****  DROP banks
C
       DO I = 1, NDROP
         LBANK = LCHAIN (LSTART,DLINKS(1,I),NDLINKS(I))
         CALL MZDROP(IXMAIN, LBANK, 'L') ! Bank dropped
C
C ****  DROP other banks in linear chain closest to LBANK
C
         IF ( DZERO(I).GT.0) THEN
           LSUP = LCHAIN(LSTART,DLINKS(1,I),DZERO(I))
           LSUP = LQ(LSUP)
           DO WHILE (LSUP.GT.0)
             LBANK=LCHAIN(LSUP,DLINKS(DZERO(I)+1,I),NDLINKS(I)-DZERO(I))
             IF (LBANK.GT.0) CALL MZDROP(IXMAIN, LBANK, 'L') ! Bank dropped               
             LSUP = LQ(LSUP)
           END DO
         END IF
       END DO
  999  RETURN
  120  FORMAT(1X,' BANKS TO DROP ',20(/1X,I4,5X,A4,5X,I4))
  130  FORMAT(1X,I7,' BANKS IN LINEAR CHAIN TO DROP UNDER - ',A4)
       END
