      SUBROUTINE TB90_CALOR_READ_PEDGNS(RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in PEDS and PULSER GAINS for
C-                         TB90_CALOR_UNPACK package
C-
C-   Inputs  : RCP_BANK [C] RCP bank that controls CAD1 unpacking
C-   Outputs : none
C-   Controls: none
C-
C-   Created  29-JUN-1990   Chip Stewart, Marcel Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_BANK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      CHARACTER*80  FILENAM1, FILENAM2, STRING
      INTEGER I,J,K,M,N,L,IER,LSTRING,N1,N2,TASK,LSTRING1,LSTRING2
      INTEGER NCRT,CRATES(15),LCPD1,GZCPDH,GZCPD1,LCPD11,LCPDH1
      LOGICAL STATUS,TRNLNM,FIRST
      LOGICAL DO_PEDSUB,DO_GNSCOR,DO_PEDCOR,INSPILL
      DATA DO_PEDSUB, DO_GNSCOR,DO_PEDCOR /3*.TRUE./
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF(.NOT. FIRST) RETURN
      FIRST = .FALSE.
      CALL EZPICK(RCP_BANK)
      CALL EZ_GET_CHARS('PEDESTAL_FILE',N1,FILENAM1,IER)
      CALL EZ_GET_CHARS('GAINS_FILE',N2,FILENAM2,IER)
      CALL EZGET('DO_PEDSUB',DO_PEDSUB,IER)
      CALL EZGET('DO_GNSCOR',DO_GNSCOR,IER)
      CALL EZGET('DO_PEDCOR',DO_PEDCOR,IER)
      CALL EZGETA ('CRATES',0,0,0,NCRT,IER)
      CALL EZGET('CRATES',CRATES(1),IER)
      CALL EZRSET
      IF (IER.NE.0)  GOTO 999
      IF (N1.EQ.0 )  FILENAM1 = 'DBL3'
      IF (N2.EQ.0 )  FILENAM2 = 'DBL3'
      IF(DO_PEDSUB) TASK = 1
      IF(DO_GNSCOR) TASK = 3
      IF(DO_PEDSUB .AND. DO_GNSCOR) TASK = 0
      IF (TASK.EQ.0 .AND. FILENAM1 .NE. FILENAM2 )
     & CALL ERRMSG('DIFFPEDGNS','TB90_CALOR_READ_PEDGNS',
     &             'Cant handle different ped and gain file','W')
C
C
C ****  Translate filename name; could be a logical
C
      CALL WORD (FILENAM1,I,J,L)
      STATUS = TRNLNM(FILENAM1(I:J),STRING,LSTRING1)
      IF ( STATUS ) THEN
        FILENAM1 = STRING
      ELSE
        LSTRING1  = L
      ENDIF
      CALL WORD (FILENAM2,I,J,L)
      STATUS = TRNLNM(FILENAM2(I:J),STRING,LSTRING2)
      IF ( STATUS ) THEN
        FILENAM2 = STRING
      ELSE
        LSTRING2  = L
      ENDIF
      IF(DO_PEDSUB) THEN
        CALL INTMSG(' Pedestal file to be read is: ')
        CALL INTMSG('      ---> '//FILENAM1(1:LSTRING1))
      ENDIF
      IF(DO_GNSCOR) THEN
        CALL INTMSG(' Gains file to be read is: ')
        CALL INTMSG('      ---> '//FILENAM2(1:LSTRING2))
      ENDIF
C
      CALL CALOR_READ_PEDGNS(FILENAM1,FILENAM2,TASK,
     &  NCRT,CRATES,IER)
C
C ****  check for INSPILL peds and copy x1 peds from DBL3
C
      INSPILL = .FALSE.
      IF(FILENAM1(1:4).NE.'DBL3') THEN
        LCPD1 = GZCPD1 ()
        IF( LCPD1 .EQ. 0) THEN
          CALL INTMSG(' No x1 PEDS in '//FILENAM1(1:LSTRING1))
          INSPILL = .TRUE.
          LCPDH = GZCPDH ()
          LCPDH1 = LC(LCPDH)
          IF(LCPDH1.GT.0) THEN
            LCPD11 = LC(LCPDH1-IZCPD1)
            IF(LCPD11.GT.0) THEN
              CALL ZSHUNT(IXSTP,LCPD11,LCPDH,-IZCPD1,1)
              CALL INTMSG(' Use DLB3 x1 PEDS')
            END IF
          END IF
        END IF
      END IF
      IF(DO_PEDSUB) CALL FLGSET('PEDS_READ',IER.EQ.0)
      IF(DO_GNSCOR) CALL FLGSET('GNS_READ',IER.EQ.0)
      IF( DO_PEDCOR .AND. .NOT. INSPILL) CALL TB90_CPD8_CORRECT
C
  999 RETURN
      END
