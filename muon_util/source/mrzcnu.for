      SUBROUTINE MRZCNU(UNIT,BANK,MODULE,TREE,READOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read a single MUON constants bank from
C-                         UNIT, and hang it in the proper place in
C-                         the STPx tree.
C-
C-   Inputs  : UNIT                     ! Logical unit to read from
C-             TREE                     ! name of the tree STPN/STPO/STPC
C-   Outputs : BANK                     ! name of the bank
C-             MODULE                   ! number of the muon module
C-             READOK                   ! .TRUE. if successful
C-   Controls: 
C-
C-   Created  11-APR-1989   
C-            J.Green
C-            J.Green  7-JUN-89  Add option to read bank MGEH
C-            J.Green 27-JUL-89  Add option to read bank SMUO
C-            J.Green  MAY-91    Add opts MPDH, MGNH, MTMH, MDTH
C-                                        MBDH, MBAD
C-            J.Green  SEP-91    Change to overwrite old data without
C-                               user prompt.
C-            J.Green OCT-93     add SCINT type
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMPDH.LINK'
      INCLUDE 'D0$LINKS:IZMGNH.LINK'
      INCLUDE 'D0$LINKS:IZMTMH.LINK'
      INCLUDE 'D0$LINKS:IZMDTH.LINK'
      INCLUDE 'D0$LINKS:IZMGEH.LINK'
      INCLUDE 'D0$LINKS:IZMMAH.LINK'
      INCLUDE 'D0$LINKS:IZMSRH.LINK'
      INCLUDE 'D0$LINKS:IZMBHD.LINK'
      INCLUDE 'D0$LINKS:IZMSTH.LINK'
      INTEGER      UNIT
      CHARACTER*4  BANK
      CHARACTER*4  TREE
      CHARACTER*1  CHYN                 ! for answering Y/N question
      INTEGER      MODULE
      LOGICAL      READOK
      INTEGER      NMUON                ! /307/
      INTEGER      LSUP                 ! address of supporting bank
      INTEGER      LSTEMP               ! address of Temporary bank
      INTEGER      LTARG                ! contents of target address 
      INTEGER      IBANK                ! Hollereth bank name
C      LOGICAL*1    LBANK(4)
C      EQUIVALENCE  (LBANK,IBANK)
      INTEGER      ITREE                ! 1/2/3   for   STPO/STPC/STPN
      INTEGER      IERR                 ! = 0 if OK
      INTEGER      IOSTAT
C      COMMON   /LATEMP/ LATEMP(2),LSTEMP  ! for temporary link area
C      INTEGER      LATEMP, LSTEMP
      LOGICAL      LFIRST               ! /.TRUE./   first call
      INTEGER      LSTPX
      INTEGER      LMDTH
      INTEGER      LMMAH
      INTEGER      LMSRH
      INTEGER      LMBHD
      INTEGER      LMSTH
      CHARACTER*60 MSGSTR               ! for INTMSG
      DATA         NMUON    /307/
      DATA         LFIRST   /.TRUE./
C----------------------------------------------------------------------
      READOK = .FALSE.                  ! assume failure
C
      IF     (TREE .EQ. 'STPO') THEN
        ITREE = 1
      ELSEIF (TREE .EQ. 'STPC') THEN
        ITREE = 2
      ELSE
        ITREE = 3
      ENDIF
C
C                                        ! read into stand-alone ZEBRA
      CALL FZIN(UNIT,IDVSTP,LSTEMP,2,' ',0,0)
      IF (IQUEST(1).LT.0) THEN
        WRITE (MSGSTR,1001) IQUEST(1)
 1001   FORMAT (' ZEBRA error on FZIN, IQUEST(1)= ',I4)
        CALL INTMSG(MSGSTR)
        GO TO 999
      ELSEIF(IQUEST(14).EQ.0) THEN
        CALL INTMSG(' Empty Data Structure read in ')
        GO TO 999
      ENDIF
C                                        ! find MODULE and BANK
      IBANK = IC(LSTEMP-4)
C      BANK =
C     &  CHAR(LBANK(1))//CHAR(LBANK(2))//CHAR(LBANK(3))//CHAR(LBANK(4))
      CALL UHTOC(IBANK,4,BANK,4)
      IF (BANK.EQ.'MPED' .OR. BANK.EQ.'MGAN' .OR. BANK.EQ.'MTIM' .OR.
     &    BANK.EQ.'MGEO' .OR. BANK.EQ.'MDTM' .OR. BANK.EQ.'MBAD' .OR.
     &    BANK.EQ. 'MSTC' ) THEN
        MODULE = IC(LSTEMP+9)
      ELSEIF (BANK.EQ.'MPDH') THEN
        MODULE = 1
      ELSEIF (BANK.EQ.'MSOP' .OR. BANK.EQ.'MMAG' ) THEN
        MODULE = 1
      ELSEIF (BANK.EQ.'MGNH' .OR. BANK.EQ.'SMUO') THEN
        MODULE = 2
      ELSEIF (BANK.EQ.'MSAU' .OR. BANK.EQ.'MMAP') THEN
        MODULE = 2
      ELSEIF (BANK.EQ.'MTMH') THEN
        MODULE = 3
      ELSEIF (BANK.EQ.'MGEH') THEN
        MODULE = 4 
      ELSEIF (BANK.EQ.'MSRH') THEN
        MODULE = 5
      ELSEIF (BANK.EQ.'MMAH') THEN
        MODULE = 6
      ELSEIF (BANK.EQ.'MDTH') THEN
        MODULE = 7
      ELSEIF (BANK.EQ.'MBHD') THEN
        MODULE = 8
      ELSEIF (BANK.EQ.'MSTH') THEN
        MODULE = 11
      ELSE
        WRITE (MSGSTR,1003) BANK
 1003   FORMAT (' Do not understand BANK ',A4)
        CALL INTMSG(MSGSTR)
        GO TO 999
      ENDIF
C                                       ! find ( or create) proper place
C                                       ! under STPx
      LSTPX = LC(LSTPH-ITREE)
      IF (LSTPX.EQ.0) THEN
        CALL MZBOOK(IDVSTP,LSTPX,LSTPH,-ITREE,'TREE',8,8,10,2,0)
        WRITE (MSGSTR,1012) TREE
 1012   FORMAT(' Created bank ',A4)
        CALL INTMSG(MSGSTR)
      ENDIF
      IF ( BANK .NE. 'SMUO' ) THEN
        LSMUO = LC(LSTPX-IZSMUO)
        IF (LSMUO.EQ.0) THEN
          CALL MZBOOK(IDVSTP,LSMUO,LSTPX,-IZSMUO,'SMUO',11,11,10,1,0)
          CALL INTMSG(' Created bank SMUO ')
        ENDIF                            
      ENDIF
C
      IF     (BANK.EQ.'MPED') THEN
        LMPDH = LC(LSMUO-IZMPDH)
        IF (LMPDH.EQ.0) THEN
          CALL MZBOOK
     &      (IDVSTP,LMPDH,LSMUO,-IZMPDH,'MPDH',NMUON,NMUON,10,1,0)
        CALL INTMSG(' Created bank MPDH ')
        ENDIF
        LSUP = LMPDH
      ELSEIF (BANK.EQ.'MGAN') THEN
        LMGNH = LC(LSMUO-IZMGNH)
        IF (LMGNH.EQ.0) THEN
          CALL MZBOOK
     &      (IDVSTP,LMGNH,LSMUO,-IZMGNH,'MGNH',NMUON,NMUON,10,1,0)
        CALL INTMSG(' Created bank MGNH ')
        ENDIF
        LSUP = LMGNH
      ELSEIF (BANK.EQ.'MTIM') THEN
        LMTMH = LC(LSMUO-IZMTMH)
        IF (LMTMH.EQ.0) THEN
          CALL MZBOOK
     &      (IDVSTP,LMTMH,LSMUO,-IZMTMH,'MTMH',NMUON,NMUON,10,1,0)
        CALL INTMSG(' Created bank MTMH ')
        ENDIF
        LSUP = LMTMH
      ELSEIF (BANK.EQ.'MDTM') THEN
        LMDTH = LC(LSMUO-IZMDTH)
        IF (LMDTH.EQ.0) THEN
          CALL MZBOOK
     &      (IDVSTP,LMDTH,LSMUO,-IZMDTH,'MDTH',NMUON,NMUON,10,1,0)
          CALL INTMSG(' Created bank MDTH ')
        ENDIF
        LSUP = LMDTH
      ELSEIF (BANK.EQ.'MGEO') THEN
        LMGEH = LC(LSMUO-IZMGEH)
        IF (LMGEH.EQ.0) THEN
          CALL MZBOOK
     &      (IDVSTP,LMGEH,LSMUO,-IZMGEH,'MGEH',NMUON,NMUON,10,1,0)
          CALL INTMSG(' Created bank MGEH ')
        ENDIF
        LSUP = LMGEH
      ELSEIF (BANK.EQ.'MBAD') THEN
        LMBHD = LC(LSMUO-IZMBHD)
        IF (LMBHD.EQ.0) THEN
          CALL MZBOOK
     &      (IDVSTP,LMBHD,LSMUO,-IZMBHD,'MBHD',NMUON,NMUON,10,1,0)
          CALL INTMSG(' Created bank MBHD ')
        ENDIF
        LSUP = LMBHD
      ELSEIF (BANK.EQ.'MMAP' .OR. BANK.EQ.'MMAG') THEN
        LMMAH = LC(LSMUO-IZMMAH)        ! 
        IF (LMMAH.EQ.0) THEN
          CALL MZBOOK (IDVSTP,LMMAH,LSMUO,-IZMMAH,'MMAH',2,2,10,1,0)
          CALL INTMSG(' Created bank MMAH ')
        ENDIF
        LSUP = LMMAH
      ELSEIF (BANK.EQ.'MSAU' .OR. BANK.EQ.'MSOP') THEN
        LMSRH = LC(LSMUO-IZMSRH)        ! 
        IF (LMSRH.EQ.0) THEN
          CALL MZBOOK (IDVSTP,LMSRH,LSMUO,-IZMSRH,'MSRH',2,2,10,1,0)
          CALL INTMSG(' Created bank MSRH ')
        ENDIF
        LSUP = LMSRH
      ELSEIF (BANK.EQ.'MSTC') THEN
        LMSTH = LC(LSMUO-IZMSTH)
        IF (LMSTH.EQ.0) THEN
          CALL MZBOOK
     &      (IDVSTP,LMSTH,LSMUO,-IZMSTH,'MSTH',NMUON,NMUON,10,1,0)
          CALL INTMSG('Created bank MSTH ')
        ENDIF
        LSUP = LMSTH
      ELSEIF (BANK.EQ.'MPDH') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MGNH') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MTMH') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MGEH') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MSRH') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MMAH') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MDTH') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MBHD') THEN
        LSUP = LSMUO
      ELSEIF (BANK.EQ.'MSTH') THEN
        LSUP = LSMUO
      ELSEIF ( BANK.EQ.'SMUO') THEN
        LSUP = LSTPX
      ENDIF                             ! branch on bank name
C                                      ! move the bank to proper place
      LTARG = LC(LSUP-MODULE)
      IF (LTARG.NE.0) THEN              ! Something already there
        WRITE ( MSGSTR,1000) BANK,MODULE
 1000   FORMAT ( ' MRZCNU: Overwriting bank ',A,' for module ',I4)
        CALL MZDROP (IXSTP, LTARG, ' ')       ! drop old bank
      ENDIF
      CALL ZSHUNT(IXSTP,LSTEMP,LSUP,-MODULE,1)
C
      READOK = .TRUE.
C
  999 RETURN
      END
