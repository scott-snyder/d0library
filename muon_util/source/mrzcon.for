      SUBROUTINE MRZCON(TP,FILE,MOD,OK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   READS IN MUON  CONSTANT BANK FOR FILE
CCC   type ='PEDS','GAIN','TIME','DTIM','GEOM','ALL','MMAP'
CCC   'ALL' option overwrites whole data structure from SMUO
CCC   'MMAP' reads magnetic field map
CCC   INFO ON FILE file.
CCC   IF NO PROBLEMS THEN ok = .true.
CCC   4/88 ADD: for electronic constants, lowest leav now MPED (etc)
CCC   bank. Now also include MOD
CCC
CCC     HEDIN 12/87
CCC     J.Green 1/89    Add DTIM
CCC     J.Green 6/89    Add 'ALL' option
CCC     S.Kunori 11/89  Add 'CALL INZSTP' for the case that
CCC                     STP header bank does not exist.
CCC     S.Kunori  6/90  quick fix for SMUO link area by
CCC                     calling MZPUSH.
CCC     SDP & AKl 2/91      Add field map
CCC    DH 3/92 use D0OPEN
CCC     A.Taketani 1/94  Add 'GEO','MAG','SCINT', 'ALLGEO' 
CCC                                 option for database
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      CHARACTER*(*) TP,FILE
      CHARACTER*40  MSG, MSG1
      LOGICAL OK,OK2
      INTEGER LIN,USER,ERR,MOD,LNG,I1,I2
      INTEGER IDUMMY
      INTEGER KMPDH,KMTMH,KMGNH,KMDTH,KSMUO,KSTPC
      INTEGER IOS,LOGLEV,TASKNO,N
      INTEGER NS
      INTEGER BKSMUO
      EXTERNAL BKSMUO
      INTEGER  TLSMUO, LMDROP
      INTEGER  GZMSGH,GZMGEH,GZMMAH,GZSMUO
      INTEGER  LSH,JBIAS
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMPDH.LINK'
      INCLUDE 'D0$LINKS:IZMGNH.LINK'
      INCLUDE 'D0$LINKS:IZMTMH.LINK'
      INCLUDE 'D0$LINKS:IZMDTH.LINK'
      INCLUDE 'D0$LINKS:IZMGEH.LINK'
      INCLUDE 'D0$LINKS:IZMMAH.LINK'
      INCLUDE 'D0$LINKS:IZMSGH.LINK'
      DATA LOGLEV/-2/
      DATA N/310/                   ! NUMBER OF MUON MODULE LINKS
      DATA USER/533/                ! RANDOM USER NUMBER
C
      OK=.TRUE.
      CALL GTUNIT(USER,LIN,ERR)
CC         OPEN FILE
      CALL D0OPEN (LIN,FILE,'IU',OK2)
      IF(.NOT.OK2) THEN
        CALL SWORDS (FILE,I1,I2,LNG)
        MSG = ' Can not open file:'//FILE(I1:I2)
        CALL SWORDS (TP,I1,I2,LNG)
        MSG1 = ' TYPE '//TP(I1:I2)
        CALL ERRMSG(MSG,'MRZCON',MSG1,'F')
        CALL RLUNIT(USER,LIN,ERR)
        GO TO 999
      END IF
C
      CALL FZFILE(LIN,0,'I')
      CALL FZLOGL (LIN,LOGLEV)
C
C  CREATE HEADER BANK FOR STP STRUCTURE IF IT DOES NOT EXIST. 
C
      IF(LSTPH.EQ.0) THEN
          CALL INZSTP
      ENDIF
C
C  SEE IF MUON CONSTANT HEADER EXISTS
C
      KSTPC=LC(LSTPH-IZSTPC)
      IF(KSTPC.NE.0) THEN
         KSMUO=LC(KSTPC-IZSMUO)
      ELSE
         KSMUO=0
      ENDIF
C
C      READ IN NEW FILE AND PLACE ON ZEBRA STRUCTURE
C  
      IF (TP .EQ. 'ALL') THEN         ! read whole tree
        IF (KSMUO .NE. 0) THEN
          CALL MZDROP(IXSTP, KSMUO, ' ')        ! delete old tree
        ENDIF
        CALL FZIN (LIN,IDVSTP,KSTPC,-IZSMUO,' ',0,0)
        KSMUO=LC(KSTPC-IZSMUO)
        NS=10-IC(KSMUO-3)
        IF(NS.GT.0) THEN
           CALL MZPUSH(IXSTP,KSMUO,NS,0,' ')
        ENDIF
      ELSE  
C                                       ! read one bank
C
        IF(KSMUO.EQ.0) THEN     ! BOOK MUON HEADER
C          CALL MZBOOK(IDVSTP,KSMUO,KSTPC,-IZSMUO,'SMUO',7,7,10,2,0)
           KSMUO=BKSMUO('STPC',IDUMMY)
        ENDIF
        IF(TP.EQ.'PEDS') THEN
C   SEE IF HEADER; IF NOT CREATE IT
          KMPDH=LC(KSMUO-IZMPDH)
          IF(KMPDH.EQ.0) THEN
            CALL MZBOOK (IDVSTP,KMPDH,KSMUO,-IZMPDH,'MPDH',N,N,10,2,0)
C  
            IC(KMPDH+ 1)= 1210               ! TYPE
            IC(KMPDH+ 2)= 0                  ! STATUS
            IC(KMPDH+ 3)= 100                ! QUALITY
            IC(KMPDH+ 4)= 1                  ! LOWEST RUN VALID
            IC(KMPDH+ 5)= 1                  ! HIGHEST RUN VALID
            IC(KMPDH+ 6)= 1                  ! RUN GENERATED
            IC(KMPDH+ 7)= 21                 ! DATE GENERATED
            IC(KMPDH+ 8)= 14                 ! TIME GENERATED
            IC(KMPDH+ 9)= 0                  ! SPARE
            IC(KMPDH+10)= 0                  ! SPARE
          ENDIF
          CALL FZIN (LIN,IDVSTP,KMPDH,-MOD,' ',0,0)
        ENDIF
        IF(TP.EQ.'GAIN') THEN
C   SEE IF HEADER; IF NOT CREATE IT
          KMGNH=LC(KSMUO-IZMGNH)
          IF(KMGNH.EQ.0) THEN
            CALL MZBOOK (IDVSTP,KMGNH,KSMUO,-IZMGNH,'MGNH',N,N,10,2,0)
C  
            IC(KMGNH+ 1)= 1210               ! TYPE
            IC(KMGNH+ 2)= 0                  ! STATUS
            IC(KMGNH+ 3)= 100                ! QUALITY
            IC(KMGNH+ 4)= 1                  ! LOWEST RUN VALID
            IC(KMGNH+ 5)= 1                  ! HIGHEST RUN VALID
            IC(KMGNH+ 6)= 1                  ! RUN GENERATED
            IC(KMGNH+ 7)= 21                 ! DATE GENERATED
            IC(KMGNH+ 8)= 14                 ! TIME GENERATED
            IC(KMGNH+ 9)= 0                  ! SPARE
            IC(KMGNH+10)= 0                  ! SPARE
          ENDIF
          CALL FZIN (LIN,IDVSTP,KMGNH,-MOD,' ',0,0)
        ENDIF
        IF(TP.EQ.'TIME') THEN
C   SEE IF HEADER; IF NOT CREATE IT
          KMTMH=LC(KSMUO-IZMTMH)
          IF(KMTMH.EQ.0) THEN
            CALL MZBOOK (IDVSTP,KMTMH,KSMUO,-IZMTMH,'MTMH',N,N,10,2,0)
C  
            IC(KMTMH+ 1)= 1210               ! TYPE
            IC(KMTMH+ 2)= 0                  ! STATUS
            IC(KMTMH+ 3)= 100                ! QUALITY
            IC(KMTMH+ 4)= 1                  ! LOWEST RUN VALID
            IC(KMTMH+ 5)= 1                  ! HIGHEST RUN VALID
            IC(KMTMH+ 6)= 1                  ! RUN GENERATED
            IC(KMTMH+ 7)= 21                 ! DATE GENERATED
            IC(KMTMH+ 8)= 14                 ! TIME GENERATED
            IC(KMTMH+ 9)= 0                  ! SPARE
            IC(KMTMH+10)= 0                  ! SPARE
          ENDIF
          CALL FZIN (LIN,IDVSTP,KMTMH,-MOD,' ',0,0)
        ENDIF
        IF(TP.EQ.'DTIM') THEN
C   SEE IF HEADER; IF NOT CREATE IT
          KMDTH=LC(KSMUO-IZMDTH)
          IF(KMDTH.EQ.0) THEN
            CALL MZBOOK (IDVSTP,KMDTH,KSMUO,-IZMDTH,'MDTH',N,N,10,2,0)
C  
            IC(KMDTH+ 1)= 1210               ! TYPE
            IC(KMDTH+ 2)= 0                  ! STATUS
            IC(KMDTH+ 3)= 100                ! QUALITY
            IC(KMDTH+ 4)= 1                  ! LOWEST RUN VALID
            IC(KMDTH+ 5)= 1                  ! HIGHEST RUN VALID
            IC(KMDTH+ 6)= 1                  ! RUN GENERATED
            IC(KMDTH+ 7)= 21                 ! DATE GENERATED
            IC(KMDTH+ 8)= 14                 ! TIME GENERATED
            IC(KMDTH+ 9)= 0                  ! SPARE
            IC(KMDTH+10)= 0                  ! SPARE
          ENDIF
          CALL FZIN (LIN,IDVSTP,KMDTH,-MOD,' ',0,0)
        ENDIF
        IF(TP.EQ.'GEOM') CALL FZIN (LIN,IDVSTP,KSMUO,-IZMGEH,' ',0,0)
        IF(TP.EQ.'MMAP') CALL FZIN (LIN,IDVSTP,KSMUO,-IZMMAH,' ',0,0)
C
C  add by A.T. JAN-94 for database
C
C ------------ GEO, MAG, SCINT -------------------------
C
        IF ( TP.EQ.'GEO' .OR. TP.EQ.'MAG' .OR. TP.EQ.'SCINT' ) THEN
          IF      ( TP.EQ.'GEO' ) THEN    ! drop existing bank
            LMDROP = GZMGEH(0)
            JBIAS = IZMGEH
          ELSE IF ( TP.EQ.'MAG' ) THEN
            LMDROP = GZMMAH(0)
            JBIAS = IZMMAH
          ELSE IF ( TP.EQ.'SCINT' ) THEN
            LMDROP = GZMSGH(0)
            JBIAS = IZMSGH
          END IF
          IF (LMDROP.NE.0) THEN
            CALL MZDROP( IDVSTP,LMDROP,' ' )
          END IF
C
          CALL FZIN( LIN,IDVSTP,TLSMUO,2,' ',0,0) ! Read STP
          KSMUO = GZSMUO('STPC')    ! SMUO pointer at STPC
          IF ( JBIAS.EQ.IZMSGH) THEN
            IF ( IC(TLSMUO-3).LT.10 ) GOTO 979  ! Skip to shunt MSGH
          END IF
          LSH = LC(TLSMUO-JBIAS)    ! source bank pointer
          IF ( LSH.EQ.0 ) THEN      ! source bank not existed
            IOS = 0
            GOTO 998
          END IF
          CALL ZSHUNT( IDVSTP,LSH,KSMUO,-JBIAS,1)
  979     CONTINUE
        END IF
C
C -------------  ALLGEO ------------------
C
        IF ( TP.EQ.'ALLGEO' ) THEN
C
          LMDROP = GZMGEH(0)
          IF ( LMDROP.NE.0 ) THEN
            CALL MZDROP( IDVSTP, LMDROP, ' ' )
          END IF
          LMDROP = GZMMAH(0)
          IF ( LMDROP.NE.0 ) THEN
            CALL MZDROP( IDVSTP, LMDROP, ' ' )
          END IF
          LMDROP = GZMSGH(0)
          IF ( LMDROP.NE.0 ) THEN
            CALL MZDROP( IDVSTP, LMDROP, ' ' )
          END IF
C
          CALL FZIN( LIN,IDVSTP,TLSMUO,2,' ',0,0) ! Read STP
C
          JBIAS = IZMGEH
          LSH = LC(TLSMUO-JBIAS)    ! source bank pointer
          KSMUO = GZSMUO('STPC')    ! SMUO pointer at STPC
          IF ( LSH.NE.0 ) THEN      ! source bank not existed
            CALL ZSHUNT( IDVSTP,LSH,KSMUO,-JBIAS,1)
          END IF
C
          JBIAS = IZMMAH
          LSH = LC(TLSMUO-JBIAS)    ! source bank pointer
          KSMUO = GZSMUO('STPC')    ! SMUO pointer at STPC
          IF ( LSH.NE.0 ) THEN      ! source bank not existed
            CALL ZSHUNT( IDVSTP,LSH,KSMUO,-JBIAS,1)
          END IF
C
          IF ( IC(TLSMUO-3).LT.10 ) GOTO 899  ! Skip to shunt MSGH
          JBIAS = IZMSGH
          LSH = LC(TLSMUO-JBIAS)    ! source bank pointer
          KSMUO = GZSMUO('STPC')    ! SMUO pointer at STPC
          IF ( LSH.NE.0 ) THEN      ! source bank not existed
            CALL ZSHUNT( IDVSTP,LSH,KSMUO,-JBIAS,1)
          END IF
  899     CONTINUE
        END IF
C  
      ENDIF                             ! (TP .EQ. 'ALL')
C
      IOS = IQUEST(1)
C                             CLOSE FILE
  998 CALL FZENDI(LIN,'T')
      CLOSE(UNIT=LIN)
      CALL RLUNIT(USER,LIN,ERR)
      IF(IOS.LT.0.OR.IOS.GE.4) GO TO 999
      RETURN
C
  999 CONTINUE
      OK=.FALSE.
      RETURN
      END
