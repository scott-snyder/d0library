      SUBROUTINE D0DAD_CPARSE(CMDLIN,IDOPT)
C-----------------------------------------------------------------------
C  Parse the command line options passed to D0DAD.  This routine uses
C  cmlin variable filled in main using LIB$GET_FOREIGN
C  Define defaults...

C  Author:    John D. Hobbs
C  Date:       1-NOV-1993
C
C  INPUTS:  CMDLIN - The command line as returned from LIB$GET_FOREIGN
C  OUTPUS:  IDOPT  - major running mode identifier
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:d0dadcom.inc'
      INCLUDE  'D0$INC:d0dad.inc'
      INTEGER  IDOPT,I,IMODE,IFIL,IPOS,NOPT,NCMD,IC,ILEN,IVAL,IERR
      INTEGER  LENOCC
      EXTERNAL LENOCC
      LOGICAL  LFAIL
      CHARACTER*128 CLOPT,CSMASK*64,CTEMP*128,CDBG*2,CMDLIN*(*)
C
      INTEGER  MAXARG
      PARAMETER(MAXARG=20)
      CHARACTER*132 ARGV(MAXARG)
      INTEGER  IARGC,LENARG(MAXARG),NARG
      LOGICAL  LOPT(MAXARG)
C
C-----------------------------------------------------------------------
C
      IMODE=0
      CLOPT='user'
      LFAIL=.FALSE.
      CALL VZERO(LOPT,MAXARG)
C
C  Convert into c-link argc,argv form...
C
      DO I=1,MAXARG
        ARGV(I)=' '
      ENDDO
      CALL ALLWORDS(CMDLIN,MAXARG,IARGC,ARGV,LENARG)
C
C  Parse (options first)...
C
      NOPT=0
      DO 10 I=1,IARGC
         CTEMP=ARGV(I)
         CALL CUTOL(CTEMP)
         IF( CTEMP(1:6).EQ.'/mode=') then
            CLOPT=CTEMP(7:LENARG(I))
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:7).EQ.'/debug=' ) THEN
            READ(CTEMP(8:LENARG(I)),*) LDDBG
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:9).EQ.'/generic=' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
            CGNAME=CTEMP(10:LENARG(I))
         ELSEIF( CTEMP(1:6).EQ.'/tape=' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
            CTAPE=CTEMP(7:LENARG(I))
         ELSEIF( CTEMP(1:9).EQ.'/comment=' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
            CFCCOM=CTEMP(10:LENARG(I))
         ELSEIF( CTEMP(1:5).EQ.'/run=' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
            CALL D0DAD_RANGE(CTEMP(6:LENARG(I)),ISELR)
         ELSEIF( CTEMP(1:7).EQ.'/event=' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
            CALL D0DAD_RANGE(CTEMP(8:LENARG(I)),ISELE)
            LECDEV=.TRUE.
         ELSEIF( CTEMP(1:6).EQ.'/event' ) THEN
            LECDEV=.TRUE.
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:8).EQ.'/runoff=' ) THEN
            READ(CTEMP(9:LENOCC(CTEMP)),*) IROFF
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:8).EQ.'/evtoff=' ) THEN
            READ(CTEMP(9:LENOCC(CTEMP)),*) IEOFF
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:8).EQ.'/zbtype=' ) THEN
            CZBTYP=CTEMP(9:LENOCC(CTEMP))
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:6).EQ.'/test=' ) THEN
            READ(CTEMP(7:LENOCC(CTEMP)),*) ITEST
            IF( ITEST.NE.0 ) ITEST=1
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:6).EQ.'/blind' ) THEN
            LBLIND=.TRUE.
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:6).EQ.'/page=' ) THEN
            READ(CTEMP(7:LENOCC(CTEMP)),*) IVAL
            CALL ECFSET('KRPREC',IVAL,IERR)
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:7).EQ.'/delta=' ) THEN
            READ(CTEMP(8:LENOCC(CTEMP)),*) IVAL
            CALL ECFSET('KECDLT',IVAL,IERR)
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
         ELSEIF( CTEMP(1:11).EQ.'/timestamp=' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
           READ(CTEMP(12:LENOCC(CTEMP)),*)
     >       DATTIM_STAMP(1),DATTIM_STAMP(2)
         ELSEIF( CTEMP(1:9).EQ.'/evtcheck' ) THEN
           LEVT_CHECK=.TRUE.
           LOPT(I)=.TRUE.
           NOPT=NOPT+1
         ELSEIF( CTEMP(1:7).EQ.'/header' ) THEN
           HEADER_ONLY=.TRUE.
           LOPT(I)=.TRUE.
           NOPT=NOPT+1
         ELSEIF( CTEMP(1:7).EQ.'/prefix' ) THEN
           PREFIX=CTEMP(9:LENOCC(CTEMP))
           CALL CUTOL(PREFIX)
           LOPT(I)=.TRUE.
           NOPT=NOPT+1
         ELSEIF( CTEMP(1:6).EQ.'/skip=' ) THEN
           CALL D0DAD_SETUP_SKIP_RUNS(CTEMP(7:LENOCC(CTEMP)))
           LOPT(I)=.TRUE.
           NOPT=NOPT+1
         ELSEIF( CTEMP(1:10).EQ.'/diskscan=' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
            CTEMP=ARGV(I)  ! Preserve the original case of the file name
            CALL D0DAD_CATCHECK_WILDCARD(CTEMP(11:LENARG(I)))
         ELSEIF( CTEMP(1:10).EQ.'/scanonly' ) THEN
            LOPT(I)=.TRUE.
            NOPT=NOPT+1
            CALL D0DAD_CATCHECK_SCANONLY
C&IF VAXVMS
C         ELSE
C           WRITE(*,1001) CTEMP(1:LENOCC(CTEMP))
C 1001      FORMAT(' ** Unknown command line option: ',A)
C           LFAIL=.TRUE.
C&ENDIF
         ENDIF
 10   CONTINUE
C
      IF( LFAIL ) THEN
        IDOPT=IMNULL
        GOTO 999
      ENDIF
C
C  now parse for each different option...
C
      NCMD=IARGC-NOPT
*
      IF( CLOPT.EQ.'rcpscan' ) THEN
*
         IF( NCMD.LT.2 ) THEN
            IDOPT=IMNULL
            GOTO 999
         ENDIF
         IDOPT=IMRCP
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FUENAM)
         CALL D0DAD_GETFNO(FUENAM,UECOPT)
*
      ELSE IF( CLOPT.EQ.'scan' ) THEN
*
         IDOPT=IMSCAN
         IF( NCMD.LT.2 ) THEN
           IDOPT=IMNULL
           GOTO 999
         ENDIF
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FUENAM)
         CALL D0DAD_GETFNO(FUENAM,UECOPT)
*
      ELSE IF( CLOPT.EQ.'update' ) THEN
*
         IDOPT=IMUPDT
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FUENAM)
         CALL D0DAD_GETFNO(FUENAM,UECOPT)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FECNAM)
         CALL D0DAD_GETFNO(FECNAM,ECCOPT)
         CALL FILENAME_PARSE(FECNAM,'NAM',CECTAG,IC)
         CFCTAG=CECTAG
         FFCNAM=FECNAM
         CALL FILENAME_PARSE(FFCNAM,'DIR+NAM',FFCNAM,IC)
         FFCNAM(IC+1:IC+8)='.filecat'
         CALL D0DAD_GETCMD(3,IARGC,ARGV,LENARG,LOPT,FFCNAM)
         CALL D0DAD_GETFNO(FFCNAM,FCCOPT)
*
      ELSE IF( CLOPT(1:4).EQ.'edit' ) THEN
*
         IDOPT=IMEDIT
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,EDSTRING)
*
      ELSE IF( CLOPT(1:6).EQ.'stream' ) THEN
*
         IDOPT=IMSTRM
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FECNAM)
         CALL D0DAD_GETFNO(FECNAM,ECCOPT)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,CSMASK)
         CALL D0DAD_INTEGER(CSMASK,ISTBIT,ISTTYP)
         FDFNAM=FECNAM
         CALL FILENAME_PARSE(FDFNAM,'DIR+NAM',FDFNAM,IC)
         FDFNAM(IC+1:IC+7)='.d0dadf'
         CALL D0DAD_GETCMD(3,IARGC,ARGV,LENARG,LOPT,FDFNAM)
         CALL D0DAD_GETFNO(FDFNAM,DFCOPT)
*
      ELSE IF( CLOPT.EQ.'user' ) THEN
*
         IDOPT=IMUSER
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FDFNAM)
         CALL D0DAD_GETFNO(FDFNAM,DFCOPT)
         CALL D0DAD_GETCMD(3,IARGC,ARGV,LENARG,LOPT,FECNAM)
*
      ELSE IF( CLOPT.EQ.'create' ) THEN
*
         IDOPT=IMCREA
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
*
      ELSE IF( CLOPT.EQ.'dump' ) THEN
*
         IDOPT=IMDUMP
         LDDBG=2
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
*
      ELSE IF( CLOPT.EQ.'test' ) THEN
*
         IDOPT=IMTEST
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FDFNAM)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FFCNAM)
*
      ELSE IF( CLOPT.EQ.'copy' ) THEN
*
         IDOPT=IMCOPY
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FDFNAM)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FZBNAM)
*
      ELSE IF( CLOPT.EQ.'translate' ) THEN
*
         IDOPT=IMXLAT
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FFCNAM)
         CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FZBNAM)
*
      ELSE IF( CLOPT.EQ.'check' ) THEN
*
         IDOPT=IMCHEK
         CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
*
      ELSE IF( CLOPT.EQ.'info' ) THEN

        IDOPT=IMINFO
        CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FUENAM)
        CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FZBNAM)
        
      ELSE IF( CLOPT.EQ.'repair' ) THEN
        
        IDOPT=IMFIX
        CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FECNAM)

      ELSE IF( CLOPT.EQ.'text_stream' ) THEN
        
        IDOPT=IMTSTR
        CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FZBNAM)
        CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FDFNAM)
        CALL D0DAD_GETCMD(3,IARGC,ARGV,LENARG,LOPT,FECNAM)

      ELSE IF( CLOPT.EQ.'merge' ) THEN
        
        IDOPT=IMMERG
        CALL D0DAD_GETCMD(1,IARGC,ARGV,LENARG,LOPT,FNTEXT)
        CALL D0DAD_GETCMD(2,IARGC,ARGV,LENARG,LOPT,FECNAM)

      ELSE
          IDOPT=IMNULL
      ENDIF
C
C  Do good stuff to file(s) with no extension.  FZBNAM is untouched...
C
      I=LENOCC(FUENAM)
      CALL FILENAME_PARSE(FUENAM,'EXT',CTEMP,ILEN)
      IF( ILEN.EQ.0 .AND. I.GT.0 ) FUENAM=FUENAM(1:I)//'.uevtcat'
      I=LENOCC(FECNAM)
      CALL FILENAME_PARSE(FECNAM,'EXT',CTEMP,ILEN)
      IF( ILEN.EQ.0 .AND. I.GT.0 ) FECNAM=FECNAM(1:I)//'.evtcat'
      I=LENOCC(FFCNAM)
      CALL FILENAME_PARSE(FFCNAM,'EXT',CTEMP,ILEN)
      IF( ILEN.EQ.0 .AND. I.GT.0) FFCNAM=FFCNAM(1:I)//'.filecat'
      I=LENOCC(FDFNAM)
      CALL FILENAME_PARSE(FDFNAM,'EXT',CTEMP,ILEN)
      IF( ILEN.EQ.0 .AND. I.GT.0) FDFNAM=FDFNAM(1:I)//'.d0dadf'
C
C  A little friendly I/O
C
      IF( LDDBG.GT.5 ) THEN
         WRITE(*,*) ' '
         WRITE(*,*) ' --------- Run configuration ----------'
         WRITE(*,*) '  Run mode = ',IDOPT
         WRITE(*,*) '  Zebra file = ',FZBNAM(1:LENOCC(FZBNAM))
         WRITE(*,*) '  Unsorted EC = ',FUENAM(1:LENOCC(FUENAM))
         WRITE(*,*) '  Sorted EC = ',FECNAM(1:LENOCC(FECNAM))
         WRITE(*,*) '  D0dad file = ',FDFNAM(1:LENOCC(FDFNAM))
         WRITE(*,*) '  File catalog = ',FFCNAM(1:LENOCC(FFCNAM))
         WRITE(*,*) '  Selected run range=',ISELR(1),' to ',ISELR(2)
         WRITE(*,*) '  Selected event range=',ISELE(1),' to ',ISELE(2)
         WRITE(*,*) ' '
      ENDIF
C
 999  CONTINUE
      RETURN
      END
