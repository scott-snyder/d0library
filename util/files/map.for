      PROGRAM MAP
C---------------------------------------------------------------------
C-                                                                   -
C-      General User Program to look at DATA                         -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
C
C  ZEBCOM is the main zebra common block for event data storage
C
      INTEGER IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      INTEGER NNQ,NREF
      PARAMETER (NNQ=2000000)
      PARAMETER (NREF=9)
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,
     &  ZSTOR,ENDZS
      INTEGER IXCOM    ! store number
     &       ,IXMAIN   ! event division number
     &       ,IXDVR    ! run division number
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS
      INTEGER LHEAD     ! pointer to event HEAD bank
      INTEGER LHEADR    ! pointer to begin run HEAD bank
      REAL Q(NNQ)
      INTEGER IQ(NNQ),LQ(NNQ)
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))
C
C ZEBSTP.INC
      INTEGER NNC
      PARAMETER (NNC=200000)
      COMMON/ZEBSTP/IXSTP,IDVSTP,IDVSUM,FENSTP,LSTPH,LZSUM,
     1 LSLV0,LLPDH,LLGNH,LLTMH,LLGEH,LLV,   ! level 0 links
     2 LSMUO,LMPDH,LMGNH,LMTMH,LMGEH,LMU,   ! muon links
     3 LSVTX,LVPDH,LVGNH,LVTMH,LVGEH,LVT,   ! vertex chamber links
     4 LSCDC,LDPDH,LDGNH,LDTMH,LDGEH,LDALH,LDC,   ! cdc links
     5 LSTRD,LTPDH,LTGAI,LTGEN,LTGEO,LTR,   ! trd links
     6 LSFDC,LFPDH,LFGNH,LFTMH,LFGEH,LFD,   ! fdc links
     7 LSCAL,LCPDH,LCGNH,LCGEH,LCA,         ! calorimeter links
     8 ZCONS,ENDZC
      INTEGER IXSTP,IDVSTP,IDVSUM,FENSTP(10),LZSUM,ZCONS(NNC),ENDZC
      REAL C(NNC)
      INTEGER IC(NNC),LC(NNC)
      EQUIVALENCE (LSTPH,LC(1)),(LC(9),IC(1),C(1))
      INTEGER LSTPH     ! static parameter header
     $,  LSLV0      ! level 0 constants header
     $,  LLPDH      ! level 0 pedestal header
     $,  LLGNH      ! level 0 gain header
     $,  LLTMH      ! level 0 time constants header
     $,  LLGEH      ! level 0 geometry constants header
     $,  LLV(10)    ! available
C
      INTEGER LSMUO      ! Muon constants header
     $,  LMPDH      ! Muon pedestal header
     $,  LMGNH      ! Muon gain header
     $,  LMTMH      ! Muon time constants header
     $,  LMGEH      ! Muon geometry constants header
     $,  LMU(10)    ! available
C
      INTEGER LSVTX      ! Vertex chamber constants header
     $,  LVPDH      ! Vertex chamber pedestal header
     $,  LVGNH      ! Vertex chamber gain header
     $,  LVTMH      ! Vertex chamber time constants header
     $,  LVGEH      ! Vertex chamber geometry constants header
     $,  LVT(10)    ! available
C
      INTEGER LSCDC      ! CDC constants header
     $,  LDPDH      ! CDC pedestal header
     $,  LDGNH      ! CDC gain header
     $,  LDTMH      ! CDC time constants header
     $,  LDGEH      ! CDC geometry constants header
     $,  LDALH      ! CDC wire alignment constants header
     $,  LDC(9)     ! available
C
      INTEGER LSTRD      ! TRD constants header
     $,  LTPDH      ! TRD pedestal header
     $,  LTGAI      ! TRD gains header
     $,  LTGEN      ! TRD general header
     $,  LTGEO      ! TRD geometry constants header
     $,  LTR(10)    ! available
C
      INTEGER LSFDC      ! FDC constants header
     $,  LFPDH      ! FDC pedestal header
     $,  LFGNH      ! FDC gain header
     $,  LFTMH      ! FDC time constants header
     $,  LFGEH      ! FDC geometry constants header
     $,  LFD(10)    ! available
C
      INTEGER LSCAL      ! Calorimeter constants header
     $,  LCPDH      ! Calorimeter pedestal header
     $,  LCGNH      ! Calorimeter gain header
     $,  LCGEH      ! Calorimeter geometry constants header
     $,  LCA(11)    ! available
C
      INTEGER I,LEN,LUN,IOS,NRUN,NEVO,ISKIP,NSKIP,OPT,STRPOS,FLAG
      INTEGER NEVI1,NEVI2,GRUN,KEY,IKEY,IKEY1,IKEY2,HBANK,LUN2
      INTEGER TRULEN,NCOM,WORD2,WORD3,DELIN,TLEN,LZFIDH,LBANK,NEVI
      INTEGER NARGS,IADDR,IUSR,IERR,NUH,IUHEAD,NTRY
      INTEGER LUNPR,READI
      REAL*8 RNEVI
      CHARACTER*1 XOPT
      CHARACTER*3 FTYPE
      CHARACTER*4 PATH
      CHARACTER*10 BANK
      CHARACTER*80 FILNAM,STRING,CITEM,OUTCHR,PROMPT,FNAME,CTEMP,AREA
      CHARACTER*80 ARG,RGUMNTS(20),UAREA,PRFILE
C
      LOGICAL OK,EDIT,FIND,DELETE
C
      DATA IUSR/269/,FTYPE/'FZ'/,LUNPR/6/
C
      CALL MZEBRA(0)              ! initialize ZEBRA
C
C     here's how you get rid of the dreaded "FOR003" (argh!)
C
      IQPRNT = 6
      IQPR2 = 6
      IQLOG = 6
C
C     now finish the initialization
C
      CALL INZCOM(2)              ! initialize ZEBCOM
      CALL INZSTP                 ! initialize ZEBSTP
      CALL INZLNK                 ! initialize ZLINKA
      CALL EZINIT
C
C     set defaults and go for it!
C
cccC&IF VAXVMS
ccc      XOPT = ' '
cccC&ELSE
      XOPT = 'X'
cccC&ENDIF
c
   12 continue
c
c     get commands
c
      call getcom(citem)
c
c     parse command into arguments
c
      call getarg(citem,'=,/ ',rgumnts,nargs)
c
c     upper case for 1st one, this is the command
c
      call str$upcase(rgumnts(1),rgumnts(1))
      arg = rgumnts(1)
c
c     NOW CHECK THE COMMAND AGAINST THE LIST
c
c
      if (arg(1:1).eq.'H') then
c
c       user requests help
c
        call maphelp
c
      else if (arg(1:2).eq.'RE') then
        close(lun)
        write(*,'('' Closing '',a60,/,
     &    '' ...and...reopening.....'')') filnam
        call myopen(filnam,xopt,lun,ok)
      else if (arg(1:2).eq.'FZ') then
c
c       open file
c
        if (nargs.lt.2) then
c
c         toggle data--stp
c
          ftype = 'FZ'
          goto 12
        endif
c
        filnam = rgumnts(2)
        if (filnam(1:1).eq.'X'.or.filnam(1:1).eq.'x')
     &    stop 'OKOK, you give up!'
        if (nargs.gt.2) then
c
c         XOPT should be either 'X' or 'N' or ' '
c
          call str$upcase(xopt,rgumnts(3))
        endif
c
c       open the file here:
c
        call myopen(filnam,xopt,lun,ok)
        ftype = 'FZ'
C&IF VAXVMS
C&ELSE
C&        close(unit=3)
C&        call system('more fort.3; rm fort.3')
C&ENDIF
        goto 12
c
      else if (arg(1:3).eq.'STP') then
C
C       the following will NOT work on the IBM yet!!!
c
c       open file
c
        if (nargs.eq.1) then
          ftype = 'STP'
          write(*,'('' If you want to open a file, type: '',
     &      '' STP <filename> '')')
        else 
          filnam = rgumnts(2)
        endif
        if (filnam(1:1).eq.'X'.or.filnam(1:1).eq.'x')
     &    stop 'OKOK, you give up!'
        call gtunit(iusr,lun2,ierr)
c
c       STP files are native mode...
c
C&IF VAXVMS
        Open (Unit=Lun2,File=Filnam,Status='Old',Readonly,
     1   Form='Unformatted',Err=500,Iostat=IERR)
C&ELSE
C&        Open (Unit=Lun2,File=Filnam,Status='Old',
C&     1   Form='Unformatted',Err=500,Iostat=IERR)
C&ENDIF
        call FZFILE(lun2,0,'I')
        call FZLOGL(lun2,-2)
        call MZWIPE(IXSTP)
        CALL FZIN(lun2,IXSTP,LSTPH,-5,' ',NUH,IUHEAD)
        call EZINIT
        ftype = 'STP'
C&IF VAXVMS
C&ELSE
C&        close(unit=3)
C&        call system('more fort.3; rm fort.3')
C&ENDIF
        goto 12
  500   continue
        write(*,'('' Error opening file - status '',I10,Z8)') ierr,ierr
c
      else if (arg(1:2).eq.'OP') then
        if (nargs.lt.2) then
          write(*,'('' **** MUST SPECIFY FILENAME - TRY AGAIN ****'')')
          goto 12
        endif
c
c       open file for output
c
        call gtunit(iusr,lunpr,ierr)
        prfile = rgumnts(2)
        open(unit=lunpr,file=prfile,status='NEW')
        iqprnt = lunpr
c
      else if (arg(1:2).eq.'CL') then
        if (lunpr.ne.6) close(unit=lunpr)
        iqprnt = 6
c
      else if (arg(1:3).eq.'DIR') then
c
c       do a directory (or ls) on an area
c
        area = rgumnts(2)
C&IF VAXVMS
        call lib$spawn('dir/date/size=all '//area)
        write(*,'('' Enter <cr> to continue...'',$)')
        read(*,'(a1)') xopt
C&ELSE
C&        tlen = trulen(area)
C&        if (tlen.eq.80) then
C&          call system('ls -l | more')
C&        else
C&          call getarg(citem,'=,',rgumnts,nargs)
C&          area = rgumnts(2)
C&          tlen = trulen(area)
C&          uarea(1:6) = 'ls -l'
C&          uarea(7:6+tlen) = area(1:tlen)
C&          uarea(7+tlen:14+tlen) = ' | more'
C&          call system(uarea)
C&        endif
C&ENDIF
c
      else if (arg(1:1).eq.'Q'.or.arg(1:2).eq.'EX') then
c
c       that all folks?
c
        stop ' OKOK '
c
      else if (arg(1:1).eq.'N') then
c
c       read next record
c
        call readit(lun,nrun,nevo,nevi1,nevi2,grun,ok)
        goto 12
c
      else if (arg(1:2).eq.'SC') then
C
        ok = .true.
        do while (ok)
          call readit(lun,nrun,nevo,nevi1,nevi2,grun,ok)
          if (.not.ok) goto 12
        enddo
c
      else if (arg(1:2).eq.'FI') then
c
c       loop over events, stop appropriately
c
        call getfi(citem,key,ikey1,ikey2,bank,ok)
        if (.not.ok) goto 12
        find = .false.
        do while (.not.find)
          call readit(lun,nrun,nevo,nevi1,nevi2,grun,ok)
          if (.not.ok) goto 12
          if (key.eq.1) then
            if (nrun.eq.ikey1) goto 12
          else if (key.eq.2) then
            if (grun.eq.ikey1) goto 12
          else if (key.eq.3) then
            if (nevo.eq.ikey1) goto 12
          else if (key.eq.4) then
            if (nevi1.eq.ikey1.and.nevi2.eq.ikey2) goto 12
          else if (key.eq.5) then
            call blocat(ixmain,bank,lbank)
            IF (LBANK.ne.0) goto 12
          endif
        enddo
c
      else if (arg(1:2).eq.'SK') then
        if (nargs.eq.1) then
          write(*,'('' How many to skip? '',$)')
          read(*,*) nskip
        else
          nskip = readi(rgumnts(2))
        endif
        do iskip=1,nskip
          call readit(lun,nrun,nevo,nevi1,nevi2,grun,ok)
          if (.not.ok) goto 12
        enddo
        goto 12
c
      else if (arg(1:2).eq.'PA') then
c
c       set Zebra PATH
c
        if (nargs.eq.1) then
          call pathst('RECO')
        else
          call str$upcase(path,rgumnts(2))
          if (path(1:1).eq.'?') then
            call pathgt(path)
            write(*,'('' ===> ZEBRA path set to '',A4)') path
          else
            call pathst(path)
          endif
        endif
c
      else if (arg(1:2).eq.'DB') then
c
c       CALL DBANK
c
        call dbank
        goto 12
c
      else if (arg(1:2).eq.'DA') then
c
c       CALL DADDR
c
        if (nargs.eq.1) then
          write(*,'('' enter address: '',$)')
          read(*,*) iaddr
        else
          iaddr = readi(rgumnts(2))
        endif
        call daddr(iaddr)
        goto 12
c
      else if (arg(1:2).eq.'PR') then
        if (nargs.gt.1) then
          call str$upcase(string,rgumnts(2))
          edit = string(1:2).eq.'ED'
          if (nargs.gt.2) then
            call str$upcase(string,rgumnts(3))
            delete = string(1:2).eq.'DE'
          else
            delete = .false.
          endif
        else
          edit = .false.
          delete = .false.
        endif
        if (edit) then
          call gtunit(iusr,lun2,ierr)
          prfile = 'PR'//arg(3:6)//'.DAT'
          open(unit=lun2,file=prfile,status='NEW')
          call prbank(arg(3:6),lun2)
          close(unit=lun2)
C&IF VAXVMS
          call dmpscr(prfile)
          if (delete) then
            tlen = trulen(prfile)
            prfile(tlen+1:tlen+1) = ';'
            call lib$spawn('delete/noconfirm '//prfile)
          endif
C&ELSE
C&          call system('vi '//prfile)
C&          if (delete) call system('rm '//prfile)
C&ENDIF
          call rlunit(iusr,lun2,ierr)
        else
          call prbank(arg(3:6),lunpr)
        endif
c
      else if (arg(1:2).eq.'EZ') then
c
c       CALL EZBANK (FOR STP FILES)
c
        call ezbank
        goto 12
c
      else if (arg(1:1).eq.'V') then
        if (ftype.eq.'FZ') then
          write(outchr,'(
     &      '' Store VERIFICATION for ZEBCOM: Run/Event '',2I8)')
     &          nrun,nevo
          call DZVERI(outchr,ixcom,'LSU')
        else if (ftype.eq.'STP') then
          write(outchr,'('' Store VERIFICATION for ZEBSTP'')')
          call DZVERI(outchr,ixSTP,'LSU')
        endif
c
      else if (arg(1:2).eq.'IN') then
c
c       CALL DZSTOR AND DZSHOW (for experts!)
c
        if (nargs.gt.1) then
          call str$upcase(string,rgumnts(2))
          edit = string(1:1).eq.'E'
        else
          edit = .false.
        endif
        if (edit) then
          call gtunit(iusr,lun2,ierr)
          prfile = 'DZSURV.DAT'
          open(unit=lun2,file=prfile,status='NEW')
          iqprnt = lun2
        endif
        call dzstor('Store dump',ixcom)
        call dzshow('store dump ',ixcom,lhead,'BLV',0,0,0,0)
        if (edit) then
          close(unit=lun2)
          iqprnt = 6
C&IF VAXVMS
          call dmpscr(prfile)
C&ELSE
C&          call system('vi '//prfile)
C&ENDIF
        endif
c
      else if (arg(1:2).eq.'DU') then
        if (nargs.gt.1) then
          call str$upcase(string,rgumnts(2))
          edit = string(1:1).eq.'E'
        else
          edit = .false.
        endif
        if (edit) then
          call gtunit(iusr,lun2,ierr)
          prfile = 'DZSURV.DAT'
          open(unit=lun2,file=prfile,status='NEW')
          iqprnt = lun2
        endif
        if (ftype.eq.'FZ') then
          write(outchr,'('' Store dump for ZEBCOM: Run/Event '',2I8)')
     &          nrun,nevo
          call DZSURV(outchr,IXcom,LHEAD)
        else if (ftype.eq.'STP') then
          write(outchr,'('' Store dump for ZEBSTP'')')
          call DZSURV(outchr,IXstp,Lstph)
        endif
        if (edit) then
          close(unit=lun2)
          iqprnt = 6
C&IF VAXVMS
          call dmpscr(prfile)
C&ELSE
C&          call system('vi '//prfile)
C&ENDIF
        endif
      else
        write(*,'('' *** Command Not Found or Ambiguous ***'')')
      endif 
c
      goto 12
 1000 continue
      write(*,'('' *** OPEN ERROR:  IOS = '',I7)') IOS
      stop ' oh shit!'
      END                                    
c-------------------------------------------------------------------------------
      FUNCTION delin(string,char)
C
C     this routine will return the position of the first
C     non-blank character after the first "CHAR" 
C     (or " WORD1 WORD2 " will return position of WORD2 FOR CHAR=' ')
C
C     we assume that the first character is NOT A BLANK!!!
C
      IMPLICIT NONE
C
      INTEGER delin
      CHARACTER*1 char
      character*(*) string
C
      INTEGER TLEN,TRULEN,I,J
C
      delin = -1
C
C     get position of first occurance of 'char'
C
      TLEN = TRULEN(STRING)
      DO J=1,TLEN
        IF (STRING(J:J).EQ.char) THEN
          GOTO 200
        ENDIF
      ENDDO
      RETURN
c
c     now get position of first non-blank character
c
  200 CONTINUE
      DO I=J+1,TLEN
        IF (STRING(I:I).NE.' ') THEN
          delin = I
          RETURN
        ENDIF
      ENDDO
C
      RETURN
      END
c-------------------------------------------------------------------------------
      FUNCTION strpos(string,char)
C
C     this routine will return the position of the first occurance of "char"
C     inside "string" - char must be A*1
C
      IMPLICIT NONE
C
      INTEGER strpos
      CHARACTER*1 char
      character*(*) string
C
      INTEGER TLEN,trulen,I,J
C
      strpos = 0
C
C     get position of first occurance of 'char'
C
      TLEN = TRULEN(STRING)
      DO J=1,TLEN
        IF (STRING(J:J).EQ.char) THEN
          strpos = j
          return
        ENDIF
      ENDDO
c
      RETURN
      END
c-------------------------------------------------------------------------------
      subroutine blocat(main,bank,pointer)
c
c     returns pointer to bank with name "BANK"
c
      implicit none
c
      integer main,pointer
      character*(*) bank
c
      integer hbank,lzfidh
c
      call str$upcase(bank,bank)
      CALL UCTOH(BANK,HBANK,4,4)
      pointer = LZFIDH(MAIN,HBANK,0)
c
      return
      end
c-------------------------------------------------------------------------------
      subroutine readit(lun,nrun,nevo,nevi1,nevi2,grun,ok)
c
c     my own version which calls D0 utilities and prints out stuff
c
      implicit none
C
C  ZEBCOM is the main zebra common block for event data storage
C
      INTEGER NNQ,NREF
      PARAMETER (NNQ=2000000)
      PARAMETER (NREF=9)
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,
     &  ZSTOR,ENDZS
      INTEGER IXCOM    ! store number
     &       ,IXMAIN   ! event division number
     &       ,IXDVR    ! run division number
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS
      INTEGER LHEAD     ! pointer to event HEAD bank
      INTEGER LHEADR    ! pointer to begin run HEAD bank
      REAL Q(NNQ)
      INTEGER IQ(NNQ),LQ(NNQ)
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))
c
      integer lun,ios,nrun,nevo,nevi1,nevi2,grun
      logical ok
c
      CALL EVTRD(lun,IOS)  ! read events from file
      ok = .true.
      nrun = iq(lhead+6)
      nevo = iq(lhead+9)
      nevi1 = iq(lhead+7)
      nevi2 = iq(lhead+8)
      grun = iq(lhead+12)
      IF(IOS.EQ.0) THEN
        write(*,'('' *** Event record - Run# '',I6,'' Global# '',I6,/,
     &    ''                    Out # '',I7,'' In#2/In#1 '',2I8)')
     &    nrun,grun,nevo,nevi2,nevi1
C
C           Begin run record
      ELSE IF(IOS.EQ.1) THEN
        write(*,'('' *** Begin run record found...'')')
C
C           End run record
      ELSE IF(IOS.EQ.2) THEN ! ENDRUN 
        write(*,'('' *** End run record found...'')')
C
C         end-of-file
      ELSE IF(IOS.GT.2) THEN
        write(*,'('' *** End-of-file reached...closing...'')')
        close(lun)
        ok = .false.
C
      ENDIF
c
      return
      end
c-------------------------------------------------------------------------------
      subroutine getfi(citem,key,ikey1,ikey2,bank,ok)
c
c     allows you to use vax-like options, e.g. find/output=...
c
      implicit none
c
      integer key,ikey1,ikey2
      character*(*) citem
      character*4 bank
      logical ok
c
      integer nargs,tlen,trulen
      character*80 rgumnts(20),arg2,arg3,arg4,string
c
c     parse command into arguments
c
      call getarg(citem,'=,/ ',rgumnts,nargs)
c
c     if there are more than 1 arguments, then use command line,
c     otherwise prompt
c
      ok = .true.
      if (nargs.eq.1) then
20      write(*,'('' You can specify one of the following keys: '',/,
     &    ''   Key = 1   Search for LOCAL RUN NUMBER'',/,
     &    ''         2              GLOBAL "    "   '',/,
     &    ''         3              OUTPUT EVENT "  '',/,
     &    ''         4              INPUT EVENT  "  '',/,
     &    ''         5              BANK NAME '',//,
     &    '' Which one? '',$)')
        read(*,*) key
        if (key.lt.4) then
          write(*,'('' Input integer for match: '',$)')
          read(*,*) ikey1
        else if (key.eq.4) then
          write(*,'('' Input 2 integers to match: '',$)')
          read(*,*) ikey1,ikey2
        else if (key.eq.5) then
          write(*,'('' TYPE IN BANK NAME>'',$)') 
          read(*,'(a10)') bank
        else
          ok = .false.
        endif
      else
c
c       user must have input as command line
c
        call str$upcase(arg2,rgumnts(2))
        arg3 = rgumnts(3)
        arg4 = rgumnts(4)
        if (arg2(1:1).eq.'L') then
          key = 1
          tlen = trulen(arg3)
          write(string,'(''(i'',i3.3,'')'')') tlen
          read(arg3,string) ikey1
        else if (arg2(1:1).eq.'G') then
          key = 2
          tlen = trulen(arg3)
          write(string,'(''(i'',i3.3,'')'')') tlen
          read(arg3,string) ikey1
        else if (arg2(1:1).eq.'O') then
          key = 3
          tlen = trulen(arg3)
          write(string,'(''(i'',i3.3,'')'')') tlen
          read(arg3,string) ikey1
        else if (arg2(1:1).eq.'I') then
          key = 4
          tlen = trulen(arg3)
          write(string,'(''(i'',i3.3,'')'')') tlen
          read(arg3,string) ikey1
          tlen = trulen(arg4)
          write(string,'(''(i'',i3.3,'')'')') tlen
          read(arg4,string) ikey2
        else if (arg2(1:1).eq.'B') then
          key = 5
          bank = arg3(1:4)
        else
          write(*,'('' ***** Illegal argument '')')
          ok = .false.
        endif
      endif
c
      return
      end
c-------------------------------------------------------------------------------
      subroutine getarg(string,cdelin,subs,n)
c
c     pass string along, if there are "words", or substrings, separated
c     by delineartors as in delin, then each substring is shoved into subs
c
c     note: delin is a c*(*) which consists of the "n" c*1 delineators
c     e.g. ' ,/=' (and a ' ' (blank) will always be assumed!)
c
      implicit none
c
      character*(*) string,cdelin
      character*80 subs(*)
      integer n
c
      integer nseps,tlen,trulen,delin,len,i,j,f,l
      character*80 temp
      character*1 seps(100)
      logical match
c
c     special case - 0 substrings
c
      tlen = trulen(string)
      if (tlen.eq.0) then
        n = 0
        return
      endif
c
c     count delineators
c
      nseps = trulen(cdelin)
      do i=1,nseps
        seps(i) = cdelin(i:i)
      enddo
      nseps = nseps + 1
      seps(nseps) = ' '
c
c     loop, look for delineators, fill string
c
      f = 1
      n = 0
      do i=1,tlen
        match = .false.
        do j=1,nseps
          if (string(i:i).eq.seps(j)) match = .true.
        enddo
        if (match) then
          l = i - 1
          if (f.ne.l+1) then
            temp = string(f:l)
            n = n + 1
            subs(n) = temp
            f = i + 1
          endif
        else if (i.eq.tlen) then
          n = n + 1
          temp = string(f:tlen)
          subs(n) = temp
        endif
      enddo
c
      return
      end
c-------------------------------------------------------------------------------
      subroutine getcom(command)
c
      implicit none
c
      character*(*) command
      integer tlen,trulen
c
  200 continue
      write(*,'(
     &  1x,''--------- M I S C E L L A N E O U S --------------'',
     &          26(''-''),/,
     &  '' Quit or EXit           Exit the program'',/,
     &  '' Help                   Get some help'',/,
     &  1x,''--------- F I L E    M A N I P U L A T I O N -----'',
     &          26(''-''),/,
     &  '' Next                   Read in the next record'',/,
     &  '' SCan                   Read through all events,'',
     &               '' typout run/event information'',/,
     &  '' SKip [n]               Read, skip next "n" events'',/,
     &  '' REwind                 Rewind the file'')')
      write(*,'(
     &  '' FInd                   Loop over events, '',
     &                  ''stop based on either:'',/,
     &  ''                        /LOCAL_RUN=integer   '',
     &  ''  /GLOBAL_RUN=integer '',/,
     &  ''                        /OUTPUT_EVENT=integer'',
     &  ''  /INPUT_EVENT=integer'',/,
     &  ''                        /BANK=name'')')
      write(*,'(
     &  1x,''--------- Z E B R A    S T U F F -----------------'',
     &          26(''-''),/,
     &  '' PAth [path]            '',
     &             ''Set ZEBRA Path (blank=RECO, ?=show)'',/,
     &  '' Verify                 Verify ZEBRA structure via DZVERI'',/,
     &  '' DUmp[/Edit]            Dump via DZSURV'',/,
     &  '' INfo[/Edit]            Calls DZSHOW and DZSTOR'',//,
     &  ''     for DUMP and INFO, can use OUTPUT (see below)'')')
      write(*,'(
     &  1x,''--------- D 0    S T U F F -----------------------'',
     &          26(''-''),/,
     &  '' DBank                  Call DBANK'',/,
     &  '' DAddress [address]     Call DADDR '',/,
     &  '' EZbank                 Call EZBANK'',/,
     &  '' PRbank                 Will call '',
     &        ''PRbank(LUN,0,0,''''ALL'''',0)'')')
      write(*,'(
     &  1x,''--------- F I L E S    &    D I R E C T O R I E S '',
     &          26(''-''),/,
     &  '' FZopen [file] [mode]   Open FZ data file (def mode=X)'',/,
     &  '' STPopen [file]         Open STP file'',/,
     &  '' OPen filename          Opens "filename" for output from'',
     &  '' DUMP, INFO, PRbank'',/,
     &  '' CLose                  Closes above file if opened'',/,
     &  '' DIR [area]             Do a "directory" listing'',/,
     &                    1x,76(''-''))')
 1000 continue
      write(*,'('' Which one? '',$)')
      read(*,'(a)',end=2000) command
      tlen = trulen(command)
      if (tlen.lt.1) goto 1000
      if (command(1:1).eq.'?') goto 200
c
      return
 2000 continue
      stop 'OKOK (nice nice)'
c
      end
c-------------------------------------------------------------------------------
      subroutine maphelp
c
      character*1 xopt
      write(*,'(
     &    '' The program is intended to be a "mini-frame" to '',
     &    ''allow fast access to'',/,
     &    '' data files and STP files without having to relink '',
     &    ''a D0USER or CALOR_OFF'',/,
     &    '' or etc. program.  The following sets of commands are '',
     &    ''available - note'',/,
     &    '' that the following syntax CO[mmand] means you only '',
     &    ''have to type in CO,'',/,
     &    '' and <parameter> is the name of a parameter '',
     &    ''you need to give:'',/)')
      write(*,'(
     &    '' FILE commands (see DUMP commands below for more info):'',/,
     &    '' -----------------------------------------------------'',/,
     &    '' FZ <name> <mode>   will open a data file with mode=X or '',
     &    ''N. The default'',/,
     &   20x,''is "X" for all computers  Note that the first record'',/,
     &    20x,''is NOT read in!  You have to do this by hand. '',/,
     &    '' STP <name>         will open and READ IN the '',
     &    ''first record.'',/,
     &    '' DIR <area>         will do a directory listing '',
     &    ''on the area '')')
      write(*,'(
     &    '' OPEN <name>        will open a file for output from'',
     &    '' DUMP, INFO, or PRbank'',//)')
      write(*,'(
     &    '' LOOP commands - these commands read in events:'',/,
     &    '' ---------------------------------------------'',/,
     &    '' N[ext]       go to next event - information is typed '',
     &    ''out telling you'',/,
     &    6x,''        what kind of record:  begin_run, event, etc.'',/,
     &    '' SC[an]       run thru the entire file - the program '',
     &    ''will print out'',/,
     &    6x,''        run and event numbers, and will stop at EOF'',/,
     &    '' SK[ip] n     will read the next "n" events and '',
     &    ''stop for orders'',/,
     &    '' RE[wind]     will rewind the file'')')
      write(*,'(
     &    '' FI[nd]       will read till it comes to a target '',
     &    ''record selected by'',/,
     &    ''              either LOCAL_RUN [IQ(LHEAD+6)], '',
     &    ''GLOBAL_RUN [IQ(LHEAD+12)],'',/,
     &    ''              OUTPUT_EVENT [IQ(LHEAD+9)], '',
     &    ''INPUT_EVENT [IQ(LHEAD+7) and'',/,
     &    ''              [IQ(LHEAD+8)] (you have to specify '',
     &    ''both numbers here), or'',/,
     &    ''              BANK (you specify a bank name).  '',
     &    ''This command is particulary'')')
      write(*,'(
     &    ''              useful.  For instance, if you want to '',
     &    ''run through the file'',/,
     &    ''              and stop when it gets to an event with '',
     &    ''a MUOT bank, you type'',/,
     &    ''              FI/BANK=MUOT.  Or if you want to stop '',
     &    ''when output event '',/,
     &    ''              number is 1608, FI/OU=1608.  '',
     &    ''Unfortunately, a .OR. of the'',/,
     &    6x,''        above conditions is not (yet?) implemented.''/)')
      write(*,'(
     &    '' ZEBRA commands - calls CERNLIB and D0 Zebra routines:'',/,
     &    '' ----------------------------------------------------'',/,
     &    '' PAth [path]'',11X,''sets the Zebra PATH.  PATH=" "=>'',
     &    ''RECO, "?"==>shows path'',/,
     &    '' DB[ank]               calls DBANK on ZEBCOM common'',/,
     &    '' DA[ddress] [address]  calls DADDR (like DBANK, '',
     &    ''specify ZEBRA address)'',/,
     &    '' PRbank                calls PRbank routine for bank'',
     &    '' "bank" (e.g. PRJETS)'',/,
     &    10X,''             type PRxxxx to get active list'')')
      write(*,'(
     &    '' EZ[bank]    '',10x,''calls EZBANK on the ZEBSTP common'',/,
     &    '' DU[mp] [/edit]        calls DZSURV (map of what '',
     &    ''banks are present)'',/,
     &    '' IN[fo] [/edit]        calls DZSHOW and DZSTOR '',
     &    ''(ZEBRA dumps for experts!)'',/,
     &    '' V[erify]              calls DZVERI to verify '',
     &    ''structure'',//)')
      write(*,'(
     &    '' NOTE:  o if you have opened an FZ file, ZEBCOM '',
     &    ''is the target for these,'',/,
     &    ''          but if you have opened an STP file, '',
     &    ''ZEBSTP is used.  you CAN have'',/,
     &    ''          both an FZ and a STP file opened.  if '',
     &    ''you want to switch, you'',/,
     &    ''          can just type FZ or STP with no argument.  '',
     &    ''For instance, say'')')
      write(*,'(
     &    ''          you first open an FZ file via "FZ file '',
     &    ''X".  Then if you do'',/,
     &    ''          "DU", you will get a dump of the ZEBCOM '',
     &    ''bank.  If you then '',/,
     &    ''          open an STP file via "STP file" and do '',
     &    ''DU, you will get a listing'',/,
     &    ''          of the banks in ZEBSTP.  If you want to '',
     &    ''dump out ZEBCOM again,'',/,
     &    ''          type "FZ" and then "DU".'')')
      write(*,'('' Enter <cr> to continue...'',$)')
      read(*,'(a1)') xopt
c
      return
      end
c-------------------------------------------------------------------------------
      subroutine prbank(bank,lun)
c
c     dumps out bank info for bank "BANK" via prBANK routines
c
      implicit none
c
      character*4 bank
      integer lun
c
      integer i,names
      parameter (names=51)
      character*4 banks(names)
c
      data banks/
     &  'CACL','CAD1','CAD2','CADT','CAEH','CAEP','CAPH','CATD',
     &  'CATE','CDD1','CDD2','CDD3','CDD4','DTRK','ESUM','FILT',
     &  'FRES','HEAD','HITS','HMTE','HMTP','HSTR','ISAE','ISAJ',
     &  'ISAQ','ISAZ','JAUX','JETS','JNEP','JPTS','JTSH','L2EM',
     &  'MUD1','MUOT','PARH','PELC','PJET','PMUO','PNUT','PPHO',
     &  'RECO','TPRL','TRDT','TRGR','TSUM','TTRH','VERH','VERT',
     &  'VTRH','ZFIT','ZTRK'/
c
      call str$upcase(bank,bank)
c
c     check against known list:
c
      if (bank.eq.'CACL') then
        call PRCACL(lun,0,0,'ALL',0)
      else if (bank.eq.'CAD1') then
        call PRCAD1(lun,0,0,'ALL',1)
      else if (bank.eq.'CAD2') then
        call PRCAD2(lun,0,0,'ALL',1)
      else if (bank.eq.'CADT') then
        call PRCADT(lun,0,0,'ALL',0)
      else if (bank.eq.'CAEH') then
        call PRCAEH(lun,0,0,'ALL',0)
      else if (bank.eq.'CAEP') then
        call PRCAEP(lun,0,0,'ALL',1)
      else if (bank.eq.'CAPH') then
        call PRCAPH(lun,0,0,'ALL',0)
      else if (bank.eq.'CATD') then
        call PRCATD(lun,0,0,'ALL',0)
      else if (bank.eq.'CATE') then
        call PRCATE(lun,0,0,'ALL',0)
      else if (bank.eq.'CDD1') then
        call PRCDD1(lun,0,0,'ALL',3)
      else if (bank.eq.'CDD2') then
        call PRCDD2(lun,0,0,'ALL',3)
      else if (bank.eq.'CDD3') then
        call PRCDD3(lun,0,0,'ALL',3)
      else if (bank.eq.'CDD4') then
        call PRCDD4(lun,0,0,'ALL',3)
      else if (bank.eq.'DTRK') then
        call PRDTRK(lun,0,0,'ALL',6)
      else if (bank.eq.'ESUM') then
        call PRESUM(lun,0,0,'ALL',0)
      else if (bank.eq.'FILT') then
        call PRFILT(lun,0,0,'ALL',0)
      else if (bank.eq.'FRES') then
        call PRFRES(lun,0,0,'ALL',2)
      else if (bank.eq.'HEAD') then
        call PRHEAD(lun,0,0,'ALL',0)
      else if (bank.eq.'HITS') then
        call PRHITS(lun,0,0,'ALL',0)
      else if (bank.eq.'HMTE') then
        call PRHMTE(lun,0,0,'ALL',0)
      else if (bank.eq.'HMTP') then
        call PRHMTP(lun,0,0,'ALL',0)
      else if (bank.eq.'HSTR') then
        call PRHSTR(lun,0,0,'ALL',0)
      else if (bank.eq.'ISAE') then
        call PRISAE(lun,0,0,'ALL',0)
      else if (bank.eq.'ISAJ') then
        call PRISAJ(lun,0,0,'ALL',0)
      else if (bank.eq.'ISAQ') then
        call PRISAQ(lun,0,0,'ALL',0)
      else if (bank.eq.'ISAX') then
        call PRTEVZ(lun)
      else if (bank.eq.'JAUX') then
        call PRJAUX(lun,0,0,'ALL',0)
      else if (bank.eq.'JETS') then
        call PRJETS(lun,0,0,'ALL',0)
      else if (bank.eq.'JNEP') then
        call PRJNEP(lun,0,0,'ALL',0)
      else if (bank.eq.'JPTS') then
        call PRJPTS(lun,0,0,'ALL',0)
      else if (bank.eq.'JTSH') then
        call PRJTSH(lun,0,0,'ALL',0)
      else if (bank.eq.'L2EM') then
        call PRL2EM(lun,0,0,'ALL',0)
      else if (bank.eq.'MUD1') then
        call PRMUD1(lun,0,0,'ALL',0)
      else if (bank.eq.'MUOT') then
        call PRMUOT(lun,0,0,'ALL',0)
      else if (bank.eq.'PARH') then
        call PRPARH(lun,0,0,'ALL',0)
      else if (bank.eq.'PELC') then
        call PRPELC(lun,0,0,'ALL',0)
      else if (bank.eq.'PJET') then
        call PRPJET(lun,0,0,'ALL',0)
      else if (bank.eq.'PMUO') then
        call PRPMUO(lun,0,0,'ALL',0)
      else if (bank.eq.'PNUT') then
        call PRPNUT(lun,0,0,'ALL',1)
      else if (bank.eq.'PPHO') then
        call PRPPHO(lun,0,0,'ALL',0)
      else if (bank.eq.'RECO') then
        call PRRECO(lun,0,0,'ALL',0)
      else if (bank.eq.'TPRL') then
        call PRTPRL(lun,0,0,'ALL',0)
      else if (bank.eq.'TRDT') then
        call PRTRDT(lun,0,0,'ALL',0)
      else if (bank.eq.'TRGR') then
        call PRTRGR(lun,0,0,'ALL',0)
      else if (bank.eq.'TSUM') then
        call PRTSUM(lun,0,0,'ALL',0)
      else if (bank.eq.'TTRH') then
        call PRTTRH(lun,0,0,'ALL',0)
      else if (bank.eq.'VERH') then
        call PRVERH(lun,0,0,'ALL',0)
      else if (bank.eq.'VERT') then
        call PRVERT(lun,0,0,'ALL',0)
      else if (bank.eq.'VTRH') then
        call PRVTRH(lun,0,0,'ALL',0)
      else if (bank.eq.'ZFIT') then
        call PRZFIT(lun,0,0,'ALL',0)
      else if (bank.eq.'ZTRK') then
        call PRZTRK(lun,0,0,'ALL',0)
      else
        write(*,'('' Printout for only the following banks: '')')
        write(*,'(10X,8(3X,A4))') (banks(i),i=1,8)
        write(*,'(10X,8(3X,A4))') (banks(i),i=9,16)
        write(*,'(10X,8(3X,A4))') (banks(i),i=17,24)
        write(*,'(10X,8(3X,A4))') (banks(i),i=25,32)
        write(*,'(10X,8(3X,A4))') (banks(i),i=33,40)
        write(*,'(10X,8(3X,A4))') (banks(i),i=41,48)
        write(*,'(10X,2(3X,A4))') (banks(i),i=49,50)
        write(*,'('' ==> e-mail requests to FNALD0::DREW'')')
      endif
c
      return
      end
c-------------------------------------------------------------------------------
      subroutine myopen(filnam,xopt,lun,ok)
c
      implicit none
c
      integer lun
      logical ok
      character*(*) filnam
      character*1 xopt
c
      integer ntry
c
      ntry = 0
  120 continue
      CALL EVOPIN(FILNAM,XOPT,LUN,OK)  ! OPEN INPUT FILE
      ntry = ntry + 1
      if (ok) then
        if (xopt.eq.'X') then
          write(*,'('' ===> FILE '',A60,/,
     &       '' Successfully opened in EXCHANGE mode'')') filnam
        else
          write(*,'('' ===> FILE '',A60,/,
     &       '' Successfully opened in NATIVE mode'')') filnam
        endif
      else
c
c       open failed - try alternative mode (X/N) if haven't already         
c
        if (ntry.eq.2) then
          write(*,'('' ===> Still cannot open '',a60,/,
     &        '' ===> I give up, you''''re on your own!'')') filnam
          return
        else
          if (xopt.eq.'X') then
            write(*,'('' ===> FILE '',A60,/,
     &          '' ===> Cannot be opened in EXCHANGE mode'',
     &          ''.....trying NATIVE....'')') filnam
            xopt = 'N'
          else
            write(*,'('' ===> FILE '',A60,/,
     &          '' ===> Cannot be opened in NATIVE mode'',
     &          ''.....trying EXCHANGE....'')') filnam
            xopt = 'X'
          endif
          goto 120
        endif
      endif
c
      return
      end
c-------------------------------------------------------------------------------
      function readi(str)
c
c     translates "str" into "integer" - works on unix
c
      implicit none
c
      integer readi
      character*(*) str
c
      integer trulen,tlen
      character*80 string
c
      tlen = trulen(str)
      write(string,'(''(i'',i3.3,'')'')') tlen
      read(str,string) readi
c
      return
      end
