!!!      OPTIONS /EXTEND_SOURCE   (not necessary any more  SA)
c-IC_SIGNAL: if it times out, check if the master really exists
c-IC_WRITE to 'P' cache by watcher after 'L' crashes
c-improve IC_MATCH -- A (all) option to include system locks as well.
c P option to print output.
c-put hole-finding into IC_BOOK
c-IC_EXIST,IC_WATCH: multiple notification. 3 times for RUN_SE, followed by
c fake IC_EXIST=.TRUE.
c-clean up IC_EXISTX
c-convert IC and CC to ef-based timeout.
c-convert ic_lkwait to use event flag, like ic_signal. ic_wtast, ic_wkast
c-document broadcast lockhandling
c-document process locks better
c************************************************************************
c*
c*       INFOCACHE
c*
c*       Set of routines to provide an 'information cache' of
c*       16 bytes associated with a symbolic name, accessible
c*       from any process on any CPU in a VAXcluster with low
c*       overhead and with an interprocess signalling capability.
c*
c*       Uses the VMS distributed lock manager. Cache is the
c*       lock value block (LVB).
c*
c*       There several modes of usage:
c*
c*         1) INFORMATION CACHE
c*         2) BROADCAST SENDING/RECEIVING
c*         3) PROCESS STATUS CHECKER / CRASH NOTIFIER
c*         4) MASTER/CLIENT COMMUNICATIONS (MAILBOX MODE)
c*         5) ALARM SYSTEM
c*         6) RESOURCE LOCK
c*
c*       See the documentation for the details. See TW for the
c*       documentation.
c*
c*       Usage notes:
c*         - SYSLCK is needed to use 'Y' option (system-wide caches)
c*         - options should be specified in UPPER CASE.
c*         - example link:
c*              $ LINK myprogram,CPC$DEV:CPC/LIB/INCL=CPC
c*         - all programs which use CPC should call CPC_FINISH
c*           before exiting.
c*
c*       Present version: DEV
c*
c*  1.4 -> DEV changes:
c*  -------------------
c*  - ASTs disabled during broadcasts. ASTs are queued and complete when
c*    ASTs are reenabled at broadcast completion. Protects against a
c*    process being interrupted during a bcast by other bcasts, signals
c*  - ENQ-completion AST disabled on crash-AST delivery in IC_DELIVER
c*  - IC_SEND modified to settle for CR mode if EX mode times out. With
c*    CR mode the master will still be BASTed on the next signal, and
c*    will be returned to EX mode when the next signal completes.
c*
c*  1.3 -> 1.4 changes:    July 89
c*  ------------------------------
c*  - IC_LIST_CACHES to list booked caches. For internal use; used in
c*    error tracebacks.
c*  - IC_E_DEADLOCK return code when deadlock is encountered. User code
c*    should retry the call (IC_SIGNAL probably) immediately. Do tell
c*    me of deadlock occurrences and whether you think they can be
c*    accounted for by two processes signalling one another at the
c*    same time.
c*  - IC_WRITE can now be used by a process lock holder. IC_STATUS just
c*    calls IC_WRITE. IC_STATUS is obsolete.
c*  - temporarily queued locks in IC_INFO are dropped (why did I comment
c*    out the drop code at some point? Beats me)
c*  - requeue instead of convert in IC_WATCH
c*  - changes to broadcast and alarm mode to speed them up and ensure
c*    secure synchronization
c*  - IC_INFO cleaned up
c*  - IC_GET_CACHE added: reads a BOOKED cache fast using $GETLKI.
c*    Internal cache reads via IC_INFO replaced by this routine.
c*  - ungranted lock requests (due to timeout) are now cancelled
c*  - extremely nasty bug fixed. A result of using LIB$WAIT. When the
c*    LIB$WAIT delay in IC_LOCKUP was interrupted by a broadcast AST,
c*    resulting in execution of IC_GETBC, when the execution returned to
c*    normal level, ie returned to the LIB$WAIT, the process never woke.
c*    All LIB$WAITs switched to L3_WAIT which uses $HIBER and $SCHDWK.
c*  - 'X' option in IC_INFO to return whether there is an exclusive holder,
c*    ie master (process lock holder), on a mailbox (process watcher) cache
c*  - an error condition is now returned from IC_BOOK (IC_E_DBLEXBOOK)
c*    when an attempt is made to double book master mode or process lock
c*    mode.
c*  - IC_EXIST function added to tell whether a cache exists. 'X' option to
c*    tell whether an exclusive holder exists. Cache need not be booked.
c*    If cache is not booked, usage mode must be passed.
c*  - double length (32 byte) type of information cache introduced. Option
c*    D in IC_BOOK.
c*  - 'U' option in resource lock mode to use all 16 bytes of cache as
c*    user-defined bytes. Only constraint is that byte 1 MUST BE NONZERO.
c*  - wait in IC_SIGNAL changed to event flag wait, and made more secure.
c*  - IC_ID_NUM protected against corruption by AST execution
c*  - on double booking attempt, return is quiet.
c*  - logical IC_BOOKED(NAME) is true if cache NAME is booked
c*  - IC_DELIVER modified for WATCHER mode such that the CR lock is released
c*    immediately (back to NL) when a crash AST is delivered.
c*  - IC_READ modified for watchers to use GETLKI rather than ENQ.
c*  - LCK$M_NODLCKWT used to prevent deadlock searches
c*  - IC_E_INACTIVE returned quietly from IC_DELIVER in the case of an
c*    interrupt delivered during the releasing (IC_RELEASE) of a
c*    crash watcher cache on which IC_WATCH has been called.
c*  - lockholder cache in WATCHER mode switched from primary lock to system
c*    lock, so primary lock carries status info and system lock carries
c*    the EX mode lock. Because NL lock conversions were not being delivered
c*    when there were outstanding EX requests by watchers.
c*  - more timeout-proofing introduced and better debug dump when timeouts
c*    occur
c*
c*  1.2 -> 1.3 changes:    Apr 89
c*  -----------------------------
c*  - bug fix in TIMEOUT for IC_SIGNAL
c*  - IERR=IC_E_STIMOUT (severity=warning) if the reply AST times out
c*    in IC_SIGNAL.
c*  - booking of client before master in mailbox mode allowed
c*  - event flag allocation and deallocation moved into IC_LOCKUP
c*    routine, so EFs are no longer held for each cache. Allocation
c*    time is negligible.
c*  - cache timeouts increased and error message made more comprehensible
c*    (VS 2000s are slow!)
c*  - IC_LOCKUP split into AST and NON-AST routines.
c*  - IC_INFO cache list output improved.
c*  - VALNOTVALID notification turned off. Empirically, signifies nothing.
c*  - resource lock mode added. Book option 'R'. Allows process to lock
c*    a 'resource', excluding other processes from locking the resource
c*    until it is released. Other processes can check whether the resource
c*    is booked and find out who has it booked. Routines:
c*      IC_TAKE_LOCK
c*      IC_DROP_LOCK
c*      IC_CHECK_LOCK
c*
c*  1.1 -> 1.2 changes:    Jan 89
c*  -----------------------------
c*    - handling multiple caches from a single AST now works.
c*      To use it: your ast should be declared with a single
c*      integer parameter, eg.   SUBROUTINE MYAST(NUM)
c*      Then to obtain the name of the cache which triggered
c*      the AST,  CALL IC_NUM_ID(NUM,NAME,' ',IERR).
c*    - master/client mode now has full 16 bytes available
c*    - string 'L3%' added to all lock names to avoid conflicts.
c*      Invisible to the user.
c*    - mode information is now carried in the lock name; a
c*      single leading character invisible to the user.
c*      More checking added for mode compatibility, double
c*      booking, etc.
c*    - alarm system mode introduced. Multiple senders -- which do
c*      not want to receive interrupts -- can send alarms, and
c*      multiple receivers (alarm handlers) receive alarm interrupts
c*      in just the same way as broadcast mode. Options T and S
c*      in IC_BOOK.
c*
c*  1.0 -> 1.1 changes:    Nov 88
c*  -----------------------------
c*    - TIMEOUT fixed in IC_SIGNAL
c*    - error handling improved. Errors generated by the MESSAGE
c*      utility are returned in IERR. LIB$SIGNAL can be used to
c*      display them (message info is in the CPC library).
c*      LIB$SIGNAL is used internally so all errors generate
c*      an output message.
c*      Error codes are in the file L3$MODEL:[CPC]CPCERR.FOR.
c*      IERR=0 as before if the routine executed error-free.
c*    - some work on IC_INFO done. Option L lists locks on a
c*      given cache or lists all locks.
c*    - LENOCC -> LST, where LST is a function in the CPC library,
c*      so the CERN libraries are no longer needed in the link
c*    - maximum length of ID names increased to 20
c*
c*
c*  Enhancements under consideration:
c*  ---------------------------------
c*
c* - INFOCACHE extension: option to use internal string-passing mechanism
c*   which stores strings internally in a CLUSCOM section. The section has
c*   an array of structures with each structure containing
c*        STRING
c*        USED/UNUSED LOGICAL
c*        CACHE NAME
c*        ID NUMBER FOR CROSSCHECK (pass this number in cache itself,
c*        together with the element number in the structure array;
c*        crosscheck with this number.)
c*   When a string is to be sent:
c*      - sender goes down the array looking for an unused element
c*      - sets the element USED, loads the string, the cache name,
c*        and puts a random number in the cache itself and in the
c*        structure ID
c*      - sends signal with IC_SIGNAL
c*      - receiver gets from the cache the element number in the structure
c*        and extracts the string. ID number is crosschecked between the
c*        cache and the structure. Receiver sets element UNUSED.
c*        CACHE NAME in structure not strictly necessary; it is
c*        for debugging.
c*   This would be an option in MASTER/CLIENT mode. New routines:
c*   IC_SEND_STRING same as IC_SIGNAL, with additional STRING parameter.
c*   IC_DELIVER_STRING same as IC_DELIVER,    "         "
c*   Very low priority. L3_MESSCOM serves now.
c*
c*                                       T. Wenaus     Aug 1988
c*
c************************************************************************
      SUBROUTINE IC_BOOK(NAME1,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_BOOK
c*
c*       Create a cache with name NAME (or link to an existing cache
c*       of this name)
c*       This is the first routine called for all usage modes. Usage
c*       mode is specified by an option selection.
c*
c*       Options:
c*
c*       INFO CACHE MODE
c*              16-byte info cache is the default mode.
c*         D    double length, 32 byte. Price: it is slower.
c*
c*       MASTER/CLIENT (MAILBOX) MODE
c*         M    Master in master/client mode (only one master)
c*         C    Client in master/client mode (there can be several)
c*
c*       BROADCAST MODE
c*         B    Broadcast mode (there can be several)
c*
c*       PROGRAM STATUS MONITOR
c*         L    Program lock (program to be watched takes out a lock)
c*         P    Program status checker (watcher program; may be several)
c*
c*       ALARM SYSTEM
c*         T    Task which sends out alarms (may be several)
c*         S    Alarm server which receives and process alarms (may be
c*              several)
c*
c*       RESOURCE LOCK
c*         R    Book a resource lock cache.
c*
c*       Other options:
c*         X    existing. Cache should already exist, or an error
c*              condition IC_E_NOEXIST is returned (but no error dump
c*              is printed)
c*         Y    system mode. Cache is recognised across all UICs.
c*              Requires SYSLCK privilege.
c*         N    no check on whether cache already exists
c*
c************************************************************************
      IMPLICIT INTEGER (A-Z)
      CHARACTER*(*) NAME1,CHOPT
      CHARACTER*30 NAME, BLANK
      INTEGER LVB(4)
      INCLUDE '($LCKDEF)'
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c**        lock value block of system cache
      STRUCTURE /SYSLVB/
        BYTE MODE
        BYTE IDUM(15)
      END STRUCTURE
      RECORD /SYSLVB/ LVBS
      CHARACTER*1 PREFIX
c*
      IERR=0
c*         set up lock mode values
      IC__NL = LCK$K_NLMODE
      IC__CR = LCK$K_CRMODE
      IC__EX = LCK$K_EXMODE
      IC__PW = LCK$K_PWMODE
c*
c*         LST is a duplicate of LENOCC, kept in CPCUTIL
      IF (LST(NAME1).GT.IC__MAXLEN) THEN
c*           cache identifier too long
        CALL LIB$SIGNAL(%VAL(IC_E_BADNAME),%VAL(1),%VAL(IC__MAXLEN))
        IERR=IC_E_BADNAME
        RETURN
      ENDIF
      IF (NCACHE.GE.IC__MAXCACHE) THEN
c*           too many caches
        CALL LIB$SIGNAL(%VAL(IC_E_MAXEX),%VAL(1),%VAL(IC__MAXCACHE))
        IERR=IC_E_MAXEX
        RETURN
      ENDIF
      NAME = BLANK
      NAME = NAME1(1:LST(NAME1))
c*
      IF (INDEX(CHOPT,'M').NE.0) THEN
        IMODE  = IC__MAILBOX
        ITYPE  = IC__MASTER
        PREFIX = C__MAILBOX
      ELSE IF (INDEX(CHOPT,'C').NE.0) THEN
        IMODE  = IC__MAILBOX
        ITYPE  = IC__CLIENT
        PREFIX = C__MAILBOX
      ELSE IF (INDEX(CHOPT,'B').NE.0) THEN
        IMODE  = IC__BROADCAST
        ITYPE = IMODE
        PREFIX = C__BROADCAST
      ELSE IF (INDEX(CHOPT,'L').NE.0) THEN
        IMODE  = IC__WATCHER
        ITYPE  = IC__PLOCK
        PREFIX = C__WATCHER
      ELSE IF (INDEX(CHOPT,'P').NE.0) THEN
        IMODE  = IC__WATCHER
        ITYPE  = IC__PSTAT
        PREFIX = C__WATCHER
      ELSE IF (INDEX(CHOPT,'T').NE.0) THEN
        IMODE  = IC__ALARM
        ITYPE  = IC__ATASK
        PREFIX = C__ALARM
      ELSE IF (INDEX(CHOPT,'S').NE.0) THEN
        IMODE  = IC__ALARM
        ITYPE  = IC__ASERV
        PREFIX = C__ALARM
      ELSE IF (INDEX(CHOPT,'R').NE.0) THEN
        IMODE  = IC__LOCK
        ITYPE  = IC__LOCK
        PREFIX = C__LOCK
      ELSE IF (INDEX(CHOPT,'D').NE.0) THEN
        IMODE  = IC__INFOCACHE
        ITYPE = IC__INFODBL
        PREFIX = C__INFOCACHE
      ELSE
c*           default is information cache mode
        IMODE  = IC__INFOCACHE
        ITYPE = IMODE
        PREFIX = C__INFOCACHE
      ENDIF
c*
c*         check that this cache is not already booked
      CALL IC_ID_NUM(NAME,NUM,'N',IERR)
      IF (IERR.EQ.0) THEN
c*           cache has already been booked
c*           return quietly
        IERR=IC_E_DBLBOOK
        RETURN
      ENDIF
c*
c**        use IC_INFO to see if a cache of this name already exists.
c*         If it does, check that the usage mode is the same.
c*
      IF (INDEX(CHOPT,'X').NE.0) THEN
        CALL IC_INFO(NAME,IMODE,INUM,'N',IERR)
        IF (IERR.NE.0) GOTO 999
        IF (INUM.EQ.0) THEN
          IERR = IC_E_NOEXIST
          GOTO 999
        ENDIF
      ENDIF
      IF (ITYPE.EQ.IC__MASTER.OR.ITYPE.EQ.IC__PLOCK) THEN
c*           check for attempt to double book an exclusive lock
        CALL IC_INFO(NAME,IMODE,INUM,'X',IERR)
        IF (INUM.EQ.1) THEN
          CALL LIB$SIGNAL(%VAL(IC_E_DBLEXBOOK),%VAL(1),NAME(:LST(NAME)))
          IERR=IC_E_DBLEXBOOK
          GOTO 999
        ENDIF
      ENDIF
c*
c**        increment number of known caches
      NCACHE = NCACHE +1
c*
c***       All caches have an associated system cache, used for system
c*         info, and for access coordination in some usage modes.
c*
c*           first create or read the system cache
      LN=LST(NAME)
      CACHE(NCACHE).ID = PREFIX//'S_'//NAME(1:LN)
      CACHE(NCACHE).LKST.NAME = PREFIX//'S_'//NAME(1:LN)
      CACHE(NCACHE).TYPE = IC__SYSTEM
      CACHE(NCACHE).LOCK  = 0
      CACHE(NCACHE).AUTH  = 0
c**        at present used only for broadcast caches. Broadcast identifier.
c*         Unique to each broadcast on the cache (mod a big number)
      CACHE(NCACHE).BCID  = -100
      IF (INDEX(CHOPT,'Y').NE.0) THEN
c*           system mode bit
        CACHE(NCACHE).AUTH  = CACHE(NCACHE).AUTH .OR. 2
      ENDIF
c*
      IF (IMODE.EQ.IC__INFOCACHE.AND.ITYPE.EQ.IMODE) THEN
c*           for default info cache mode, do not create the system lock.
c*           it is not used.
      ELSE IF (ITYPE.EQ.IC__PSTAT) THEN
c*           system cache left alone until IC_WATCH is called
      ELSE
        CALL IC_LOCKUP(IC__NL,NCACHE,LVBS,'N',0.,IERR)
        IF (IERR.NE.0) GOTO 998
      ENDIF
c*
c**        fill the primary cache structure
      NCACHE = NCACHE +1
      LN=LST(NAME)
      CACHE(NCACHE).ID = PREFIX//NAME(:LN)         ! ID of the cache
      CACHE(NCACHE).LKST.NAME = PREFIX//NAME(:LN)  ! name of the lock resource
      CACHE(NCACHE).STAT  = 0              ! cache status eg. 'inactive'
      CACHE(NCACHE).MODE  = IMODE
      CACHE(NCACHE).TYPE  = ITYPE
      CACHE(NCACHE).LOCK  = 0              ! lock status
      CACHE(NCACHE).BCID  = -100
c**         create/access the lock resource, read LVB, and get a lock
      IF (ITYPE .EQ. IC__BROADCAST) THEN
c*           broadcast mode
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
      ELSE IF (ITYPE .EQ. IC__MASTER) THEN
c*           master mode
        CALL IC_LOCKUP(IC__EX,NCACHE,LVB,'N',0.,IERR)
      ELSE IF (ITYPE .EQ. IC__CLIENT) THEN
c*           client mode
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
      ELSE IF (ITYPE .EQ. IC__PSTAT) THEN
c*           process status watcher
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
      ELSE IF (ITYPE .EQ. IC__PLOCK) THEN
c*           process to be watched takes out lock
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
c*           sys cache left alone until IC_WATCH
        CALL IC_LOCKUP(IC__EX,NCACHE-1,LVB,' ',0.,IERR)
      ELSE IF (ITYPE .EQ. IC__ASERV) THEN
c*           alarm server same as broadcast.
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
      ELSE IF (ITYPE .EQ. IC__ATASK) THEN
c*           alarm task in NL mode; does not receive BASTs
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
      ELSE IF (ITYPE .EQ. IC__LOCK) THEN
c*           resource lock in NL mode
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
      ELSE
c*           default: information cache
        CALL IC_LOCKUP(IC__NL,NCACHE,LVB,'N',0.,IERR)
      ENDIF
      IF (IERR.NE.0) GOTO 999
      RETURN
  998 CONTINUE
c*         exit point for errors discovered after system cache booking.
      NCACHE = NCACHE -1
  999 END
      SUBROUTINE IC_WATCH(NAME,LVB,IR,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_WATCH
c*
c*       Routine for use in process status checking, to establish a
c*       watch on a lock-holder
c*
c*          Options:
c*             W      Wait for lock release. Process hibernates until
c*                    lock is released. Cache at time of release is
c*                    returned to caller in LVB
c*             A      AST routine IR is called when lock is released.
c*                    IC_DELIVER can be called in the AST routine to
c*                    obtain the LVB at time of release.
c*             X      exclusive notification. Means that only one
c*                    process will be notified if lock is freed.
c*                    This option can be used together with W or A.
c*
c*  $$$ watcher becomes watched when AST is delivered if X is selected
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      EXTERNAL IC_WKAST, IC_CRASH_UAST
      LOGICAL IC_EXIST
c*
      IERR=0
      CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__PSTAT) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_BADWATCH),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_BADWATCH
        GOTO 999
      ENDIF
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).LOCK.EQ.IC__EX) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_DBLWATCH),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_DBLWATCH
        GOTO 999
      ENDIF
c*
c**        check that the EX-holder does exist
      IF (.NOT.IC_EXIST(NAME,0,'X',IERR)) THEN
        IERR=IC_E_NOLKHOLD
        GOTO 999
      ENDIF
      IF (INDEX(CHOPT,'W').NE.0) THEN
        CACHE(NUM-1).AST=%LOC(IC_WKAST)
        TIMEOUT=0.
        IF (INDEX(CHOPT,'X').NE.0) THEN
          CALL IC_LKWAIT(IC__EX,NUM-1,LVB,TIMEOUT,'N',IERR)
        ELSE
          CALL IC_LKWAIT(IC__CR,NUM-1,LVB,TIMEOUT,'N',IERR)
        ENDIF
      ELSE IF (INDEX(CHOPT,'A').NE.0) THEN
c*           user AST is called indirectly from IC_CRASH_UAST
        CACHE(NUM-1).UAST=%LOC(IR)
        CACHE(NUM-1).AST=%LOC(IC_CRASH_UAST)
c*           requeue, to ensure conversions (by lockholder) do not let this
c*           guy in
        IF (INDEX(CHOPT,'X').NE.0) THEN
          CALL IC_LOCKUP(IC__EX,NUM-1,LVB,'N',0.,IERR)
        ELSE
          CALL IC_LOCKUP(IC__CR,NUM-1,LVB,'N',0.,IERR)
        ENDIF
      ELSE
        CALL LIB$SIGNAL(%VAL(IC_E_BADWOPT),
     &    %VAL(2),CHOPT,NAME(:LST(NAME)))
        IERR=IC_E_BADWOPT
        GOTO 999
      ENDIF
  999 END
      SUBROUTINE IC_STATUS(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_STATUS
c*
c*       Routine to allow a lock-holder to store status in the lock
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      CALL IC_WRITE(NAME,LVB,CHOPT,IERR)
      END
      SUBROUTINE IC_MATCH(MATCH,NMATCH,IDNUM,IMODE,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_MATCH
c*
c*       Given a string to match MATCH, return in array IDNUM a list of
c*       caches whose names contain the string MATCH. Array IMODE
c*       contains usage mode codes for the different caches.
c*       NMATCH is the number of matches found.
c*       example usage:
c*          CALL IC_MATCH('/MU/',NCACHE,ID,IM,' ',IERR)
c*          DO I=1,NCACHE
c*            CALL IC_INFO(CNAME,0,ID(I),'I',IERR) ! cache name is returned
c*            IF (IM(I).EQ.IC__MAILBOX) CALL IC_SIGNAL(CNAME,...)
c*          ENDDO
c*
c************************************************************************
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      CHARACTER MATCH*(*), CNAME*30
      INTEGER LDATA(IC__MAXCACHE*2+1), IDNUM(*), IMODE(*)
c*
      CALL IC_INFO(MATCH(1:LST(MATCH)),0,LDATA,'S',IERR)
      IMODE(1)=0
      NMATCH=0
      DO I=1,LDATA(1)
        ID=LDATA(1+I*2-1)
        CNAME = ' '
        IMD=0
        CALL IC_INFO_I(CNAME,IMD,ID,IDUM,'I',IERR1) ! cache name is returned
        IF (CNAME(5:6).NE.'S_') THEN
c**            cache is not a system cache; add to the list
          NMATCH=NMATCH+1
          IDNUM(NMATCH)=ID
          IMODE(NMATCH)=LDATA(1+I*2)
        ENDIF
      ENDDO
      END
      SUBROUTINE IC_INFO(NAME,IMODE,LDATA,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_INFO
c*
c*       General information retrieval routine.
c*          NAME = cache (=lock) name for which info is needed
c*                 OR
c*          NAME = '*' will give info for all caches (on
c*                 the screen only at present, so use the P option)
c*
c*       The cache need not be booked by the requesting process to
c*       obtain information.
c*       If the cache has not been booked, a NL lock is taken in order to
c*       ensure a copy of the lock's data structure exists on the local
c*       node. In this case the parameter IMODE must be the usage mode
c*       code.
c*
c*   Options:
c*     C         return cache contents in LDATA (4 words)
c*     N         number of processes holding a lock on NAME in LDATA(1)
c*     L         list processes holding a lock on this cache
c*               If NAME = '*', all locks on the system are listed
c*               Nothing returned in LDATA
c*     M         return usage mode of cache NAME. If it doesn't exist,
c*               return code is 0. Mode is returned in IMODE.
c*     P         print to the screen. Can be used in conjunction with
c*               options C,N,M,S to have info printed on screen as well
c*               as returned in LDATA
c*     S         return ID numbers of all caches containing the string
c*               NAME. LDATA(1) is number of caches returned;
c*               thereafter follow ID numbers. After ID numbers,
c*               in LDATA(1+LDATA(1)+I), are mode codes.
c*     I         a cache ID is passed in LDATA. The name of the cache is
c*               returned in NAME.
c*     Y         system cache. Needed when IC_INFO is called for caches
c*               which are not booked.
c*     X         gives number of exclusive holders of mailbox, process
c*               lock caches (can only be 0 or 1!)
c*
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LDATA(*), LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($LKIDEF)'
      INCLUDE '($LCKDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($SYIDEF)'
      INCLUDE '($JPIDEF)'
      INTEGER SYS$GETLKIW, SYS$GETSYIW, LIB$GETJPI
      PARAMETER NMXLK = 2000
      CHARACTER LOCKNAME(NMXLK)*31
      INTEGER LOCKID(NMXLK),LKL(NMXLK)
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 BUFLEN
            INTEGER*2 ITMCODE
            INTEGER BUFADR
            INTEGER RETADR
          END MAP
          MAP
            INTEGER END_LIST
          END MAP
        END UNION
      END STRUCTURE
      RECORD /ITMLST/ LKILIST(10)
      STRUCTURE /IOSBLK/
        INTEGER STS, RESERVED
      END STRUCTURE
      RECORD /IOSBLK/ IOSB
      RECORD /LKIDEF/ LKLS(30)
      PARAMETER (LKLSLN=690)
      INTEGER ILEN, NMLEN
      LOGICAL LOCAL_BOOK, MATCH, WILDCARD
      CHARACTER OPT*1
      INTEGER SYS$DEQ
      INTEGER IRETLN
      INTEGER*2 RETLEN(2)
      EQUIVALENCE(IRETLN,RETLEN)
      CHARACTER NODE*16, RNODE*16, PROC*15, RQMODE*4, GRMODE*4, QUEUE*7
      CHARACTER BLANK*20 /'                    '/, CTYPE*7
      CHARACTER INTNAME*31
c*
      MATCH = .FALSE.
      LOCAL_BOOK = .FALSE.
      IF (INDEX(CHOPT,'X').NE.0) LDATA(1)=0
      IF (INDEX(CHOPT,'N').NE.0) LDATA(1)=0
      IF (NAME(1:LST(NAME)).EQ.'*') WILDCARD = .TRUE.
      IERR=0
c*
      IF (INDEX(CHOPT,'M').NE.0) THEN
c*           for 'M' mode, if cache is booked, it is fast and easy.
        CALL IC_ID_NUM(NAME,NUM,'N',IERR)
        IF (NUM.NE.0) THEN
          IMODE = CACHE(NUM).MODE
          GOTO 999
        ENDIF
      ENDIF
      IF (INDEX(CHOPT,'I').NE.0) THEN
        CALL IC_INFO_I(INTNAME,IMODE,LDATA,IDUM,CHOPT,IERR)
c**          trim the internal component of the name
        NAME = INTNAME(5:)
        GOTO 999
      ENDIF
      IF (NAME.NE.'*'.AND.INDEX(CHOPT,'M').EQ.0
     +      .AND.INDEX(CHOPT,'S').EQ.0) THEN
c*           check if cache is booked
CCC          CALL IC_ID_NUM(NAME,NUM,'N',IERR)
        DO I=1,NCACHE
          IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
            NUM=I
            IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
          ENDIF
        ENDDO
        IERR=IC_E_NOMATCH
    1   CONTINUE
        IF (IERR.EQ.IC_E_NOMATCH) THEN
c*             if not, get lock in NL mode to get data struct on local node
          IF (IMODE.LE.0.OR.IMODE.GT.IC__NMODES) THEN
c*               IMODE invalid
            CALL LIB$SIGNAL(%VAL(IC_E_NEEDMODE),
     &        %VAL(2),NAME(:LST(NAME)), %VAL(IMODE))
            IERR=IC_E_NEEDMODE
            GOTO 999
          ENDIF
          IF (INDEX(CHOPT,'Y').NE.0) THEN
            OPT='Y'
          ELSE
            OPT=' '
          ENDIF
c*               queue lock temporarily to get lock info on local node
          CALL IC_TMPLOCK(C__MODE(IMODE)//NAME(1:LST(NAME)),LVB,
     +      LKID,OPT,IERR)
c*               check that N>1, or lock doesn't exist
          CALL IC_INFO_I(NAME,IMODE,LKID,NLCK,'N',IERR)
c*             For 'N' mode, err mess not returned. Used for
c*             checking lock existence.
          IF (NLCK.LE.1.AND.INDEX(CHOPT,'N').EQ.0
     +          .AND.INDEX(CHOPT,'X').EQ.0) THEN
            CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
            IERR=IC_E_NOMATCH
            GOTO 999
          ENDIF
          LOCAL_BOOK = .TRUE.
        ENDIF
      ENDIF
      LDATA(1)=0
      ICODE=-1
      NITER=0
      NMAT=0
   10 NITER=NITER+1
      LKILIST(1).BUFADR = %LOC(LOCKID(NITER))
      LKILIST(1).BUFLEN = 4
      LKILIST(1).ITMCODE = LKI$_LOCKID
      LKILIST(1).RETADR = %LOC(ILEN)
      LKILIST(2).BUFADR = %LOC(LOCKNAME(NITER))
      LKILIST(2).BUFLEN = 31
      LKILIST(2).ITMCODE = LKI$_RESNAM
      LKILIST(2).RETADR = %LOC(LKL(NITER))
      LKILIST(3).BUFADR = %LOC(LVB)
      LKILIST(3).BUFLEN = 16
      LKILIST(3).ITMCODE = LKI$_VALBLK
      LKILIST(3).RETADR = %LOC(IRETLN)
      LKILIST(4).BUFADR = %LOC(LKLS)
      LKILIST(4).BUFLEN = LKLSLN
      LKILIST(4).ITMCODE = LKI$_LOCKS
      LKILIST(4).RETADR = %LOC(IRETLN)
      LKILIST(5).BUFADR = %LOC(LK_CSID)
      LKILIST(5).BUFLEN = 4
      LKILIST(5).ITMCODE = LKI$_MSTCSID
      LKILIST(5).RETADR = %LOC(IL)
      LKILIST(6).BUFADR = %LOC(LK_CVTCOUNT)
      LKILIST(6).BUFLEN = 4
      LKILIST(6).ITMCODE = LKI$_CVTCOUNT
      LKILIST(6).RETADR = %LOC(IL)
      LKILIST(7).BUFADR = %LOC(LK_GRANTCOUNT)
      LKILIST(7).BUFLEN = 4
      LKILIST(7).ITMCODE = LKI$_GRANTCOUNT
      LKILIST(7).RETADR = %LOC(IL)
      LKILIST(8).END_LIST = 0
c*
c*       Note that this wildcard search returns duplicate records for
c*       the same lock with different lockids on the same node, generally
c*       a confused mess.
c*
      ISTAT = SYS$GETLKIW(,ICODE,LKILIST,IOSB,,,)
      IF (ISTAT.EQ.SS$_NOMORELOCK) THEN
        GOTO 11
      ELSE IF (ISTAT.NE.SS$_NORMAL) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
      ENDIF
      IF (IOSB.STS.EQ.SS$_NOMORELOCK) THEN
        GOTO 11
      ELSE IF (IOSB.STS.NE.SS$_NORMAL) THEN
        CALL LIB$SIGNAL (%VAL(IOSB.STS))
        IERR = IOSB.STS
      ENDIF
      IF (RETLEN(2).NE.0) THEN
        NENT = RETLEN(1)/RETLEN(2)
      ELSE
        NENT = 0
      ENDIF
c*
c**        decide whether we are interested in this lock
      IF ((.NOT.WILDCARD).AND.
     +      (INDEX(CHOPT,'S').EQ.0).AND.
     +      (INDEX(CHOPT,'X').EQ.0)) THEN
        IF(LOCKNAME(NITER)(5:LKL(NITER)).NE.NAME(1:LST(NAME))) GOTO 10
      ELSE
c*           be sure we haven't seen this cache name before
        IF (NITER.GT.1) THEN
          DO J=1,NITER-1
            IF (LOCKNAME(J)(1:LKL(J)).EQ.
     +            LOCKNAME(NITER)(1:LKL(NITER))) GOTO 10
          ENDDO
        ENDIF
c*           ignore non-CPC caches
        IF (LOCKNAME(NITER)(1:3).NE.'L3%') GOTO 10
        IF (INDEX(CHOPT,'S').NE.0) THEN
          IF(INDEX(LOCKNAME(NITER)(5:LKL(NITER)),
     &      NAME(1:LST(NAME))).NE.0) THEN
            NMAT=NMAT+1
            LDATA(1) = NMAT
            LDATA(1+NMAT*2-1) = LOCKID(NITER)
            CALL IC_MODE(LOCKNAME(NITER)(4:4),IM)
            LDATA(1+NMAT*2) = IM
            IF (INDEX(CHOPT,'P').NE.0) THEN
              TYPE *,'Match cache '//
     &          LOCKNAME(NITER)(1:LST(LOCKNAME(NITER))),
     +          '  ID',LOCKID(NITER),'  mode',IM
            ENDIF
          ELSE
            GOTO 10
          ENDIF
        ENDIF
        IF (INDEX(CHOPT,'X').NE.0) THEN
          IF(LOCKNAME(NITER)(5:LKL(NITER)).EQ.NAME(1:LST(NAME))) THEN
            DO K=1,NENT
              IF (LKLS(K).LKI$B_GRMODE.EQ.LCK$K_EXMODE .OR.
     +              LKLS(K).LKI$B_GRMODE.EQ.LCK$K_PWMODE) THEN
                LDATA(1)=1
                IF (INDEX(CHOPT,'P').NE.0)
     +            TYPE *,'Exclusive holder found for cache ',NAME(:
     +            LST(NAME))
                GOTO 999
              ENDIF
            ENDDO
          ELSE
            GOTO 10
          ENDIF
        ENDIF
      ENDIF
c*
c**        we are interested. Deal with it.
      MATCH = .TRUE.
      IF (INDEX(CHOPT,'C') .NE.0) THEN
        DO K=1,4
          LDATA(K)=LVB(K)
        ENDDO
        IF (INDEX(CHOPT,'P').NE.0) THEN
          TYPE '(1X,3A,4I10)','Cache contents for cache ',NAME,': ',
     +      (LDATA(M),M=1,4)
        ENDIF
      ELSE IF (INDEX(CHOPT,'N') .NE.0) THEN
        LDATA(1) = NENT
        IF (INDEX(CHOPT,'P').NE.0) THEN
          TYPE '(1X,3A,I3)','Number of processes with cache ',NAME,
     +      ' booked =',LDATA(1)
        ENDIF
      ELSE IF (INDEX(CHOPT,'I') .NE.0) THEN
        NAME = LOCKNAME(NITER)
        GOTO 999
      ELSE IF (INDEX(CHOPT,'L') .NE.0) THEN
        NOF=4
        IF (LOCKNAME(NITER)(NOF:NOF+2).EQ.'CC_') THEN
          CTYPE='CCOM'
          NOF = NOF+3
        ELSE IF (LOCKNAME(NITER)(NOF:NOF).EQ.'X') THEN
          CTYPE='MBX'
          NOF = NOF+1
        ELSE IF (LOCKNAME(NITER)(NOF:NOF).EQ.'I') THEN
          CTYPE='INFO'
          NOF = NOF+1
        ELSE IF (LOCKNAME(NITER)(NOF:NOF).EQ.'A') THEN
          CTYPE='ALARM'
          NOF = NOF+1
        ELSE IF (LOCKNAME(NITER)(NOF:NOF).EQ.'B') THEN
          CTYPE='BCAST'
          NOF = NOF+1
        ELSE IF (LOCKNAME(NITER)(NOF:NOF).EQ.'W') THEN
          CTYPE='WATCH'
          NOF = NOF+1
        ELSE IF (LOCKNAME(NITER)(NOF:NOF).EQ.'R') THEN
          CTYPE='RESLK'
          NOF = NOF+1
        ELSE
          CTYPE='???'
        ENDIF
        IF (LOCKNAME(NITER)(NOF:NOF+1).EQ.'S_') THEN
          CTYPE=CTYPE(1:LST(CTYPE))//'_S'
C            NOF = NOF+2
        ENDIF
        TYPE '(2X,2A,4Z9.8)',
     +    LOCKNAME(NITER)(NOF:MIN(LEN(LOCKNAME(NITER)),25)),
     +    ' Contents ',(LVB(K),K=1,4)
c**            this always returns the local node, not the master node
        NODE = ' '
        ICPU = LK_CSID
        LKILIST(1).BUFADR = %LOC(NODE)
        LKILIST(1).BUFLEN = 15
        LKILIST(1).ITMCODE = SYI$_NODENAME
        LKILIST(1).RETADR = %LOC(LN)
        LKILIST(2).END_LIST = 0
        ISTAT = SYS$GETSYIW(,ICPU,,LKILIST,IOSB,,)
        IF (.NOT.ISTAT) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
        ENDIF
        TYPE '(6X,A,I4,A,I4)','Type '//CTYPE//'  Owner node '//NODE(1:6)
     &    //'           CVT queue:',LK_CVTCOUNT,'  GRANT:',LK_GRANTCOUNT
c*           loop over entries
        DO K=1,NENT
C              same as NODE above
C            ICPU = LKLS(K).LKI$L_SYSID
C            LKILIST(1).BUFADR = %LOC(NODE)
C            LKILIST(1).BUFLEN = 15
C            LKILIST(1).ITMCODE = SYI$_NODENAME
C            LKILIST(1).RETADR = %LOC(LN)
C            LKILIST(2).END_LIST = 0
C            ISTAT = SYS$GETSYIW(,ICPU,,LKILIST,IOSB,,)
C            IF (.NOT.ISTAT) THEN
C              CALL LIB$SIGNAL (%VAL(ISTAT))
C              IERR = ISTAT
C            ENDIF
          ICPU = LKLS(K).LKI$L_REMSYSID
          LKILIST(1).BUFADR = %LOC(RNODE)
          LKILIST(1).BUFLEN = 15
          LKILIST(1).ITMCODE = SYI$_NODENAME
          LKILIST(1).RETADR = %LOC(LRN)
          LKILIST(2).END_LIST = 0
          ISTAT = SYS$GETSYIW(,ICPU,,LKILIST,IOSB,,)
          IF (.NOT.ISTAT) THEN
            CALL LIB$SIGNAL (%VAL(ISTAT))
            IERR = ISTAT
          ENDIF
          ISTAT = LIB$GETJPI(JPI$_PRCNAM,LKLS(K).LKI$L_PID,,IDUM,PROC,
     &      LP)
          LP = MIN(LP,13)
          IF (ISTAT.EQ.SS$_NONEXPR) THEN
            PROC = 'Not available'
            LP = 13
          ELSE IF (ISTAT.EQ.SS$_NOPRIV) THEN
            PROC = 'No privilege'
            LP = 12
          ELSE IF (.NOT.ISTAT) THEN
            CALL LIB$SIGNAL (%VAL(ISTAT))
            IERR = ISTAT
          ENDIF
          IF (LKLS(K).LKI$B_RQMODE.EQ.LCK$K_NLMODE) THEN
            RQMODE = ' NL '
          ELSE IF (LKLS(K).LKI$B_RQMODE.EQ.LCK$K_CRMODE) THEN
            RQMODE = ' CR '
          ELSE IF (LKLS(K).LKI$B_RQMODE.EQ.LCK$K_EXMODE) THEN
            RQMODE = ' EX '
          ELSE IF (LKLS(K).LKI$B_RQMODE.EQ.LCK$K_PWMODE) THEN
            RQMODE = ' PW '
          ELSE
            CALL LIB$SIGNAL(%VAL(IC_E_BADMODE),%VAL(2),'RQ',
     +        %VAL(LKLS(K).LKI$B_RQMODE))
            RQMODE = ' ?? '
          ENDIF
          IF (LKLS(K).LKI$B_GRMODE.EQ.LCK$K_NLMODE) THEN
            GRMODE = ' NL '
          ELSE IF (LKLS(K).LKI$B_GRMODE.EQ.LCK$K_CRMODE) THEN
            GRMODE = ' CR '
          ELSE IF (LKLS(K).LKI$B_GRMODE.EQ.LCK$K_EXMODE) THEN
            GRMODE = ' EX '
          ELSE IF (LKLS(K).LKI$B_GRMODE.EQ.LCK$K_PWMODE) THEN
            GRMODE = ' PW '
          ELSE
            CALL LIB$SIGNAL(%VAL(IC_E_BADMODE),%VAL(2),'GR',
     +        %VAL(LKLS(K).LKI$B_GRMODE))
            GRMODE = ' ?? '
          ENDIF
          IF (LKLS(K).LKI$B_QUEUE.EQ.LCK$C_GRANTED) THEN
            QUEUE = 'granted'
          ELSE IF (LKLS(K).LKI$B_QUEUE.EQ.LCK$C_CONVERT) THEN
            QUEUE = 'convert'
          ELSE IF (LKLS(K).LKI$B_QUEUE.EQ.LCK$C_WAITING) THEN
            QUEUE = 'waiting'
          ELSE
            QUEUE = 'none   '
          ENDIF
          TYPE *,'                  ',RQMODE,'by ',RNODE(1:LN),'::',
     +      PROC(1:LP),BLANK(1:13-LP),'  Gr:',GRMODE,'Que: ',QUEUE
        ENDDO
      ENDIF
c**        loop, if wildcarding or matching
      IF (WILDCARD.OR.(INDEX(CHOPT,'S').NE.0)) THEN
        GOTO 10
      ELSE
      ENDIF
   11 CONTINUE
      IF ((.NOT.MATCH).AND.(INDEX(CHOPT,'M').EQ.0).AND.
     +      (.NOT.WILDCARD)) THEN
        IF (INDEX(CHOPT,'N').NE.0.OR.INDEX(CHOPT,'X').NE.0) THEN
          LDATA(1) = 0
          GOTO 999
        ENDIF
        CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_NOMATCH
      ENDIF
  999 IF (LOCAL_BOOK) THEN
c*         if the lock was queued just to get info, drop it now.
        ISTAT = SYS$DEQ(%VAL(LKID),,,)
        IF (ISTAT.NE.SS$_NORMAL) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
          GOTO 999
        ENDIF
c*           for 'N' mode, subtract off the lock held by this process
        IF (INDEX(CHOPT,'N').NE.0) THEN
          LDATA(1)=LDATA(1)-1
        ENDIF
      ENDIF
      END
c************************************************************************
      SUBROUTINE IC_INFO_I(NAME,IMODE,LKID,LDATA,CHOPT,IERR)
      CHARACTER*(*) NAME,CHOPT
      CHARACTER INTNAME*31
      INTEGER LDATA(*)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($LKIDEF)/LIST'
      INCLUDE '($SSDEF)'
      INTEGER SYS$GETLKIW, SYS$GETSYIW, LIB$GETJPI
      CHARACTER NODE*16, RNODE*16, PROC*15, RQMODE*4, GRMODE*4, QUEUE*7
      CHARACTER BLANK*20 /'                    '/, LOCKNAME*31, CTYPE*7
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 BUFLEN
            INTEGER*2 ITMCODE
            INTEGER BUFADR
            INTEGER RETADR
          END MAP
          MAP
            INTEGER END_LIST
          END MAP
        END UNION
      END STRUCTURE
      RECORD /ITMLST/ LKILIST(10)
      STRUCTURE /IOSBLK/
        INTEGER STS, RESERVED
      END STRUCTURE
      RECORD /IOSBLK/ IOSB
      RECORD /LKIDEF/ LKLS(30)
      PARAMETER (LKLSLN=690)
      INTEGER IRETLN
      INTEGER*2 RETLEN(2)
      EQUIVALENCE(IRETLN,RETLEN)
c*
      IERR=0
      IF (LKID.EQ.0) THEN
CCC          CALL IC_ID_NUM(NAME,NUM,' ',IERR)
        DO I=1,NCACHE
          IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
            NUM=I
            IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
          ENDIF
        ENDDO
        CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_NOMATCH
    1   CONTINUE
        LOCKID = CACHE(NUM).LKST.ID
      ELSE
        LOCKID = LKID
      ENDIF
      IF (IERR.NE.0) GOTO 999
      IF (INDEX(CHOPT,'N') .NE.0) THEN
        LKILIST(1).BUFADR = %LOC(LKLS)
        LKILIST(1).BUFLEN = LKLSLN
        LKILIST(1).ITMCODE = LKI$_LOCKS
        LKILIST(1).RETADR = %LOC(IRETLN)
        LKILIST(2).END_LIST = 0
        ISTAT = SYS$GETLKIW(,LOCKID,LKILIST,IOSB,,,)
        IF (ISTAT.NE.SS$_NORMAL) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
        ENDIF
        IF (RETLEN(2).NE.0) THEN
          LDATA(1) = RETLEN(1)/RETLEN(2)
        ELSE
          LDATA(1) = 0
        ENDIF
        IF (INDEX(CHOPT,'P').NE.0) THEN
          TYPE '(1X,3A,I3)','Number of processes with cache ',NAME,
     +      ' booked =',LDATA(1)
        ENDIF
      ELSE IF (INDEX(CHOPT,'I') .NE.0) THEN
c*           get lock name
        LKILIST(1).BUFADR = %LOC(INTNAME)
        LKILIST(1).BUFLEN = 31
        LKILIST(1).ITMCODE = LKI$_RESNAM
        LKILIST(1).RETADR = %LOC(NLEN)
        LKILIST(2).END_LIST = 0
        ISTAT = SYS$GETLKIW(,LOCKID,LKILIST,IOSB,,,)
        IF (ISTAT.NE.SS$_NORMAL) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
        ENDIF
        NAME=INTNAME
        IF (INDEX(CHOPT,'P').NE.0) THEN
          TYPE '(1X,3A,I3)','Name of cache with ID number ',LOCKID,
     &      ' is ', NAME(1:NLEN)
        ENDIF
      ENDIF
  999 END
c************************************************************************
      SUBROUTINE IC_GET_CACHE(NAME,LVB,CHOPT,IERR)
      CHARACTER*(*) NAME,CHOPT
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INTEGER LVB(4)
c*
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
    1 CONTINUE
      IF (IERR.NE.0) GOTO 999
      CALL IC_GET_CACHE_I(NUM,LVB,CHOPT,IERR)
  999 END

      SUBROUTINE IC_GET_CACHE_I(NUM,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_GET_CACHE
c*
c*       Routine to read the contents of a BOOKED cache fast, using
c*       $GETLKI.
c*
c************************************************************************
      CHARACTER*(*) CHOPT
      INTEGER LVB(4)
      INCLUDE '($LKIDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 BUFLEN
            INTEGER*2 ITMCODE
            INTEGER BUFADR
            INTEGER RETADR
          END MAP
          MAP
            INTEGER END_LIST
          END MAP
        END UNION
      END STRUCTURE
      RECORD /ITMLST/ LKILIST(2)
      STRUCTURE /IOSBLK/
        INTEGER STS, RESERVED
      END STRUCTURE
      RECORD /IOSBLK/ IOSB
      INTEGER SYS$GETLKIW
c*
      IERR=0

      LKILIST(1).BUFADR = %LOC(LVB)
      LKILIST(1).BUFLEN = 16
      LKILIST(1).ITMCODE = LKI$_VALBLK
      LKILIST(1).RETADR = %LOC(IRETLN)
      LKILIST(2).END_LIST = 0
      ISTAT = SYS$GETLKIW(,CACHE(NUM).LKST.ID,LKILIST,IOSB,,,)
      IF (ISTAT.EQ.SS$_IVLOCKID) THEN
        OPEN(UNIT=6,TYPE='OLD',NAME='TT:')
        WRITE(6,*)'IC_GET_CACHE: Invalid lock ID on cache '//
     +    '! Try IC_INFO.'
        CLOSE(6)
        IMODE = CACHE(NUM).MODE
        CALL IC_INFO(NAME,IMODE,LVB,'C',IERR)
      ELSE IF (ISTAT.NE.SS$_NORMAL) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
      ENDIF
  999 END
      LOGICAL FUNCTION IC_EXIST(NAME,IMODE,CHOPT,IERR)
c************************************************************************
c*
c*       FUNC. IC_EXIST
c*
c*       Routine to check on existence of a cache.
c*
c*       IMODE must be provided if the cache NAME has not been booked.
c*       If the cache has been booked (only relevant with the 'X' option)
c*       IMODE can just be zero.
c*
c*       Options:
c*         'X'   returns TRUE only if the exclusive holder exists
c*               (master or process lock). Only relevant for mailbox and
c*               process watcher usage modes.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      LOGICAL IC_EXISTX
c*
      IERR=0
      IC_EXIST=.FALSE.
      IF (INDEX(CHOPT,'X').NE.0) THEN
        IC_EXIST = IC_EXISTX(NAME,IMODE,CHOPT,IERR)
      ELSE
        CALL IC_INFO(NAME,IMODE,NH,'N',IERR)
        IF (NH.GT.0) IC_EXIST=.TRUE.
      ENDIF
  999 END

      LOGICAL FUNCTION IC_EXISTX(NAME,IMODE,CHOPT,IERR)
c************************************************************************
c*
c*       FUNC. IC_EXISTX
c*
c*       Checks if exclusive holder exists
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($LCKDEF)'
      INCLUDE '($SSDEF)'
      LOGICAL IC_BOOKED
      RECORD /ICLSTB/ LKSTBK       ! lock status block
      INTEGER SYS$ENQ, SYS$DEQ
      LOGICAL EXISTING
c*
      IERR=0
      IC_EXISTX=.FALSE.
      CALL IC_ID_NUM(NAME,NUM,'N',IERR)
      EXISTING = NUM.NE.0
      IF (EXISTING) EXISTING = CACHE(NUM).LOCK .NE.0
      IF (EXISTING) THEN
        IF (CACHE(NUM).MODE.EQ.IC__WATCHER) NUM=NUM-1
        LKSTBK = CACHE(NUM).LKST
        IMASK = LCK$M_NOQUEUE .OR. LCK$M_CONVERT
        IMD = CACHE(NUM).MODE
      ELSE
        IF (NUM.NE.0) THEN
          IMD = CACHE(NUM).MODE
        ELSE
          IF (IMODE.LE.0.OR.IMODE.GT.IC__NMODES) THEN
            TYPE *,'*** IC_EXISTX ERROR:  BAD IMODE=',IMODE
            RETURN
          ENDIF
          IMD = IMODE
        ENDIF
        IF (IMD.EQ.IC__WATCHER) THEN
          LKSTBK.NAME = C__MODE(IMD)//'S_'//NAME(1:LST(NAME))
        ELSE
          LKSTBK.NAME = C__MODE(IMD)//NAME(1:LST(NAME))
        ENDIF
        IMASK = LCK$M_NOQUEUE
        ISTAT = SYS$ENQ (
     +    ,%VAL(IC__NL),LKSTBK,
     +    %VAL(IMASK),'L3%'//LKSTBK.NAME,,,,,,)
        IMASK = LCK$M_NOQUEUE .OR. LCK$M_CONVERT
      ENDIF
      IF (ISTAT.NE.SS$_NOTQUEUED) ISTAT = SYS$ENQ (
     +    ,%VAL(IC__EX),LKSTBK,
     +    %VAL(IMASK),'L3%'//LKSTBK.NAME,,,,,,)
D     CALL LIB$SIGNAL(%VAL(ISTAT))
D     CALL LIB$SIGNAL(%VAL(LKSTBK.STATUS))
      IF (ISTAT.EQ.SS$_NOTQUEUED) THEN
D       TYPE *,'TRUE DUE TO NOTQUEUED'
        IC_EXISTX = .TRUE.
      ELSE IF (.NOT.ISTAT) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ELSE
        IF (EXISTING) THEN
          ISTAT = SYS$ENQ (
     +      ,%VAL(CACHE(NUM).LOCK),LKSTBK,
     +      %VAL(LCK$M_CONVERT),'L3%'//LKSTBK.NAME(:LST(LKSTBK.
     +      NAME)),,,,,,)
        ELSE
          ISTAT = SYS$DEQ(%VAL(LKSTBK.ID),,,)
        ENDIF
        IF (.NOT.ISTAT) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
          GOTO 999
        ENDIF
        IF (.NOT.IC_EXISTX) THEN
          CALL IC_INFO(LKSTBK.NAME(2:),IMD,NH,'X',IERR)  !$$$
          IF (NH.GT.0) THEN
D           TYPE *,'TRUE DUE TO COUNT'
            IC_EXISTX=.TRUE.
          ENDIF
        ENDIF
      ENDIF
  999 END

      SUBROUTINE IC_READ(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_READ
c*
c*       Read the cache NAME. Just do reconversion to current mode
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
c*
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      TYPE *,'NUmber of caches =',NCACHE
      IERR=IC_E_NOMATCH
    1 CONTINUE
c*
      IF (IERR.NE.0) GOTO 999
c**          for watchers, a lock conversion cannot be used because
c**          the 'watch' is an already pending lock conversion.
      IF (CACHE(NUM).TYPE.EQ.IC__PSTAT .OR.
     +      CACHE(NUM).LOCK.EQ.IC__EX) THEN
c**          in EX, cannot do conversion, or LVB will be WRITTEN
        CALL IC_GET_CACHE_I(NUM,LVB,' ',IERR)
      ELSE
        CALL IC_LOCKUP(CACHE(NUM).LOCK,NUM,LVB,' ',0.,IERR)
        IF (IERR.NE.0) GOTO 999
        IF (CACHE(NUM).MODE.EQ.IC__INFOCACHE.AND.
     +        CACHE(NUM).TYPE.EQ.IC__INFODBL) THEN
c**            for double length info caches, read the other 16 bytes
          CALL IC_LOCKUP(CACHE(NUM).LOCK,NUM-1,LVB(5),' ',0.,IERR)
        ENDIF
      ENDIF
  999 END
      SUBROUTINE IC_BROADCAST(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_BROADCAST
c*
c*       Write LVB to the cache NAME, broadcast mode. Convert to EX
c*       and back to CR mode.
c*
c* Need for AST deferment:
c If A sends 2 commands in rapid succession to B, and B
c replies to each with IC_ALARM, B can be inside the IC_ALARM after the
c first command when the second command arrives. Result: B holds the
c alarm cache of A in EX mode at normal level, while it is in IC_SEND
c at AST level. A is stuck in GETBC which fails because B cannot complete
c the IC_SEND IT is stuck in. Result: the MBX cache now clears itself up OK
c but A loses its BAST on its alarm cache because it was unable to get
c CR mode.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4)
      LOGICAL LIB$AST_IN_PROG
      IF (LIB$AST_IN_PROG()) THEN
        CALL IC_BROADCAST_A(NAME,LVB,CHOPT,IERR)
      ELSE
        CALL IC_BROADCAST_N(NAME,LVB,CHOPT,IERR)
      ENDIF
      END
      SUBROUTINE IC_ALARM(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_ALARM
c*
c*       Write LVB to the cache NAME, broadcast mode. Convert to EX
c*       and back to CR mode.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4)
      LOGICAL LIB$AST_IN_PROG
      IF (LIB$AST_IN_PROG()) THEN
        CALL IC_BROADCAST_A(NAME,LVB,CHOPT,IERR)
      ELSE
        CALL IC_BROADCAST_N(NAME,LVB,CHOPT,IERR)
      ENDIF
      END
      SUBROUTINE IC_BROADCAST_A(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_BROADCAST
c*
c*       Write LVB to the cache NAME, broadcast mode. Convert to EX
c*       and back to CR mode.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4), LVBR(4), LVBDUM(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($SSDEF)'
      INCLUDE '($LCKDEF)'
      INTEGER SYS$DEQ
      COMMON /IC_C_BCASTW/ BCAST_WAIT
      LOGICAL BCAST_WAIT
      CHARACTER DATETIME*23
c*
      IERR=0
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
      CALL IC_LIST_CACHES
    1 CONTINUE
c*
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__BROADCAST.AND.
     +      CACHE(NUM).MODE.NE.IC__ALARM) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_NOBCAST),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_NOBCAST
        GOTO 999
      ENDIF
c*         get EX on system cache to reserve. (any previous broadcasts are
c*         finished when EX is obtained, because the last thing receivers
c*         do is release CR on the system cache)
c*
c**        dequeue first, to ensure that a broadcast in progress is
c*         not interrupted (this must not interrupt the EX->PW conversion
c*         of another broadcast)
      BCAST_WAIT=.TRUE.
      ISTAT = SYS$DEQ(%VAL(CACHE(NUM-1).LKST.ID),,,)
      IF (ISTAT.EQ.SS$_IVLOCKID) THEN
c**        lock already dropped, perhaps from a previous bcast attempt
c**        that failed on the -->EX new lock. Proceed.
      ELSE IF (ISTAT.NE.SS$_NORMAL) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 998
      ENDIF
      CALL IC_LOCKUP_A(IC__EX,NUM-1,LVBR,'N',0.,IERR)
      IF (.NOT.BCAST_WAIT) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_BCOUTINT),%VAL(1),NAME(:LST(NAME)))
        IERR = IC_E_BCOUTINT
        GOTO 998
      ENDIF
      BCAST_WAIT=.FALSE.
      IF (IERR.NE.0) THEN
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        CALL IC_INFO('S_'//NAME,0,LVBDUM,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBDUM,'LP',IERR)
        GOTO 999
      ENDIF
c*         mark as used, converting to PW to allow others to get CR
      IF (LVBR(4).EQ.1) THEN
c**          error: marked as in use
        CALL LIB$SIGNAL(%VAL(IC_E_BCBUSY),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_BCBUSY
        GOTO 998
      ENDIF
      LVBR(4)=1
c*         in element 3, keep a number identifying this broadcast
      IDBC = LVBR(3) +1
      LVBR(3)=MOD(IDBC,30000)
      CALL IC_LOCKUP_A(IC__PW,NUM-1,LVBR,' ',0.,IERR)
      IF (IERR.NE.0) GOTO 998
c*         Convert to EX on primary, triggering BASTS.
c**        In EX, suppress the BAST of this process, so that its bcast
c*         handler will not be called
      ITER=0
   10 CALL IC_LOCKUP_A(IC__EX,NUM,LVBDUM,'B',0.,IERR)
      IF (IERR.NE.0) THEN
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        CALL IC_INFO('S_'//NAME,0,LVBDUM,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBDUM,'LP',IERR)
        IF (IERR.EQ.IC_E_LKFAIL) THEN
c**            more than one chance
          ITER=ITER+1
          IF (ITER.LE.0) THEN
            TYPE *,'Repeat, ITER',ITER
            GOTO 10
          ENDIF
          GOTO 997
        ELSE
          GOTO 997
        ENDIF
      ENDIF
c*         write LVB to the primary and back to CR
  997 IF (CACHE(NUM).TYPE.EQ.IC__ATASK) THEN
        CALL IC_LOCKUP_A(IC__NL,NUM,LVB,' ',0.,IERR)
      ELSE
        CALL IC_LOCKUP_A(IC__CR,NUM,LVB,' ',0.,IERR)
      ENDIF
c*         requeue system (give other broadcasters a chance), mark as unused
  998 LVBR(4)=0
c*         back to NL on system cache, writing unused bit
      CALL IC_LOCKUP_A(IC__NL,NUM-1,LVBR,' ',0.,IERR1)
      IF (IERR.EQ.0) IERR = IERR1
  999 END
      SUBROUTINE IC_BROADCAST_N(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_BROADCAST
c*
c*       Write LVB to the cache NAME, broadcast mode. Convert to EX
c*       and back to CR mode.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4), LVBR(4), LVBDUM(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($SSDEF)'
      INCLUDE '($LCKDEF)'
      INTEGER SYS$DEQ
      COMMON /IC_C_BCASTW/ BCAST_WAIT
      LOGICAL BCAST_WAIT
      CHARACTER DATETIME*23
c*
c**        disable ASTs during the broadcast
      ISTAT = SYS$SETAST(%VAL(0))
c*
      IERR=0
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
      CALL IC_LIST_CACHES
    1 CONTINUE
c*
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__BROADCAST.AND.
     +      CACHE(NUM).MODE.NE.IC__ALARM) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_NOBCAST),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_NOBCAST
        GOTO 999
      ENDIF
c*         get EX on system cache to reserve. (any previous broadcasts are
c*         finished when EX is obtained, because the last thing receivers
c*         do is release CR on the system cache)
c*
c**        dequeue first, to ensure that a broadcast in progress is
c*         not interrupted (this must not interrupt the EX->PW conversion
c*         of another broadcast)
      BCAST_WAIT=.TRUE.
      ISTAT = SYS$DEQ(%VAL(CACHE(NUM-1).LKST.ID),,,)
      IF (ISTAT.EQ.SS$_IVLOCKID) THEN
c**        lock already dropped, perhaps from a previous bcast attempt
c**        that failed on the -->EX new lock. Proceed.
      ELSE IF (ISTAT.NE.SS$_NORMAL) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 998
      ENDIF
      CALL IC_LOCKUP_N(IC__EX,NUM-1,LVBR,'N',0.,IERR)
      IF (.NOT.BCAST_WAIT) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_BCOUTINT),%VAL(1),NAME(:LST(NAME)))
        IERR = IC_E_BCOUTINT
        GOTO 998
      ENDIF
      BCAST_WAIT=.FALSE.
      IF (IERR.NE.0) THEN
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        CALL IC_INFO('S_'//NAME,0,LVBDUM,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBDUM,'LP',IERR)
        GOTO 999
      ENDIF
c*         mark as used, converting to PW to allow others to get CR
      IF (LVBR(4).EQ.1) THEN
c**          error: marked as in use
        CALL LIB$SIGNAL(%VAL(IC_E_BCBUSY),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_BCBUSY
        GOTO 998
      ENDIF
      LVBR(4)=1
c*         in element 3, keep a number identifying this broadcast
      IDBC = LVBR(3) +1
      LVBR(3)=MOD(IDBC,30000)
      CALL IC_LOCKUP_N(IC__PW,NUM-1,LVBR,' ',0.,IERR)
      IF (IERR.NE.0) GOTO 998
c*         Convert to EX on primary, triggering BASTS.
c**        In EX, suppress the BAST of this process, so that its bcast
c*         handler will not be called
      ITER=0
   10 CALL IC_LOCKUP_N(IC__EX,NUM,LVBDUM,'B',0.,IERR)
      IF (IERR.NE.0) THEN
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        CALL IC_INFO('S_'//NAME,0,LVBDUM,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBDUM,'LP',IERR)
        IF (IERR.EQ.IC_E_LKFAIL) THEN
c**            more than one chance
          ITER=ITER+1
          IF (ITER.LE.0) THEN
            TYPE *,'Repeat, ITER',ITER
            GOTO 10
          ENDIF
          GOTO 997
        ELSE
          GOTO 997
        ENDIF
      ENDIF
c*         write LVB to the primary and back to CR
  997 IF (CACHE(NUM).TYPE.EQ.IC__ATASK) THEN
        CALL IC_LOCKUP_A(IC__NL,NUM,LVB,' ',0.,IERR)
      ELSE
        CALL IC_LOCKUP_A(IC__CR,NUM,LVB,' ',0.,IERR)
      ENDIF
c*         requeue system (give other broadcasters a chance), mark as unused
  998 LVBR(4)=0
c*         back to NL on system cache, writing unused bit
      CALL IC_LOCKUP_N(IC__NL,NUM-1,LVBR,' ',0.,IERR1)
      IF (IERR.EQ.0) IERR = IERR1
  999 CONTINUE
c*
c**        reenable ASTs after the broadcast
      ISTAT = SYS$SETAST(%VAL(1))
c*
      END
      SUBROUTINE IC_WRITE(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_WRITE
c*
c*       Write LVB to the cache NAME. Convert to EX and back to
c*       current mode.
c*
c*       Option S:  system. Do not do cache mode checking.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4), LVBR(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
    1 CONTINUE
c*
      IF (IERR.NE.0) GOTO 999
      IF (INDEX(CHOPT,'S').EQ.0.AND.CACHE(NUM).MODE.NE.IC__INFOCACHE
     +    .AND.CACHE(NUM).MODE.NE.IC__WATCHER) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
      IMODE = CACHE(NUM).LOCK
      IF (IMODE.NE.IC__EX) THEN
c*           if not already in EX mode, convert to EX mode
        CALL IC_LOCKUP(IC__EX,NUM,LVBR,' ',0.,IERR)
        IF (IERR.NE.0) GOTO 999
      ENDIF
      CALL IC_LOCKUP(IMODE,NUM,LVB,' ',0.,IERR)
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).MODE.EQ.IC__INFOCACHE.AND.
     +      CACHE(NUM).TYPE.EQ.IC__INFODBL) THEN
c**          for double length info caches, write the other 16 bytes
        CALL IC_LOCKUP(IC__EX,NUM-1,LVBR,' ',0.,IERR)
        IF (IERR.NE.0) GOTO 999
        CALL IC_LOCKUP(IMODE,NUM-1,LVB(5),' ',0.,IERR)
      ENDIF
  999 END
      SUBROUTINE IC_WAIT(NAME,LVB,TIMEOUT,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_WAIT
c*
c*       Hibernate until the cache NAME is loaded by another program
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      EXTERNAL IC_WTAST
c*
      IERR=0
      CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__BROADCAST.AND.
     +      CACHE(NUM).TYPE.NE.IC__ASERV.AND.
     +      CACHE(NUM).TYPE.NE.IC__MASTER  ) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
c*         wait for blocking-AST
      CACHE(NUM).BAST = %LOC(IC_WTAST)
      CALL IC_LKWAIT(IC__CR,NUM,LVB,TIMEOUT,' ',IERR)
      IF (IERR.NE.0) GOTO 999
  999 END
      SUBROUTINE IC_ASSOCIATE(NAME,LVB,IROUTINE,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_ASSOCIATE
c*
c*       Event flag EF is set when cache NAME is loaded by another
c*       program. Current contents of LVB is returned.
c*       Inside the AST routine, IC_DELIVER must be called to retrieve
c*       the latest cache contents.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      EXTERNAL IC_CALL_UAST
      INTEGER LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
      CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__BROADCAST.AND.
     +      CACHE(NUM).TYPE.NE.IC__ASERV.AND.
     +      CACHE(NUM).TYPE.NE.IC__MASTER  ) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
c*         set blocking AST
      IF (CACHE(NUM).TYPE.EQ.IC__MASTER) THEN
c*           lock is held in EX mode by masters
        CACHE(NUM).BAST=%LOC(IROUTINE)
        CALL IC_LOCKUP(IC__EX,NUM,LVB,' ',0.,IERR)
      ELSE
c*           lock is held in CR mode by broadcasters, alarm servers.
c*
c*           user AST is called indirectly from IC_CALL_UAST
        CACHE(NUM).UAST=%LOC(IROUTINE)
        CACHE(NUM).BAST=%LOC(IC_CALL_UAST)
        CALL IC_LOCKUP(IC__CR,NUM,LVB,' ',0.,IERR)
      ENDIF
      IF (IERR.NE.0) GOTO 999
  999 END
      SUBROUTINE IC_SEND(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_SEND
c*
c*       Used by a cache master, inside a cache's AST routine, to send
c*       data to a querying client in a signal operation
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
    1 CONTINUE
c*
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__MASTER) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_NONMASTER),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_NONMASTER
        GOTO 999
      ENDIF
c*         convert to NL mode, allowing sender to get EX mode,
c*         and writing LVB
      CALL IC_LOCKUP_A(IC__NL,NUM,LVB,' ',0.,IERR)
c*         reconvert to EX mode, with blocking AST
D     TYPE *,'IC_SEND: reconvert to EX after ->NL. CACHE = ',NAME
      NITER=0
    2 CALL IC_LOCKUP_A(IC__EX,NUM,LVB,' ',0.,IERR)
      IF (IERR.EQ.IC_E_LKFAIL) THEN
c**        ohhh ohhh. Cluster freeze? Little excuse for a timeout here.
        IF (NITER.GT.5) THEN
c**          well damn. Settle for CR.
          TYPE *,'IC_SEND: Give up! Settle for CR'
          CALL IC_LOCKUP_A(IC__CR,NUM,LVB,' ',0.,IERR)
          GOTO 999
        ENDIF
        TYPE *,
     &    'IC_SEND: ERROR cannot get back EX mode. Retrying  '//NAME
        NITER=NITER+1
        GOTO 2
      ENDIF
  999 END
      SUBROUTINE IC_DELIVER(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_DELIVER
c*
c*       Delivers the cache LVB to the caller.
c*         Options:
c*            C     return current cache value from the cache data
c*                  structure
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4), LVBR(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($SSDEF)'
c*
      IERR=0
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          GOTO 1
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
    1 CONTINUE
c*
      IF (IERR.NE.0) GOTO 999
c*         ignore bogus ints from PSTAT crash watcher releases
      IF (CACHE(NUM).STAT.EQ.IC__RELEASING) THEN
        IERR = IC_E_INACTIVE
        RETURN
      ENDIF
      IF (INDEX(CHOPT,'C').NE.0) THEN
c*           read current cache. No lock conversion.
D       TYPE *,'IC_DELIVER: from structure. CACHE = ',NAME
        DO I=1,4
          LVB(I)=CACHE(NUM).LKST.LVB(I)
        ENDDO
      ELSE IF (CACHE(NUM).TYPE.EQ.IC__PSTAT) THEN
c*           deliver process status.
D       TYPE *,'IC_DELIVER: Pstat. CACHE = ',NAME
        CALL IC_GET_CACHE_I(NUM,LVB,' ',IERR)
c*           turn off the ENQ-obtained AST
        CACHE(NUM-1).AST=0
c*           convert to NL mode unless EX is held
c*           exclusive 'X' mode was not used in IC_WATCH)
        IF (CACHE(NUM-1).LOCK.NE.IC__EX) THEN
          CALL IC_LOCKUP_A(IC__NL,NUM-1,LVBR,' ',0.,IERR)
        ENDIF
      ELSE IF (CACHE(NUM).TYPE.EQ.IC__BROADCAST.OR.
     +           CACHE(NUM).TYPE.EQ.IC__ASERV) THEN
c*           get broadcast or alarm message
D       TYPE *,'IC_DELIVER: broadcast. CACHE = ',NAME
        CALL IC_GETBC(NAME,LVB,' ',IERR)
        IF (IERR.NE.0) GOTO 999
      ELSE IF (CACHE(NUM).TYPE.EQ.IC__MASTER) THEN
c*           read data from LVB of system cache
        CALL IC_READ('S_'//NAME,LVBR,'S',IERR)
        IF (IERR.NE.0) GOTO 999
D       TYPE *,'IC_DELIVER: master. CACHE = ',NAME
c*           client has sent info in system LVB; read it.
        DO I=1,4
          LVB(I)=LVBR(I)
        ENDDO
      ELSE
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
  999 END
      SUBROUTINE IC_SIGNAL(NAME,LVB,TIMEOUT,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_SIGNAL
c*
c*       For signalling from client to master.
c*       Communication is 2-way; LVB is sent to master, and on return
c*       LVB contains the master's response. The master must call
c*       IC_DELIVER and IC_SEND in its AST routine associated with
c*       the cache.
c*
c*       Options: A   autobooking. The cache need not be (but may have
c*                    been) previously booked. If cache was not
c*                    previously booked, it is booked, the signal is
c*                    sent, and the cache is released before return.
c*                    NOTE of course the signalling takes more time
c*                    if the booking/releasing must be done.
c*   $$$          'R' to release after autobook?
c*
c*   Lockhandling:
c*                                           Sender           Master
c*                                      PRIMARY  SYSTEM  PRIMARY  SYSTEM
c* - At start:                            NL       NL      EX       NL
c* - Client converts to EX on system to   NL     ->EX      EX       NL
c*   lock out other senders. No BAST.
c* - Client loads system with message     NL     ->EX      EX       NL
c* - Client requests EX on primary to   ->EX       EX     <BAST>    NL
c*   trigger master's BAST
c* - Master reads message               ->EX       EX     <BAST>  ->NL
c* - Master loads reply in primary      ->EX       EX     ->NL      NL
c*   and converts to NL to send reply
c* - Client gets EX on primary, reads     EX       EX       NL      NL
c*   reply, and waits for BAST
c* - Master converts to EX on primary    <BAST>    EX     ->EX      NL
c*   triggering BAST in client
c* - Client requeues primary in NL       <DEQ>     EX     ->EX      NL
c* - Client converts system cache to      NL     ->NL       EX      NL
c*   NL, completing the operation.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4), LVBR(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      EXTERNAL IC_SIGAST
      COMMON /CIC_AST/ ICASTFL, HIBERFL
      LOGICAL ICASTFL, HIBERFL, STIMOUT
c*
      EXTERNAL LIB$K_DELTA_SECONDS_F
      INTEGER LIB$CVTF_TO_INTERNAL_TIME, SYS$SETIMR, SYS$WAITFR
      INTEGER SYS$CANTIM, LIB$GET_EF, LIB$FREE_EF, SYS$CLREF
      INTEGER ITIME(2), ITID /324872/, LVBDUM(4)
      LOGICAL FIRST /.TRUE./
      COMMON /C_IC_SIGNAL_IEF/ IEF
c*
      CHARACTER*23 DATETIME
      LOGICAL LIB$AST_IN_PROG
c*
      IF (LIB$AST_IN_PROG()) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_ASTLVL),%VAL(1),NAME(:LST(NAME)))
        IERR = IC_E_ASTLVL
        GOTO 999
      ENDIF
      IF (FIRST) THEN
        FIRST = .FALSE.
        ISTAT = LIB$GET_EF(IEF)
        IF (ISTAT.NE.1) CALL LIB$SIGNAL(ISTAT)
      ENDIF
      IERR=0
      IF (TIMEOUT.GT.0.0) THEN
        TMOUT = TIMEOUT
      ELSE
        TMOUT = -1.
      ENDIF
      HIBERFL=.FALSE.
      ICASTFL=.FALSE.
      STIMOUT=.FALSE.
      IF (INDEX(CHOPT,'A').NE.0) THEN
c**        autobooking
      ENDIF
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 2
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
    2 CONTINUE
c*
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__CLIENT) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
      IMODE = CACHE(NUM).LOCK
      IF (IMODE.EQ.IC__EX) THEN
c*           attempted write while in EX mode. Master cannot signal
c*           itself.
        CALL LIB$SIGNAL(%VAL(IC_E_EXWRITE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_EXWRITE
        GOTO 999
      ENDIF
c*
c*   Need to dequeue here, like IC_BROADCAST, to protect against
c*   another would-be signaller from getting EX when this does the
c*   EX->EX conversion (write)? No, because whoever gets past the
c*   EX->EX first will complete the signal, then release the sys lock,
c*   allowing the other guy to proceed.
c*
c*         get system cache lock in EX mode
      CALL IC_LOCKUP_N(IC__EX,NUM-1,LVBR,' ',TMOUT,IERR)
c*
c**        2-way  send+receive
D     TYPE *,'IC_SIGNAL: CACHE = ',NAME
c*         send LVB to master via the system cache.
      CALL IC_LOCKUP_N(IC__EX,NUM-1,LVB,' ',TMOUT,IERR)
      IF (IERR.NE.0) THEN
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        CALL IC_INFO('S_'//NAME,0,LVBDUM,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBDUM,'LP',IERR)
        GOTO 998
      ENDIF
c*           get EX mode and receive new LVB  !$$$ change to CR   TW 30/6/89
c*           BAST to wake process
      CACHE(NUM).BAST=%LOC(IC_SIGAST)
C        CALL IC_LOCKUP_N(IC__EX,NUM,LVB,' ',TMOUT,IERR)
      CALL IC_LOCKUP_N(IC__CR,NUM,LVB,' ',TMOUT,IERR)
      IF (IERR.NE.0) THEN
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        CALL IC_INFO('S_'//NAME,0,LVBDUM,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBDUM,'LP',IERR)
        GOTO 997
      ENDIF
c*           wait for BAST when master tries to get EX mode back
    1 IF (.NOT. ICASTFL) THEN
        ISTAT = SYS$CLREF(%VAL(IEF))
        IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        IF (TIMEOUT.GT.0.0) THEN
          IOP = %LOC(LIB$K_DELTA_SECONDS_F)
          ISTAT = LIB$CVTF_TO_INTERNAL_TIME(IOP,TIMEOUT,ITIME)
          IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
          ISTAT = SYS$SETIMR(%VAL(IEF),ITIME,,%VAL(ITID),)
          IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ENDIF
        HIBERFL = .TRUE.
        IF (.NOT.ICASTFL) THEN
          ISTAT = SYS$WAITFR(%VAL(IEF))
          IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ENDIF
        IF (TIMEOUT.GT.0.0) THEN
          ISTAT = SYS$CANTIM(%VAL(ITID),)
          IF (ISTAT.NE.1) CALL LIB$SIGNAL(%VAL(ISTAT))
        ENDIF
        IF (.NOT. ICASTFL) THEN
          TYPE '(1X,A,F7.2,A)','IC_SIGNAL timed out after ',
     +      TIMEOUT,' sec for cache '//NAME(:LST(NAME))
          STIMOUT=.TRUE.
        ENDIF
      ENDIF
      HIBERFL=.FALSE.
c*         requeue NL mode on primary cache
  997 CALL IC_LOCKUP_N(IC__NL,NUM,LVB,'QC',0.,IER1)
c*         convert system cache back to NL mode, rewriting the system
c*         byte(s).
  998 CALL IC_LOCKUP_N(IC__NL,NUM-1,LVBR,' ',0.,IER1)
      IF (STIMOUT) IERR=IC_E_STIMOUT
  999 END
c*
      SUBROUTINE IC_RELEASE(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_RELEASE
c*
c*       Release the cache NAME.
c*       Options:
c*               W    write the LVB before releasing. Will work only if
c*                    the mode at time of release is EX or PW (eg. for
c*                    master in master/client mode or lock-holder in
c*                    process status checker mode.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4), SYS$DEQ
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($SSDEF)'
      INCLUDE '($LCKDEF)'
c*
      IERR=0
      CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      CACHE(NUM).STAT = IC__RELEASING
c*         dequeue cache and its system cache
      IF (INDEX(CHOPT,'W').NE.0) THEN
        ISTAT = SYS$DEQ(%VAL(CACHE(NUM).LKST.ID),LVB,,)
      ELSE
c**          first, cancel pending lock requests (eg crash watch)
D       TYPE *,'CANCEL MAST'
        ISTAT = SYS$DEQ(%VAL(CACHE(NUM).LKST.ID),,,%VAL(LCK$M_CANCEL))
D       TYPE *,'DEQ MAST'
        ISTAT = SYS$DEQ(%VAL(CACHE(NUM).LKST.ID),,,)
      ENDIF
      IMODE = CACHE(NUM).MODE
      ITYPE = CACHE(NUM).TYPE
      IF (IMODE.EQ.IC__INFOCACHE.AND.ITYPE.EQ.IMODE) THEN
c*            system lock does not exist
      ELSE
D       TYPE *,'CANCEL SYS'
        ISTAT = SYS$DEQ(%VAL(CACHE(NUM-1).LKST.ID),,,%VAL(LCK$M_CANCEL))
D       TYPE *,'DEQ SYS'
        ISTAT = SYS$DEQ(%VAL(CACHE(NUM-1).LKST.ID),,,)
      ENDIF
  999 CACHE(NUM).STAT = IC__INACTIVE
      END
c*
c*=======================================================================
c*       UTILITY ROUTINES
c*=======================================================================
c*
      SUBROUTINE IC_ID_NUM(ID,NUM,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_ID_NUM
c*
c*       Given a cache ID, gets the associated number
c*
c*       Option N: no error messages reported. Just return code.
c*
c************************************************************************
      CHARACTER*(*) ID, CHOPT
      LOGICAL LIB$AST_IN_PROG
c*
      IF (LIB$AST_IN_PROG()) THEN
        CALL IC_ID_NUM_A(ID,NUM,CHOPT,IERR)
      ELSE
        CALL IC_ID_NUM_N(ID,NUM,CHOPT,IERR)
      ENDIF
      END
c************************************************************************
      SUBROUTINE IC_ID_NUM_A(ID,NUM,CHOPT,IERR)
      CHARACTER*(*) ID, CHOPT
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      LOGICAL ERRS
      IERR=0
      ERRS=.TRUE.
      IF (INDEX(CHOPT,'N').NE.0) ERRS=.FALSE.
      DO I=1,NCACHE
        IF (ID(1:LST(ID)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      IF (ERRS) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),ID(:LST(ID)))
        CALL IC_LIST_CACHES
      ENDIF
      IERR=IC_E_NOMATCH
      NUM=0
      RETURN
    1 CONTINUE
      END
c************************************************************************
      SUBROUTINE IC_ID_NUM_N(ID,NUM,CHOPT,IERR)
      CHARACTER*(*) ID, CHOPT
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      LOGICAL ERRS
      IERR=0
      ERRS=.TRUE.
      IF (INDEX(CHOPT,'N').NE.0) ERRS=.FALSE.
      DO I=1,NCACHE
        IF (ID(1:LST(ID)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 1
        ENDIF
      ENDDO
      IF (ERRS) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),ID(:LST(ID)))
        CALL IC_LIST_CACHES
      ENDIF
      IERR=IC_E_NOMATCH
      NUM=0
      RETURN
    1 CONTINUE
      END
      SUBROUTINE IC_NUM_ID(NUM,ID,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_NUM_ID
c*
c*       Given a cache number, gets the associated ID
c*
c************************************************************************
      CHARACTER ID*(*), CHOPT*(*)
      LOGICAL LIB$AST_IN_PROG
c*
      IF (LIB$AST_IN_PROG()) THEN
        CALL IC_NUM_ID_A(NUM,ID,CHOPT,IERR)
      ELSE
        CALL IC_NUM_ID_N(NUM,ID,CHOPT,IERR)
      ENDIF
      END
c*-----------------------------------------------------------------------
      SUBROUTINE IC_NUM_ID_A(NUM,ID,CHOPT,IERR)
      CHARACTER ID*(*), CHOPT*(*), BLANK*60
     +  /'                                                            '/
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
      IF (NUM.GT.NCACHE.OR.NUM.LT.1) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_OUTRNG),%VAL(1),%VAL(NUM))
        IERR=IC_E_OUTRNG
        RETURN
      ENDIF
      ID = BLANK(1:MIN(LEN(BLANK),LEN(ID)))
      ID = CACHE(NUM).ID(2:LST(CACHE(NUM).ID))
      IF (CACHE(NUM).STAT.EQ.IC__INACTIVE) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INACTIVE),%VAL(1),ID(:LST(ID)))
        IERR=IC_E_INACTIVE
        RETURN
      ENDIF
      END
c*-----------------------------------------------------------------------
      SUBROUTINE IC_NUM_ID_N(NUM,ID,CHOPT,IERR)
      CHARACTER ID*(*), CHOPT*(*), BLANK*60
     +  /'                                                            '/
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
      IF (NUM.GT.NCACHE.OR.NUM.LT.1) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_OUTRNG),%VAL(1),%VAL(NUM))
        IERR=IC_E_OUTRNG
        RETURN
      ENDIF
      ID = BLANK(1:MIN(LEN(BLANK),LEN(ID)))
      ID = CACHE(NUM).ID(2:LST(CACHE(NUM).ID))
      IF (CACHE(NUM).STAT.EQ.IC__INACTIVE) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INACTIVE),%VAL(1),ID(:LST(ID)))
        IERR=IC_E_INACTIVE
        RETURN
      ENDIF
      END
      SUBROUTINE IC_LIST_CACHES
c************************************************************************
c*
c*       SUBR. IC_LIST_CACHES
c*
c*       List the booked caches
c*
c************************************************************************
      CHARACTER ID*40
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
      TYPE *,'List of cache locks: (1st char is cache type) '
      DO NUM=1,NCACHE
        ID = CACHE(NUM).ID(1:LST(CACHE(NUM).ID))
        IF (CACHE(NUM).STAT.EQ.IC__INACTIVE) THEN
          TYPE '(1X,A)',ID(:LST(ID))//' INACTIVE'
        ELSE
          TYPE '(1X,A)',ID(:LST(ID))
        ENDIF
      ENDDO
      END
c************************************************************************
      SUBROUTINE IC_LOCKUP(MODE,NUM,LVB,CHOPT,TIMEOUT,IERR)
      IMPLICIT INTEGER (A-Z)
      CHARACTER CHOPT*(*)
      INTEGER LVB(4)
      LOGICAL LIB$AST_IN_PROG
      IF (LIB$AST_IN_PROG()) THEN
        CALL IC_LOCKUP_A(MODE,NUM,LVB,CHOPT,TIMEOUT,IERR)
      ELSE
        CALL IC_LOCKUP_N(MODE,NUM,LVB,CHOPT,TIMEOUT,IERR)
      ENDIF
      END
      SUBROUTINE IC_LOCKUP_A(MODE,NUM,LVB,CHOPT,TIMEOUT,IERR)
c************************************************************************
c*
c*       SUBR. IC_LOCKUP
c*
c*       Does conversion of lock NUM to mode MODE. If lock NUM has an
c*       AST or BAST defined, it is used in the conversion.
c*       The LVB is read in the conversion (unless option Q is selected).
c*       Writes LVB if conversion is EX ->
c*
c*       A value of TIMEOUT >0 overrides the default timeout value for
c*       the requested lock to be granted.
c*       If TIMEOUT <0, there is no timeout (infinite wait).
c*
c*       Options:
c*          N   New lock. Not a conversion.
c*          Q   Suppress reading/writing of LVB
c*          C   Clear the ASTs
c*          T   Timeout quiet: do not signal timeout error.
c*          B   no BAST
c*
c************************************************************************
      IMPLICIT INTEGER (A-Z)
      CHARACTER CHOPT*(*)
      INTEGER LVB(4)
      INTEGER SYS$ENQ, SYS$DEQ, SYS$READEF
      INCLUDE '($LCKDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      CHARACTER CMODE*4, NAME*60
      INTEGER NAST(IC__MAXCACHE)
      REAL TIMEOUT,WTIME,DELAY
      LOGICAL FIRST /.TRUE./
c*
      IERR=0
      ISTAT = LIB$GET_EF(IEF)
      IF (.NOT.ISTAT) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ENDIF
      IF (FIRST) THEN
        FIRST=.FALSE.
        DO I=1,IC__MAXCACHE
          NAST(I)=I
        ENDDO
      ENDIF
c**         convert mode
      IF (MODE.EQ.IC__NL) THEN
        CMODE = ' NL '
      ELSE IF (MODE.EQ.IC__CR) THEN
        CMODE = ' CR '
      ELSE IF (MODE.EQ.IC__EX) THEN
        CMODE = ' EX '
      ELSE IF (MODE.EQ.IC__PW) THEN
        CMODE = ' PW '
      ELSE
        CMODE = ' ?? '
      ENDIF
      CALL IC_NUM_ID_A(NUM,NAME,' ',IERR)
      IF (IERR.NE.0) GOTO 999
D     TYPE *,'********* IC_LOCKUP ',CMODE,'  CACHE = ',NAME(1:LST(NAME))
      IF (INDEX(CHOPT,'C').NE.0) THEN
        CACHE(NUM).AST=0
        CACHE(NUM).BAST=0
      ENDIF
   10 ITER=0
      IMASK=0
      IF (INDEX(CHOPT,'Q').EQ.0) IMASK = IMASK .OR. LCK$M_VALBLK
      IF (INDEX(CHOPT,'N').EQ.0) THEN
        IMASK = IMASK .OR. LCK$M_CONVERT
      ELSE
        CACHE(NUM).LKST.ID = 0
      ENDIF
      IF (MOD(CACHE(NUM).AUTH/2,2).EQ.1) IMASK = IMASK .OR. LCK$M_SYSTEM
c**         tell the system that lock waits are OK, don't deadlock search
      IMASK = IMASK .OR. LCK$M_NODLCKWT
c**         write the LVB if conversion is from EX, PW mode
      IF ((CACHE(NUM).LOCK.EQ.IC__EX.OR.CACHE(NUM).LOCK.EQ.IC__PW) .AND.
     +  INDEX(CHOPT,'Q').EQ.0) THEN
D       TYPE *,'IC_LOCKUP',CMODE,': Convert from EX,PW mode. LVB write'
        DO I=1,4
          CACHE(NUM).LKST.LVB(I)=LVB(I)
        ENDDO
      ENDIF
      IF (CACHE(NUM).BAST.NE.0.AND.INDEX(CHOPT,'B').EQ.0) THEN
D       TYPE *,'IC_LOCKUP',CMODE,CHOPT,': set blocking AST'
        ISTAT = SYS$ENQ (
     +    %VAL(IEF),%VAL(MODE),CACHE(NUM).LKST,
     +    %VAL(IMASK),'L3%'//CACHE(NUM).LKST.NAME,,,
     +    NAST(NUM),%VAL(CACHE(NUM).BAST),,)
      ELSE IF (CACHE(NUM).AST.NE.0) THEN
D       TYPE *,'IC_LOCKUP',CMODE,CHOPT,': set grant AST'
        ISTAT = SYS$CLREF(%VAL(IEF))
        IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ISTAT = SYS$ENQ(
     +       ,%VAL(MODE),CACHE(NUM).LKST,
     +       %VAL(IMASK),'L3%'//CACHE(NUM).LKST.NAME
     +       ,,%VAL(CACHE(NUM).AST),NAST(NUM),,,)
      ELSE
D       TYPE *,'IC_LOCKUP',CMODE,CHOPT,': ENQ, no AST'
        ISTAT = SYS$ENQ (
     +    %VAL(IEF),%VAL(MODE),CACHE(NUM).LKST,
     +    %VAL(IMASK),'L3%'//CACHE(NUM).LKST.NAME,,,
     +    ,,,)
      ENDIF
      IF (ISTAT.EQ.SS$_VALNOTVALID) THEN
c*              do not report this, it is a misleading warning and not
c*              an error.
      ELSE IF (ISTAT.EQ.SS$_IVLOCKID) THEN
        CALL LIB$SIGNAL (%VAL(IC_E_IVLOCKID),%VAL(1),NAME(1:LST(NAME)))
        CALL IC_INFO(NAME,0,IDUM,'LP',IERR)
        IERR = IC_E_IVLOCKID
      ELSE IF (ISTAT.EQ.SS$_CVTUNGRANT) THEN
c**          a previous $ENQ has not completed; cancel it and redo.
        TYPE *,'Cancelling incomplete $ENQ on cache '//NAME(:LST(NAME))
        ISTC = SYS$DEQ(%VAL(CACHE(NUM).LKST.ID),,,%VAL(LCK$M_CANCEL))
        IF (ISTC.NE.SS$_NORMAL) CALL LIB$SIGNAL (%VAL(ISTC))
        GOTO 10
      ELSE IF (.NOT.ISTAT) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ENDIF
      IF (CACHE(NUM).AST.EQ.0) THEN
        WTIME=0.
    1   ISTAT = SYS$READEF (%VAL(IEF),IDUM)
        IF (ISTAT.EQ.SS$_WASCLR) THEN
          ITER=ITER+1
          IF (ITER.GE.4) THEN
            DELAY = 0.005*(ITER-3)
            IF (DELAY.GT.1.0) DELAY=1.0
            CALL L3_WAIT(DELAY)
            WTIME = WTIME + DELAY
          ENDIF
          IF (TIMEOUT.GT.0.01) THEN
c*               timeout has been specified. Follow it.
            IF (WTIME.LT.TIMEOUT) GOTO 1
          ELSE IF (TIMEOUT.LT.-0.01) THEN
c*               infinite timeout has been specified.
            GOTO 1
          ELSE
c*               use the default
            IF (WTIME.LE.DEFTIMOUT) GOTO 1
          ENDIF
          IF (INDEX(CHOPT,'T').EQ.0) THEN
            ITIM = WTIME
            CALL LIB$SIGNAL(%VAL(IC_E_LKFAIL),%VAL(3),
     +        CMODE,NAME(:LST(NAME)),%VAL(ITIM))
          ENDIF
          IERR=IC_E_LKFAIL
c**            cancel the ungranted lock request
          ISTC = SYS$DEQ(%VAL(CACHE(NUM).LKST.ID),,,%VAL(LCK$M_CANCEL))
          IF (ISTC.NE.SS$_NORMAL) CALL LIB$SIGNAL (%VAL(ISTC))
          GOTO 999
        ELSE IF (ISTAT.EQ.SS$_WASSET) THEN
D         TYPE *,'IC_LOCKUP',CMODE,': EF set at ITER =',ITER
        ELSE
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
          GOTO 999
        ENDIF
        IF (.NOT.CACHE(NUM).LKST.STATUS) THEN
          IF (CACHE(NUM).LKST.STATUS.EQ.SS$_VALNOTVALID) THEN
c*              do not report this, it is a misleading warning and not
c*              an error.
          ELSE IF (CACHE(NUM).LKST.STATUS.EQ.SS$_DEADLOCK) THEN
c**              wow, a deadlock.
            CALL LIB$SIGNAL (%VAL(IC_E_DEADLOCK),
     &        %VAL(2),NAME(:LST(NAME)),CMODE)
            IERR = IC_E_DEADLOCK
            GOTO 999
          ELSE
            CALL LIB$SIGNAL (%VAL(CACHE(NUM).LKST.STATUS))
            IERR = CACHE(NUM).LKST.STATUS
            GOTO 999
          ENDIF
        ENDIF
c*           retrieve the current LVB
        DO I=1,4
          LVB(I)=CACHE(NUM).LKST.LVB(I)
        ENDDO
      ENDIF
c*         save the new mode
      CACHE(NUM).LOCK = MODE
  999 ISTAT = LIB$FREE_EF(IEF)
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      END
      SUBROUTINE IC_LOCKUP_N(MODE,NUM,LVB,CHOPT,TIMEOUT,IERR)
c************************************************************************
c*
c*       SUBR. IC_LOCKUP
c*
c*       Does conversion of lock NUM to mode MODE. If lock NUM has an
c*       AST or BAST defined, it is used in the conversion.
c*       The LVB is read in the conversion (unless option Q is selected).
c*       Writes LVB if conversion is EX ->
c*
c*       A value of TIMEOUT >0 overrides the default timeout value for
c*       the requested lock to be granted.
c*       If TIMEOUT <0, there is no timeout (infinite wait).
c*
c*       Options:
c*          N   New lock. Not a conversion.
c*          Q   Suppress reading/writing of LVB
c*          C   Clear the ASTs
c*          T   Timeout quiet: do not signal timeout error.
c*          B   no BAST
c*
c************************************************************************
      IMPLICIT INTEGER (A-Z)
      CHARACTER CHOPT*(*)
      INTEGER LVB(4)
      INTEGER SYS$ENQ, SYS$DEQ, SYS$READEF
      INCLUDE '($LCKDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      CHARACTER CMODE*4, NAME*60
      INTEGER NAST(IC__MAXCACHE)
      REAL TIMEOUT,WTIME,DELAY
      LOGICAL FIRST /.TRUE./
c*
      IERR=0
      ISTAT = LIB$GET_EF(IEF)
      IF (.NOT.ISTAT) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ENDIF
      IF (FIRST) THEN
        FIRST=.FALSE.
        DO I=1,IC__MAXCACHE
          NAST(I)=I
        ENDDO
      ENDIF
c**         convert mode
      IF (MODE.EQ.IC__NL) THEN
        CMODE = ' NL '
      ELSE IF (MODE.EQ.IC__CR) THEN
        CMODE = ' CR '
      ELSE IF (MODE.EQ.IC__EX) THEN
        CMODE = ' EX '
      ELSE IF (MODE.EQ.IC__PW) THEN
        CMODE = ' PW '
      ELSE
        CMODE = ' ?? '
      ENDIF
      CALL IC_NUM_ID_N(NUM,NAME,' ',IERR)
      IF (IERR.NE.0) GOTO 999
D     TYPE *,'********* IC_LOCKUP ',CMODE,'  CACHE = ',NAME(1:LST(NAME))
      IF (INDEX(CHOPT,'C').NE.0) THEN
        CACHE(NUM).AST=0
        CACHE(NUM).BAST=0
      ENDIF
   10 ITER=0
      IMASK=0
      IF (INDEX(CHOPT,'Q').EQ.0) IMASK = IMASK .OR. LCK$M_VALBLK
      IF (INDEX(CHOPT,'N').EQ.0) THEN
        IMASK = IMASK .OR. LCK$M_CONVERT
      ELSE
        CACHE(NUM).LKST.ID = 0
      ENDIF
      IF (MOD(CACHE(NUM).AUTH/2,2).EQ.1) IMASK = IMASK .OR. LCK$M_SYSTEM
c**         tell the system that lock waits are OK, don't deadlock search
      IMASK = IMASK .OR. LCK$M_NODLCKWT
c**         write the LVB if conversion is from EX, PW mode
      IF ((CACHE(NUM).LOCK.EQ.IC__EX.OR.CACHE(NUM).LOCK.EQ.IC__PW) .AND.
     +  INDEX(CHOPT,'Q').EQ.0) THEN
D       TYPE *,'IC_LOCKUP',CMODE,': Convert from EX,PW mode. LVB write'
        DO I=1,4
          CACHE(NUM).LKST.LVB(I)=LVB(I)
        ENDDO
      ENDIF
      IF (CACHE(NUM).BAST.NE.0.AND.INDEX(CHOPT,'B').EQ.0) THEN
D       TYPE *,'IC_LOCKUP',CMODE,CHOPT,': set blocking AST'
        ISTAT = SYS$ENQ (
     +    %VAL(IEF),%VAL(MODE),CACHE(NUM).LKST,
     +    %VAL(IMASK),'L3%'//CACHE(NUM).LKST.NAME,,,
     +    NAST(NUM),%VAL(CACHE(NUM).BAST),,)
      ELSE IF (CACHE(NUM).AST.NE.0) THEN
D       TYPE *,'IC_LOCKUP',CMODE,CHOPT,': set grant AST'
        ISTAT = SYS$CLREF(%VAL(IEF))
        IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ISTAT = SYS$ENQ(
     +       ,%VAL(MODE),CACHE(NUM).LKST,
     +       %VAL(IMASK),'L3%'//CACHE(NUM).LKST.NAME
     +       ,,%VAL(CACHE(NUM).AST),NAST(NUM),,,)
      ELSE
D       TYPE *,'IC_LOCKUP',CMODE,CHOPT,': ENQ, no AST'
        ISTAT = SYS$ENQ (
     +    %VAL(IEF),%VAL(MODE),CACHE(NUM).LKST,
     +    %VAL(IMASK),'L3%'//CACHE(NUM).LKST.NAME,,,
     +    ,,,)
      ENDIF
      IF (ISTAT.EQ.SS$_VALNOTVALID) THEN
c*              do not report this, it is a misleading warning and not
c*              an error.
      ELSE IF (ISTAT.EQ.SS$_IVLOCKID) THEN
        CALL LIB$SIGNAL (%VAL(IC_E_IVLOCKID),%VAL(1),NAME(1:LST(NAME)))
        CALL IC_INFO(NAME,0,IDUM,'LP',IERR)
        IERR = IC_E_IVLOCKID
      ELSE IF (ISTAT.EQ.SS$_CVTUNGRANT) THEN
c**          a previous $ENQ has not completed; cancel it and redo.
        TYPE *,'Cancelling incomplete $ENQ on cache '//NAME(:LST(NAME))
        ISTC = SYS$DEQ(%VAL(CACHE(NUM).LKST.ID),,,%VAL(LCK$M_CANCEL))
        IF (ISTC.NE.SS$_NORMAL) CALL LIB$SIGNAL (%VAL(ISTC))
        GOTO 10
      ELSE IF (.NOT.ISTAT) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ENDIF
      IF (CACHE(NUM).AST.EQ.0) THEN
        WTIME=0.
    1   ISTAT = SYS$READEF (%VAL(IEF),IDUM)
        IF (ISTAT.EQ.SS$_WASCLR) THEN
          ITER=ITER+1
          IF (ITER.GE.4) THEN
            DELAY = 0.005*(ITER-3)
            IF (DELAY.GT.1.0) DELAY=1.0
            CALL L3_WAIT(DELAY)
            WTIME = WTIME + DELAY
          ENDIF
          IF (TIMEOUT.GT.0.01) THEN
c*               timeout has been specified. Follow it.
            IF (WTIME.LT.TIMEOUT) GOTO 1
          ELSE IF (TIMEOUT.LT.-0.01) THEN
c*               infinite timeout has been specified.
            GOTO 1
          ELSE
c*               use the default
            IF (WTIME.LE.DEFTIMOUT) GOTO 1
          ENDIF
          IF (INDEX(CHOPT,'T').EQ.0) THEN
            ITIM = WTIME
            CALL LIB$SIGNAL(%VAL(IC_E_LKFAIL),%VAL(3),
     +        CMODE,NAME(:LST(NAME)),%VAL(ITIM))
          ENDIF
          IERR=IC_E_LKFAIL
c**            cancel the ungranted lock request
          ISTC = SYS$DEQ(%VAL(CACHE(NUM).LKST.ID),,,%VAL(LCK$M_CANCEL))
          IF (ISTC.NE.SS$_NORMAL) CALL LIB$SIGNAL (%VAL(ISTC))
          GOTO 999
        ELSE IF (ISTAT.EQ.SS$_WASSET) THEN
D         TYPE *,'IC_LOCKUP',CMODE,': EF set at ITER =',ITER
        ELSE
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
          GOTO 999
        ENDIF
        IF (.NOT.CACHE(NUM).LKST.STATUS) THEN
          IF (CACHE(NUM).LKST.STATUS.EQ.SS$_VALNOTVALID) THEN
c*              do not report this, it is a misleading warning and not
c*              an error.
          ELSE IF (CACHE(NUM).LKST.STATUS.EQ.SS$_DEADLOCK) THEN
c**              wow, a deadlock.
            CALL LIB$SIGNAL (%VAL(IC_E_DEADLOCK),
     &        %VAL(2),NAME(:LST(NAME)),CMODE)
            IERR = IC_E_DEADLOCK
            GOTO 999
          ELSE
            CALL LIB$SIGNAL (%VAL(CACHE(NUM).LKST.STATUS))
            IERR = CACHE(NUM).LKST.STATUS
            GOTO 999
          ENDIF
        ENDIF
c*           retrieve the current LVB
        DO I=1,4
          LVB(I)=CACHE(NUM).LKST.LVB(I)
        ENDDO
      ENDIF
c*         save the new mode
      CACHE(NUM).LOCK = MODE
  999 ISTAT = LIB$FREE_EF(IEF)
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      END
      SUBROUTINE IC_TMPLOCK(NAME,LVB,LKID,CHOPT,IERR)
c************************************************************************
c*
c*       IC_TMPLOCK
c*
c*       To get a NL lock on a cache, without the booking paraphernalia
c*
c************************************************************************
      IMPLICIT INTEGER (A-Z)
      CHARACTER CHOPT*(*), NAME*(*)
      INTEGER LVB(4)
      INTEGER SYS$ENQ, SYS$READEF
      INCLUDE '($LCKDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      REAL DELAY, WTIME
      RECORD /ICLSTB/ LKSTBK       ! lock status block
c*
      IERR=0
      LKSTBK.NAME = NAME(1:LST(NAME))
      ITER=0
      IMASK=0
      IMASK = IMASK .OR. LCK$M_VALBLK
      IF (INDEX(CHOPT,'Y').NE.0) IMASK = IMASK .OR. LCK$M_SYSTEM
      ISTAT = LIB$GET_EF(IEF)
      IF (.NOT.ISTAT) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ENDIF
      ISTAT = SYS$ENQ (
     +  %VAL(IEF),%VAL(LCK$K_NLMODE),LKSTBK,
     +  %VAL(IMASK),'L3%'//LKSTBK.NAME,,,,,,)
c*
      IF (ISTAT.EQ.SS$_VALNOTVALID) THEN
c*              do not report this, it is a misleading warning and not
c*              an error.
      ELSE IF (.NOT.ISTAT) THEN
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ENDIF
      ITER=0
    1 ISTAT = SYS$READEF (%VAL(IEF),IDUM)
      IF (ISTAT.EQ.SS$_WASCLR) THEN
        ITER=ITER+1
        IF (ITER.GE.4) THEN
          DELAY = 0.005*(ITER-3)
          IF (DELAY.GT.1.0) DELAY=1.0
          CALL L3_WAIT(DELAY)
          WTIME = WTIME + DELAY
        ENDIF
        IF (WTIME.LE.DEFTIMOUT) GOTO 1
        IF (INDEX(CHOPT,'T').EQ.0) THEN
          ITIM = WTIME
          CALL LIB$SIGNAL(%VAL(IC_E_LKFAIL),%VAL(3),
     +        CMODE,NAME(:LST(NAME)),%VAL(ITIM))
        ENDIF
        IERR=IC_E_LKFAIL
c**            cancel the ungranted lock request
        ISTC = SYS$DEQ(%VAL(LKSTBK.ID),,,%VAL(LCK$M_CANCEL))
        IF (ISTC.NE.SS$_NORMAL) CALL LIB$SIGNAL (%VAL(ISTC))
        GOTO 999
      ELSE IF (ISTAT.EQ.SS$_WASSET) THEN
D       TYPE *,'IC_TMPLOCK',CMODE,': EF set at ITER =',ITER
      ELSE
        CALL LIB$SIGNAL (%VAL(ISTAT))
        IERR = ISTAT
        GOTO 999
      ENDIF
      IF (.NOT.LKSTBK.STATUS) THEN
        IF (LKSTBK.STATUS.EQ.SS$_VALNOTVALID) THEN
c*              do not report this, it is a misleading warning and not
c*              an error.
        ELSE IF (LKSTBK.STATUS.EQ.SS$_DEADLOCK) THEN
c**              wow, a deadlock.
          CALL LIB$SIGNAL (%VAL(IC_E_DEADLOCK),%VAL(2),NAME(:LST(NAME)),
     +          CMODE)
          IERR = IC_E_DEADLOCK
          GOTO 999
        ELSE
          CALL LIB$SIGNAL (%VAL(LKSTBK.STATUS))
          IERR = LKSTBK.STATUS
          GOTO 999
        ENDIF
      ENDIF
c*           retrieve the current LVB
      DO I=1,4
        LVB(I)=LKSTBK.LVB(I)
      ENDDO
      LKID = LKSTBK.ID
  999 ISTAT = LIB$FREE_EF(IEF)
      END
      SUBROUTINE IC_LKWAIT(MODE,NUM,LVB,TIMEOUT,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_LKWAIT
c*
c*       Queues a lock conversion and waits for the AST (either grant AST
c*       or BAST). Waits for a period TIMEOUT. If TIMEOUT=0., waits
c*       forever.
c*
c************************************************************************
      CHARACTER CHOPT*(*)
      INTEGER LVB(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      COMMON /CIC_AST/ ICASTFL, HIBERFL
      LOGICAL ICASTFL, HIBERFL
c*
      IERR=0
      HIBERFL=.FALSE.
      ICASTFL=.FALSE.
c*
      CALL IC_LOCKUP(MODE,NUM,LVB,CHOPT,0.,IERR)
c**        wait for AST
      HIBERFL=.TRUE.
      IF (TIMEOUT.EQ.0.0) THEN
D       TYPE *,'IC_LKWAIT: hibernating'
        IF (.NOT. ICASTFL) CALL SYS$HIBER
      ELSE
D       TYPE *,'IC_LKWAIT: wait TIMEOUT =',TIMEOUT
        IF (.NOT. ICASTFL) CALL L3_WAIT(TIMEOUT)
      ENDIF
      HIBERFL=.FALSE.
      IF (.NOT. ICASTFL) THEN
c*           Timeout, if AST was never called
        CALL LIB$SIGNAL(%VAL(IC_E_WTIMOUT),%VAL(1),CACHE(NUM).LKST.NAME)
        IERR=IC_E_WTIMOUT
        GOTO 999
      ENDIF
c*         retrieve the LVB
      CALL CPC_UCOPY(CACHE(NUM).LKST.LVB,LVB,4)
  999 END
      SUBROUTINE IC_WTAST(NUM)
c************************************************************************
c*
c*       SUBR. IC_WTAST
c*
c*       AST routine used for IC_WAIT in broadcast mode
c*
c************************************************************************
      INTEGER LVB(4), LVBR(4)
      INTEGER SYS$WAKE
      INCLUDE '($SSDEF)'
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      COMMON /CIC_AST/ ICASTFL, HIBERFL
      LOGICAL ICASTFL, HIBERFL
      CHARACTER NAME*60
c*
      ICASTFL=.TRUE.
c*         read the broadcast
D     TYPE *,'IC_WTAST: BROADCAST read'
      CALL IC_NUM_ID(NUM,NAME,' ',IERR)
      CALL IC_GETBC(NAME,LVB,'K',IERR)
c*         wake the process
      IF (HIBERFL) THEN
        CALL L3_CANWAIT
        ISTAT = SYS$WAKE(,)
        IF (.NOT.ISTAT) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
          GOTO 999
        ENDIF
      ENDIF
c*             system cache back to NL mode
      CALL IC_LOCKUP(IC__NL,NUM-1,LVBR,'Q',0.,IERR)
      IF (IERR.NE.0) GOTO 999
  999 END
      SUBROUTINE IC_WKAST(NUM)
c************************************************************************
c*
c*       SUBR. IC_WKAST
c*
c*       AST routine used various places to wake from hibernation
c*
c************************************************************************
      INTEGER SYS$WAKE
      COMMON /CIC_AST/ ICASTFL, HIBERFL
      LOGICAL ICASTFL, HIBERFL
c*
      ICASTFL=.TRUE.
c*         wake the process, if it is hibernating
      IF (HIBERFL) THEN
        CALL L3_CANWAIT
        ISTAT = SYS$WAKE(,)
        IF (.NOT.ISTAT) CALL LIB$SIGNAL (%VAL(ISTAT))
      ENDIF
  999 END
      SUBROUTINE IC_SIGAST(NUM)
c************************************************************************
c*
c*       SUBR. IC_SIGAST
c*
c*       AST routine used various places to wake from hibernation
c*
c************************************************************************
      INCLUDE '($SSDEF)'
      COMMON /CIC_AST/ ICASTFL, HIBERFL
      LOGICAL ICASTFL, HIBERFL
      COMMON /C_IC_SIGNAL_IEF/ IEF
      INTEGER SYS$SETEF
c*
      ICASTFL=.TRUE.
      IF (HIBERFL) THEN
        ISTAT = SYS$SETEF(%VAL(IEF))
        IF (ISTAT.NE.1) CALL LIB$SIGNAL(%VAL(ISTAT))
      ENDIF
      END
      SUBROUTINE IC_GETBC(NAME,LVB,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_GETBC
c*
c*       Routine to read a broadcast. Used by IC_WAIT and IC_DELIVER.
c*
c************************************************************************
      CHARACTER*(*) NAME, CHOPT
      INTEGER LVB(4), LVBR(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($SSDEF)'
      INCLUDE '($LCKDEF)'
      LOGICAL SYSTMOUT
      INTEGER SYS$DEQ
      COMMON /IC_C_BCASTW/ BCAST_WAIT
      LOGICAL BCAST_WAIT
      CHARACTER DATETIME*23
c*
      IERR=0
CCC        CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      DO I=1,NCACHE
        IF (NAME(1:LST(NAME)).EQ.CACHE(I).ID(2:LST(CACHE(I).ID))) THEN
          NUM=I
          IF (CACHE(I).STAT.NE.IC__INACTIVE) GOTO 2
        ENDIF
      ENDDO
      CALL LIB$SIGNAL(%VAL(IC_E_NOMATCH),%VAL(1),NAME(:LST(NAME)))
      IERR=IC_E_NOMATCH
    2 CONTINUE
      IF (IERR.NE.0) GOTO 999
c*         in broadcast mode, if a broadcast comes in while waiting for
c*         the interlock to send one out, cancel the EX lock request
      IF ((CACHE(NUM).TYPE.EQ.IC__BROADCAST).AND.BCAST_WAIT) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_BCININT),%VAL(1),NAME(:LST(NAME)))
        BCAST_WAIT = .FALSE.
c*           cancel the EX mode wait on the system cache
        ISTAT = SYS$DEQ(%VAL(CACHE(NUM-1).LKST.ID),,,%VAL(LCK$M_CANCEL))
        IF (ISTAT.NE.SS$_NORMAL.AND.ISTAT.NE.SS$_CANCELGRANT
     +      .AND.ISTAT.NE.SS$_IVLOCKID) CALL LIB$SIGNAL (%VAL(ISTAT))
        CALL IC_LOCKUP_A(IC__CR,NUM-1,LVBR,'N',0.,IERR)
      ELSE
c*             system cache in CR mode
        CALL IC_LOCKUP_A(IC__CR,NUM-1,LVBR,'Q',0.,IERR)
        IF (IERR.NE.0) THEN
          TYPE *,'*** CPC ERR: Broadcast TIMEOUT, cache '//
     &      NAME(:LST(NAME))//' Cannot get CR on system cache.'
          CALL LIB$DATE_TIME(DATETIME)
          TYPE *,'*** At '//DATETIME//'  Cache stat:'
          CALL IC_INFO('S_'//NAME,0,LVBR,'LP',IERR)
          CALL IC_INFO(NAME,0,LVBR,'LP',IERR)
          GOTO 1
        ENDIF
      ENDIF
c*             release the primary
      CALL IC_LOCKUP_A(IC__NL,NUM,LVB,'Q',0.,IERR)
      IF (IERR.NE.0) GOTO 1
c*             wait for broadcaster to release the system cache
      WAITED=0.
      PAUSE=0.008
      SYSTMOUT=.FALSE.
      CALL LIB$DAY(IDAY,,ITIME1)
c**        timeout should be less than the default LOCKUP timeout
      DO WHILE (WAITED.LT.DEFTIMOUT-2.)
        PAUSE=PAUSE+.008
        CALL L3_WAIT(PAUSE)
        CALL IC_GET_CACHE('S_'//NAME,LVBR,' ',IERR)
        IF (IERR.NE.0) GOTO 1
c*           wait until system cache is marked unused
        IF (LVBR(4).EQ.0) GOTO 1
        CALL LIB$DAY(IDAY,,ITIME2)
        WAITED=(ITIME2-ITIME1)/100.
      ENDDO
      SYSTMOUT=.TRUE.
c*             primary back to CR and read LVB
    1 CALL IC_LOCKUP_A(IC__CR,NUM,LVB,' ',0.,IERR)
      IF (IERR.NE.0) THEN
        TYPE *,'*** CPC ERR: BCAST -> CR failure, cache '//
     &    NAME(:LST(NAME))
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        IERR = IC_E_BCASTREL
        CALL IC_INFO('S_'//NAME,0,LVBR,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBR,'LP',IERR)
      ENDIF
D     TYPE *,'IC_GETBC: system cache release ITER =',K
      IF (INDEX(CHOPT,'K').EQ.0) THEN
c*             system cache back to NL mode
        CALL IC_LOCKUP_A(IC__NL,NUM-1,LVBR,'Q',0.,IERR1)
      ENDIF
      IF (SYSTMOUT) THEN
C          CALL LIB$SIGNAL(%VAL(IC_E_BCASTREL),%VAL(1),NAME(:LST(NAME)))
        TYPE *,'*** CPC ERR: Broadcast TIMEOUT, cache '//
     &    NAME(:LST(NAME))//' Sender does not release cache.'
        CALL LIB$DATE_TIME(DATETIME)
        TYPE *,'*** At '//DATETIME//'  Cache stat:'
        IERR = IC_E_BCASTREL
        CALL IC_INFO('S_'//NAME,0,LVBR,'LP',IERR)
        CALL IC_INFO(NAME,0,LVBR,'LP',IERR)
      ENDIF
  999 END
      SUBROUTINE IC_FINISH
c************************************************************************
c*
c*       SUBR. IC_FINISH
c*
c*       Please call CPC_FINISH instead.
c*
c************************************************************************
      CALL CPC_FINISH
      END
c************************************************************************
      SUBROUTINE IC_MODE(CHAR,ICODE)
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      CHARACTER*1 CHAR
      IF (CHAR.EQ.C__MAILBOX) THEN
        ICODE = IC__MAILBOX
      ELSE IF (CHAR.EQ.C__BROADCAST) THEN
        ICODE = IC__BROADCAST
      ELSE IF (CHAR.EQ.C__WATCHER) THEN
        ICODE = IC__WATCHER
      ELSE IF (CHAR.EQ.C__ALARM) THEN
        ICODE = IC__ALARM
      ELSE IF (CHAR.EQ.C__INFOCACHE) THEN
        ICODE = IC__INFOCACHE
      ELSE
        ICODE = IC__UNKNOWN
      ENDIF
      END

      SUBROUTINE IC_TAKE_LOCK(NAME,LVB,IDUM,TIMEOUT,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_TAKE_LOCK
c*
c*       Acquires resource lock, if available. If currently held by
c*       someone else, waits for a period TIMEOUT.
c*       IDUM for future use  (event flag, AST rtn...)
c*       Two words of LVB are loaded into cache. The procid and
c*       csid (cluster system id of the node) are automatically loaded
c*       into words 3,4 of the cache.
c*
c*       If lock is not obtained in time TIMEOUT, routine returns
c*       QUIETLY with IERR=IC_E_LKHELD.
c*
c-LOCK MODE: book in NL. Take out lock (to EX) with timeout, non-signalled
c  error return (informational) if EX is held by someone else; in the
c  cache, an identifier code indicating the lockholder (so a slave lock
c  is needed as usual to lock out other transactions, protecting the
c  EX->EX write conversion to load the identifier). TEST lock to return
c  TRUE/FALSE and lockholder if true. RELEASE lock.
c  'R' for resource lock
c*
c*       Options:
c*         U  entire contents of cache user defined. Otherwise, only 8
c*            bytes are user defined, the rest is used by the system
c*            (process and node IDs, etc.)
c*            If the U option is used here, it should be used in the
c*            IC_CHECK_LOCK call, too.
c*            WITH THE 'U' OPTION IS IS THE USER'S RESPONSIBILITY TO
c*            ENSURE THAT BYTE 1 IS LOADED WITH A NON-ZERO VALUE WHEN
c*            THE LOCK IS TAKEN. IF IT IS ZERO, THIS ROUTINE OVERWRITES
c*            IT WITH 1. THIS IS USED TO ASCERTAIN THAT THE LOCK IS
c*            HELD.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVB(4), LVBLD(4), LVBR(4)
      INTEGER LVBB(16)
      EQUIVALENCE (LVBB,LVBLD)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE '($SYIDEF)'
      INCLUDE '($JPIDEF)'
      INTEGER IPROCID, ICSID
      INTEGER LIB$GETSYI, LIB$GETJPI
c*
      IERR=0
      IF (TIMEOUT.GT.0.0) THEN
        TMOUT = TIMEOUT
      ELSE
        TMOUT = -1.
      ENDIF
      CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__LOCK) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
      IMODE = CACHE(NUM).LOCK
      IF (IMODE.EQ.IC__EX) THEN
c*           lock already held. Just update the LVB.
        GOTO 100
      ENDIF
c*         get system cache lock in EX mode
      CALL IC_LOCKUP(IC__EX,NUM-1,LVBR,'T',TMOUT,IERR)
      IF (IERR.EQ.IC_E_LKFAIL) THEN
c*           timeout. Cannot get lock.
        IERR = IC_E_LKHELD
        RETURN
      ENDIF
c*         get cache lock in EX mode
      CALL IC_LOCKUP(IC__EX,NUM,LVBR,'T',TMOUT,IERR)
      IF (IERR.EQ.IC_E_LKFAIL) THEN
c*           timeout. Cannot get lock.
        IERR = IC_E_LKHELD
        RETURN
      ENDIF
c*         load the holder info
  100 IF (INDEX(CHOPT,'U').EQ.0) THEN
        LVBLD(1) = LVB(1)
        LVBLD(2) = LVB(2)
        ISTAT = LIB$GETJPI(JPI$_PID,,,IPROCID,,)
        IF (.NOT.ISTAT) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
        ENDIF
        LVBLD(3) = IPROCID
        ISTAT = LIB$GETSYI(SYI$_NODE_CSID,ICSID)
        IF (.NOT.ISTAT) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
        ENDIF
        LVBLD(4) = ICSID
      ELSE
        DO K=1,4
          LVBLD(K) = LVB(K)
        ENDDO
        IF (LVBB(1).EQ.0) THEN
c*             first byte non-zero! A no-no!
          LVBB(1)=1
          CALL LIB$SIGNAL(%VAL(IC_E_BADURES),%VAL(1),NAME(:LST(NAME)))
          IERR = IC_E_BADURES
        ENDIF
      ENDIF
      CALL IC_WRITE(NAME,LVBLD,'S',IERR1)
c*         write errors take priority!
      IF (IERR1.NE.0) IERR = IERR1
  999 END
c*
      SUBROUTINE IC_DROP_LOCK(NAME,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_DROP_LOCK
c*
c*       Drops resource lock.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT
      INTEGER LVBR(4)
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
c*
      IERR=0
      CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__LOCK) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
      IMODE = CACHE(NUM).LOCK
c**        convert system lock, resource lock to NL
      CALL IC_LOCKUP(IC__NL,NUM-1,LVBR,'Q',0.,IERR)
      DO I=1,4
        LVBR(I)=0
      ENDDO
      CALL IC_LOCKUP(IC__NL,NUM,LVBR,' ',0.,IERR)
  999 END
c*
      SUBROUTINE IC_CHECK_LOCK(NAME,LKHELD,
     +    IUSR,IPID,ICPU,PRCNM,CPUNM,CHOPT,IERR)
c************************************************************************
c*
c*       SUBR. IC_CHECK_LOCK
c*
c*       Checks resource lock. LKHELD is TRUE if the lock is held by
c*       a process somewhere.
c*       IPID,ICPU are the PID and CSID of the holder.
c*       PRCNM,CPUNM are the Process name and node name.
c*       IUSR is the user defined portion of the cache (2 words)
c*
c*       Options:
c*       U        All 16 bytes of the cache are user defined. The 16
c*                bytes are returned at IUSR. The IPID,ICPU,PRCNM,CPUNM
c*                parameters are not returned.
c*
c************************************************************************
      CHARACTER*(*) NAME,CHOPT,PRCNM,CPUNM
      CHARACTER PROCNAME*20
      INTEGER LVBR(4), IUSR(2)
      BYTE LVBB(16)
      EQUIVALENCE (LVBB,LVBR)
      LOGICAL LKHELD
      INCLUDE '($SYIDEF)'
      INCLUDE '($JPIDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      STRUCTURE /ITMLST/
        UNION
          MAP
            INTEGER*2 BUFLEN
            INTEGER*2 ITMCODE
            INTEGER BUFADR
            INTEGER RETADR
          END MAP
          MAP
            INTEGER END_LIST
          END MAP
        END UNION
      END STRUCTURE
      RECORD /ITMLST/ LKILIST(3)
      STRUCTURE /IOSBLK/
        INTEGER STS, RESERVED
      END STRUCTURE
      RECORD /IOSBLK/ IOSB
      CHARACTER NODE*15
      INTEGER SYS$GETSYIW, LIB$GETJPI
c*
      IERR=0
      CALL IC_ID_NUM(NAME,NUM,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      IF (CACHE(NUM).TYPE.NE.IC__LOCK) THEN
        CALL LIB$SIGNAL(%VAL(IC_E_INVMODE),%VAL(1),NAME(:LST(NAME)))
        IERR=IC_E_INVMODE
        GOTO 999
      ENDIF
      IMODE = CACHE(NUM).LOCK
      CALL IC_GET_CACHE_I(NUM,LVBR,' ',IERR)
      IF (IERR.NE.0) GOTO 999
      IF (INDEX(CHOPT,'U').EQ.0) THEN
        IF (LVBR(3).EQ.0) THEN
c**            PID field is empty; lock is not held.
          LKHELD = .FALSE.
          RETURN
        ENDIF
        LKHELD = .TRUE.
        IUSR(1) = LVBR(1)
        IUSR(2) = LVBR(2)
        IPID = LVBR(3)
        ICPU = LVBR(4)
        LKILIST(1).BUFADR = %LOC(NODE)
        LKILIST(1).BUFLEN = 15
        LKILIST(1).ITMCODE = SYI$_NODENAME
        LKILIST(1).RETADR = %LOC(LN)
        LKILIST(2).END_LIST = 0
        ISTAT = SYS$GETSYIW(,ICPU,,LKILIST,IOSB,,)
        IF (.NOT.ISTAT) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
        ENDIF
        CPUNM = NODE(:LN)
c*
        ISTAT = LIB$GETJPI(JPI$_PRCNAM,IPID,,IDUM,PROCNAME,LNP)
        PRCNM = PROCNAME(:LNP)
        IF (ISTAT.EQ.SS$_NONEXPR) THEN
          PRCNM = 'N/A'
        ELSE IF (ISTAT.EQ.SS$_NOPRIV) THEN
          PRCNM = 'NO PRIV'
        ELSE IF (.NOT.ISTAT) THEN
          CALL LIB$SIGNAL (%VAL(ISTAT))
          IERR = ISTAT
        ENDIF
      ELSE
c*           in user-defined cache mode, BYTE 1 MUST BE NONZERO WHEN
c*           BOOKED.
        IF (LVBB(1).EQ.0) THEN
c**            lock is not held.
          LKHELD = .FALSE.
        ELSE
          LKHELD = .TRUE.
          DO I=1,4
            IUSR(I)=LVBR(I)
          ENDDO
        ENDIF
      ENDIF
  999 END
c************************************************************************
      SUBROUTINE IC_CALL_UAST(NUM)
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INTEGER IPAR(2) /1,0/, LVBR(4)
      CHARACTER NAME*30
      CALL IC_NUM_ID(NUM,NAME,' ',IERR)
      IF (IERR.NE.0) GOTO 999
c*
c*         The problem that motivated this trick has gone away.
c*
c**        check that the bcast identifier has changed since the last
c*         broadcast on this cache
C        CALL IC_GET_CACHE('S_'//NAME,LVBR,' ',IERR)
C        IF (CACHE(NUM).BCID.NE.LVBR(3)) THEN
C          CACHE(NUM).BCID = LVBR(3)
      IROUTINE = CACHE(NUM).UAST
      IPAR(2) = %LOC(NUM)
c*           call the user routine
      CALL LIB$CALLG(IPAR,%VAL(IROUTINE))
C        ELSE
C          CALL IC_DELIVER(NAME,LVBR,' ',IERR)
C        ENDIF
  999 END
c************************************************************************
      SUBROUTINE IC_CRASH_UAST(NUM)
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INTEGER IPAR(2) /1,0/
      INTEGER LOCNUM /0/
c*
c**        pass to the user AST routine the index of the primary cache,
c*         not the system cache
      LOCNUM = NUM+1
      IROUTINE = CACHE(NUM).UAST
      IPAR(2) = %LOC(LOCNUM)
c*         call the user routine
      CALL LIB$CALLG(IPAR,%VAL(IROUTINE))
      END

      LOGICAL FUNCTION IC_BOOKED(NAME)
c************************************************************************
c*
c*       FUNC. IC_BOOKED
c*
c*       True if the specified cache is booked.
c*
c************************************************************************
      CHARACTER NAME*(*)
      CALL IC_ID_NUM(NAME,NUM,'N',IERR)
      IF (IERR.NE.0) THEN
        IC_BOOKED = .FALSE.
      ELSE
        IC_BOOKED = .TRUE.
      ENDIF
      END
