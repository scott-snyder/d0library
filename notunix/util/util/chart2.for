      PROGRAM CHART2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      include 'd0$inc:chart2.inc'

      integer      map_len
      character*80 map_name
      logical      map_nomore


      call parse_command
   50 continue
      call next_map(map_name,map_len,map_nomore)
      if (map_nomore) goto 999
          call doall(map_name(1:map_len),process)
      goto 50

C----------------------------------------------------------------------
  999 continue
      END
      SUBROUTINE show_options
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      include 'd0$inc:chart2.inc'

      character*70 rtlline
      integer      ibeg, iend, ilen

c      print *,' Current Option Settings: '
c      print *,' '
c      print *,'        Tree Depth: ',levls
c      print *,'          Compress: ',compress
c      print *,'         Trim Tree: ',trim
c      print *,'   UPPERCASE Input: ',forceup
c      print *,'   No Hanging Bars: ',nohang
c      print *,' Show Line Numbers: ',linenum

      write(*,8999)

      rtlline = ' Tree Depth: '
      ibeg = 14
      write(rtlline(ibeg:ibeg+1),8101) levls
      ibeg = ibeg+2
      if (trim) then
          ilen = 6
          write(rtlline(ibeg:ibeg+ilen-1),8102) ', Trim'
          ibeg = ibeg + ilen
      endif
      if (tight) then
          ilen = 7
          write(rtlline(ibeg:ibeg+ilen-1),8102) ', Tight'
          ibeg = ibeg + ilen
      elseif (compress) then
          ilen = 10
          write(rtlline(ibeg:ibeg+ilen-1),8102) ', Compress'
          ibeg = ibeg + ilen
      endif
      if (.not.nohang) then
          ilen = 19
          write(rtlline(ibeg:ibeg+ilen-1),8102) ', Allow hanging "!"'
          ibeg = ibeg + ilen
      endif
      if (.not.process) then
          ilen = 11
          write(rtlline(ibeg:ibeg+ilen-1),8102) ', NOprocess'
          ibeg = ibeg + ilen
      endif
      if (.not.linenum) then
          ilen = 15
          write(rtlline(ibeg:ibeg+ilen-1),8102) ', NOlinenumbers'
          ibeg = ibeg + ilen
      endif
      ilen = ibeg
 8000 format(2x,'Options - ',a<ilen>)
      write(*,8000) rtlline

      rtlline = ' '
      if (xppl) rtlline = 'PPL$ '//rtlline
      if (xdtk) rtlline = 'DTK$ '//rtlline
      if (xsmg) rtlline = 'SMG$ '//rtlline
      if (xstr) rtlline = 'STR$ '//rtlline
      if (xots) rtlline = 'OTS$ '//rtlline
      if (xsys) rtlline = 'SYS$ '//rtlline
      if (xmth) rtlline = 'MTH$ '//rtlline
      if (xlib) rtlline = 'LIB$ '//rtlline
      if (xfor) rtlline = 'FOR$ '//rtlline
c      print *,' Suppress RTL calls to : '
c      print *,'       ',rtlline
c      print *,' '
      call swords(rtlline,ibeg,iend,ilen)
      if (ilen.le.1) then
          write(*,8001)
      elseif(ilen.le.50) then
          write(*,8002) rtlline
      else
          write(*,8003), rtlline
      endif
      write(*,8999)

 8101 format(i2)
 8102 format(a<ilen>)
 8001 format(5x,'All RTL calls Enabled ')
 8002 format(5x,'Suppress RTL calls to: ',a<ilen>)
 8003 format(5x,'Suppress RTL calls to: ',/,5x,a<ilen>)
 8999 format(1x,' ')

C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE read_xref(nsym_read)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer nsym_read

      integer       ipass
      character*132 line
      character*32  words(16), cursym
      integer       lenw(16), nw, nwbeg
      logical       begxref, newsym
      byte          linchr(132), nowchr
      equivalence   (line,linchr(1))

      logical       skip, ovrflsym, ovrflref

      integer jj, iscalld, jref
      integer lookup, mark

      character*8 transadd

      include 'd0$inc:chart2.inc'

      do 10 jj = 0, 127
          istart(jj) = 0
          istop(jj)  = 0
   10 continue
      do 20 jj = 0, maxsym
          ncall(jj)    = 0
          iptrcall(jj) = 1
   20 continue
      nsym = 0
      nref = 0
      ovrflsym = .false.
      ovrflref = .false.
      longsym  = 0
      transfer = ' '

      ipass = 1

  200 continue
      rewind (1)
      begxref = .false.
      nowchr = 0
  100 continue
          read(1,8001,end=9000) line
 8001     format(a132)
          if (.not. begxref) then
c-
c- if not yet found Cross Ref section look for label
c- once found skip until we find 'Referenced By'
c-
              if (index(line,'Symbol Cross Reference') .ne. 0) then
                  begxref = .true.
  110             continue
                      read(1,8001,end=9000) line
                      if (index(line,'Referenced By') .ne. 0) goto 100
                  goto 110
              else
                  goto 100
              endif
          else
c-
c- inside cross reference section:
c-   Skip blank & column header lines
c-   lines with FF's signal new page, skip next line with page header info
c- look for marker signalling end of section
c-
              if (line .eq. ' ') goto 100
              if (index(line,'Referenced By') .ne. 0) goto 100
              if (index(line,'-------------') .ne. 0) goto 100
              if (ichar(line(1:1)) .eq. '0C'x) then
                  read(1,8001,end=9000) line
                  goto 100
              endif
c-
c- end of xref section with 'Key for special' or '+---------+'
c-
              if (index(line,'Key for special') .ne. 0) goto 9000
              if (index(line,'+-------')        .ne. 0) goto 9000
          endif
c-
c- Processing begins here ....
c-
          call chop(line,words,lenw,nw)
          newsym = .true.
          if (line(1:1) .eq. ' ') newsym = .false.

          if (ipass .eq. 1) then
c-
c- Pass 1 collects all the SYMBOLS (start in first column)
c-
              if (newsym) then
                  call skip_sym(words(1),skip)
                  if (.not. skip) then
c-
c- check if this is an external reference (unresolved)
c-
                      if (words(2) .eq. '00000000-*') then
                          mark = markextrn
                      else
                          mark = novisit
                      endif
                      call add_sym(words(1),
     &                    linchr(1),nowchr,ovrflsym,mark)
                      if (ovrflsym) goto 9000
                  endif
              endif
          else
c-
c- Pass 2 looks at 'Referenced by' columns
c-
              if (newsym) then
                  if (nsym .gt. maxsym) goto 9000
c-
c- find the number of the CALLEE
c-    skip if one of the excluded symbol types
c-    skip futher processing if can't find symbol in list
c-
                  cursym = words(1)
                  call skip_sym(cursym,skip)
                  if (skip) then
                      iscalld = -1
                      goto 100
                  endif
                  iscalld = lookup(cursym)
                  if (iscalld .le. 0) goto 100
                  nwbeg = 4
c-
c- check if this is the transfer address
c-
                  if (words(2)(1:8) .eq. transadd) then
                      transfer = cursym
                  endif
c-
c- check if this is an external reference (unresolved)
c-
                  if (words(2) .eq. '00000000-*') then
                      nwbeg = 3
                  endif
              else
c-
c- old symbol, ie continuation of "Referenced By"
c-
                  if (iscalld .le. 0) goto 100
                  nwbeg = 1
              endif
c-
c- for each of the CALLERS add CALLEE to list
c-
              do 500 jref = nwbeg, nw
                  call skip_sym(words(jref),skip)
                  if (skip) goto 500
                  call enter_lst(words(jref),iscalld)
  500         continue
          endif
      goto 100

 9000 continue
      begxref = .false.
      if (ipass .eq. 1) then
          istop(nowchr) = nsym
 9001     continue
          read(1,8001,end=9002) line
          if (index(line,'User transfer address').ne.0) then
              call chop(line,words,lenw,nw)
              transadd = words(4)(1:8)
              goto 9003
          else
              goto 9001
          endif
 9002     continue
          transadd = 'XXXXXXXX'
 9003     continue
          ipass = 2
          rewind (1)
          goto 100
      endif
C----------------------------------------------------------------------
  999 continue
      nsym_read = nsym
      RETURN
      END
      SUBROUTINE add_sym(cursym,linchr,nowchr,ovrflw,mark)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) cursym
      byte          linchr, nowchr
      logical       ovrflw
      integer       mark

      include 'd0$inc:chart2.inc'

      logical     skip
      integer     ibeg,iend,ilen

      call skip_sym(cursym,skip)

      if (.not. skip) then
          nsym = nsym + 1
          if (nsym .gt. maxsym) then
              print *,' ....  Too many Symbols ... skipping from ',
     &                cursym,' on ...'
              ovrflw = .true.
              goto 9000
          endif
          call swords(cursym,ibeg,iend,ilen)
          sym(nsym)      = cursym(ibeg:iend)
          lvisit(nsym,1) = mark
          longsym = max(ilen,longsym)
c-
c- MAP puts symbols in alphabetical order ...
c- help build pointers to first/last routine starting with single char "nowchr"
c-
          if (linchr .ne. nowchr) then
              istop(nowchr)  = nsym-1
              nowchr         = linchr
              istart(nowchr) = nsym
          endif
      endif

 9000 continue
C----------------------------------------------------------------------
  999 RETURN
      END
      integer FUNCTION lookup(symbol)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) symbol

      include 'd0$inc:chart2.inc'

      integer lchr, jstart, jstop, jj

      lchr = ichar(symbol)
      jstart = istart(lchr)
      jstop  = istop(lchr)
      do 400 jj = jstart, jstop
          if(symbol.ne.sym(jj)) goto 400
          lookup = jj
          goto 401
  400 continue
cccc      print *,' LOOKUP failed to find ',symbol
      lookup = -1
  401 continue

C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE enter_lst(caller,iscalld)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) caller
      integer       iscalld

      integer j, isxref, lookup, itmp

      include 'd0$inc:chart2.inc'

      if (iscalld .le. 0) goto 999

c-    look up position
c-    make entry into their list
c-
      isxref = lookup(caller)
      if (isxref .lt. 0) then
          print *,' ... caller ',caller,' calling ',
     &                    sym(iscalld),' not found ...'
          goto 999
      endif
c-
c- found the CALLER of iscalld
c-
cx      iptrcall(1) = 1
cx      do 1100 j = 2, nsym
cx          iptrcall(j) = iptrcall(j-1) + ncall(j-1)
cx 1100 continue
c-
c- enter CALLEE into CALLER'S list
c-   shove all later caller's lists up 1 in list
c-   increment the pointers of all later callers
c-
      itmp = iptrcall(isxref) + ncall(isxref)
      do 1200 j = nref, itmp, -1
          listcall(j+1) = listcall(j)
 1200 continue

c-no      do 1300 j = isxref, nsym-1
c-no          iptrcall(j+1) = iptrcall(j) + ncall(j)
      do 1300 j = isxref+1, nsym
          iptrcall(j) = iptrcall(j) + 1
 1300 continue


      itmp = iptrcall(isxref) + ncall(isxref)
      listcall(itmp) = iscalld
      ncall(isxref) = ncall(isxref) + 1
cc      if (ncall(isxref) .eq. 0) then
cc          iptrcall(isxref) =
cc     &                        iptrcall(isxref-1) + ncall(isxref-1)
cc          ncall(isxref) = 1
cc      else
cc          ncall(isxref) = ncall(isxref) + 1
cc      endif
      nref = nref + 1

cc      print 9001,caller,iscalld,sym(iscalld),itmp
cc 9001 format(' Enter ',a16,' list -- calls iptr=',i5,' = ',
cc     &    a16,' entry=',i5)

C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE skip_sym(symbol,skip)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) symbol
      logical       skip

      include 'd0$inc:chart2.inc'

      character*4 char4

      skip = .false.
      char4 = symbol(1:4)
      if (xfor .and. (char4 .eq. 'FOR$')) skip = .true.
      if (xlib .and. (char4 .eq. 'LIB$')) skip = .true.
      if (xmth .and. (char4 .eq. 'MTH$')) skip = .true.
      if (xsys .and. (char4 .eq. 'SYS$')) skip = .true.
      if (xots .and. (char4 .eq. 'OTS$')) skip = .true.
      if (xstr .and. (char4 .eq. 'STR$')) skip = .true.
      if (xsmg .and. (char4 .eq. 'SMG$')) skip = .true.
      if (xdtk .and. (char4 .eq. 'DTK$')) skip = .true.
      if (xppl .and. (char4 .eq. 'PPL$')) skip = .true.

C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE all_trees
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      include 'd0$inc:chart2.inc'

      character*32 top
      integer      lookup, nocalls(maxsym)
      integer      iptrt, ntop, itop, j

      do 10 j = 1, nsym
          if (lvisit(j,1) .ne. markextrn) lvisit(j,1) = novisit
          lvisit(j,2) = novisit
          lvisit(j,3) = novisit
   10 continue

      call build_top(nsym,nocalls,ntop)
      itop = 0
c-
c- deal with a transfer address
c-
      top = transfer
      if (transfer .eq. ' ') goto 200
      iptrt = lookup(top)
      if (iptrt .le. 0) goto 200
          if (tight) then
              print 8000
          else
              print 8100
          endif
 8000     format(' Transfer Address Routine ',/,
     &             '--------------------------')
 8100     format(/,' Transfer Address Routine ',/,
     &             '--------------------------')
      call tree(iptrt)
      call tree_trunc
c-
c- deal with entries with no calls (skip transfer if exists)
c-
  200 continue
      do 300 itop = 1, ntop
          if (nocalls(itop) .eq. iptrt) goto 300
          if (tight) then
              print 8001
          else
              print 8101
          endif
 8001     format(/,/,' Disconnected Tree ',/,
     &               '-------------------')
 8101     format(/,' Disconnected Tree ',/,
     &               '-------------------')
          call tree(nocalls(itop))
          call tree_trunc
  300 continue

c-
c- deal with unvisited entries
c- (should never happen should be caught by Disconnected)
c-
      do 400 itop = 1, nsym
          if (lvisit(itop,1) .eq. novisit) then
              print 8011
 8011         format(/,/,' Never Visited Tree ',/,
     &                   '-------------------')
              call tree(itop)
              call tree_trunc
          endif
  400 continue

      print *,' '
      print *,' '

C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE tree_trunc
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      include 'd0$inc:chart2.inc'

      integer itop

  450 continue
      do 500 itop = 1, nsym
      if (lvisit(itop,1) .eq. marktrunc) then
          if (tight) then
              print 8002
          else
              print 8102
          endif
 8002     format(/,' Truncated Tree ',/,
     &               '----------------')
 8102     format(/,/,' Truncated Tree ',/,
     &               '----------------')
          call tree(itop)
c-
c- go back to deal with ones truncated this pass
c-
          goto 450
      endif
  500 continue
c-
c- when we fall through, no more left truncated
c-
C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE build_top(mxno,nocalls,ntop)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer mxno, ntop
      integer nocalls(mxno)

      include 'd0$inc:chart2.inc'

      integer jsym, jref

      ntop = 0
      do 100 jsym = 1, nsym
          nocalls(jsym) = 0
  100 continue
c-
c- for each SYM search LISTCALL, for any calls to it
c- if none add it to NOCALL list
c-
      do 300 jsym = 1, nsym
          do 200 jref = 1, nref
              if (jsym .eq. listcall(jref)) goto 201
  200     continue
          ntop = ntop + 1
          nocalls(ntop) = jsym
  201     continue
  300 continue

c--test
c      do 500 j = 1, ntop
c          print *,' top level - ',j,sym(nocalls(j))
c  500 continue

C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE tree(itop)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer itop

      include 'd0$inc:chart2.inc'

      integer icall(0:maxlev), iptr(0:maxlev), mxptr(0:maxlev), ilev
      integer iprev, ilast
      logical islast(0:maxlev), istrunc

c
c icall = current pointer into SYM     for this level
c iptr  =    next pointer into LISTCALL for this level
c mxptr =    last pointer into LISTCALL for this level
c

      ilev = 1

      icall(ilev) = itop
      iprev = iptrcall(icall(ilev)) - 1
      ilast = iprev + ncall(icall(ilev))
      iptr(ilev)  = iprev
      mxptr(ilev) = ilast
      islast(ilev) = (iptr(ilev) .ge. mxptr(ilev))
      call prtlev(ilev,icall(ilev),islast,itop)

  100 continue
c-
c- move across at current level
c- print new entry if possible
c-    go down next level
c- otherwise go up level
c-
          iptr(ilev) = iptr(ilev) + 1
          islast(ilev) = (iptr(ilev) .ge. mxptr(ilev))

ccc          PRINT 9001,'ACROSS',ilev,icall(ilev),iptr(ilev),mxptr(ilev)
ccc 9001     format(1x,a10,' ilev=',i2,' icall=',i5,
ccc     &          ' iptr=',i5,' mxptr=',i5)

          if (iptr(ilev) .gt. mxptr(ilev)) goto 300
c-
c- if we're at max depth don't go down
c- otherwise if current routines calls others step down
c-
              if (ilev .eq. levls) goto 100
              if (ncall(icall(ilev)) .gt. 0) goto 200
           goto 100

  200 continue
c-
c- STEP DOWN a new sub-level
c-

ccc          PRINT 9001,'DOWN',ilev,icall(ilev),iptr(ilev),mxptr(ilev)
          ilev = ilev + 1
          icall(ilev) = listcall(iptr(ilev-1))

          iprev = iptrcall(icall(ilev)) - 1
          ilast = iprev + ncall(icall(ilev))
          iptr(ilev)  = iprev
          mxptr(ilev) = ilast
          islast(ilev) = (iptr(ilev) .ge. mxptr(ilev))
c-
c- test if this this node is going to be truncated
c- if so signal it for prtlev to make note of it
c- but dont' overwrite mark if it already is
c-
          istrunc = lvisit(icall(ilev),1) .eq. marktrunc
          if (ncall(icall(ilev)) .ne. 0 .and. ilev .eq. levls) then
              istrunc = .true.
              call mark_node(icall(ilev),marktrunc,itop)
          endif
c-
c- print this level out
c-
          call prtlev(ilev,icall(ilev),islast,itop)
c-
c- if "trimming" and we've been here before we go up
c-
          if (trim) then
              if (lvisit(icall(ilev),1).ne.novisit) goto 300
          endif
c-
c- update/mark node as we come up through it (ie print it)
c- mark it here is it calls nothing
c-
          call mark_node(icall(ilev),ilev,itop)
          goto 100

  300 continue
c-
c- STEP UP a sub-level
c-

ccc          PRINT 9001,'UP',ilev,icall(ilev),iptr(ilev),mxptr(ilev)

      ilev = ilev - 1
      if (ilev .eq. 0) goto 999
c-
c- update/mark node as we come up through it
c- to ensure being marked (should already be done)
c-
      call mark_node(icall(ilev),ilev,itop)
      goto 100

C----------------------------------------------------------------------
  999 continue
c--- marked on way up and out
c- top level, first line, it's own name
      lvisit(itop,1) = 1
      lvisit(itop,2) = 1
      lvisit(itop,3) = itop
cc-overwrite any "marktrunc"      call mark_node(itop,1,itop)
      RETURN
      END
      SUBROUTINE mark_node(isym,ilev,itop)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      integer isym, ilev, itop

      include 'd0$inc:chart2.inc'

      if (lvisit(isym,1) .ne. novisit) goto 999

      if (ncall(isym) .eq. 0) then
          lvisit(isym,1) = markleaf
      else
          lvisit(isym,1) = ilev
      endif
      lvisit(isym,2) = lstline
      lvisit(isym,3) = itop
C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE prtlev(ilev,iptr,islast,itop)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      include 'd0$inc:chart2.inc'
c-
c- islast definition must come after CHART2.INC for "MAXLEV"
c-
      integer ilev, iptr, itop
      logical islast(0:maxlev)


      character*264 line
      character*132 extra
      integer j, jline, jrout
      integer ioff, ibeg, iend, ilen, lstlevl, llvl, llin, iupp
      real    rj, rjline

cc      print 9001,ilev,(islast(j),j=1,ilev)
 9001 format(' ilev=',i3,' islast= ',20l2)

c-
c- decide on EXTRA text to signal Trimmed or Truncated branches
c-
C- j = level, jline = line, jrout = routine - where this was visited
c-
      j   = lvisit(iptr,1)
      if (j.eq.novisit) then
          extra = ' '
      elseif (j.eq.markleaf) then
          extra = ' '
      elseif (j.eq.markextrn) then
          extra = '(External)'
      elseif (j.eq.marktrunc) then
          extra = '(*) '
      elseif (.not.trim) then
          extra = ' '
      else
          jline = lvisit(iptr,2)
          jrout = lvisit(iptr,3)
          extra = ' '
          rj  = max(float(j),1.0)
          rjline = max(float(jline),1.0)
          llvl = max(1,int(log10(rj )+1.))
          llin = max(1,int(log10(rjline)+1.))
          call swords(sym(jrout),ibeg,iend,ilen)
c-
c- rwh: add suppression of line number if flag is FALSE
c-
          if (itop .ne. jrout) then
            if (linenum) then
              write(extra,8801) sym(jrout)(1:ilen), jline
 8801         format('(',a<ilen>,', line ',i<llin>,')')
            else
              write(extra,8803) sym(jrout)(1:ilen)
 8803         format('(',a<ilen>,')')
            endif
          elseif (linenum) then
              write(extra,8802) jline
 8802         format('(line ',i<llin>,')')
          endif

ccc          write(extra2,8801) sym(jrout)(1:ilen), j, jline
ccc 8801     format('(',a<ilen>,
ccc     &        ' lvl ',i<llvl>,', line ',i<llin>,')')
      endif
c-
c- do the Real work
c-
      if (ilev .le. 0) then
          lstline = lstline + 1
          lstlevl = 1
          print 8000, ilev
      elseif (ilev .eq. 1) then
          lstline = 1
          lstlevl = 1
          if (tight) then
              print 8001, sym(iptr)
          else
              print 8101, sym(iptr)
          endif
      elseif (ilev .gt. maxlev) then
c-
c- level too deep
c-
          lstline = lstline + 1
          print 8999, ilev
      else

 8901     format(a<iend>)
          line = ' '
          lstline = lstline + 1
          ioff = 11
c-
c- put in other level's continuation '|'
c-
          do 100 j = 1, ilev-2
              if (islast(j).and.nohang) then
                  line(ioff:ioff) = ' '
              else
                  line(ioff:ioff) = '|'
              endif
              ioff = ioff + 5
  100     continue
          line(ioff:ioff) = '|'
c-
c- every 5 put in a line count
c-
          if (linenum .and. mod(lstline,5).eq.0)
     &        write(line(2:5),8902) lstline
c-
c- if compress skip line of '|'s if on same level
c-
          if (.not.tight) then
          if ((.not.compress) .or.
     &        (lstlevl .ne. ilev)) then
              call swords(line,ibeg,iend,ilen)
              print 8901, line
              lstline = lstline + 1
          endif
          endif

cccc          ioff = ioff-5
c-
c- every 5 put in a line count
c-
          line(1:5) = ' '
          if (linenum .and. mod(lstline,5).eq.0)
     &        write(line(2:5),8902) lstline

 8902     format(i4)

          call swords(sym(iptr),ibeg,iend,ilen)
          iupp = ioff + 4 + ilen
          line(ioff:ioff+4+ilen) = '+---'//sym(iptr)(ibeg:iend)
cc//extra
          ioff = iupp+1
          line(ioff:ioff+64) = extra
c--
          call swords(line,ibeg,iend,ilen)
          print 8901, line
ccc          if (jrout .gt. 0) then
ccc              line(ioff:ioff+60) = '|   '//extra2
ccc              call swords(line,ibeg,iend,ilen)
ccc              print 8901, line
ccc              lstline = lstline + 1
ccc          endif
          lstlevl = ilev
      endif

 8000 format(/,1X,' PRTLEV called with ilev=',i5)
 8001 format(1X,'Entry:',2X,a32)
 8101 format(/,1X,'Entry:',2X,a32)
cxxx  ,/,10X,'|')
 8002 format(
     &       3X,'|',/,
     &       3X,'+---',a32,1x,a30)
 8999 format(' PRTLEV - called with ilev=',i5,' Too many levels')

c 8001 format( 4x,a32)
c 8002 format( 8x,a32)
c 8003 format(12x,a32)
c 8004 format(16x,a32)
c 8005 format(20x,a32)
c 8006 format(24x,a32)
c 8007 format(28x,a32)
c 8008 format(32x,a32)
c 8009 format(36x,a32)
c 8010 format(40x,a32)
C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE parse_command
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      include 'd0$inc:chart2.inc'

      integer*4  chart2_tables
      external   chart2_tables

      integer*4  lib$get_foreign
      integer*4  cli$dcl_parse
      integer*4  cli$present
      integer*4  cli$get_value
      integer*4  lib$get_input
      external   lib$get_input

      integer*4  status, itmp

      character*80 temp
      character*3  char3
      integer*2    len
      logical      xall

      character*255 cmd_line

c- retrieve the command line, die if we can't for some reason
c-
      status = lib$get_foreign(cmd_line)
      if (.not.status) then
          call lib$signal(%val(status))
          call exit
      endif
c-
c- pre-pend with the correct VERB
c-
      cmd_line = 'CHART2 '//cmd_line
c-
c- submit to CLI$DCL_PARSE to make sure it is COMPLETE and VALID.
c- missing requirements will be prompted for by LIB$GET_INPUT.
c-
      status = cli$dcl_parse(cmd_line,chart2_tables,lib$get_input)
      if (.not.status) then
          call lib$signal(%val(status))
          call exit
      endif

C----------------------------------------------------------------------

 8001 format(i<len>)

      levls  = 10
      status = cli$present('DEPTH')
      if (status) then
          status = cli$get_value('DEPTH',temp,len)
          read(temp,8001) itmp
          if (itmp .ge. minlev .and. itmp .le. maxlev) then
              levls = itmp
          else
              print 8101,temp,minlev,maxlev, levls
 8101         format('  /DEPTH=',a<len>,' not in range [',
     &            i2,':',i2,'], use default ',i2)
          endif
      endif

      trim      = cli$present('TRIM')
      nohang    = .not.cli$present('HANG')
      compress  = cli$present('COMPRESS')
      tight     = cli$present('TIGHT')
      if (tight) compress = .true.

      process   = cli$present('PROCESS')
      showops   = cli$present('SHOWOPTIONS')
      linenum   = cli$present('LINENUMBERS')

      xfor = .true.
      xlib = .true.
      xmth = .true.
      xsys = .true.
      xots = .true.
      xstr = .true.
      xsmg = .true.
      xdtk = .true.
      xppl = .true.
      status   = cli$present('RTL')
      if (status) then
          status = cli$get_value('RTL',temp,len)
          xall = .not.status
          do while (status .or. xall)
              char3 = temp
              xall = char3 .eq. 'ALL'
              if (xall .or. (char3 .eq. 'FOR')) xfor = .false.
              if (xall .or. (char3 .eq. 'LIB')) xlib = .false.
              if (xall .or. (char3 .eq. 'MTH')) xmth = .false.
              if (xall .or. (char3 .eq. 'SYS')) xsys = .false.
              if (xall .or. (char3 .eq. 'OTS')) xots = .false.
              if (xall .or. (char3 .eq. 'STR')) xstr = .false.
              if (xall .or. (char3 .eq. 'SMG')) xsmg = .false.
              if (xall .or. (char3 .eq. 'DTK')) xdtk = .false.
              if (xall .or. (char3 .eq. 'PPL')) xppl = .false.
              status = cli$get_value('RTL',temp,len)
              xall = .false.
          end do
      endif

      if (showops) call show_options

C----------------------------------------------------------------------
  999 continue
c
C----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE doall(map_name,process)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) map_name
      logical       process

      integer ios, lennam, nsym_read

      lennam = len(map_name)

 8001 format(' ================',<lennam>('='))
 8002 format(' process file  : ',a<lennam>)
 8003 format(' %CHART2-I-WOULDDO ',a<lennam>)
 8004 format(' %CHART2-W-NOTMAP - File: ',a<lennam>,
     &       ' not a /CROSS_REFERENCE map file')
 8005 format(' %CHART2-W-FILOPNFAIL - IOS=',i3,' File: ',a<lennam>)

      if (process) then
          open(unit=1,file=map_name,readonly,status='old',
     &        err=900,iostat=ios)
          call read_xref(nsym_read)
          close (1)
          if (nsym_read .gt. 0) then
              print 8001
              print 8002, map_name
              print 8001
              call all_trees
          else
              print 8004, map_name
          endif
      else
          print 8003, map_name
      endif
      goto 999
  900 continue
      print 8005, ios, map_name
C----------------------------------------------------------------------
  999 RETURN
      END
      SUBROUTINE next_map(map_name,map_len,map_nomore)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-AUG-1990   Robert Hatcher
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) map_name
      integer       map_len
      logical       map_nomore

      include '($RMSDEF)'
      include '($FABDEF)'
      include '($NAMDEF)'

      record /FABDEF/ fab_wild         ! this_fab
      record /NAMDEF/ nam_wild

      common /keepit/ fab_wild, nam_wild

      character*(nam$c_maxrss) fullstr, expstr, resstr, tempstr
      byte fullname(nam$c_maxrss)
      byte expanded(nam$c_maxrss)
      byte result(nam$c_maxrss)
      byte template(nam$c_maxrss)

      equivalence(fullstr,fullname)
      equivalence( expstr,expanded)
      equivalence( resstr,result)
      equivalence(tempstr,template)

      integer*4  cli$get_value
      integer*4  sys$parse
      integer*4  sys$search

      integer*4  status, lenfull
      logical    first, stat_get, wild_done

      integer    ibeg, iend, ilen

      data tempstr   / '[]*.MAP' /
      data first     / .true.  /
      data stat_get  / .false. /
      data wild_done / .true.  /

c-
c- Initialize the wildcard FAB & NAM
c-
      if (first) then

          fab_wild.fab$b_bid = fab$c_bid        ! block id for fab  IMPORTANT
          fab_wild.fab$b_bln = fab$c_bln        ! block length      IMPORTANT
          fab_wild.fab$l_fna = %loc(fullname)   ! File spec string add
          fab_wild.fab$b_fac = fab$m_get        ! 0 !=readonly  ! File access
          fab_wild.fab$l_fop = fab$m_nam        ! processing options
          fab_wild.fab$l_nam = %loc(nam_wild)   ! name block address
          fab_wild.fab$l_dna = %loc(template)   ! default file spec address
          fab_wild.fab$b_dns = nam$c_maxrss     ! default file spec length
          fab_wild.fab$b_shr = fab$m_shrget     ! share privleges

          nam_wild.nam$b_bid = nam$c_bid        ! block id for nam  IMPORTANT
          nam_wild.nam$b_bln = nam$c_bln        ! block length      IMPORTANT
          nam_wild.nam$b_ess = nam$c_maxrss
          nam_wild.nam$l_esa = %loc(expanded)
          nam_wild.nam$b_rss = nam$c_maxrss
          nam_wild.nam$l_rsa = %loc(result)

      endif

      map_nomore = .false.
c-
c- get the CLI file name(s)
c-
  100 continue
      if (first .or. wild_done) then
          stat_get = cli$get_value('MAP_FILE',fullstr,lenfull)
          if (stat_get) then
cc              call uctoh(fullstr,fullname,4,lenfull)
          else
              if (first) then
                  fullstr = tempstr
                  lenfull = len(tempstr)
cc                  call uctoh(tempstr,fullname,4,5)
cc                  lenfull = 5
              else
                  map_nomore = .true.
                  goto 999
              endif
          endif
c-
c- First we must "parse" the wildcarded file name
c-
          first = .false.
          wild_done = .false.

c--byte=int*4          fab_wild.fab$b_fns = lenfull    ! length of file spec
c-intrins func mismat          call mvbits(lenfull,0,8,fab_wild.fab$b_fns,0)
          call sbyt(lenfull,fab_wild.fab$b_fns,1,8)

          status = sys$parse(fab_wild)
          if ((status .and. 1) .ne. 1) then
              print *,' error ',status,
     &                ' on SYS$PARSE of ',fullstr(1:lenfull)
              call lib$signal(%val(status))
              call exit
          endif
      endif
c-
c- Finally we do a search
c-
      status = sys$search(fab_wild)
      if ((status .and. 1) .ne. 1) then
          if (status .eq. RMS$_NMF .or. status .eq. 99018) then
              wild_done = .true.
              goto 100
          elseif (status .eq. RMS$_FNF) then
              wild_done = .true.
              call swords(fullstr,ibeg,iend,ilen)
              print 8200, fullstr(ibeg:iend)
 8200         format(' %CHART2-W-FILNOTFOU ',a<ilen>)
              goto 100
          else
              print *,' error on SYS$SEARCH ',status
              call lib$signal(%val(status))
              call exit
          endif
      endif

cc      map_len  = nam_wild.nam$b_rsl
cc      call uhtoc(result,4,resstr,map_len)

      map_name = resstr
      map_len  = nam_wild.nam$b_rsl

cc      print *,' map_name = ',map_name(1:map_len)
cc      print *,' map_nomore = ',map_nomore

C----------------------------------------------------------------------
  999 RETURN
      END
