      SUBROUTINE XFOPEN(file,lfile,trans,ltrans)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
c     this routine takes the filename from the c-routine, hence
c     you have to convert from integer file to character file
c
c     if file_type=0, file is data, 1=stp, 2=output
C-
C-   Inputs  : file - c char string pointer to filename
C-             lfile = length
C-   Outputs : trans - c char string pointer to translation if it's
C-             a vax file (and could be a logical)
C-             ltrans = length of output
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer file(*),trans(*),lfile,ltrans
c
      integer tlen,length,ierr,trnlog,temp,istar,icfind
      logical ok
      character*1 xopt
      character*160 filename,msg,tank,translation
c
c     convert file to filename
c
      call cfchar(0,file,filename,lfile)
      length = lfile
      ltrans = lfile
c
c     check on length
c
      if (length.lt.1) then
        call xerrmsg('No Filename Specified')
        return
      endif
c
c     default translation
c
      do ierr=1,length/4+1
	trans(ierr) = file(ierr)
      enddo
c
c     wildcards?
c
      istar = icfind('*',filename,1,lfile) !istar>lfile no * present
      wildc = istar.le.lfile
C&IF VAXVMS
c
c     translate logical
c
      if (.not.wildc) then
        call trnlog(Filename,length,translation,ierr)
        if (ierr.eq.1) translation(1:length) = filename(1:length)
        temp = tlen(translation)
        translation(temp+1:temp+1) = char(0)
        call cfchar(1,trans,translation,ltrans)
c
c       see if there's a * in the translation
c
        istar = icfind('*',translation,1,temp) 
        wildc = istar.le.temp
      endif

C&ENDIF
c
      if (file_type.eq.0) then
c
c       get file name.  if there was no wildcard, then this will just
c       return the same name as input
c
        call parse_files(filename(1:length),ierr,.false.)
        call search_files(filename,ierr)
        translation = filename
        call cfchar(1,trans,translation,ltrans)
        if (ierr.eq.2) then
c
c         no files present?
c
          if (wildc) then
            call xerrmsg('No files fitting wildcard specification')
            return
          else
            call xerrmsg('File not found???')
          endif
        endif
        length = tlen(filename)
c
c       if something is already opened, close it properly
c
        if (fzrz_opened) call xfclose(0)
c
c       store filename for data and open
c
        fzrz_file(1:length) = filename(1:length)
        fzrz_spec = .true.
        fzrz_opened = .false.
        fzrz_len = length
        if (fzrz_mode.eq.0) then
          xopt = 'X'
        else
          xopt = 'N'
        endif
c
c       open the file
c
        call myopen(filename(1:length),xopt,fzrz_lun,ok)
        fzrz_opened = ok
        dbstore(0) = ixcom
        dbdiv(0) = ixmain
        if (.not.ok) then
          write(tank,'(''('''' FZ Open Failure On '''',a'',i2.2,'')'')')
     &      length
          write(msg,tank) filename(1:length)
          call xerrmsg(msg)
          return
        else
          call xerrmsg('File Successfully Opened')
        endif
c
      else if (file_type.eq.1) then
c
c       store filename for stp and open
c
        if (wildc) then
          call xerrmsg('Wildcards NOT allowed for STP files')
          return
        endif
        stp_file(1:length) = filename(1:length)
        stp_spec = .true.
        stp_opened = .false.
        stp_len = length
        call myistp(filename(1:length),ierr)
        if (ierr.ne.0) then
          write(tank,'(''(STP Open Failure On '''',a'',i2.2,'''''')'')')
     &      length
          write(msg,tank) filename(1:length)
          call xerrmsg(msg)
          return
        else
          call xerrmsg('File Successfully Opened')
          stp_opened = .true.
        endif
        dbstore(1) = ixstp
        dbdiv(1) = idvstp
        dbhead(1) = lstph
c
      else if (file_type.eq.2) then
c
c       store filename for output
c
        if (wildc) then
          call xerrmsg('Wildcards NOT allowed for OUTPUT files!')
          return
        endif
        out_file(1:length) = filename(1:length)
        out_spec = .true.
        out_opened = .false.
        out_len = length
        call gtunit(d0xuserunit,out_lun,ierr)
        open(unit=out_lun,file=filename(1:length),status='new')
        IQPRNT = out_lun
        IQPR2 = out_lun
        IQLOG = out_lun
        out_opened = .true.
      endif
c
      return
      end
