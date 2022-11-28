      integer function lib$get_foreign(str, prompt, outlen, force)
C----------------------------------------------------------------------
C-
C-   Name: lib$get_foreign
C-
C-   Purpose:  Get command line arguments
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = lib$get_foreign(str, prompt, outlen, force)
C-
C-   Arguments:
C-
C-     str (character, write only)    - Command line arguments.
C-     prompt (character, read only)  - Prompt
C-     outlen (integer*2, write only) - Returned length of str
C-     force (integer*4, modify)   - Force prompting if odd (.true.)
C-   
C-   Created   13-DEC-1991  Herbert Greenlee
C-
C-   Modified   4-NOV-1992  Soren G. Frederiksen
C-                          If prompt is not given, do not try and read 
C-                          from stdin
C-   Modified  30-Dec-1995  sss - Compile with g77.
C-
C-   Notes:
C-
C-   1.  Input prompting occurs if the command line was empty and the
C-       prompt argument is not null, or if force is true.  In the 
C-       latter case, the command line is ignored.
C-
C----------------------------------------------------------------------
      implicit none
      character*(*) str, prompt
      integer*2 outlen
      integer force
      integer i, j, len_str, next
      integer*8 d0_loc
      integer*8 d0_loci
      integer*8 d0_locs
      integer narg
      logical get_args
C----------------------------------------------------------------------
      str = ' '
      len_str = 0
      next = 1
C-
C- Decide if we want to get the command line.
C-
      if(d0_loci(force).eq.0)then
         get_args = .true.
      else
         if((iand(force,1)).eq.0)then
            get_args = .true.
         else
            get_args = .false.
         endif
         force = ior(force, 1)
      endif
C-
C- Read the command line
C-
      if(get_args)then
         narg = iargc()
         do 100 i=1,narg
            call getarg(i, str(next:))
            do 10 j=len(str),1,-1
               if(str(j:j).ne.' ')then
                  len_str = j
                  next = len_str + 2
                  go to 20
               endif
 10         continue
            len_str = 0
            next = 1
 20         continue
 100     continue
      endif
C-
C- If the command line is empty here read one line from standard input.
C-
      if(len_str.eq.0 .and. d0_loc(prompt).ne.0 )then
         if(d0_loc(prompt).ne.0)then
            write(*,'(1x,a,$)')prompt
         endif
         read(*,'(a)')str
      endif
C-
C- Scan the input line one last time to determine its length and convert to 
C- upper case.
C-
      len_str = 0
      do 110 j=1, len(str)
         if(str(j:j).ne.' ')then
            len_str = j
C            if(lge(str(j:j),'a') .and. lle(str(j:j),'z'))
C     &         str(j:j) = char(ichar(str(j:j)) + ichar('A') - 
C     &         ichar('a'))
         endif
 110  continue
C-
C- Return length to calling program.
C-
      if(d0_locs(outlen).ne.0)outlen = len_str
      lib$get_foreign = 1
 999  return
      end
