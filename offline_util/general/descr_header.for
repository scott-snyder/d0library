      SUBROUTINE DESCR_HEADER(Ldescr_file,Loutfile)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write contents of 'description' file at the top
C-                         of an output file.
C-
C-   Inputs  : The two unit numbers which have been GTUNITed and D0OPENed
C-   Outputs : The entire contents of the description file are written
C-             to the outfil unit.
C-   Controls: Comment lines, marked with a '!' as the first character
C-             are skipped over.
C-
C-   Created  20-JAN-1993   R J Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      integer Loutfile,Ldescr_file
      character*80 Line
  100 read (Unit=Ldescr_file,fmt=810,end=999) Line
      IF (Line(1:2).eq.'!') goto 100
      write (Unit=Loutfile,fmt=800) Line
      goto 100
  800 format (1X,A80)
  810 format (A80)
  999 RETURN
      END
