      SUBROUTINE D0DAD_DFTYPE(FNAME,IERR)
C-------------------------------------------------------------------------
C  If the input file, FNAME, is a d0dad file, IERR=0.
C
C  Author:  John D. Hobbs
C  Date:     16-DEC-1993
C  Mods:
C   94/03/28 JDH - Add call to d0dad_isdisk in order to avoid opening
C     tape files too many times.
C     
C
C  INPUTS: 
C     FNAME  - C - Name of input file.
C  OUTPUTS: 
C     IERR   - I - IERR=0 ==> Input file is d0dad file 
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) FNAME
      INTEGER IERR
      INTEGER IFTYPE
      LOGICAL D0DAD_ISDISK
      EXTERNAL D0DAD_ISDISK
C ---------------------------------------------------------------------
C
      IF( .NOT.D0DAD_ISDISK(FNAME) ) THEN
        IERR = -1
        GOTO 999
      ENDIF
C
      CALL D0DAD_FTYPE(FNAME,IFTYPE)
      IF( IFTYPE.EQ.JFDF ) THEN
         IERR = 0
      ELSE
         IERR = -1
      ENDIF
C
 999  CONTINUE
      RETURN
      END
