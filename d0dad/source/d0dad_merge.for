      SUBROUTINE D0DAD_MERGE(INNAME,OUTNAME,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Allow d0dad files of similar type to be 
C-     merged.  Restrictions based on file type are described in the
C-     relevant routines.
C-
C-   Inputs  : INNAME  - Name of first file to be merged (or FILE_NAMES)
C-             OUTNAME - Name of second file (output) to be merged
C-   Outputs : IERR    - 0 ==> All is well.
C-   Controls:
C-
C-   Created   1-Mar-1995   John D. Hobbs
C-   Updated  15-May-1995   JDH - Change to D0OPEN
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      INCLUDE 'D0$INC:d0dad.inc'
      CHARACTER*(*) INNAME,OUTNAME,HERE
      CHARACTER*255 INLINE
      PARAMETER(HERE='D0DAD_MERGE')
      INTEGER IERR
      INTEGER FILETYPE
      INTEGER LENOCC
C-----------------------------------------------------------------------
C
      IF(LDDBG.GT.4) THEN
        WRITE(*,1001)INNAME(1:LENOCC(INNAME)),OUTNAME(1:LENOCC(OUTNAME))
 1001   FORMAT(/,
     >         ' ****** In D0DAD_MERGE *******',/,
     >         '   InputFile:  ',A,/,
     >         '   OutputFile: ',A,/,/)
      ENDIF
C
      IERR=0
      FILETYPE=0
      CALL D0DAD_FTYPE(INNAME,FILETYPE)
      IF( FILETYPE.LT.1 .OR. FILETYPE.GT.NFTYPE  ) THEN
        CALL D0OPEN(IDADOK,INNAME(1:LENOCC(INNAME)),'I',IERR)
        FNTMP=' '
        READ(IDADOK,'(A)') inline
        CALL GETWORD(INLINE,1,FNTMP)
        CLOSE(IDADOK)
        CALL D0DAD_FTYPE(FNTMP,FILETYPE)
        IF( FILETYPE.LT.1 .OR. FILETYPE.GT.NFTYPE ) THEN
          WRITE(D0DAD_ERRTXT,9001) INNAME(1:LENOCC(INNAME))
 9001     FORMAT('Cannot derive file type from ',A)
          IF(LDDBG.GT.0)CALL ERRMSG('UnknownFile',HERE,D0DAD_ERRTXT,'E')
          IERR = -1
          GOTO 999
        ENDIF
      ENDIF
C
      IF( FILETYPE.EQ.JFDF ) THEN
        CALL D0DAD_DFMERGE(INNAME,OUTNAME,IERR)
      ELSEIF( FILETYPE.EQ.JFEC ) THEN
        CALL D0DAD_ECMERGE(INNAME,OUTNAME,IERR)
      ELSE
        WRITE(D0DAD_ERRTXT,9003) CFTYPE(FILETYPE)
 9003   FORMAT('Impossible to merge ',A2,' files')
        IF(LDDBG.GT.0) CALL ERRMSG('CannotMerge',HERE,D0DAD_ERRTXT,'E')
        IERR = -3
        GOTO 999
      ENDIF
C
      IF(IERR.NE.0 ) THEN
        WRITE(D0DAD_ERRTXT,9002) IERR,CFTYPE(FILETYPE)
 9002   FORMAT('Error ',I3,' from ',A2,'MERGE')
        IF(LDDBG.GT.0) CALL ERRMSG('MergeError',HERE,D0DAD_ERRTXT,'E')
        IERR = -2
      ENDIF
C
  999 RETURN
      END
