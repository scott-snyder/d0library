      SUBROUTINE D0DAD_CLOSE(ILUN,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Close a file opened by D0DAD_OPEN.
C-
C-   Inputs  : ILUN  - Logical unit
C-   Outputs : IERR  - 0 ==> All is well
C-   Controls:
C-
C-   Created  26-Jan-1994   John D. Hobbs
C-   Modified 31-Mar-1994   JDH - Check KEY value(for case of CFxxx I/O)
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IERR
      INTEGER LZFIND,LTMP
      EXTERNAL LZFIND
C-----------------------------------------------------------------------
C
C  Is it an EC?
C
      LTMP=LZFIND(IXDDAD,LQ(LDADH-JFEC),ILUN,JLUN)
      IF( LTMP.GT.0 ) THEN
        CALL ECCLOS(ILUN,IERR)
        IF( IERR.NE.0 ) GOTO 998
        IQ(LDADH+JFEC)=IQ(LDADH+JFEC)-1
        LTMP=LECHD
        GOTO 10
      ENDIF
C
C Is it a DF?
C
      LTMP=LZFIND(IXDDAD,LQ(LDADH-JFDF),ILUN,JLUN)
      IF( LTMP.GT.0 ) THEN
        CALL DFCLOS(ILUN,IERR)
        IF( IERR.NE.0 ) GOTO 998
        IQ(LDADH+JFDF)=IQ(LDADH+JFDF)-1
        GOTO 10
      ENDIF
C
C  Is it a UE?
C
      LTMP=LZFIND(IXDDAD,LQ(LDADH-JFUE),ILUN,JLUN)
      IF( LTMP.GT.0 ) THEN
        CALL UECLOS(ILUN,IERR)
        IF( IERR.NE.0 ) GOTO 998
        IQ(LDADH+JFUE)=IQ(LDADH+JFUE)-1
        GOTO 10
      ENDIF
C
C  Is it an FC?
C
      LTMP=LZFIND(IXDDAD,LQ(LDADH-JFFC),ILUN,JLUN)
      IF( LTMP.GT.0 ) THEN
        CALL FCCLOS(ILUN,IERR)
        IF( IERR.NE.0 ) GOTO 998
        IQ(LDADH+JFFC)=IQ(LDADH+JFFC)-1
        GOTO 10
      ENDIF
C
C  None of the above ==> Error.
C
      GOTO 997
C
 10   CONTINUE
      IF( IQ(LTMP+JKEY).NE.0 ) CALL RLUNIT(IQ(LTMP+JKEY),ILUN,IERR)
      CALL MZDROP(IXDDAD,LTMP,' ')
C
 999  CONTINUE
      IERR=0
      RETURN
C
 997  CONTINUE
      IERR = -1
      RETURN
C
 998  CONTINUE
      WRITE(D0DAD_ERRTXT,9098) IERR
 9098 FORMAT('Error ',I3,' returned from xxCLOS')
      IF(LDDBG.GT.0) CALL ERRMSG('xxCloseError','D0DAD_CLOSE',
     + D0DAD_ERRTXT,'E')
      IERR = -2
      RETURN
      END
