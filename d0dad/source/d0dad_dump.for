      SUBROUTINE D0DAD_DUMP(FNAME,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open an  event catalog file.
C-      Called from D0DAD_OPEN.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INCLUDE 'D0$INC:d0dad.inc/NOLIST'
      INTEGER ILUN,IERR
      CHARACTER*(*) FNAME
      INTEGER IFTYPE,IOPTS(5)
C----------------------------------------------------------------------
C
      IOPTS(1)=LECDEV
      IOPTS(2)=ISELR(1)
      IOPTS(3)=ISELR(2)
      IOPTS(4)=ISELE(1)
      IOPTS(5)=ISELE(2)

      CALL D0DAD_FTYPE(FNAME,IFTYPE)
      IF( IFTYPE.EQ.JFUE ) THEN

         CALL D0DAD_OPEN(JFUE,FNAME,'R',ILUN,IERR)
         IF( IERR.NE.0 ) GOTO 998
         CALL UEDUMP(ILUN,IOPTS,5,IERR)
         CALL D0DAD_CLOSE(ILUN,IERR)
         
      ELSEIF( IFTYPE.EQ.JFDF ) THEN

         CALL D0DAD_OPEN(JFDF,FNAME,'R',ILUN,IERR)
         IF( IERR.NE.0 ) GOTO 998
         CALL DFDUMP(ILUN,IOPTS,5,IERR)
         CALL D0DAD_CLOSE(ILUN,IERR)

      ELSEIF( IFTYPE.EQ.JFEC ) THEN

         CALL D0DAD_OPEN(JFEC,FNAME,'R',ILUN,IERR)
         IF( IERR.NE.0 .AND. IERR.NE.(-6) .AND. IERR.NE.(-7)) GOTO 998
         CALL ECDUMP(ILUN,IOPTS,5,IERR)
         CALL D0DAD_CLOSE(ILUN,IERR)

      ELSE

         IERR = -1

      ENDIF
C
 999  CONTINUE
      RETURN
C
 998  CONTINUE
      WRITE(*,9001) IERR,FNAME(1:60)
 9001 FORMAT(' DUMP: Error ',I8,' opening file: ',A)
      IERR = -2
C
      RETURN
      END
