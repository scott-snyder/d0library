      SUBROUTINE D0DAD_GETFNO(FNAME,COPT)
C-----------------------------------------------------------------------
C  Look for options on the command name passed in FNAME.
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,ICPOS,LENOCC
      EXTERNAL LENOCC
      CHARACTER*(*) FNAME,COPT*1
C
      DO I=1,LENOCC(FNAME)
         IF( FNAME(I:I).EQ.',' ) THEN
            ICPOS=I
            GOTO 10
         ENDIF
      ENDDO
      GOTO 999
C
 10   CONTINUE
      FNAME(ICPOS:ICPOS)=' '
      ICPOS=ICPOS+1
      COPT=FNAME(ICPOS:ICPOS)
      CALL CLTOU(COPT)
      FNAME(ICPOS:ICPOS)=' '
C
      DO I=ICPOS+1,LENOCC(FNAME)
         FNAME(I:I)=' '
      ENDDO
C
 999  CONTINUE
      RETURN
      END
