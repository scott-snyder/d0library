      SUBROUTINE D0DAD_GTUNIT(INUNIT,IKEY,IERR)
C-------------------------------------------------------------------------
C
C  Author:  John D. Hobbs
C  Date:     16-DEC-1993
C
C  Modified: 17-MAY-1995 JDH - fix decrement of IKEY
C
C  INPUTS:
C  OUTPUTS: 
C     INUNIT - I - Unit number allocated via GTUNIT
C     IKEY   - I - Key number which gave OK error code from GTUNIT
C     IERR   - I - 0 ==> All is OK.
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUNIT,IERR,ITEST,ICOUNT,IKEY
C-------------------------------------------------------------------------
      ITEST=-1
      ICOUNT=0
      IKEY=1000
      IERR=0
C
      DO WHILE(ITEST.NE.0 )
         IKEY=IKEY-1
         CALL GTUNIT(IKEY,INUNIT,ITEST)
         ICOUNT=ICOUNT+1
         IF( ICOUNT.GT.500 ) THEN
            IERR = -1
            RETURN
         ENDIF
      ENDDO
C
      RETURN
      END
