      SUBROUTINE BITMASK_STATS_WRITE(ILUN,TITLE,NBITS,NTOT,CTITLE,
     >  COUNTER,CORRELATE,DOCORR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Print summary of arrays filled in BITMASK_STATS
C-     Each line is printed
C-
C-   Inputs  : ILUN   - Logical unit for output
C-             TITLE  - Summary title
C-             NBITS  - Number of bits for which stats were kept.
C-             NTOT   - Number of events 
C-             CTITLE - Title for each bit (up to 20 characters)
C-             COUNTER- Variable filled in BITMASK_STATS
C-             CORRELATE - Arrary filled in BITMASK_STATS
C-             DOCORR - If .TRUE. ==> Print 2x2 correlated statistics as well
C-               as total correlations
C-   Outputs :
C-   Controls:
C-
C-   Created   4-Feb-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILUN,NBITS,NTOT,COUNTER(2,*),CORRELATE(NBITS,*)
      LOGICAL DOCORR
      INTEGER I,J
      CHARACTER*(*) TITLE,CTITLE(*),DELIM*1
C&IF VAXVMS
      PARAMETER(DELIM='+')
C&ELSE
C&      PARAMETER(DELIM=' ')
C&ENDIF
C-----------------------------------------------------------------------
C
      WRITE(ILUN,1001) TITLE,NTOT
      DO I=1,NBITS
        WRITE(ILUN,1002) DELIM,CTITLE(I),COUNTER(1,I),COUNTER(2,I)
        IF( DOCORR ) THEN
          DO J=1,I
            WRITE(ILUN,1003) DELIM,CORRELATE(I,J)
          ENDDO
        ENDIF
        WRITE(ILUN,1004)
      ENDDO
      WRITE(ILUN,1005)
C
  999 RETURN
 1001 FORMAT(/,'  Statistics Summary: ',A,',',I10,' events.',/)
 1002 FORMAT(A1,A20,' | ',I7,' | ',I7,' || ',$)
 1003 FORMAT(A1,' ',I7,$)
 1004 FORMAT(' ')
 1005 FORMAT(/)
      END
