      SUBROUTINE PIKNEXFL(DIRNM,MAXITM,MAXFIL,LISFIL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get the list of files for Next Event
C-      command for Single file mode
C-   Inputs  : Dir name, no.of files to look
C-   Outputs : no.of files found, file names
C-   Controls:
C-
C-   Created   8-FEB-1993   Vipin Bhatnagar
C-   Modified  23-MAY-1993  Vipin Bhatnagar
C-     Clearing filenames in list LISFIL from previous call
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXCOMK.INC'
C
      INTEGER MAXITM,MAXFIL,KK,II,JJ,I
      INTEGER IFM,NFM,LFM
      CHARACTER*60 DIRNM,INFILE,LISFIL(*),FILENM,EXT
      LOGICAL FIRST

      DATA FIRST/.TRUE./
      DATA EXT/'_*.*'/
C----------------------------------------------------------------------
C
      CALL SWORDS(DIRNM,II,JJ,KK)
      FILENM = STREAM1(1:3)//EXT
      CALL SWORDS(FILENM,IFM,NFM,LFM)
      INFILE = DIRNM(1:KK)//FILENM(IFM:NFM)
      CALL SWORDS(INFILE,IFM,NFM,LFM)
C-
      IF (FIRST) THEN
        FIRST = .FALSE.
        GOTO 900
      ELSE
        MAXFIL = 0
        DO I=1,MAXITM
          LISFIL(I) = ' '
        ENDDO
      ENDIF
C
  900 CONTINUE
C
      CALL FILLST(INFILE(IFM:NFM),MAXITM,MAXFIL,LISFIL)

  999 RETURN
      END
