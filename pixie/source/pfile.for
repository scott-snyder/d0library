      SUBROUTINE PFILE(RUN,EVNUM,STREAM,OLDFILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Constructing file name from run# & event#
C-
C-   Inputs  : Run #,Event #,Stream
C-   Outputs : Filename(without extension)
C-   Controls: none
C-
C-   Created  15-FEB-1993   Vipin Bhatnagar
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
  
      INTEGER  RUN,EVNUM
      INTEGER  I,J,K,L,M,N,IS,JS,KS
C
      CHARACTER*60 OLDFILE
      CHARACTER*24 RUNC,EVNUMC,STREAM
      CHARACTER*24 EXT,STRINGS,STRINGE,STRINGR
C
      DATA EXT/'_'/
C----------------------------------------------------------------------
C
      STRINGS  = STREAM
      CALL SWORDS(STRINGS,IS,JS,KS)
      STREAM  = STREAM(1:3)//EXT
      STRINGS  = STREAM
      CALL SWORDS(STRINGS,IS,JS,KS)
      CALL PXITOC(RUN,8,RUNC)
      STRINGR = RUNC
      CALL SWORDS(STRINGR,I,J,K)
      RUNC = RUNC(I:J)//EXT
      STRINGR = RUNC
      CALL SWORDS(STRINGR,I,J,K)
      CALL PXITOC(EVNUM,8,EVNUMC)
      STRINGE = EVNUMC
      CALL SWORDS(STRINGE,L,M,N)
C
      OLDFILE = STRINGS(IS:KS)//RUNC(1:K)//EVNUMC(L:M)
C
  999 RETURN
      END
