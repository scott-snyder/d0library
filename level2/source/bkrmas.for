      SUBROUTINE BKRMAS(NPARIN,LRMAS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book RMAS, INV MASS RESULTS bank. 
C-
C-   Inputs  : NPARIN = number of parameter sets
C-   Outputs : LRMAS= pointer to RMAS
C-   Controls:
C-
C-   Created  15-DEC-1993 KATHY FATYGA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:RMAS.PARAMS'   ! params for RMAS
      INCLUDE 'D0$LINKS:IZRMAS.LINK'
      INTEGER LRMAS,LSUP1,NPARIN
      INTEGER IOH, GZFRES
      SAVE IOH
      INTEGER NSIZ                      ! Total number of data words
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C The size of the bank is the number of initial words filled only
C   once an event plus (the number of repeated words time the number
C   of parameter sets. 
      NSIZ = NMAIN_RMAS+(NPARIN*NREP_RMAS)
C
C The format is 3 integers as initial words, with a repeated set 
C   of 3 integers followed by 8 reals.
C
      IF ( FIRST ) THEN
        CALL MZFORM('RMAS','3I/3I8F',IOH)
        FIRST = .FALSE.
      END IF
C--- Get supporting link
        LSUP1 = GZFRES()
        IF (LSUP1 .LE. 0) RETURN
C---Book it
        CALL MZBOOK(IXMAIN,LRMAS,LSUP1,-IZRMAS,'RMAS',0,0,NSIZ,IOH,0)
C---Fill initial values
        IQ(LRMAS+MPVERS) = 1 
        IQ(LRMAS+MPPARS) = NPARIN
        IQ(LRMAS+MPSIZ)  = NREP_RMAS
C        
  999 RETURN
      END
