      SUBROUTINE MUUSER_HIST(IHOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface to a user histogram package.
C-
C-       This routine is called from F/N MURECO_HST.  In order to 
C-   this routine to be called,  user have to set a positive ID offset
C-   in MURECO.RCP with a key word, 'HIST_USER'.    For example
C-       HIST_USER   10000
C-   where 10000 is the offset for the histogram id number.   In an
C-   execution section of this routine below,  an example how to use 
C-   IHOFF is shown.
C-
C-       The HBOOK derectory has been set to that defined in the same
C-   MURECO.RCP file before this routine is called.   Unless use wants
C-   to create histograms in his/her own directory, he/she does NOT
C-   have to set the directory in this routine.
C-
C-
C-   Inputs  : IHOFF     I     offset for histogram id numbers.
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-MAR-1990   Shuichi Kunori
C-   Updated   5-OCT-1992   Daria Zieminska  (for Gene Alvarez)
C-                          select 2MU events 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IHOFF,GZPROC,LPROC,LPARH,LPMUO,QUALITY,NGOLD,NGOOD,IERR

      INCLUDE 'D0$INC:ZEBCOM.INC'
C-----------------------------------------------------------------------
      LPROC = GZPROC()
      LPARH = LQ(LPROC - 5)
      LPMUO = LQ(LPARH - 1)

      IERR  = 0
      NGOLD = 0
      NGOOD = 0
      CALL FLGSET('WRITE_STREAM_2MU',.FALSE.)

C-- To look for IFW4 = 0, 1 muons in the PMUO banks.

   10 IF (LPMUO .LE. 0) THEN
        GOTO 20
      ELSE
        QUALITY = IQ(LPMUO + 9)
        IF (QUALITY .LE. 1) THEN
          IF (QUALITY .EQ. 0) NGOLD = NGOLD + 1
          IF (QUALITY .LE. 1) NGOOD = NGOOD + 1
          LPMUO = LQ(LPMUO)
          GOTO 10
        ELSE
          LPMUO = LQ(LPMUO)
          GOTO 10
        ENDIF
      ENDIF

   20 IF (NGOLD .GT. 0 .AND. NGOOD .GT. 1) 
     & CALL FLGSET('WRITE_STREAM_2MU',.TRUE.)

  999 RETURN
      END
