      SUBROUTINE STRHST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        Store all histograms in standard HBOOK4 format
C-
C-   ENTRY SETSTR(FLAG)
C-   INIRUN calls SETSTR(.TRUE.) if histograms are to be stored
C-
C-
C-   Created   9-MAR-1988   Serban D. Protopopescu
C-   Updated  25-OCT-1993   William Cobau  - add check for logical hst4_file 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FLAG,STRFL,OK,FIRST
      INTEGER NRUN,RUNNO,OUT,ERR,ICYCLE,N
      CHARACTER*20 HISFIL
      CHARACTER*80 MSG
      DATA FIRST/.TRUE./
C
C- variables for checking if there is a logical for storing histograms
C
      CHARACTER*80 LOGNAME, LOGTRANS
      DATA logname /'HST4_FILE'/
      INTEGER ISTAT, LENGTH, TRNLNM
C----------------------------------------------------------------------
      IF(STRFL) THEN
        NRUN=RUNNO()
C
        CALL STRINT ('D0USER_',NRUN,HISFIL,N)
        HISFIL=HISFIL(1:N)//'.HST4'
        MSG=' Storing all histograms in '//HISFIL
C
C- check is logical is set pointing to histogram file,
C-   if so, store histograms in that file
C
        ISTAT=TRNLNM(LOGNAME,LOGTRANS,LENGTH)
        IF(ISTAT.AND.(LENGTH.GT.0))THEN
          MSG=' Storing all histograms in '//logtrans
          WRITE (HISFIL,1000)
 1000     format('hst4_file')
        ENDIF
C
        CALL INTMSG(MSG)
        CALL HCDIR('//PAWC',' ')
        CALL HRPUT(0,HISFIL,'NT')
      ENDIF
C
      RETURN
C
      ENTRY SETSTR(FLAG)
      DATA STRFL/.FALSE./
      STRFL=FLAG
C
      RETURN
      END
