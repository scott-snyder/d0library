C DEC/CMS REPLACEMENT HISTORY, Element RUNSTR.FOR
C *2    30-SEP-1987 22:51:37 RAJA "New VSn from JL"
C *1    15-SEP-1987 19:33:35 LINNEMANN "string from run number for file names"
C DEC/CMS REPLACEMENT HISTORY, Element RUNSTR.FOR

      SUBROUTINE RUNSTR(NRUN,STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Convert a run number into a string (for a file name, eg.)
C-   Inputs  :
C-      NRUN  a run number of 6 or less digits
C-   Outputs :
C-      STRING  an 8 character string of the form "RUN_nnnnnn"
C-   Created  15-SEP-1987   J. Linnemann
C-Based on RUNNUM by                                      
C-     AZ Dec.29, 1986, Sep 8 1987
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER NRUN
      CHARACTER*10  STRING
C
      IF(NRUN.LT.10)THEN
         WRITE(STRING,'(''RUN_00000'',I1)') NRUN
      ELSE IF(NRUN.GE.10. AND. NRUN.LE.99) THEN
         WRITE(STRING,'(''RUN_0000'',I2)')  NRUN
      ELSE IF(NRUN.GE.100.AND. NRUN.LE.999) THEN
         WRITE(STRING,'(''RUN_000'',I3)')  NRUN
      ELSE IF(NRUN.GE.1000.AND. NRUN.LE.9999) THEN
         WRITE(STRING,'(''RUN_00'',I4)')  NRUN
      ELSE IF(NRUN.GE.10000.AND. NRUN.LE.99999) THEN
         WRITE(STRING,'(''RUN_0'',I5)')  NRUN
      ELSE IF(NRUN.GE.100000) THEN
         NRUN=MOD(NRUN,1000000)
         WRITE(STRING,'(''RUN_'',I6)')   NRUN
      ENDIF
C
 999  RETURN
      END
