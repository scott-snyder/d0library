      SUBROUTINE EZRDAR (RECORD,BKNAME,WRDIDS,LSUPP,IZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Read a character array containing parameters and save records and
C-      decoded values in an SRCP bank. The records are ordered to allow
C-      the use of a binary search to retrieve data from the bank. The
C-      SRCP bank is assigned the name BKNAME. The name can be
C-      changed with EZRNAM. This routine is functionally equivalent to
C-      EZREAD.
C-
C-   Inputs  : RECORD(*) [C*]   Character array
C-             BKNAME    [C*]   Name to give to SRCP bank
C-             WRDIDS    [I]    Number of words/record
C-             LSUPP     [I]    Address of support bank if > 0.
C-             IZLINK    [I]    Link from which to hang bank.
C-
C-   Outputs : None
C-
C-                         Error codes. Use EZERR to check for code.
C-                         0 --- OK
C-                         1 --- Bank has been expanded at least once
C-                        -4 --- FATAL ERROR. IZLINK link already occupied
C-                        -5 --- Maximum bank size reached.
C-                        
C-      IMPORTANT:      The last entry in the array must contain the
C-                      command \STOP to indicate end-of-data.
C-
C-   Created  10-JAN-1989   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RECORD(*)
      CHARACTER*(*) BKNAME
      INTEGER       WRDIDS
      INTEGER       LSUPP
      INTEGER       IZLINK
C
      INTEGER LSRCP,LPTI,LPTO,LPTV,LPTT
      INTEGER IER,II,JJ,I,J,K,L,LREC
C
C----------------------------------------------------------------------
C
C **** Get FIRST RECORD and determine initial bank size
C **** and book SRCP bank
C
      LREC = LEN(RECORD(1))
      L    = LEN(BKNAME)
      CALL EZZBK (RECORD(1)(1:LREC),BKNAME(1:L),WRDIDS,LSUPP,IZLINK,
     &  LSRCP,LPTI,LPTO,LPTV,LPTT)
C
C ****  Loop over REMAINING records in file
C
      I = 1
   50 CONTINUE
      I = I + 1
C
      CALL EZZIRC
     & (RECORD(I)(1:LREC),LSRCP,LPTI,LPTO,LPTV,LPTT,IER)
C
      IF ( IER .GE. 0 ) GOTO 50
C
C ****  Complete construction of SRCP bank
C
   60 CONTINUE
      CALL EZEND
C
  999 RETURN
      END
