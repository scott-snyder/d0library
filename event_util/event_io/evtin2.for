      SUBROUTINE EVTIN2(IN,IOS)
C---------------------------------------------------------------------
C-                                                                   -
C-     Gets 2nd event from a ZEBRA file                              -
C-     and add it to a HEAD linear structure.                        -
C-     Note that no call to MZWIPE occurs and that                   -
C-     an unlimited number of events can be joined into the          -
C-     same Zebra structure.                                         -
C-     It is up to the user to open the necessary files.             -
C-                                                                   -
C-     INPUT:                                                        -
C-     IN=    unit number for input file                             -
C-                                                                   -
C-     OUTPUT:                                                       -
C-     IOS=   I/O status                                             -
C-        = 0 data record, 1 begin-run record, 2 end-run record      -
C-        = 3 end-of-file, 4 end-of-data                             -
C-        = 5 attempt to read past end-of-data                       -
C-        < 0 errors (see Zebra manual IQUEST(1) error status)       -
C-            DATA in /ZEBCOM/                                       -
C-                                                                   -
C-                                                                   -
C-       SDP Jan, 1988                                               -
C-       MODIFIED   27-APR-1993  Ian Adam
C-       Handle case when there is no begin run record in the run    -
C-       division from EVTIN (this occurs when the filelist read by  -
C-       EVTIN begins with a partition other than the first in a     -
C-       run.)                                                       -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER IN,IOS,LNEXT,LNEXTR,LLAST,LLASTR
      CHARACTER*80 MSG
C
C---       find pointer to last HEAD bank
C
      LLAST=LHEAD
    1 LNEXT=LQ(LLAST)
      IF(LNEXT.NE.0) THEN
        LLAST=LNEXT
        GOTO 1
      ENDIF
C
C---       Get an event from disk
C
      CALL FZIN (IN,IXMAIN,LLAST,0,' ',0,0)
      LNEXT=LQ(LLAST)
      IQ(LNEXT-5)=IQ(LLAST-5)+1
      IOS = IQUEST(1)
      IF(IOS.EQ.0) THEN     ! handle non-event record
        IOS=MOD(IQ(LNEXT+1),1000)
        IF(IOS.GT.4) IOS=0
        IF(IOS.EQ.1) THEN   ! begin run-record
          IF (LHEADR.GT.0) THEN
            LLASTR=LHEADR
    2       LNEXTR=LQ(LLASTR)
            IF(LNEXTR.NE.0) THEN
              LLASTR=LNEXTR
              GOTO 2
            ENDIF
C               Move header to run division
            CALL MZCOPY(IXMAIN,LNEXT,IXDVR,LLASTR,0,' ')
          ELSE
C This line will keep the begin run-record from the file read
C by evtin2 even if there is no bor record from evtin:
C               No bor record from evtin
C               Move header to run division
            CALL MZCOPY(IXMAIN,LNEXT,IXDVR,LLASTR,1,' ')
          ENDIF
        ENDIF
      ENDIF
      IF (IOS .GE. 0) RETURN
      IF (IOS .LT. 0) THEN
        WRITE (MSG,102) IQUEST(1)
  102   FORMAT ('0Error reading file encountered with IQUEST(1) = ',I5)
        CALL ERRMSG(' Read Failure','EVTIN2',MSG,'W')
      ENDIF
C
      END
