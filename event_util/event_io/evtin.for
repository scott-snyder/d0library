      SUBROUTINE EVTIN(IN,IOS)
C---------------------------------------------------------------------
C-                                                                   -
C-     Gets data from a ZEBRA file                                   -
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
C-    ENTRY EVTIN_HEADER(DOIT)
C-      to be called with DOIT=true if record header is to be
C-      read prior to event record
C-                                                                   -
C-            AZ Nov 08, 1986                                        -
C-            BG Jan 01, 1987                                        -
C-       Modified for offline use SDP Apr, 1987                      -
C-       SDP added PARH_CHECK March, 1992
C-   Updated  13-MAY-1992   Serban D. Protopopescu  
C-   to handle filtering using header record 
C-   Update    1-NOV-1993   John D. Hobbs
C-      Include d0dad code.
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      LOGICAL EVENT_READ_OK
C
      INTEGER IN,IOS,IXWIPE
      CHARACTER*80 MSG
      INTEGER RUN,EVENT,RUNNO,EVONUM,IZBREC,IZBBYTE,IREC,IBYTE
      LOGICAL READ_HEADER,DOIT,LISDAD,LSTATE,LJOBSTOP
      SAVE READ_HEADER,LISDAD,IZBREC,IZBBYTE,LJOBSTOP
      DATA READ_HEADER/.FALSE./,LISDAD/.FALSE./,LJOBSTOP/.FALSE./
C---------------------------------------------------------------------
C
C---       Check for abort job via Zebra end-of-file
C
      IF( LJOBSTOP ) THEN
        IQUEST(1)=3
        IOS=3
        GOTO 999
      ENDIF
C
C---       Get an event using a d0dad file.
C
      IF( LISDAD ) THEN
C
         IXWIPE=IXCOM+IXMAIN
         CALL MZWIPE(IXWIPE)
         CALL D0DAD_DFREAD(IN,IOS)
         IF( IOS.GE.0 ) THEN
            IOS=IQUEST(1)
            IZBREC=IQUEST(5)
            IZBBYTE=IQUEST(6)
         ENDIF

      ELSE
C
C---       Get an event from file
C
         IXWIPE=IXCOM+IXMAIN
         IF (READ_HEADER) THEN   ! check header record before reading   
    1      CONTINUE
           CALL EVREAD_HEADER(IN,IOS)
           IF(IOS.GT.3.OR.IOS.LT.-8) GOTO 999
           IF ( EVENT_READ_OK() ) THEN
             CALL MZWIPE(IXWIPE)
             CALL FZIN (IN,IXMAIN,LHEAD,1,'A',0,0)
             IOS = IQUEST(1)
             IZBREC=IQUEST(5)
             IZBBYTE=IQUEST(6)
           ELSE
             GOTO 1                               ! standard reading
           ENDIF
         ELSE                              
           CALL MZWIPE(IXWIPE)
           CALL FZIN (IN,IXMAIN,LHEAD,1,' ',0,0)
           IOS = IQUEST(1)
           IZBREC=IQUEST(5)
           IZBBYTE=IQUEST(6)
         ENDIF
      ENDIF
C
      IF(IOS.EQ.0) THEN     ! handle non-event record
        IOS=MOD(IQ(LHEAD+1),1000)
        IF(IOS.GT.4) IOS=0
C              Move header to run division
        IF(IOS.EQ.1) THEN
          CALL UNCOMPRESS_ZEBRA
          CALL INUTAG
          IXWIPE=IXCOM+IXDVR
          CALL MZWIPE(IXWIPE)
          CALL MZCOPY(IXMAIN,LHEAD,IXDVR,LHEADR,1,' ') 
        ENDIF
      ENDIF
C
      IF (IOS .EQ. 0) THEN
        CALL UNCOMPRESS_ZEBRA
        CALL HEAD_CHECK                   ! check for old MC HEAD
        CALL PARH_CHECK                   ! check for old MC PARH
        CALL INUTAG
      ELSEIF (IOS.LT.0) THEN
         WRITE (MSG,102) IQUEST(1)
 102     FORMAT ('0Error encountered with IQUEST(1) = ',I5)
        CALL ERRMSG(' Read Failure','EVTIN',MSG,'W')
      ENDIF
      GOTO 999
C
      ENTRY EVTIN_HEADER(DOIT)
      READ_HEADER=DOIT
      RETURN
C
      ENTRY EVTIN_D0DAD(LSTATE)
      LISDAD=LSTATE
      RETURN
C
      ENTRY EVTIN_ZBPOS(IREC,IBYTE)
      IREC=IZBREC
      IBYTE=IZBBYTE
      RETURN
C
      ENTRY EVTIN_JOBSTOP
      LJOBSTOP=.TRUE.
 999  RETURN
      END

