      SUBROUTINE DFHRD(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the header from a d0dad file
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-   Modified:16-MAR-1995   JDH Move to fortran direct access I/O
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IERR
      CHARACTER*8 CSTR
      INTEGER IREC(JDFHED),I,NREAD
C----------------------------------------------------------------------
C
C  Check format in order to keep backwards compatibility in reading
C  with versions prior to 1.6
C
      IERR=0
      READ(ILUN,REC=1) IREC
      IF( IREC(11).NE.TESTBSWAP ) CALL VXINVB(IREC(3),1)
      IF( IREC(JXXVER).LT.106) THEN
CJDH        CALL DFHRD_OLD(ILUN,IERR)
        GOTO 999
      ENDIF
C
C  Do the real processing now...
C
      IERR=0
      READ(ILUN,REC=1,ERR=998) DF_IOREC
      CALL UCOPY(DF_IOREC,IREC,JDFHED)
C
C  Test for need to byteswap...
C
      IF( IREC(11).NE.TESTBSWAP ) THEN
        CALL VXINVB(IREC(9),JDFHED-9)
        CALL VXINVB(IREC(3),1)
        IF( IREC(11).NE.TESTBSWAP ) GOTO 902
        IQ(LDFHD+JDFBS)=1
        IF( LDDBG.gt.10 ) THEN
          WRITE(D0DAD_ERRTXT,1001)
 1001     FORMAT(' Byte swap required for input D0DAD file.')
          CALL ERRMSG('ByteSwap','DFHRD',D0DAD_ERRTXT,'I')
        ENDIF
      ENDIF
C
C  Complete translation...
C
      CALL UHTOC(IREC,4,CSTR,8)
      CALL UHTOC(IREC(4),4,CDFTAG,20)
      CALL D0DAD_CPAD(CDFTAG)
C
      IF( LDFHD.LE.0 ) GOTO 999
      CALL UCOPY(IREC,IQ(LDFHD+NDDF+1),JDFHED)
      
C
  999 CONTINUE
      RETURN
C
 998  CONTINUE
      IERR = -1
      RETURN
C
 902  CONTINUE
      IERR = -2
      RETURN
C
      END
