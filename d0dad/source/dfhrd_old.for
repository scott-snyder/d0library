      SUBROUTINE DFHRD_OLD(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the header from an unsorted d0dad
C-      event catalog.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-   Modified 13-APR-1995   John D Hobbs - Rename and add JDFHED_OLD
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IERR
      CHARACTER*8 CSTR
      INTEGER IREC(JDFHED),I,NREAD
      INTEGER JDFHED_OLD
      PARAMETER(JDFHED_OLD=12)
C----------------------------------------------------------------------
C
      IERR=0
      CALL CFREW(ILUN,0)
      I=1
 10   CONTINUE
         IF( (I+JRECDF-1).GT.JDFHED_OLD ) GOTO 20
         NREAD=JRECDF
         CALL CFGET(ILUN,0,JRECDF,NREAD,IREC(I),IERR)
         IF( IERR.NE.0 ) GOTO 998
         I=I+JRECDF
      GOTO 10
C
 20   CONTINUE
C
C  Test for need to byteswap...
C
      IF( IREC(11).NE.TESTBSWAP ) THEN
        CALL VXINVB(IREC(9),3)
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
