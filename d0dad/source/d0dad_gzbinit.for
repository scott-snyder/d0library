      SUBROUTINE D0DAD_GZBINIT
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Initialize Zebra for standalone operation
C-     within D0DAD control program.  THIS MUST NOT BE CALLED FROM
C-     WITHIN ANY FRAMEWORKS.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE  'D0$INC:zebcom.inc'
C-----------------------------------------------------------------------
C DEC/CMS REPLACEMENT HISTORY, Element ZEBCOM.INC
C *12   19-DEC-1988 11:45:39 D0LIBRARY "added IXCOM, removed IXDIV"
C *11   29-NOV-1988 16:25:17 D0LIBRARY "added comments"
C *10   29-NOV-1988 16:21:03 D0LIBRARY "added IXDIV and IXDVR"
C *9    22-SEP-1986 11:26:53 JONCKHEERE "Increase ZEBCOM size for GEANT"
C *8     2-SEP-1986 13:45:57 SERBAN "links are now in .LINKS files"
C *7    15-JUL-1986 16:00:14 JONCKHEERE "PARAMETER FOR MUHT,CAHT,CDHT ADDED (MUHT CHANGED)"
C *6    27-MAY-1986 13:10:11 SERBAN "parameter NNQ changed from 50000 to 100000"
C *5    15-MAY-1986 16:51:11 SERBAN "added more pointers for ISAJET"
C *4    12-MAY-1986 18:41:24 HEDIN "Put in Muon Zebra Links"
C *3    12-MAY-1986 16:46:40 SERBAN "added parameters for link pointers"
C *2     7-MAY-1986 21:41:19 KUNORI "convert to standard 77 (drop *4)"
C *1    14-JAN-1986 17:22:30 PROTOPOPESCU "Zebra common block for event data"
C DEC/CMS REPLACEMENT HISTORY, Element ZEBCOM.INC
C
C  ZEBCOM is the main zebra common block for event data storage
C
      INTEGER NNQ,NREF
      PARAMETER (NNQ=7500000)
      PARAMETER (NREF=9)
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,
     &  ZSTOR,ENDZS
      INTEGER IXCOM    
     &       ,IXMAIN   
     &       ,IXDVR    
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS
      INTEGER LHEAD     
      INTEGER LHEADR    
      REAL Q(NNQ)
      INTEGER IQ(NNQ),LQ(NNQ)
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))
C
      INCLUDE  'D0$INC:quest.inc'
      INCLUDE  'D0$INC:d0dadcom.inc'
      INTEGER IXWIPE
      CHARACTER*(*) FDUMMY
C&IF VAXVMS
      PARAMETER(FDUMMY='NL:')
C&ELSE
C&      PARAMETER(FDUMMY='/dev/null')
C&ENDIF
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST/.TRUE./
C-----------------------------------------------------------------------

C
      IF( .NOT.FIRST ) RETURN
      FIRST=.FALSE.
C
C  Send the Zebra-specific output to a dummy file
C
      OPEN(3,FILE=FDUMMY,STATUS='UNKNOWN')
C
C  Initialize ZEBRA ala D0...
C
      CALL MZEBRA(0)
      CALL MZSTOR(IXCOM,'D0DADTST','Q',FENCE,LQ(1),LREF(1),ZSTOR(1),
     &   ZSTOR(40000),ENDZS)
      CALL MZDIV(IXCOM,IXDVR,'RUN DIV',100,40000,'L')
      IXMAIN=IXCOM+1
      IXWIPE=IXCOM+IXMAIN
C
      CALL D0DAD_ZBINIT
C
  999 RETURN
      END
