      SUBROUTINE UDST_GET_HEAD(NWORD,XDATA,IGRP1,NGRP1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill HEAD bank into XDATA array
C-
C-   Inputs  : IGRP1,NGRP1
C-   Outputs : NWORD,XDATA
C-   Controls: QHEAD
C-
C-   Created  18-AUG-1993   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IGRP,NGRP,IGRP1,NGRP1,IDMAX,KHEAD,I,NWORD(IGRP1),J
      INTEGER NDIMG(IGRP),IDMAX1
      PARAMETER( KHEAD = 12 )
      REAL    XDATA(NGRP1)
      CHARACTER*8 XTAGS(NGRP,IGRP),TAGS(KHEAD)
      CHARACTER*4 XGRP(IGRP)
      DATA TAGS/'RUN','EVENT','L1BIT','L2BIT0','L2BIT1','L2BIT2',
     &  'L2BIT3','XING0','XING1','DATE0','DATE1','UBLANK'/
      LOGICAL QHEAD
      DATA    QHEAD/.FALSE./
C----------------------------------------------------------------------
      IF(QHEAD)THEN
        NWORD( 1) =KHEAD
        XDATA( 1) = IQ(LHEAD+ 6)  ! RUN
        XDATA( 2) = IQ(LHEAD+ 9)  ! EVENT
        XDATA( 3) =  Q(LHEAD+11)  ! L1BIT
        XDATA( 4) =  Q(LHEAD+15)  ! L2BIT0
        XDATA( 5) =  Q(LHEAD+16)  ! L2BIT1
        XDATA( 6) =  Q(LHEAD+17)  ! L2BIT2
        XDATA( 7) =  Q(LHEAD+18)  ! L2BIT3
        XDATA( 8) = IQ(LHEAD+ 7)  ! XING0
        XDATA( 9) = IQ(LHEAD+ 8)  ! XING1
        XDATA(10) =  Q(LHEAD+ 4)  ! DATE0
        XDATA(11) =  Q(LHEAD+ 5)  ! DATE1
        XDATA(12) = IQ(LHEAD+30)  ! UBLANK
      ENDIF
C----------------------------------------------------------------------
      RETURN
C
      ENTRY UDST_HEAD_TAGS(IDMAX,NDIMG,XTAGS,XGRP,IGRP,NGRP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : add tags for words from HEAD bank to XTAGS array
C-
C-   Inputs  : IDMAX,IGRP,NGRP
C-   Outputs : IDMAX,XTAGS,XGRP
C-
C-   Created  18-AUG-1993   Ulrich Heintz
C-
C----------------------------------------------------------------------
      DO I=1,IDMAX
        IF(XGRP(I).EQ.'HEAD')THEN
          CALL ERRMSG('cannot add HEAD group','UDST_HEAD_TAGS',
     &      'group already present','W')
          GOTO 999
        ENDIF
      ENDDO
      IF(IDMAX.GE.IGRP)THEN
        CALL ERRMSG('cannot add HEAD group','UDST_HEAD_TAGS',
     &    'number of groups exceeds maximum','W')
        GOTO 999
      ENDIF
      DO J=IDMAX,1,-1
        XGRP(J+1)=XGRP(J)
        NDIMG(J+1)=NDIMG(J)
        DO I=1,NGRP
          XTAGS(I,J+1)=XTAGS(I,J)
        ENDDO
      ENDDO
      DO I=1,KHEAD
        XTAGS(I,1)=TAGS(I)
      ENDDO
      NDIMG(1)=KHEAD
      XGRP(1)='HEAD'
      IDMAX=IDMAX+1
      IDMAX1=IDMAX
      QHEAD=.TRUE.
  999 RETURN
      END
