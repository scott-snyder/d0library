      SUBROUTINE GTTSUM
     &  (NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      give list of triggers that are ON for an event
C-      by fetching them from the TSUM bank
C-
C-   Inputs  : The TSUM bank
C-   Outputs :
C-     NTRIGON= number of trigger bits on
C-     TRIGBON=  list of trigger bits on
C-     TRIGNON=  list of names of triggers on
C-     NFILTON= number of filter bits on
C-     FILTBON= list of filter bits on
C-     FILTNON= list of names of filters on
C-
C-   ENTRY GET_L1_BIT_NUMBER, GET_L2_BIT_NUMBER(_EXCL): see below
C-   ENTRY GET_TRIG_LISTS  Returns bit numbers and names of all triggers
C-                         found during this run (will send error message
C-                         if trigger definition changes during running!!)
C-
C-   Created  20-MAR-1992   Serban D. Protopopescu
C-   Updated  23-DEC-1992   Serban Protopopescu  turned off ERRMSG
C-   Updated  20-OCT-1994   John Hobbs protect against corrupted TSUM banks
C-   Updated  15-JAN-1996   Bob Hirosky  add entry GET_L2_BIT_NUMBER_EXCL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FOUND,FLAG
C
      INTEGER NFILTON,NTRIGON
      INTEGER TRIGBON(*),FILTBON(*),BITNUM
      INTEGER GZTSUM,LTSUM,LENG,TLENG,TRULEN
      INTEGER NR,NFIX,I,POINT
      INTEGER IS,IE,LEN1
C
      CHARACTER*(*) TRIGNON(*),FILTNON(*),NAME
      CHARACTER*32 TNAME
      CHARACTER*12 L2_LIST(0:127),L1_LIST(0:31),NULL
      CHARACTER*(*) XL2_LIST(0:127),XL1_LIST(0:31)
      CHARACTER*80 MSG
C
      DATA L1_LIST / 32 * '............' /
      DATA L2_LIST / 128 * '............' /
      DATA NULL         / '............' /
C----------------------------------------------------------------------
C
      FLAG=.TRUE.
      NTRIGON=0
      NFILTON=0
      LTSUM=GZTSUM()
C
      IF(LTSUM.GT.0) THEN
        NFIX=IQ(LTSUM+2)
        NR=IQ(LTSUM+3)
        POINT=LTSUM+NFIX
C
        NTRIGON=IQ(LTSUM+4)
        DO I=1,NTRIGON
          TRIGBON(I)=IQ(POINT+1)
          IF( TRIGBON(I).LT.0 .OR. TRIGBON(I).GT.31 ) THEN
            WRITE(MSG,*) 'Trigger bit number =',TRIGBON(I)
            IF(FLAG)CALL ERRMSG('Corrupted TSUM bank','GTTSUM',MSG,'W')
            FLAG=.FALSE.
            GOTO 10
          ENDIF
          CALL UHTOC(IQ(POINT+2),8,TRIGNON(I),32)
          CALL WORD(TRIGNON(I),IS,IE,LEN1)
          LEN1 = MIN0(12,LEN1)
          IF ( L1_LIST(TRIGBON(I)) .EQ. NULL ) THEN
            L1_LIST(TRIGBON(I)) = TRIGNON(I)(1:LEN1)
          ELSE
            IF (L1_LIST(TRIGBON(I))(1:LEN1) .NE. TRIGNON(I)(1:LEN1))
     &        THEN
C              WRITE (MSG,1000) TRIGBON(I),L1_LIST(TRIGBON(I))(1:LEN1),
C     &          TRIGNON(I)(1:LEN1)
C             CALL ERRMSG('Trigger def changed','GTTSUM',MSG,'W')
 1000         FORMAT(' Bit',I3,' old ',A12, ' new ',A12)
              L1_LIST(TRIGBON(I)) = TRIGNON(I)(1:LEN1)
            ENDIF
          ENDIF
          POINT=POINT+NR
 10     ENDDO
C
C
        NFILTON=IQ(LTSUM+5)
        DO I=1,NFILTON
          FILTBON(I)=IQ(POINT+1)
          IF( FILTBON(I).LT.0 .OR. FILTBON(I).GT.127 ) THEN
            WRITE(MSG,*) 'Filter bit number =',FILTBON(I)
            IF(FLAG)CALL ERRMSG('Corrupted TSUM bank','GTTSUM',MSG,'W')
            FLAG=.FALSE.
            GOTO 20
          ENDIF
          CALL UHTOC(IQ(POINT+2),8,FILTNON(I),32)
          CALL WORD(FILTNON(I),IS,IE,LEN1)
          LEN1 = MIN0(12,LEN1)
          IF ( L2_LIST(FILTBON(I)) .EQ. NULL ) THEN
            L2_LIST(FILTBON(I)) = FILTNON(I)(1:LEN1)
          ELSE
            IF (L2_LIST(FILTBON(I))(1:LEN1) .NE. FILTNON(I)(1:LEN1))
     &        THEN
C              WRITE (MSG,1000) TRIGBON(I),L1_LIST(TRIGBON(I))(1:LEN1),
C     &          TRIGNON(I)(1:LEN1)
C             CALL ERRMSG('Trigger def changed','GTTSUM',MSG,'W')
              L2_LIST(FILTBON(I)) = FILTNON(I)(1:LEN1)
            ENDIF
          ENDIF
          POINT=POINT+NR
 20     ENDDO
      ENDIF
      GO TO 999
C
      ENTRY GET_TRIG_LISTS(XL1_LIST,XL2_LIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get list of trigger and filter definitions
C-         found so far in the input files
C-
C-   Inputs  : NONE
C-   Outputs : XL1_LIST  C*12 array (0:31)  Contains a name string for
C-                          each level 1 trigger bit which was found to
C-                          be on for some event in this set of input files
C-             XL2_LIST  C*12 array (0:127)  Contains a name string for
C-                          each level 2 trigger bit which was found to
C-                          be on for some event in this set of input files
C-   Controls:
C-
C-   Created  18-SEP-1992   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      DO I = 0,31
        XL1_LIST(I) = L1_LIST(I)
      ENDDO
      DO I = 0,127
        XL2_LIST(I) = L2_LIST(I)
      ENDDO
      GO TO 999
C
 
      ENTRY GET_L1_BIT_NUMBER(NAME,BITNUM,FOUND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get L1 bit number corresponding to L1 bit name
C-          for this event, IF the bit was ON
C-
C-   Inputs  : NAME   32-character bit name
C-   Outputs : BITNUM Level 1 bit number 0-31
C-             FOUND  .TRUE. if the bit was ON for this event
C-   Controls:
C-
C-   Created  20-JUL-1992   James T. Linnemann
C-   Updated   9-FEB-1993   James T. McKinley - fix bug in pointer POINT
C----------------------------------------------------------------------
      FOUND = .FALSE.
      BITNUM = 0
      LTSUM=GZTSUM()
      LENG = MIN(TRULEN(NAME),32)
C
      IF(LTSUM.GT.0) THEN
        NFIX=IQ(LTSUM+2)
        NR=IQ(LTSUM+3)
        POINT=LTSUM+NFIX
C...loop over known level 1 names with bit set ON
        DO I=1, IQ(LTSUM+4)
          CALL UHTOC(IQ(POINT+2),8,TNAME,32)
          IF (TNAME(1:LENG).EQ.NAME(1:LENG)) THEN
            BITNUM = IQ(POINT+1)
            FOUND = .TRUE.
            GO TO 999
          ENDIF
          POINT=POINT+NR
        ENDDO
      ENDIF
      GO TO 999
      ENTRY GET_L2_BIT_NUMBER(NAME,BITNUM,FOUND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get L2 bit number corresponding to L2 bit name
C-          for this event, IF the bit was ON
C-
C-   Inputs  : NAME   32-character bit name
C-   Outputs : BITNUM Level 2 bit number 0-31
C-             FOUND  .TRUE. if the bit was ON for this event
C-   Controls:
C-
C-   Created  20-JUL-1992   James T. Linnemann
C-   Updated   9-FEB-1993   James T. McKinley - fix bug in pointer POINT
C-
C----------------------------------------------------------------------
      FOUND = .FALSE.
      BITNUM = 0
      LTSUM=GZTSUM()
      LENG = MIN(TRULEN(NAME),32)
C
      IF(LTSUM.GT.0) THEN
        NFIX=IQ(LTSUM+2)
        NR=IQ(LTSUM+3)
        POINT=LTSUM+NFIX+IQ(LTSUM+4)*NR
C...loop over known L2 bit names with the bit set ON this event
        DO I=1, IQ(LTSUM+5)
          CALL UHTOC(IQ(POINT+2),8,TNAME,32)
          IF (TNAME(1:LENG).EQ.NAME(1:LENG)) THEN
            BITNUM = IQ(POINT+1)
            FOUND = .TRUE.
            GO TO 999
          ENDIF
          POINT=POINT+NR
        ENDDO
      ENDIF
      GOTO 999
      ENTRY GET_L2_BIT_NUMBER_EXCL(NAME,BITNUM,FOUND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get L2 bit number corresponding to L2 bit name
C-          for this event, IF the bit was ON
C-
C-   Inputs  : NAME   32-character bit name
C-   Outputs : BITNUM Level 2 bit number 0-31
C-             FOUND  .TRUE. if the bit was ON for this event
C-   Controls:
C-
C-   Created  20-JUL-1992   James T. Linnemann
C-   Updated   9-FEB-1993   James T. McKinley - fix bug in pointer POINT
C-   Updated  15-JAN-1996   Bob Hirosky  do exclusive name matching 
C-
C----------------------------------------------------------------------
      FOUND = .FALSE.
      BITNUM = 0
      LTSUM=GZTSUM()
      LENG = MIN(TRULEN(NAME),32)
C
      IF(LTSUM.GT.0) THEN
        NFIX=IQ(LTSUM+2)
        NR=IQ(LTSUM+3)
        POINT=LTSUM+NFIX+IQ(LTSUM+4)*NR
C...loop over known L2 bit names with the bit set ON this event
        DO I=1, IQ(LTSUM+5)
          CALL UHTOC(IQ(POINT+2),8,TNAME,32)
          TLENG = TRULEN(TNAME)
          IF (TNAME(1:TLENG).EQ.NAME(1:LENG)) THEN
            BITNUM = IQ(POINT+1)
            FOUND = .TRUE.
            GO TO 999
          ENDIF
          POINT=POINT+NR
        ENDDO
      ENDIF
  999 RETURN
      END
