      SUBROUTINE GET_L1_L2_INFO(L2_BIT,L1_BIT,L1_NAME,L2_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read the FILT_TRIG_RUN.INFO file and get the
C-   association between l2 and l1 bit, and the names of each
C-
C-   Inputs  : L2_BIT   the level 2 bit number
C-   Outputs : L1_BIT   the level 1 bit on which it depends
C-             L1_NAME  the name of the level 1 bit
C-             L2_NAME  the name of the level 2 bit
C-   Controls:
C-
C-   Created   1-FEB-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER L2_BIT,L1_BIT
      CHARACTER*64 L1_NAME,L2_NAME
      INTEGER MAXT,MAXF
      PARAMETER (MAXT=64)
      PARAMETER (MAXF=128)
      INTEGER NTRIG,NFILT
      INTEGER ITRIG(MAXT),IFILT(MAXF)
      INTEGER RUN_TRIG(MAXT),RUN_FILT(MAXF)
      INTEGER TRIG_FILT(MAXF)
      INTEGER I,J,IERR,IER
      CHARACTER*64 CTRIG(MAXT),CFILT(MAXF)
      SAVE NTRIG,NFILT,ITRIG,IFILT,RUN_TRIG,RUN_FILT,TRIG_FILT
      LOGICAL GOT_FILE
      SAVE CTRIG,CFILT,GOT_FILE
      DATA GOT_FILE/.FALSE./
C----------------------------------------------------------------------
      IF (.NOT.GOT_FILE) THEN
        CALL GET_TRIG_FILT_RUN(NTRIG,ITRIG,CTRIG,RUN_TRIG,NFILT,
     &    IFILT,CFILT,TRIG_FILT,RUN_FILT,IERR)
        GOT_FILE = .TRUE.
      ENDIF
      L1_BIT = -1
      L1_NAME = ' '
      L2_NAME = ' '
      DO I = 1,NFILT
        IF (L2_BIT.EQ.IFILT(I)) THEN
          L1_BIT = TRIG_FILT(I)
          L2_NAME = CFILT(I)
          DO J = 1,NTRIG
            IF (L1_BIT.EQ.ITRIG(J)) THEN
              L1_NAME = CTRIG(J)
              GO TO 999
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      GO TO 999
C#######################################################################
      ENTRY GET_L1_INFO(L1_BIT,L1_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Purpose and Methods : read the FILT_TRIG_RUN.INFO file and get the
C-   name of the specified l1 bit
C-
C-   Inputs  : L1_BIT   the level 2 bit number
C-   Outputs : L1_NAME  the name of the level 1 bit
C-   Controls:
C-
C-   Created  10-APR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IF (.NOT.GOT_FILE) THEN
        CALL GET_TRIG_FILT_RUN(NTRIG,ITRIG,CTRIG,RUN_TRIG,NFILT,
     &    IFILT,CFILT,TRIG_FILT,RUN_FILT,IERR)
        GOT_FILE = .TRUE.
      ENDIF
      L1_NAME = ' '
      DO J = 1,NTRIG
        IF (L1_BIT.EQ.ITRIG(J)) THEN
          L1_NAME = CTRIG(J)
          GO TO 999
        ENDIF
      ENDDO
      GO TO 999
C#######################################################################
      ENTRY GET_L1_FROM_L2_NAME(L2_NAME,L2_BIT,L1_BIT,L1_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : from a l2 bit name, get the l2 bit and the 
C-   name and number of the l1 bit on which it depends
C-
C-   Inputs  : L2_NAME  name of the level 2 bit
C-   Outputs : L2_BIT   number 0-127 of that bit
C-             L1_BIT   number 0-31 of the Level 1 bit on which it depended
C-             L1_NAME  name of that level 1 bit
C-   Controls: 
C-
C-   Created  15-APR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IF (.NOT.GOT_FILE) THEN
        CALL GET_TRIG_FILT_RUN(NTRIG,ITRIG,CTRIG,RUN_TRIG,NFILT,
     &    IFILT,CFILT,TRIG_FILT,RUN_FILT,IERR)
        GOT_FILE = .TRUE.
      ENDIF
      L1_BIT = -1
      L1_NAME = ' '
      L2_BIT = -1
      DO I = 1,NFILT
        IF (L2_NAME.EQ.CFILT(I)) THEN
          L1_BIT = TRIG_FILT(I)
          L2_BIT = IFILT(I)
          DO J = 1,NTRIG
            IF (L1_BIT.EQ.ITRIG(J)) THEN
              L1_NAME = CTRIG(J)
              GO TO 999
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      GO TO 999
C#######################################################################
      ENTRY SETUP_L1_L2_INFO(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : init for get_l1_l2.info
C-
C-   Inputs  : none
C-   Outputs : stored arrays from file
C-             IER  = 0 if file found
C-   Controls:
C-
C-   Created   1-FEB-1992   James T. Linnemann
C----------------------------------------------------------------------
      CALL GET_TRIG_FILT_RUN(NTRIG,ITRIG,CTRIG,RUN_TRIG,NFILT,
     &  IFILT,CFILT,TRIG_FILT,RUN_FILT,IER)
      GOT_FILE = .TRUE.
  999 RETURN
      END
