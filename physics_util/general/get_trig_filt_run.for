      SUBROUTINE GET_TRIG_FILT_RUN(NTRIG,ITRIG,CTRIG,RUN_TRIG,NFILT,
     &  IFILT,CFILT,TRIG_FILT,RUN_FILT,IER)
C=========================================================================
C
C  Description:  Reads Current Trigger/Filter file and returns information.
C  ============
C
C... the  file is pointed to by the logical TRIG_FILT_RUN
C... in the online environment, this will point to the appropriate
C    TRIG_FILT_RUN.INFO
C
C  Argument Description:
C  ======================
C  NTRIG - INTEGER - Output - Number of triggers
C  NFILT - INTEGER - Output - Number of filters
C  ITRIG - INTEGER ARRAY - Output - Trigger bit #'s
C  IFILT - INTEGER ARRAY - Output - Filter bit #'s
C  CTRIG - CHARACTER ARRAY - Output - Character trigger descriptor
C  CFILT - CHARACTER ARRAY - Output - Character filter descriptor
C  RUN_TRIG - INTEGER - Array of RUN NUMBER for each trigger bit
C  RUN_FILT - INTEGER - Array of RUN NUMBER for each filter bit
C  TRIG_FILT _ INTEGER - Array of trigger bit owning each filter bit
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - March 3, 1989
C  Major Revision - March 13, 1990
C-   Updated  15-APR-1992   James T. Linnemann  add ERRMSG and IER and SET entry
C
C===========================================================================
C
      IMPLICIT NONE
C
C  Argument Declarations:
C  =======================
C
      INTEGER MAXT,MAXF,IER,IERR
      PARAMETER (MAXT=64)
      PARAMETER (MAXF=128)
      INTEGER NTRIG,NFILT
      INTEGER ITRIG(MAXT),IFILT(MAXF)
      INTEGER RUN_TRIG(MAXT),RUN_FILT(MAXF)
      INTEGER TRIG_FILT(MAXF)
      CHARACTER*64 CTRIG(MAXT),CFILT(MAXF)
      CHARACTER*132 MESSAGE
      CHARACTER*45 NEW_NAME
C
C  Local Declarations:
C  ====================
C
      INTEGER IUSER,IUNIT,I,J,TRULEN
      CHARACTER*4 ASTRG
      CHARACTER*45 TRIG_FILT_FILE
      LOGICAL OK
      DATA IUSER/12/
      DATA TRIG_FILT_FILE/'TRIG_FILT_RUN'/
C===========================================================================
C
C  Executable Code:
C  =================
C
      CALL GTUNIT(IUSER,IUNIT,IERR)

      CALL D0OPEN(IUNIT,TRIG_FILT_FILE,'IF',OK)
      IF(.NOT.OK) GO TO 888
      READ(IUNIT,101,END=999) NTRIG,ASTRG
  101 FORMAT(I4,1X,A4)
      DO 10 I = 1,NTRIG
         READ(IUNIT,103,END=999) ITRIG(I),CTRIG(I),RUN_TRIG(I)
  103    FORMAT(I4,1X,A64,1X,I10)
   10 CONTINUE
      READ(IUNIT,101,END=999) NFILT,ASTRG
      DO 20 I = 1,NFILT
         READ(IUNIT,102,END=999) IFILT(I),CFILT(I),TRIG_FILT(I)
  102    FORMAT(I6,1X,A64,' ',I2)
         DO 21 J = 1,NTRIG
            IF (ITRIG(J) .EQ. TRIG_FILT(I)) THEN
               RUN_FILT(I) = RUN_TRIG(J)
            ENDIF
   21    CONTINUE
   20 CONTINUE
      IER = 0
      GO TO 999
  888 CONTINUE
      WRITE (MESSAGE,300)TRIG_FILT_FILE(1:TRULEN(TRIG_FILT_FILE))
  300 FORMAT(A,' does not point to a trig_filt_run file')
      CALL ERRMSG('NO_TRG_FLT_RN','GET_TRIG_FILT_RUN',MESSAGE,'W')
      IER = -1
  999 CONTINUE
      CLOSE(IUNIT)
      CALL RLUNIT(IUSER,IUNIT,IERR)
      RETURN
C#######################################################################
      ENTRY SET_TRIG_FILT_RUN(NEW_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reset trig_filt_file
C-
C-   Inputs  : NEW_NAME resets the file searched for
C-             If it has an extension and/or directory, that file is sought
C-             If it has no extension, it is tested as a logical
C-                if no such logical is defined, it looks for xxx.DAT
C- the default is TRIG_FILT_RUN, a logical which should point to 
C-        xxxx:TRIG_FILT_RUN.INFO
C-             
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-APR-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      TRIG_FILT_FILE = NEW_NAME
      RETURN
      END
