      SUBROUTINE EZGET_BANK_PARAMS(BNAME,
     &  PARAM_COUNT,PARAM_NAMES,PARAM_TYPES,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get information about the parameters
C-      of an RCP bank
C-
C-
C-   Inputs  : BNAME          : Name of this bank
C-   Outputs : PARAM_COUNT(*) : Number of parameters for this bank
C-             PARAM_NAMES(*) : Names of parameters
C-             PARAM_TYPES(*) : Type of parameters (or 1st parameter if this
C-                              parameter is a mixed array)
C-                              I   Integer
C-                              R   Real (internally R or E format)
C-                              L   Logical
C-                              C   Character
C-             IER            : = 0 if everything went OK.
C-   Controls:
C-
C-   Created  31-MAR-1993   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_COUNT
      CHARACTER*(*) BNAME
      CHARACTER*16 PARAM_NAMES(*)
      CHARACTER*1  PARAM_TYPES(*)
      CHARACTER*32 PARAM_NAME
      LOGICAL OK
      INTEGER IER,IER2,IPOINT,INDEX,TYPE,LENVAL,VALUE
      CHARACTER*80 CHARV
C------------------------------------------------------------------------
      CALL EZPICK_NOMSG(BNAME,IER)       ! Select bank
      OK = IER.EQ.0
      PARAM_COUNT = 0
      IF (IER .EQ. 0) THEN
        IPOINT = 1
        DO WHILE (IPOINT.GT.0)
          CALL EZGET_NEXT_NAME(PARAM_NAME,IPOINT)
          PARAM_COUNT = PARAM_COUNT + 1
          PARAM_NAMES(PARAM_COUNT) = PARAM_NAME
          INDEX = 1
          CALL EZGET_NEXT_VALUE_TYPE(PARAM_NAME,VALUE,CHARV,
     &        TYPE,LENVAL,IER2,INDEX)
          IF (TYPE.EQ.1) PARAM_TYPES(PARAM_COUNT) = 'I'
          IF (TYPE.EQ.2) PARAM_TYPES(PARAM_COUNT) = 'R'
          IF (TYPE.EQ.3) PARAM_TYPES(PARAM_COUNT) = 'R'
          IF (TYPE.EQ.4) PARAM_TYPES(PARAM_COUNT) = 'L'
          IF (TYPE.GE.10) PARAM_TYPES(PARAM_COUNT) = 'C'
        ENDDO
        IF (OK) CALL EZRSET
      ENDIF
  999 RETURN
      END
