      LOGICAL FUNCTION GOOD_PHOTON(LPPHO,TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : select good photons from PPHO banks
C-
C-   Returned value  : true if bank with address LPPHO satisfies
C-                     standard "tight" or "loose" photon cuts.
C-   Inputs  :         LPPHO - address of PPHO bank
C-                           == OR == the STATUS WORD TO BE CHECKED IF
C-                                    TYPE contains (_NOLINK)
C-                     TYPE  - 'LOOSE' or 'TIGHT'
C-                              (add '_NOLINK' if LPPHO is STATUS instead
C-                              of pointer to PPHO bank)
C-   Controls:         masks in GOOD_ELECTRON_PHOTON.PARAMS
C-
C-   Created  12-FEB-1993   Ulrich Heintz
C-   Updated  20-FEB-1994   Meenakshi Narain  add '_NOLINK' feature
C-   Updated  20-MAR-1994   Rajendran Raja  cleaned up logic 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:GOOD_ELECTRON_PHOTON.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPPHO, STATUS, CHECK_MASK
      CHARACTER*(*) TYPE
      CHARACTER*80 STRING
      LOGICAL OK
      LOGICAL QNOLINK,QSHLIB,QTIGHT,QNOTRK,QLOOSE
C----------------------------------------------------------------------
      OK=.FALSE.
C
C ****  parse TYPE
C
      CALL UPCASE(TYPE,STRING)                    ! upper case
C
      QSHLIB=INDEX(STRING,'SHLIB').NE.0
      QNOLINK=INDEX(STRING,'NOLINK').NE.0
      QTIGHT=INDEX(STRING,'TIGHT').NE.0
      QNOTRK=INDEX(STRING,'NOTRK').NE.0
      QLOOSE=INDEX(STRING,'LOOSE').NE.0
C
      IF(QTIGHT)THEN        ! tight shape cuts
        CHECK_MASK = TIGHT_PHOTON_MASK 
      ELSE                                      ! loose shape cuts (default)
        IF(.NOT.QLOOSE)CALL ERRMSG('UNKNOWN CUT',
     &    'GOOD_PHOTON','using "LOOSE" cuts as default','W')
        CHECK_MASK = LOOSE_PHOTON_MASK 
      ENDIF
C
C **** turn off NCELL cut for Shower lib events
C
      IF(QSHLIB)CHECK_MASK=IAND(CHECK_MASK,ALL_BITS_ON-NCELLS_MASK)
C
C ****  check status
C
      IF(QNOLINK)THEN
        STATUS = LPPHO  !STATUS WORD IN THE BANK LINK PLACE
        OK = IAND(STATUS,CHECK_MASK).EQ.0
      ELSE
        CALL CHECK_EM_QUALITY(LPPHO,CHECK_MASK,OK)
      ENDIF
C----------------------------------------------------------------------
  999 GOOD_PHOTON=OK
      RETURN
      END
