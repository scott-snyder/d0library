C
C----------------------------------------------------------------------
C-   TKR_EVT_ANALYZE.INC
C-   Purpose and Methods : Include file for Event_Dnload
C-
C-   Inputs  : None
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created   12-December-1990   Jay A. Wightman
C-
C----------------------------------------------------------------------
C
C     TYPE DECLARATIONS
C
      COMMON/EVENT_ANAL_UNITS/IUSER,IUNIT,IERR,JUSER,JUNIT
      INTEGER IUSER
      INTEGER IUNIT
      INTEGER IERR
      INTEGER JUSER
      INTEGER JUNIT
C
      COMMON/EVENT_ANAL_BANKS/ANALYZE_BANK,CARDS_LOADED,MAX_CRATE,
     &       COMMAND
      LOGICAL ANALYZE_BANK(8)
      INTEGER CARDS_LOADED(8,0:11,2)
      INTEGER MAX_CRATE(8)
      CHARACTER*4 COMMAND(9)
      DATA ANALYZE_BANK/8*.FALSE./
      DATA CARDS_LOADED/192*-1/
      DATA MAX_CRATE/10,6,12,8,6,7,0,0/
      DATA COMMAND/'CDD1','CDD2','CDD3','CDD4','CAD1','CAD2',
     &             'MUD1','TRGR','END '/
C
