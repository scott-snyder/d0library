C
C----------------------------------------------------------------------
C-   TRK_EVT_ARRAYS.INC
C-   Purpose and Methods : Include file Event_Dnload Data Arrays
C-
C-   Inputs  : None
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created   25-January-1991   Jay A. Wightman
C-   Modified   5-February-1991  Jay A. Wightman   Add Calorimeter
C-
C----------------------------------------------------------------------
C
C     TYPE DECLARATIONS
C
      COMMON/EVENT_DATA_ARRAYS/FADC_DATA,CAL_DATA,CABLE_DATA,
     &       FADC_POINTERS,ADC_POINTERS
      INTEGER FADC_DATA(0:15,0:15,128)
      INTEGER CAL_DATA(0:11,0:383,4)
      INTEGER CABLE_DATA(65536)
      INTEGER FADC_POINTERS(0:11,0:15,0:15,2)
      INTEGER ADC_POINTERS(0:6,0:11,0:383,2)
C
