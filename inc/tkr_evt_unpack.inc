C
C----------------------------------------------------------------------
C-   TKR_EVT_UNPACK.INC
C-   Purpose and Methods : Include file for Event_Dnload (Unpack Event)
C-
C-   Inputs  : None
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created   25-January-1991   Jay A. Wightman
C-
C----------------------------------------------------------------------
C
C     TYPE DECLARATIONS
C
      COMMON/CD_MAP/MONTE_EVENT,PATTERN1_EVENT,PATTERN2_EVENT,
     &   VTX_WIRES_LOG_MAP,VTX_STRIPS_LOG_MAP,VTX_PHY_MAP,
     &   CDC_LOG_MAP,CDC_PHY_MAP,
     &   FDC_THETA_LOG_MAP,FDC_PHI_LOG_MAP,FDC_PHY_MAP,
     &   TRD_LOG_MAP,TRD_PHY_MAP
      INTEGER MONTE_EVENT,PATTERN1_EVENT,PATTERN2_EVENT
      INTEGER VTX_WIRES_LOG_MAP(0:2,0:31,0:7,0:1)
      INTEGER VTX_STRIPS_LOG_MAP(0:5,0:191,0:1)
      INTEGER VTX_PHY_MAP(0:9,0:15,0:15)
      INTEGER CDC_LOG_MAP(0:3,0:31,0:12)
      INTEGER CDC_PHY_MAP(0:6,0:15,0:15)
      INTEGER FDC_THETA_LOG_MAP(0:1,0:7,0:7,0:9)
      INTEGER FDC_PHI_LOG_MAP(0:1,0:35,0:15)
      INTEGER FDC_PHY_MAP(0:11,0:15,0:15)
      INTEGER TRD_LOG_MAP(0:5,0:15,0:15)
      INTEGER TRD_PHY_MAP(0:7,0:15,0:15)
C
