      PROGRAM CDWSTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create STP files for CDC
C-
C- Input: none
C-
C- Output:
C-    STP-bank sturcture
C-
C-   Created  16-FEB-1988   Ghita Rahal-Callot
C-   Updated  17-FEB-1988   Oivier Callot  : add pedestals-gains- time to
C-                                           distance banks
C-   Updated   5-MAR-1992   Qizhong Li-Demarteau  use D0OPEN, modify error
C-                                                handling and clean up
C-   Updated  27-MAR-1992   Qizhong Li-Demarteau  build two STPFILEs for
C-                             CDC: one is for MC data; one is for D0 data 
C-   Updated  15-JUN-1992   Qizhong Li-Demarteau  Update D0STPfile with
C-                             offline calibrated Gains and Guido's new
C-                             delay line velocities and T0s
C-   Updated  11-JUL-1992   Qizhong Li-Demarteau  make output in X-mode 
C-   Updated  15-SEP-1992   Domenico Pizzuto  Added rotines to create
C-                             banks DSWP, DDLP, DTVA, DNLI, DNLO 
C-                             and modify DCBD.
C-   Updated  11-DEC-1992   Qizhong Li-Demarteau  use INZSTP and RCP file 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INTEGER LUNDA, IER
      CHARACTER*11  FLNAME1
      CHARACTER*13  FLNAME2
      CHARACTER*(*) RCPFIL 
      PARAMETER( RCPFIL = 'CDCSTP_RCP' )   ! Logical name of RCP file
      LOGICAL OK
      DATA    FLNAME1/'CDC_STPFILE'/
      DATA    FLNAME2/'CDC_D0STPFILE'/
C----------------------------------------------------------------------
C
C ****  Initialize the Zebra structure
C
      CALL MZEBRA( 0 )
      CALL INZSTP
      CALL INRCP(RCPFIL,IER)
      OK = IER .EQ. 0
      IF (.NOT. OK) CALL ERRMSG('CDCSTP','CDWSTP',
     &    'Reading CDCSTP_RCP failed','F')
      CALL BKSCDC(LSCDC)
C
C ****  Create and fill DGEH and banks hanging to it : DMAT, DWAL, DRFT
C
      CALL BLDGEH
C
C ****  Create and fill DALH and banks hanging to it : DALL, DALS
C
      CALL BLDALH
C
C ****  Pedestal, gain and time_to_position
C
      CALL BLDPDH
      CALL BLDGNH
      CALL BLDTMH
C
      CALL DCSTOR(9,FLNAME1)
C
C- Create banks DTVA containing side dependent drift velocities for sense wires
      CALL BLDTVA
C- Create banks DNLI and DNLO containing sense wire nonlinearity parameters
      CALL BLDNLX
C- Create and fill banks DSWP and DDLP containing cable length T0 corrections
      CALL BLDXXP
C- Drop existing DCBD and create new DCBD
      CALL BLDCBD_D0
C- Place delay line velocities, T0s, and nonlinearity correction function 
C- coefficients into STP
      CALL DRDDLY      
      CALL BLDALH_0
      CALL DRDGNS
      CALL DCSTOR(10,FLNAME2)
C
  999 CONTINUE
      END
