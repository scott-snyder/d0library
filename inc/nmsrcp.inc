C----------------------------------------------------------------------
C-
C-   Name     NMSRCP.INC
C-   Purpose  Permanent name area for SRCP banks.
C-
C-   Created  27-SEP-1988   Harrison B. Prosper
C-   Updated  11-MAY-1990   Harrison B. Prosper  
C-      Removed scratch array 
C-
C----------------------------------------------------------------------
      CHARACTER*32 MSRCP(MXSRCP)
      COMMON /NMSRCP/ MSRCP
