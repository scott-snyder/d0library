C----------------------------------------------------------------------
C-
C-   Created 19-JAN-1992   James T. Linnemann   
C-      hold sums over bins of filter counts
C----------------------------------------------------------------------
      REAL GRAND_FILTER_COUNT(2,0:MAX_TOOL_FILTER,0:127),
     &        GRAND_TOT_EVENTS(3)
      REAL GRAND_FILTER_ERR(2,0:MAX_TOOL_FILTER,0:127),
     &        GRAND_TOT_ERR(3)
      COMMON /GRAND_FILTER_COUNT/ 
     &                    GRAND_FILTER_COUNT, GRAND_TOT_EVENTS,
     &                    GRAND_FILTER_ERR, GRAND_TOT_ERR
