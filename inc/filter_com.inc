C----------------------------------------------------------------------
C-
C-   Created  23-JAN-1989   Jan S. Hoftun
C-   Updated  18-JUL-1989   Jan S. Hoftun  (add counters for filters) 
C-   Updated  17-AUG-1989   Jan S. Hoftun  (add HARDWARE_BIT) 
C-   Updated  18-SEP-1989   Jan S. Hoftun  (add FILTER_SET_COUNT + TOT_EVENTS)
C-   Updated   3-AUG-1990   Jan S. Hoftun  (add UNFILTER_COUNTER) 
C-   Updated  20-SEP-1990   Jan S. Hoftun  (add TUSED+TIME_POINTER) 
C-   Updated   2-NOV-1991   Jan S. Hoftun  (add TOOL_NAMES) 
C-   Updated  13-APR-1992   James T. Linnemann (add write/unbiased counters)
C-   Updated   1-JUN-1992   Jan S. Hoftun  (Add SEND_RAW) 
C-   Updated  10-JUL-1992   Jan S. Hoftun  (SAVED_MARK_AND_PASS; BAD counters))
C-   Updated  4-Nov-1992    James T. Linnemann (Add Level1 Statistics)
C-
C----------------------------------------------------------------------
      INTEGER MAX_TOOLS
      PARAMETER (MAX_TOOLS=52)
      INTEGER MAX_TOOL_FILTER
      PARAMETER (MAX_TOOL_FILTER=52)
      INTEGER MAX_INDEX_L1
      PARAMETER (MAX_INDEX_L1=63)
      INTEGER TOOL_TABLE(MAX_TOOLS),MAX_IN_USE,FILTER_MAX(0:127),
     &        FILTER_BIT_SET(4,0:MAX_INDEX_L1),
     &        MAX_TOOL_USE,
     &        UNFILTER_RATE(0:127),
     &        FILTER_COUNT(2,0:MAX_TOOL_FILTER,0:127),TRIG_MASK,
     &        HARDWARE_BIT(0:127),FILTER_SET_COUNT(0:127),TOT_EVENTS(3),
     &        RESULT_TYPE(20),FILTER_MAX_USE,UNFILTER_COUNTER(0:127),
     &        TIME_POINTER,FILTER_WRITE_COUNT(0:127),
     &        WRITE_UNFILT_COUNT(0:127),FILT_EVENTS(4),
     &        L1_SET(0:31),L1_TRY(0:31),L1_PASS(0:31),
     &        L1_UNBIAS(0:31),L1_WRITE(0:31),TIMED_OUT_COUNT
      LOGICAL*1 FILTER_FORCE(0:127)
      BYTE FILTER_ORDER(1:128),FILTER_SCRIPT(2,MAX_TOOL_FILTER,0:127)
      INTEGER MAXTIME
      PARAMETER(MAXTIME=100)
      REAL TUSED(MAXTIME)
      CHARACTER*16 TOOL_NAMES(MAX_TOOLS)
      LOGICAL SEND_RAW,SAVED_MARK_AND_PASS
      INTEGER BAD_CAL_COUNTER,BAD_MU_COUNTER,BAD_CD_COUNTER
      LOGICAL SAVED_BAD_CAL_FLAG,SAVED_BAD_MU_FLAG,SAVED_BAD_CD_FLAG
      INTEGER SHADOW_OFFSET
      COMMON /FILTER_COM/ TOOL_TABLE,MAX_IN_USE,FILTER_MAX,
     &                    FILTER_BIT_SET,FILTER_SCRIPT,MAX_TOOL_USE,
     &                    UNFILTER_RATE,FILTER_FORCE,FILTER_COUNT,
     &                    TRIG_MASK,HARDWARE_BIT,FILTER_SET_COUNT,
     &                    TOT_EVENTS,RESULT_TYPE,FILTER_ORDER,
     &                    FILTER_MAX_USE,UNFILTER_COUNTER,TIME_POINTER,
     &                    TUSED,TOOL_NAMES,FILTER_WRITE_COUNT,
     &                    WRITE_UNFILT_COUNT,FILT_EVENTS,SEND_RAW,
     &                    SAVED_MARK_AND_PASS,BAD_CAL_COUNTER,
     &                    BAD_MU_COUNTER,BAD_CD_COUNTER,
     &                    SAVED_BAD_CAL_FLAG,SAVED_BAD_MU_FLAG,
     &                    SAVED_BAD_CD_FLAG,SHADOW_OFFSET,
     &                    L1_SET,L1_TRY,L1_PASS,L1_UNBIAS,L1_WRITE,
     &                    TIMED_OUT_COUNT
