C----------------------------------------------------------------------
C-
C- common block for filter timing 
C-   Created   4-FEB-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      LOGICAL TIME_SCRIPT,TIME_TOOL,HISTO_SCRIPT,HISTO_TOOL
      INTEGER ID0FILT,ID0SCRIPT,ID0TOOL 
      INTEGER ICPU           
      INTEGER IDFILT,IDSCRIPT(0:127),IDTOOL(1:MAXTOOL)
      INTEGER IGOOD 
      COMMON/FILTER_TIMING/TIME_SCRIPT,TIME_TOOL,
     &  HISTO_SCRIPT,HISTO_TOOL,ID0FILT,ID0SCRIPT,ID0TOOL,
     &  ICPU,IDFILT,IDSCRIPT,IDTOOL


