C DEC/CMS REPLACEMENT HISTORY, Element CC_IUSET.INC
C *2    28-DEC-1988 15:37:12 FUESS "DIFFERENT IDTYPE FOR EACH VOLUME"
C *1     8-DEC-1988 14:43:10 FUESS "Initial entry of CC .INC files"
C DEC/CMS REPLACEMENT HISTORY, Element CC_IUSET.INC
C----------------------------------------------------------------------
C-
C-      /CC_IUSET_CHAR/
C-      /CC_IUSET/
C-
C-      Central Calorimeter Detector Set SRCP Descriptor
C-
C-   Created   8-DEC-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER MAX_NV
      PARAMETER ( MAX_NV = 10 )
      CHARACTER*32 IUSET_LABEL
      INTEGER IUSET_NAME
      INTEGER IUSET_NV
      INTEGER IUSET_VOLUME_NAME(MAX_NV)
      INTEGER IUSET_IDTYPE(MAX_NV)
      COMMON /CC_IUSET_CHAR/ IUSET_LABEL
      COMMON /CC_IUSET/ IUSET_NAME,
     &                  IUSET_NV,
     &                  IUSET_VOLUME_NAME,
     &                  IUSET_IDTYPE
