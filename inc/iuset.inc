C----------------------------------------------------------------------
C-
C-      /IUSET_CHAR/
C-      /IUSET/
C-
C-      Detector Set SRCP Descriptor
C-
C-   Created  11-OCT-1989   Stuart Fuess   
C-   Updated  10-DEC-1989   Stuart Fuess  Use more CHARACTERs, raise
C-                                        dimensions 
C-   Updated  21-MAR-1990   Stuart Fuess  Raise dimensions to 500 
C-   Updated  10-MAY-1990   Stuart Fuess  Raise dimensions to 1000 
C-
C----------------------------------------------------------------------
      INTEGER MAX_NV
      PARAMETER ( MAX_NV = 1000 )
      CHARACTER*32 IUSET_LABEL
      CHARACTER*4 IUSET_NAME
      CHARACTER*4 IUSET_VOLUME_NAME(MAX_NV)
      INTEGER IUSET_NV
      INTEGER IUSET_IDTYPE(MAX_NV)
      COMMON /IUSET_CHAR/ IUSET_LABEL,
     &                    IUSET_NAME,
     &                    IUSET_VOLUME_NAME
      COMMON /IUSET/ IUSET_NV,
     &               IUSET_IDTYPE
