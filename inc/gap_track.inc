
C----------------------------------------------------------------------
C-
C-   Created   23-JAN-1995  Tracy L. Taylor 
C-   Updated   02-SEP-1995  Andrew G. Brandt  rename to GAP_TRACK
C-   Updated   25-JAN-1996  Bob Hirosky  ADD SAMUS HITS 
C-   Updated   29-FEB-1996  Andrew G. Brandt move SAMUS to GAP_SAMULT
C-                                                       
C----------------------------------------------------------------------
C
      INTEGER NTRACKS,NT_BCUT,NT_BTHCUT,NT_BZTHCUT
      INTEGER NT_ETA13,NT_ETA10,NT_IETA13,NT_IETA10
      INTEGER NTCAL_R1,NTCAL_R2,NTCAL_R3
      INTEGER NTCAL_EMR1,NTCAL_EMR2,NTCAL_EMR3

      COMMON/GAP_TRACK/NTRACKS,NT_BCUT,NT_BTHCUT,NT_BZTHCUT,
     &             NT_ETA13,NT_ETA10,NT_IETA13,NT_IETA10,
     &             NTCAL_R1,NTCAL_R2,NTCAL_R3,
     &             NTCAL_EMR1,NTCAL_EMR2,NTCAL_EMR3

      CHARACTER*255 GAP_TRACK_TAG
      DATA GAP_TRACK_TAG/'NTRACKS:I,NT_BCUT:I,
     &     NT_BTHCUT:I,NT_BZTHCUT:I,NT_ETA13:I,NT_ETA10:I,
     &     NT_IETA13:I,NT_IETA10:I,NTCAL_R1:I,NTCAL_R2:I,
     &     NTCAL_R3:I,NTCAL_EMR1:I,NTCAL_EMR2:I,NTCAL_EMR3:I'/
