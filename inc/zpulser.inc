C----------------------------------------------------------------------
C-
C-   Created   1-NOV-1990   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      COMMON /ZPULSER/ AMPLTDA,AMPLTDB,SHHALF,SHCARD,POLARITY,QUADRANT,
     &  PREAMP,CRTSEL,NPULSE,GOT_TARGET,TARGET,DECT,PULSER(50)

C
      INTEGER AMPLTDA,AMPLTDB
      INTEGER SHHALF,SHCARD,CRTSEL
      INTEGER POLARITY,QUADRANT,PREAMP
      INTEGER NPULSE
      CHARACTER*12 TARGET,PULSER
      CHARACTER*3 DECT
      LOGICAL GOT_TARGET
C
C  AMPLTDA    = Amplitude of Step pulse A
C  AMPLTDB    = Amplitude of Step pulse B
C  POLARITY   = Polarity of the test pulse 
C  SHHALF     = Indicates upper or lower half of shaper crate
C  SHCARD     = Shaper card number
C  PREAMP     = Indicates whether pulsing through preamps or shapers
C  NPULSE     = Number of pulsers chosen
C  CRTSEL     = Shaper crate select bits. Turn ON or OFF the shaper crate
C  QUADRANT   = Determines whether No/Odd/Even or all channels are being pulsed
C  DECT       = Detector name (VTX,CDC,TRD or FDC))
C  TARGET     = Pulser device name as in HDB
C  PULSER     = Array of names of chosen pulsers
C  GOT_TARGET = Logical indicating whether a pulser device is being owned.
C
