C VAX/DEC CMS REPLACEMENT HISTORY, Element TRKDGT.INC
C *5     8-SEP-1992 11:58:46 WILCOX "allow up to 4 mc track hits per fiber"
C *4    17-DEC-1991 15:37:36 SKEENS "modified array sizes less memory"
C *3    13-DEC-1991 11:36:14 SKEENS "new memory parameters to save memory space"
C *2     4-OCT-1991 15:16:52 SKEENS "digitization common"
C *1     3-OCT-1991 10:46:20 SKEENS "scifi digitization common"
C VAX/DEC CMS REPLACEMENT HISTORY, Element TRKDGT.INC
C
C TRKDGT.INC
C
C      The common TRKDGT contains the parameters which define how the hit
C occuring at time T is digitized.  The readout for each channel consists of
C aseries of on/off bits assigned to contiguous time slices of fixed length.
C The signal is given a fixed width in time and all channels with which the
C signal overlaps are turned on.
 
C Maximum number of layers in a volume
      INTEGER MAXVL
      PARAMETER ( MAXVL = 1 )
 
C Maximum number of channels (fibers) in a layer
      INTEGER MAXLC
      PARAMETER ( MAXLC = 5000 )
 
C maximum number of digitization elements for defining array sizes
      INTEGER MAXDGT
      PARAMETER( MAXDGT=16 )
 
C maximum number of saved track numbers per hit fiber
      INTEGER MAXSIT
      PARAMETER ( MAXSIT=4 )
 
C minimum distance (cm) and energy (GeV) to digitize with
      REAL SMIN, EMIN
      PARAMETER( SMIN = 1.0E-5 )
      PARAMETER( EMIN = 1.0E-10 )
 
C The flag LDGINI is set to true after the values for NDGTIM, ..., VSIG have
C been set in either TRKDET or TRKDGI.
      LOGICAL LDGINI
 
C number of time slices saved [TRKDET,TRKDGI]
      INTEGER NDGTIM
 
C time offset and width of buckets [TRKDET,TRKDGI]
C ITIM == INT(T-DGTOFF)/DGDTIM
      REAL DGTOFF, DGDTIM
 
C time spread of signal [TRKDET,TRKDGI]
      REAL DGTWID
 
C Light production and attenuation parameters [TRKDET,TRKDGI]:
C PHOGEV - number of photons produced /GeV energy loss,
C FINTER - fraction inside internal reflection cone (1-cos(tht)) in one dirn.
C ATTLEN - attenuation length (cm) in active fiber.
C ATTOPT - attenuation in the optical fiber.
C QUANEF - quantum efficiency of the readout device
C
      REAL PHOGEV, FINTER, ATTLEN, ATTOPT, QUANEF
 
C speed of signals in fiber (cm/s) [TRKDET,TRKDGI]
      REAL VSIG
 
C maximum number of fibers and array containing hits
       INTEGER MAXCHV
       PARAMETER( MAXCHV=10000 )
 
C arrays indicating which channels have fired and which time bits [TRKSIG]
C Two channels are allocated for each fiber so that hits in both ends may be
C stored.
      INTEGER ISIG( MAXLC, 2, MAXVL )
      INTEGER ISIGTR( MAXSIT, MAXLC, 2, MAXVL )
 
C Temporary storage for the current GEANT track number.
      INTEGER IGTRAK
 
C Count of digitizations which occur when there were already MAXSIT hits
C on a fiber in TRKFIL.
      INTEGER ICNTMH
 
      COMMON /RTRKDGT/ LDGINI, NDGTIM, DGTOFF, DGDTIM, DGTWID,
     1                PHOGEV, FINTER, ATTLEN, ATTOPT, QUANEF,
     2                VSIG, ISIG, ISIGTR, IGTRAK, ICNTMH
