CDECK  ID>, TRFCMN. 
C-- Modified :    John Skeens   02/03/92  for D0
C-- Author :    David Adams   17/02/92  for SDC
C This deck contains pieces of TRFCMN which may be different for
C different applications.
CDECK  ID>, TRFPIN. 
C
C  deck to pull in the trfcmn block for d0
C
C Internal common used to pass data between routines in TRF.

C-----------------------------------------------------------------------

C Superlayer parameters

C The following parameters are defined in TRFMEM1.
C MAXSUP = maxumum # superlayers
C MAXSCL = maximum number of superclusters in a superlayer.
C The memory required for clustering is
C 100*MAXSUP*MAXSCL bytes = 100*60*500 bytes = 3 MB.
CC maximum number of superlayers.
      INTEGER MAXSUP
      PARAMETER( MAXSUP = 60 )
CC maximum number of superclusters per superlayer
      INTEGER MAXSCL
      PARAMETER( MAXSCL = 2000 )

CC maximum number of layers per superlayer
      INTEGER MAXSLL
      PARAMETER( MAXSLL = 16 )

CC number of superlayers (this is actually the largest superlyer number
CC that has been asigned -- there may be gaps)
      INTEGER NSUPER

CC number of layers in each superlayer
CC NSLL < 0 is a superlayer composed of -NSLL other superlayers
CC NSLL = 0 superlayer composed of digitizations taken from a volume
CC          without any sublayers
CC NSLL > 0 is a superlayer made up of digitizations from NSLL layers
      INTEGER NSLL(MAXSUP)

CC pointers to assign the volume/layer/ends to superlayers
CC IPTEND = 1 or 2 for - or + end.
CC Note that for SDC, the bank locations of the superclusters and
CC coordinates are specified by the two indices IV = IPTVOL(1,ISL)
CC and IVL = IPTVLA(1,ISL) and consequently this combination must be
CC unique for a given shape.  Each volume may have one vector
CC coordinate super-superlayer for which IPTVLA is set to 0.
      INTEGER IPTVOL(MAXSLL,MAXSUP)
      INTEGER IPTVLA(MAXSLL,MAXSUP)
      INTEGER IPTEND(MAXSLL,MAXSUP)

CC Pointers for assigning superlayers to superlayers (e.g. to build a
CC 3D superlayer form a set of 2D superlayers.
      INTEGER IPTLAY(MAXSLL,MAXSUP)

CC superlayer shape: This index indicates how the superclusters and
CC digitizations are stored
CC  ISLSHP = 0  ==>  superlayer has not been defined
CC           1  ==>  cylinder -- fibers along z
CC           2  ==>  disk -- fibers along u
CC           3  ==>  spiral -- like disk, but fibers in spiral shape
CC           4  ==>  wedge
CC          11  ==>  super-superlayer measuring phi
CC          12  ==>  super-superlayer measuring phi, z
CC          13  ==>  super-superlayer measuring phi, tan(alpha)
CC          14  ==>  super-superlayer measuring phi, z, tan(alpha)
CC         101  ==>  silicon barrel
CC         102  ==>  silicon disk (z<0)
CC         103  ==>  silicon disk (z>0)
      INTEGER ISLSHP(MAXSUP)

CC Position of the center of each superlayer
      REAL XSUP(3,MAXSUP)

CC PSUP converts from fiber coordinates to spatial coordinates.
CC Analogous to PFIB in ANLTRK.
CC for ISLSHP=1  r = PSUP(1,ISL)
CC               phi = PSUP(3,ISL)*[IF-1-PSUP(2,ISL)] + PSUP(4,ISL)*z
CC               z is internal in the above expression
CC               z-range is PSUP(7,ISL) < z < PSUP(8,ISL) (external)
CC for ISLSHP=2  z = PSUP(1,ISL) (external)
CC               u is parallel to the fibers
CC               v = -R + PSUP(3,ISL)*[IF-1-PSUP(2,ISL)]
CC               tht = PSUP(4,ISL)
CC               y = cos(tht)*v + sin(tht)*u
CC               x = cos(tht)*u - sin(tht)*v
CC               r-range is PSUP(7,ISL) < r < PSUP(8,ISL)
CC               u-range is PSUP(9,ISL) < u < PSUP(10,ISL)
CC for ISLSHP=3  z = PSUP(1,ISL) (external)
CC               r0 = PSUP(4,ISL)
CC               phi = ( SQRT((r/r0)**2-1) - ACOS(r0/r) ) * PSUP(7,ISL)
CC                     + (IF-1-PSUP(2,ISL)) * PSUP(3,ISL)
CC               x = r*cos(phi)
CC               y = r*sin(phi)
CC               PSUP(9,ISL) < r < PSUP(10,ISL)
CC               PSUP(7,ISL) = direction of spiral (-1 or 1)
CC                             1 = counterclockwise looking down -z
CC for ISLSHP=11 same as ISLSHP = 1 (phi only varied in fit)
CC for ISLSHP=12 same as ISLSHP = 1 (phi and z varied)
CC for ISLSHP=13 same as ISLSHP = 1 (phi and talf varied)
CC for ISLSHP=14 same as ISLSHP = 1 (phi, z and talf varied)
CC for ISLSHP=101   r = PSUP(1,ISL)
CC                  PSUP(7,ISL) < z < PSUP(8,ISL)
CC for ISLSHP=102   z = PSUP(1,ISL)
CC         or 103   PSUP(7,ISL) < r < PSUP(8,ISL)
      REAL PSUP(10,MAXSUP)

CC Size of a superlayer in fiber coordinates.  Cylinder and spiral fiber
CC positions are calculated modulus this number.
      REAL FIBMAX(MAXSUP)

CC position of each layer w.r.t. layer position in fiber spacing units
      REAL XLAY(MAXSLL,MAXSUP)

CC offset of each layer in fiber spacing units
      REAL OFFLAY(MAXSLL,MAXSUP)

CC PTIM converts from the digitization time W in time slice units to
CC the time the particle crossed the fiber T in ns.  We assume that the
CC fibers in a cylinder superlayer will all give the same digitization
CC time if they are all crossed at the same time and z-coordinate.  For
CC a disk we impose a similar requirement for the u-coordinate except
CC we allow a cos(tht) dependence as well where tan(tht) = v/u.
CC PTIM(4,ISL) should be set to R/VFIBER if the readout fibers are
CC effectively the same length and to zero if the optical and
CC scintillating fibers together are a constant length.
CC See DLA SSC notes p 29.
CC FOR ISLSHP=1  T = PTIM(1,ISL) + PTIM(2,ISL)*W + PTIM(3,ISL)*Z
CC FOR ISLSHP=2  T = PTIM(1,ISL) + PTIM(2,ISL)*W +
CC                   PTIM(3,ISL)*U + PTIM(4,ISL)*(1-cos(THT))
CC                   where tan(THT) = v/u
CC FOR ISLSHP=3  T = PTIM(1,ISL) + PTIM(2,ISL)*W + PTIM(3,ISL)*A where
CC               A = (PSUP(10,ISL)*PSUP(10,ISL) - R*R) / (2*PSUP(4,ISL))
      REAL PTIM(4,MAXSUP)


C-----------------------------------------------------------------------

C supercluster parameters

CC maximum number of tracks to which a supercluster can be assigned
      INTEGER MAXSTR
      PARAMETER( MAXSTR = 1 )

CC maximum number of parameters in a coordinate
      INTEGER MAXCOV
      PARAMETER( MAXCOV = 3 )

CC maximum number of scl's contributing to ssl's.
      INTEGER MAXPSS
      PARAMETER ( MAXPSS = 50000 )

CC number of superclusters per superlayer end
      INTEGER NSCL(MAXSUP)

CC supercluster position and uncertainty in fiber units
      REAL XFIB(MAXSCL,MAXSUP), DXFIB(MAXSCL,MAXSUP)

CC supercluster slope and uncertainty
      REAL SFIB(MAXSCL,MAXSUP), DSFIB(MAXSCL,MAXSUP)

CC super cluster time and uncertainty in time slice units
      REAL TFIB(MAXSCL,MAXSUP), DTFIB(MAXSCL,MAXSUP)

CC super cluster position along fiber and uncertainty in cm
CC for cylinder, ZFIB is the internal z
CC for disk, ZFIB is u
CC for spiral, ZFIB is r
      REAL ZFIB(MAXSCL,MAXSUP), DZFIB(MAXSCL,MAXSUP)

C chi-square for position-slope and time fits
      REAL FCHISQ(MAXSCL,MAXSUP), TCHISQ(MAXSCL,MAXSUP)

CC Coordinates for each supercluster:
CC NCOOR is the # coordinates for the superlayer (this may differ from
CC       NSCL because we may fill in only the coordinate info or only
CC       the cluster info (XFIB ...) for some superlayers).
CC FITPHI is r or z (cylinder or disk) approx constant for superlayer
CC FITPHI is phi
CC FITZR is z or r (cylinder or disk)
CC FITTAL is tan(alpha)
CC FITERR is the error matrix for (phi, z) or (phi, r)
CC For ordinary superlayers, FITTAL is not used and the error matrix
CC is 2x2.  These are called coordinates.  For super-superlayers,
CC FITTAL is used and the error matrix is 3x3  These are called vector
CC coordinates.  Presently the latter only include fiber barrels.  The
CC SSL shape parameter indicates what was actually varied in the fit,
CC but elements which were not varied are assigned default values.
      INTEGER NCOOR(MAXSUP)
      REAL FITRZ(MAXSCL,MAXSUP)
      REAL FITPHI(MAXSCL,MAXSUP)
      REAL FITZR(MAXSCL,MAXSUP)
      REAL FITTAL(MAXSCL,MAXSUP)
      REAL FITERR(MAXCOV,MAXCOV,MAXSCL,MAXSUP)
      REAL FITERI(MAXCOV,MAXCOV,MAXSCL,MAXSUP)

CC Number of 3D scl's discarded in NSCSC3.
      INTEGER N3DISC(MAXSUP)

CC Number of tracks to which a supercluster has been assigned and a
CC list of such tracks.  The list for each sl/scl is limited to
CC MAXSTR elements but the number of tracks is not subject to this
CC limitation.  If tracks are added past the limit and then other
CC tracks are deleted to bring the list back into range, there wiil
CC be positions on the list for which we do not know the track number.
CC These will be filled with -1.  See TRFASC and TRFDSC for details.
      INTEGER NSCTRK(MAXSCL,MAXSUP), ISCTRK(MAXSTR,MAXSCL,MAXSUP)

C Number of hit fibers and hits (time slices).
      INTEGER NTRFFB(MAXSLL,MAXSUP), NTRFHT(MAXSLL,MAXSUP)

C-----------------------------------------------------------------------

C control indices and parameter

C These provide directions for finding tracks through the superclusters
C ICON(1,i)  (2)  (3)  (4)
C         0    0    0    0  ==> terminator
C         1   IT   IS    0  ==> use all unallocated superclusters in
C                               superlayer IS to start tracks in set IT
C         2   IT   IS    0  ==> try to extend each track in the set IT
C                               by adding a supercluster from
C                               superlayer IS
C         3  IS1  IS2  ISF  ==> try to extend each track in set IS1 by
C                               adding a track from set IS2.  Match at
C                               surface type ISF with parameter RCON.
C        11   IT    0    0  ==> clear set 11
C        12  IT1  IT2    0  ==> add the tracks from set IT2 to set IT1
C        19   IT    0    0  ==> close all the tracks in set IT -- i.e.,
C                               extrapolate to the vertex

CC maximum number of control lines and control parameters per line
      INTEGER MAXCLI, MAXCPA
      PARAMETER (MAXCLI=1000)
      PARAMETER (MAXCPA=4)

CC number of control lines
      INTEGER NCLI

CC control indices
      INTEGER ICON(MAXCPA,MAXCLI)

CC control parameter
      REAL RCON(MAXCLI)

C-----------------------------------------------------------------------

C fitted track parameters

C TRACK(1,ITR) is the transverse radius ( sqrt(x**2+y**2) )
C TRACK(2,ITR) is the polar coordinate phi
C TRACK(3,ITR) is the coordinate z
C TRACK(4,ITR) is the direction angle alpha where tan(alpha+phi) = dy/dx
C TRACK(5,ITR) is the direction angle lambda where
C              tan(lambda) = dz/dst = pz/pt
C TRACK(6,ITR) is the transverse curvature ?????
C TRACK(7,ITR) is the time
C TRACK(8,ITR) is 1/speed = dt/ds
C TRACK(9,ITR) is the chi-square for the positions
C TRACK(10,ITR) is the chi-square for the times
C TRACK(11-31,ITR) contains the upper triangle of the error matrix for
C                  the six parameters TRACK(1-6,ITR).
C TRACK(32-34,ITR) contains the upper triangle of the error matrix for
C                  the two parameters TRACK(7-8,ITR).
C TRACK(35,ITR) is the floating form of IFIX define below.
C TRACK(36,ITR) is LFIXC (0=false,-1.0 is true)
C TRACK(40,ITR) is the largest candidate chi-square estimate from TRFTRE

CC Definition of IFIX.
CC Fixed parameter for track fit -- 1 ==> r, 2 ==> phi, ...
CC IFIX = -1 ==> impose radial vertex constraint (r=0, alpha=0)
CC IFIX = -2 ==> impose radial and z vertex constraints
CC               (r=0, alpha=0, z=0)
CC Definition of LFIX.
CC Logical to indicate that curvature should be fixed to zero in
CC addition to some other parameter specified by IFIX.


C MAXTRA = maximum number of tracks,
C MAXTSC =                   superclusters contributing to a track,
C MAXTSM =                   superlayers missed by a track
C MAXTSS =                   superlayers skipped by a track
C MAXSET =                   track sets
C MAXTRK =                   tracks in a set
C The memory required for track-finding is
C (MAXTPA+3*MAXTSC+MAXTSM+MAXTSS+5)*MAXTRA + MAXSET*MAXTRK  words =
C (  40  +  3*25  +  10  +  10  +5)* 20k   +   5   * 20k    words
C                                                     =  2.9 MW
C                                                     = 11.6 MB
C
CC maximum number of tracks,
C                    superclusters contributing to a track,
C                    missed superlayers on a track
C                    track sets
C                    tracks in a set
      INTEGER MAXTRA, MAXTSC, MAXTSM, MAXTSS, MAXSET, MAXTRK
      PARAMETER (MAXTRA=25000)
      PARAMETER (MAXTSC=20)
      PARAMETER (MAXTSM=5)
      PARAMETER (MAXTSS=10)
      PARAMETER (MAXSET=5)
      PARAMETER (MAXTRK=25000)

C number of track parameters
      INTEGER MAXTPA
      PARAMETER (MAXTPA=40)

C maximum number of track hit counters
      INTEGER MAXSC0
      PARAMETER (MAXSC0=4)

C maximum number of elements in the ISLLAM list.
      INTEGER MAXSLM
      PARAMETER (MAXSLM=5)

CC number of tracks defined
      INTEGER NTRACK

CC number of and list of tracks belonging to each track set
      INTEGER NTRK(MAXSET)
      INTEGER ITRK(MAXTRK,MAXSET)

C Set associated with each track.  This is set to zero to indicate the
C track can be overwritten with TRFCOP.
      INTEGER ISETTR(MAXTRA)

C Mother track for each track (set in TRFCOP).
      INTEGER ITRMOM(MAXTRA)

CC track fit parameters
      REAL TRACK(MAXTPA,MAXTRA)

CC Number of superclusters contributing to each track and lists of the
CC contributing superlayers and superclusters.  ITSFIT indicates whether
CC fiber measurement (0), coordinate (1) or vector coordinate (2) is
CC used to fit that supercluster.
      INTEGER NTSCL(MAXTRA)
      INTEGER ITSL(MAXTSC, MAXTRA)
      INTEGER ITSCL(MAXTSC, MAXTRA)
      INTEGER ITSFIT(MAXTSC, MAXTRA)

CC Number of superclusters last used to fit in TRFFIT.
      INTEGER NTSCLF(MAXTRA)

CC Saved number of superclusters contributing to each track.
      INTEGER NTSCL0(MAXTRA,MAXSC0)

CC Number of superlayers the track missed and a list of such
CC superlayers.  A superlayer is missed if it was tried (TRFTRE) but
CC not added.  See note in the TSKIP section.
      INTEGER NTMISS(MAXTRA)
      INTEGER ITMISS(MAXTSM,MAXTRA)

CC List of superlayers which the track is assumed not to cross.
CC Note: A superlayer may be included in both the TMISS and TSKIP lists
CC       if it is not certain whether or not a superlayer is crossed.
      INTEGER NTSKIP(MAXTRA)
      INTEGER ITSKIP(MAXTSS,MAXTRA)

CCCCCCC  11SEP91 - this parameter is now kept in TRACK(35)
CCCCCCC      INTEGER IFIX(MAXTRA)

C GEANT track number associated with each supercluster and track.
      INTEGER IGTSCL(MAXSCL,MAXSUP)
      INTEGER IGTTRK(MAXTRA)

C Match chi-square.
      REAL CHSQGT(MAXTRA)

C Number of closed sets and the set numbers.
      INTEGER NCLOSE, ISETCL(MAXSET)

C-----------------------------------------------------------------------

C Clustering constants.

CC maximum spacing at which to combine clusters into superclusters
      REAL FIBSEP

CC Maximum chi-square for TRFSCO -- above this value errors are
CC increased so that the chi-square is this value.
      REAL CHSQLM

C Chi-square max for use in TRFSC3.
      REAL CHQSC3

C Maximum number of 3D superclusters for each 2D supercluster in the
C anchor layer.
      INTEGER NSCSC3

C Significance value for test in TRFSC3.  After the first two
C superlayer superclusters are used to define a candidate, additional
C superclusters are require to fall within this significance
C (difference/uncertainty) to be added to the super-supercluster.
      REAL SIGSC3

C Control parameter for dr corrections in TRFC3F.
C IVAC3F = 0 - no corrections
C          1 - correct for dz/dr
C          2 - correct for dphi/dr
C          3 - correct for both
      INTEGER IVAC3F

C Maximum allowed curvature for TRFSC3.
      REAL CRVSC3

C-----------------------------------------------------------------------

C Fit parameters.  These are modified in TRFTRF and used by lower level
C routines

CC Accuracy required in calculating positions along tracks.  (See
CC TRFCYL.)
      REAL STPMIN

CC Chi-square cut in TRFCAN.
      REAL CHMCAN

CC Flag to indicate that TRFTRE should use the current supercluster and
CC a full vertex constraint to estimate lambda.
      LOGICAL LLAM

CC Flag to indicate that TRFTRE should use two z-measuremnts from
CC ISLLAM to estimate lambda and z0.  If a z-vertex contraint is
CC present, it will be removed.
      LOGICAL LLAM2

CC Mininum error for which LLAM2 may take effect.  If the uncertainty
CC in lambda is smaller than this value, then that section of code in
CC TRFTRE is skipped (as though LLAM2 were false).
      REAL XLLAM2

CC ISLLAM contains a list of superlayers to be used to estimate lambda
CC and z0 when LLAM2 is true.
      INTEGER ISLLAM(MAXSLM,MAXSET)

CC Flag to indicate that TRFTRE should use the current and preceeding
CC superclusters to estimate phi0 and C when the second scl or
CC coordinate is added to the track.
      LOGICAL LCURV2

CC Maximum allowed curvature when LCURV2 is set.
      REAL CRMAX2

CC Flags indicating whether z-limits should be checked in TRFTRE.
CC LZLIM1 - check vertex: z0 < ZRANGE before fitting in TRFTRE
CC LZLIM2 - check vertex: z0 < ZRANGE after fitting in TRFTRE
CC LZLIM3 - check z-boundary of the next superlayer in TRFCAN
      LOGICAL LZLIM1, LZLIM2, LZLIM3

C Range of z0 to allow in TRFTRE when LLAM2 is set.
      REAL ZRANGE

C Control parameter for TRFTRE indicating whether tracks should be
C extended using fiber measurements (0), coordinates (1) or vector
C coordinates (2).
      INTEGER KTSFIT

CC Active range of superlayer in TRFTRE.
      REAL SRFMIN, SRFMAX

CC Transfer sets (TRFTRE).
CCC 1 - low
CCC 2 - high
      INTEGER ISETXF(2,MAXSET)

CC Chi-square test flag and limit.
      LOGICAL LCHSQ
      REAL CHSQMX

CC Minimum # hits flag and value.
      LOGICAL LNHIT
      INTEGER NHITMN

CC Maximum # misses flag and value.  Note that this test is applied
CC to the number of superlayers which appear in the TMISS list and
CC do not appear in the TSKIP list.
      LOGICAL LNMIS
      INTEGER NMISMX

CC Maximum numbers of elements allowed for the TSKIP and TMISS lists.
      INTEGER MTSKIP, MTMISS

CC Flag indicating whether to fit tracks in TRFTRE.
      LOGICAL LTRFIT

CC Tracks with this number or fewer hits will always be fit.  Above
CC this value, the above flag is used.
      INTEGER MSCFIT

CC If LTRFIM is set then tracks will always be fit in TRFTRE if there
CC is more than one candidate.
      LOGICAL LTRFIM

CC Limits for track parameters in TRFFIT.
      REAL FITMIN(6), FITMAX(6)

CC Errors to assign when starting tracks.  The only elements used are
CC 2 - phi (rad)
CC 5 - lambda (rad)
CC 6 - curvature (cm-1)
      REAL DTRSTV(6)

CC Coordinate ZR limits for starting tracks.
CC Logical indicating whether to test and allowed range.
      LOGICAL LZRCHK
      REAL ZRMIN, ZRMAX


C-----------------------------------------------------------------------

      COMMON /TRFCMN/ NSUPER, NSLL, ISLSHP, XSUP, PSUP, FIBMAX,
     1                XLAY, OFFLAY, PTIM,
     1                NSCL, XFIB, DXFIB, SFIB, DSFIB, TFIB, DTFIB,
     1                ZFIB, DZFIB,
     1                NCOOR, FITRZ, FITPHI, FITZR, FITTAL,
     1                FITERR, FITERI, N3DISC,
     1                FCHISQ, TCHISQ, NSCTRK, ISCTRK,
     1                NCLI, ICON, RCON,
     1                NTRACK, NTRK, ITRK, ISETTR, ITRMOM, TRACK,
     1                NTSCL, ITSL, ITSCL, ITSFIT, NTSCLF,
     1                NTSCL0, NTMISS, ITMISS, NTSKIP, ITSKIP,
     1                NTRFFB, NTRFHT, NCLOSE, ISETCL

      COMMON /TRFGEA/ IGTSCL, IGTTRK, CHSQGT

      COMMON /TRFTRK/ IPTVOL, IPTVLA, IPTLAY, IPTEND

      COMMON /TRFPAR/ STPMIN, CHMCAN, LLAM, LLAM2, XLLAM2, ISLLAM,
     1                LCURV2, CRMAX2,
     1                LZLIM1, LZLIM2, LZLIM3,
     1                ZRANGE, KTSFIT, SRFMIN, SRFMAX, ISETXF,
     1                LCHSQ, CHSQMX, LNHIT, NHITMN, LNMIS, NMISMX,
     1                MTSKIP, MTMISS, LTRFIT, MSCFIT, LTRFIM,
     1                FITMIN, FITMAX, DTRSTV,
     1                LZRCHK, ZRMIN, ZRMAX

      COMMON /TRFCLC/ FIBSEP, CHSQLM, CHQSC3, NSCSC3, SIGSC3, IVAC3F,
     1                CRVSC3

