C----------------------------------------------------------------------
C-
C-   Created  24-MAY-1992   JFG
C-                          Information on clustering in TRD layers
C-                          Maximum 30 groups of maximum 50 hits (?)
C- NGROUP_PER_LAYER(L):number of  grouped set of wires found in layer L
C- NWIRE_PER_GROUP(NW,L): number of wires in group NW in layer L
C- LISTW_INCL(List,NW,L): list of wires for group NW in layer L
C----------------------------------------------------------------------
      INTEGER NWRMAX,NCLMAX
      integer nlm
      parameter (nlm=6)
      PARAMETER (NWRMAX = 50)
      PARAMETER (NCLMAX = 30)
      COMMON /TRD_GROUPS/ NGROUP_PER_LAYER(nlm),NWIRE_PER_GROUP
     &(NCLMAX,nlm),LISTW_INCL(NWRMAX,NCLMAX,nlm),GOOD_WIRE(256,nlm)
      INTEGER NGROUP_PER_LAYER,NWIRE_PER_GROUP,LISTW_INCL
      LOGICAL GOOD_WIRE
