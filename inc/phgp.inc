C----------------------------------------------------------------------
C-
C-   Created  30-AUG-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
C     PHGP is a conversion table that for an IETAC,IPHIC indices gives
C     the base hex address, to w/i a ROT, of an area in the calorimeter,
C     and the individual addresses of all the gaps (MG's,ICD,..?)
C     --The parameters the dimensioning depend on are found in 
C     D0$PARAMS:Cal_offline.params 
*
      COMMON /PHGP/PHGP
      INTEGER PHGP(-NETAL:NETAL,NPHIL,2,(1+MXLYMG-MNLYMG+1))
C                                       ^  ^ |             |
C                 1:CRATE 2:IADDR_______|  | |             |
C                                          | |_____________| 
C                 For the address of the___|        |
C                 first calorimeter cell,     The number of
C                 modulo bits 0-5.            indiv. gap addrs.
C             
C               PHGP(IETAC,IPHIC,1 for CRATE,CCGPTP   for CC MG)  
C                                2 for IADDR,ICDGPTYP for ICD
C                                            ECGPTP   for EC MG
C                   ----> the HEX address of the gap there.
C               CCGPTP,ICDGPTP,ECGPTP, are parameters defined
C               in D0$PARAMS:CAL_LEVEL2.PARAMS
