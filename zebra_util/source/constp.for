      SUBROUTINE CONSTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Construct TOP-level STP banks, namely STPH,
C-                         STPO, STPC, STPN and SCPH. The latter bank
C-                         is created and initialized with a call to
C-                         BKSCPH. This structure resides in ZEBRA store
C-                         ZEBSTP. CONSTP is called by INIZEB (in 
C-                         ZEBRA_UTIL).
C-   Inputs : None
C-   Outputs: None
C-
C-   Created     OCT-1986   Serban D. Protopopescu
C-   Updated  30-JUN-1988   Harrison B. Prosper
C-   Updated  13-SEP-1988   Ghita Rahal-Callot   add new link to SGEN
C-   Updated  16-APR-1991   Alan M. Jonckheere   add SSAM link + spare
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INTEGER IO,LSTPO,LSTPC,LSTPN,LSCPH
      INTEGER ILINK
      DATA ILINK /10/
C----------------------------------------------------------------------
C
C **** NOTE: The banks STPx contain no data, flag is set to integer
C
      IO=2     
      LSTPH=0
C
      CALL MZWIPE(IDVSTP)
      CALL MZBOOK(IDVSTP,LSTPH,LSTPH,1,'STPH',10,10,10,IO,0)
      CALL MZBOOK(IDVSTP,LSTPO,LSTPH,-IZSTPO,'STPO',ILINK,ILINK,10,IO,0)
      CALL MZBOOK(IDVSTP,LSTPC,LSTPH,-IZSTPC,'STPC',ILINK,ILINK,10,IO,0)
      CALL MZBOOK(IDVSTP,LSTPN,LSTPH,-IZSTPN,'STPN',ILINK,ILINK,10,IO,0)
C
C ****  Create and initialize the bank SCPH
C
      CALL BKSCPH (LSCPH)
C
  999 RETURN
      END
