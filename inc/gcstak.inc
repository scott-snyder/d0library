C----------------------------------------------------------------------
C-
C-   Created  26-APR-1991   Alan M. Jonckheere - Geant 3.14
C-
C----------------------------------------------------------------------
      INTEGER   NWSTAK,NWINT,NWREAL,NWTRAC
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV
