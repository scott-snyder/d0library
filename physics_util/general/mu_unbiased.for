C DEC/CMS REPLACEMENT HISTORY, Element MU_UNBIASED.FOR
C *2    29-MAR-1995 09:23:14 MEENA "improvements"
C *1    28-MAR-1995 10:37:02 MEENA "from Paul Quintas : select event passing any non-muon filter"
C DEC/CMS REPLACEMENT HISTORY, Element MU_UNBIASED.FOR
      LOGICAL FUNCTION mu_unbiased(filter_name)

C       purpose: return TRUE if this event passed any non-muon filter
C       created: 30-Oct-94      Paul Quintas
C       modified: 26-Mar-95     Paul Quintas: remove "LUM..." filters
C                               since they are used for the MONITOR stream
C                               and therefore are not unbiased
C       modified: 28-Mar-95     Paul Quintas: switch to a loop over the
C                               biased strings.

      IMPLICIT NONE

      INTEGER ntrigon, nfilton, i, j
      INTEGER trigbon(32), filtbon(128)
      INTEGER icloc, lenocc, filtlen
      CHARACTER*32 trignon(32), filtnon(128), filter_name

      INTEGER nveto
      PARAMETER (nveto=2)
      CHARACTER*12 veto(nveto)
      INTEGER lveto(nveto)

      DATA veto /'MU','LUM'/
      DATA lveto / 2,3 /

      mu_unbiased = .false.
      filter_name = 'NONE'

C LOOP OVER ALL THE FILTERS WHICH PASSED LOOKING FOR AN UNBIASED ONE

      CALL gttsum(ntrigon,trigbon,trignon,nfilton,filtbon,filtnon)
      DO 1 i = 1, nfilton
        filtlen = lenocc(filtnon(i))
        CALL cltou(filtnon(i))

C CHECK EACH FILTER FOR OCCURENCES OF THE VETO STRINGS

        DO 2 j = 1, nveto
          IF (icloc(veto(j),lveto(j),filtnon(i),1,filtlen).gt.0) goto 1
    2   CONTINUE

C IF I GET HERE THEN THIS FILTER HAS NONE OF THE 'BAD' STRINGS IN IT
C THEREFORE IT IS UNBIASED

        mu_unbiased = .true.
        filter_name = filtnon(i)
        RETURN

    1 CONTINUE
      RETURN

      END
