      LOGICAL FUNCTION TOP_EVENT_CLEANUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pick Clean Events
C-
C-   Returned value  : TRUE = good event, FALSE = poor event
C-   Inputs  : none
C-   Outputs : see entry point TOP_EVENT_CLEANUP_VALUES
C-   Controls: 
C-
C-   Created  18-JUL-1995   William Cobau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'

C- stuff for CAPH selection of jets...

      INTEGER ier
      REAL    template(5)
      DATA    template/1.0, 6.0, 0.5, 0.0, 0.0/      ! CONE R=0.5

C- here are event variables

      REAL    evtemf, evtchf, evticdmgf, evtdifchem
      REAL    emf, chf, icdmgf, difchem

C- pointer to jets bank

      INTEGER gzjets, ljets

C- variable for entry point

      LOGICAL TOP_EVENT_CLEANUP_VALUES

      REAL    minemf, maxchf, maxicdmgf, maxdifchem
C----------------------------------------------------------------------

      TOP_EVENT_CLEANUP = .FALSE.

C- set CAPH for 0.5 cone jets...

      CALL set_caph('CONE_JET',template,ier)

C- initial event variables

      evtemf = +10.0
      evtchf = -10.0
      evticdmgf = -10.0
      evtdifchem = -10.0

C- loop thru 0.5 cone jets

      ljets = gzjets()

      DO WHILE ( ljets.gt.0 ) 

        emf = q(ljets+14)
        evtemf = min(evtemf,emf)

        chf = q(ljets+18)
        evtchf = max(evtchf,chf)

        icdmgf = q(ljets+17)
        evticdmgf = max(evticdmgf,icdmgf)

        difchem = chf - emf
        evtdifchem = max(evtdifchem,difchem)

        ljets = lq(ljets)

      ENDDO

      call reset_caph

C- now set function

      IF ( evtdifchem.lt.0.5 ) THEN
        TOP_EVENT_CLEANUP = .TRUE.
      ENDIF

  998 return

      ENTRY top_event_cleanup_values(minemf,maxchf,maxicdmgf,maxdifchem)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return values calculated by TOP_EVENT_CLEANUP 
C-
C-   Inputs  : NONE
C-   Outputs : top_event_cleanup_values = top_event_cleanup
C-             minemf     = minimum em fraction for all 0.5 cone jets
C-             maxchf     = maximum ch fraction for all 0.5 cone jets
C-             maxicdmgf  = maximum icd/mg fraction for all 0.5 cone jets
C-             maxdifchem = maximum em - ch fraction for all 0.5 cone jets
C-   Controls: none
C-
C-   Created  18-JUL-1995   William Cobau
C-
C----------------------------------------------------------------------

      IF ( evtdifchem.lt.0.5 ) THEN
        TOP_EVENT_CLEANUP_VALUES = .TRUE.
      ELSE
        TOP_EVENT_CLEANUP_VALUES = .FALSE.
      ENDIF

      minemf = evtemf
      maxchf = evtchf
      maxicdmgf  = evticdmgf
      maxdifchem = evtdifchem

  999 RETURN
      END
