C----------------------------------------------------------------------
C-
C-   Created  26-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      COMMON /CIMPACT/ EM3AV(3),DEM32,ALAM,RIMPACT(3),DEL_IMPACT(3)
      REAL    EM3AV,DEM32,ALAM,RIMPACT,DEL_IMPACT
C- EM3AV is the Center of Energy of the shower.
C- DEM32 is the square of the magnitude of EM3AV
C- RIMPACT is the position of impact of the Isajet Track on
C- a plane perpendicular to EM3AV
C- DEL_IMPACT is RIMPACT - EM3AV
