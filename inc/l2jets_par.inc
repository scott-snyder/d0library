C----------------------------------------------------------------------
C-                              *** This file is L2JETS_PAR.INC  ***
C-
C-   Created   6-MAY-1990   Richard V. Astur
C-
C- Common block L2JETS_PAR:Hold Filter parameter sets for the JETS tool
C----------------------------------------------------------------------
      REAL ETMIN( NPAR_MAX )            
      INTEGER ICON_CEN( NPAR_MAX)       
C                                       
      INTEGER ICON_ENG( NPAR_MAX)       
C                                       
      REAL MAXRAD( NPAR_MAX )           
      REAL MINRAD( NPAR_MAX )           
      REAL EMFRACT_MAX( NPAR_MAX )      
c                                       
      REAL EMFRACT_MIN( NPAR_MAX )      
      LOGICAL VETO_THIS( NPAR_MAX )     
      INTEGER NJET_COUNT( NPAR_MAX )    
      COMMON /L2JETS_PAR/ ETMIN,ICON_CEN,ICON_ENG,MINRAD,NJET_COUNT,
     &  MAXRAD, EMFRACT_MAX,EMFRACT_MIN, VETO_THIS

