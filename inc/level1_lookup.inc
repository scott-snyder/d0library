C----------------------------------------------------------------------
C- 
C-   Purpose and Methods : This common block is internally used by the lookup
C-                         system management routines. It is initialized with 
C-                         the values read from the lookup system management
C-                         file.
C-                         
C-   Usable Variables :    Some quantities are directly usable variables, the
C-                         list below shows the directly used variables.
C-                         The rest of the variables are accessed and used by
C-                         specialized routines.
C-                         
C-     DAC_BYTE ( SIGN_ETA, MAGN_ETA, PHI, EM_TOWER/HD_TOWER )
C-                        is an array specifying all of the pedestal correction
C-                        bytes to be downloaded into the DAC of the analog
C-                        section of the CTFE cards for each Trigger Tower.
C-                        These bytes must be downloaded into the hardware
C-                        every time the calorimeter trigger is initialized in
C-                        order to guaranty that the ADC zero energy responses
C-                        match what the lookup table describes.
C-                        
C-     ELEC_NOISE_SIGMA ( SIGN_ETA, MAGN_ETA, PHI, EM_TOWER/HD_TOWER )
C-                        is an array specifying the expected electronics noise
C-                        in each Trigger Tower
C-                        
C-     GLOBAL_ENERGY_SCALE ( EM_ET_QUANT/EM_L2_QUANT
C-                          /HD_ET_QUANT/HD_L2_QUANT
C-                          /TOT_ET_QUANT/TOT_L2_QUANT
C-                          /PX_QUANT/PY_QUANT )
C-                        is an array specifying the energy scale globally used
C-                        for each of the quantities summed over the whole
C-                        detector.If the need arises in the future, the scale
C-                        might become locally modified on certain trigger
C-                        towers. 
C-                        
C-     GLOBAL_ADC_SCALE 
C-                        is a number specifying the energy scale globally used
C-                        for the output of the ADCs for each of the EM and HD
C-                        Trigger Tower channels. If the need arises in the
C-                        future, the scale might become locally modified on
C-                        certain trigger towers. 
C-                        
C-   Comments :
C-                         A call to INIT_LOOKUP_ROUTINES must have been made
C-                         before these variables can be accessed.
C-
C-   Defined  7-FEB-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  2-AUG-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated 20-AUG-1990 place non-minimal variables in LEVEL1_LOOKUP_EXPANSION
C----------------------------------------------------------------------
C
C       This array variable holds the upper and lower bounds of the segment
C       along the z-axis associated with each Level 0 Fast Vertex Position Bin.
C       (not referenced outside of the library)
C      
      REAL         L0_BIN_COVERAGE ( L0_BIN_MIN:L0_BIN_MAX, 
     &                               Z_LOW:Z_HIGH )
C
C       ----------------------------------------------------------------------
C       These variables specify what quantity is being constructed for each 
C       of the two successive lookups. The same quantity must be looked up for
C       both the EM and HD channels. The only two possible choices currently
C       available are (1) the transverse energy and (2) the deposited energy.
C
      INTEGER      FIRST_LOOKUP_TYPE, SECOND_LOOKUP_TYPE
C
C       ----------------------------------------------------------------------
C       These array variables link the Level 0 Bins to the Level 1 Lookup
C       memory page used to evaluate the z-corrected quantities.
C       
C       Two different levels of specifications are involved:
C       
C       The LUQ_PAGE_NUMBER array gives the Logical lookup page number
C       associated with each Level 0 Bin for each of the constructed
C       quantities. The logical page number is a signed integer which it is
C       required to pick positive for bins on the positive side of the
C       z-axis, and negative for bins on the negative side. The Page number for
C       the Level 0 Bin #0 containing the origin of the z-axis (i.e. the center
C       of the detector) is required to be 0. The set of level 0 bins
C       pointing to one logical page must be contiguous along the z-axis for
C       consistency. With a maximum of 8 pages of memory, the logical page
C       numbers must absolutely lie inside [-3,+3] and are required to be
C       chosen in a contiguous sequence symmetric around 0. 
C       
C       The LUQ_PAGE_INDEX array gives the Physical page index in the lookup
C       PROM that was allocated to implement each logical page number for each
C       constructed quantity. With 8 pages of memory, the physical page number
C       is bound to [1,8]. It is recommended to use contiguous physical page
C       numbers for contiguous logical page numbers, as it is then easier to
C       interpret the variations in slope,.. across physical pages, but this is
C       not an enforced rule.
C       
C       Warning: The hardware can only accommodate combinations where the
C       physical page indices of the EM_Et and HD_Et (respectively EM_L2 and
C       HD_L2, or Px and Py) are identical accross all Level 0 Bins since it
C       uses the same path to select the lookup page of the two quantities.
C       (not referenced outside of the library)
C      
      INTEGER      LUQ_PAGE_INDEX  ( EM_ET_QUANT:PY_QUANT, 
     &                               PAGE_NUM_MIN:PAGE_NUM_MAX )
      INTEGER      LUQ_PAGE_NUMBER ( EM_ET_QUANT:PY_QUANT, 
     &                               L0_BIN_MIN:L0_BIN_MAX )
C
C       ----------------------------------------------------------------------
C       These array of variables give the overall energy scales used in the
C       lookup system. Local departure from the global scales are allowed and
C       defined further down this set of variables.
C       
C       The global adc scale is the amount of energy corresponding to one least
C       count of digitized information at the output of the ADCs. This is the
C       scale relevant to the information of the data block. This is the scale
C       seen at the input of the lookup proms, that is before z-correction of
C       the quantities.
C       
C       The global energy scales are the amount of energy of one least count of
C       digitized information seen at the end of each energy summing tree.
C       A consistent choice will have the same scale for the EM_Et and HD_Et 
C       global sums (respectively EM_L2 and HD_L2, or Px and Py).
C       (not referenced outside of the library)
C      
      REAL         GLOBAL_ADC_SCALE 
      REAL         GLOBAL_ENERGY_SCALE ( EM_ET_QUANT:TOT_L2_QUANT )
C
C       ----------------------------------------------------------------------
C       This array variable defines the expected amount of electronics noise in
C       each trigger tower by its standard deviation.
C       The standard deviation for the EM and HD towers add in quadrature for
C       the Total trigger tower.
C       
      REAL         ELEC_NOISE_SIGMA ( POS_ETA:NEG_ETA, 
     &                                ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                                PHI_MIN:PHI_MAX,
     &                                EM_TOWER:HD_TOWER )
C
C       ----------------------------------------------------------------------
C       This array represents the error of the incoming signal due to (1)
C       amplitude error and (2) due to timing differences that cannot be
C       compensated on the ctfe card (i.e. between towers of a same card and
C       between EM and HD channels of a same tower)
C       (not referenced outside of the library)
C       
      REAL         INPUT_ENERGY_ERROR   ( POS_ETA:NEG_ETA, 
     &                                    ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                                    PHI_MIN:PHI_MAX,
     &                                    EM_TOWER:HD_TOWER )
C
C       ----------------------------------------------------------------------
C       This array variable defines the scaling factor applied to the incoming
C       Energy signal from the BLS cards. This factor is usually simply
C       sin(theta) to scale E down to Et, but could be different (1) to locally
C       alter the energy scale at the output of the ADC or (2) to compensate
C       for an error in the input energy scale. 
C       (not referenced outside of the library)
C      
      REAL         ANALOG_INPUT_SCALING ( POS_ETA:NEG_ETA, 
     &                                    ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                                    PHI_MIN:PHI_MAX,
     &                                    EM_TOWER:HD_TOWER )
C
C       ----------------------------------------------------------------------
C       This array variable represents the linear transformation factor between
C       the incoming (raw) energy and the output of the ADC. It takes into
C       account (1) the input energy error (2) the analog input scaling factor
C       and (3) the LSB scale of the ADC.
C       (also depends on the voltage references given to the ADC). 
C       
C       In order to calculate the ADC response to an incoming energy, the raw
C       input energy expressed in GeV must be scaled by this factor, the result
C       must then be rounded to the nearest integer and added to the ADC offset.
C       (not referenced outside of the library)
C      
      REAL         ADC_CNT_VS_RAW_E     ( POS_ETA:NEG_ETA,
     &                                    ETA_RANGE_MIN:ETA_RANGE_MAX, 
     &                                    PHI_MIN:PHI_MAX, 
     &                                    EM_TOWER:TOT_TOWER )
C      
C       ----------------------------------------------------------------------
C       The offsets observed at the output of the ADCs (and generated by the
C       circuitry of the CTFE card) can be compensated for by loading the
C       appropriate byte in the front-end correction DACs.  
C       
C       This DAC_BYTE will also allow to force an arbitrary offset at the
C       output of the ADCs.  
C       
C       The DAC_BYTE array are the variables to download into the DACs when
C       initializing the hardware and in order to produce the ADC_ZERESP array
C       of offsets at the outputs of the ADCs.
C       
C       Operational values of DAC_BYTE are in the range 0<n<=20. This variable
C       is also used as an implementation flag:  A Trigger Tower is understood
C       as implemented if and only if its DAC_BYTE is different from zero.
C       
      INTEGER      DAC_BYTE    ( POS_ETA:NEG_ETA, 
     &                           ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                           PHI_MIN:PHI_MAX,
     &                           EM_TOWER:HD_TOWER )
C
C       ----------------------------------------------------------------------
C       This array variable represents the offsets at the output of the ADCs,
C       that is the zero energy response of the ADC
C
      INTEGER      ADC_ZERESP ( POS_ETA:NEG_ETA, 
     &                          ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                          PHI_MIN:PHI_MAX,
     &                          EM_TOWER:HD_TOWER )
C
C       ----------------------------------------------------------------------
C       This array defines the local departure from the global energy scales
C       for each constructed z-corrected quantity at the ouput of each PROM.
C       A local rescaling of 0 means no rescaling, a local rescaling of +n
C       (respectively -n) means that the local scale is 2**+n (respectively
C       2**-n) times the global scale. 
C       (not referenced outside of the library)
C      
      INTEGER         LUQ_LOCAL_RESCALING ( POS_ETA:NEG_ETA, 
     &                                   ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                                   EM_ET_QUANT:PY_QUANT )
C
C       ----------------------------------------------------------------------
C       These coeeficients describe the transfer function of each page of each
C       lookup quantity.
C       
C       The LOOKUP_ZERESP array variable defines the zero energy response of
C       the transfer function for the ADC_ZERESP input. This number must
C       absolutely be constant accross all pages of each lookup.
C       
C       The PROM_CUT array variable holds the actual cut applied to each lookup 
C       page expressed in z-corrected units (output count). This variable is
C       computed for each page of each lookup and takes into account the
C       electronics noise cut and the physics cut. All z-corrected quantities
C       strictly below their cut are forced to zero.  
C       
C       The PROM_SLOPE array variable hold the slopes of the transfer functions
C       of each lookup page.
C       
C       These quantities are organized along physical page indices of each
C       lookup prom, and not along logical page number of lookup quantities.
C       This is an attempt to limit storage space requirements to the minimum
C       needed.
C       (not referenced outside of the library)
C      
      INTEGER      LOOKUP_ZERESP     ( POS_ETA:NEG_ETA, 
     &                                 ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                                 PHI_MIN:PHI_MAX,
     &                                 EM_ET_QUANT:PY_QUANT )
C
      INTEGER      PROM_CUT          ( POS_ETA:NEG_ETA, 
     &                                 ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                                 PHI_MIN:PHI_MAX,
     &                                 EM_PROM:PY_PROM,
     &                                 PAGE_INDEX_MIN:PAGE_INDEX_MAX )
C
      REAL         PROM_SLOPE        ( POS_ETA:NEG_ETA, 
     &                                 ETA_RANGE_MIN:ETA_RANGE_MAX,
     &                                 PHI_MIN:PHI_MAX,
     &                                 EM_PROM:PY_PROM,
     &                                 PAGE_INDEX_MIN:PAGE_INDEX_MAX )
C
C       ----------------------------------------------------------------------
C       This array variable holds for each of the constructed quantities 
C       the sum carried over the whole detector of all individual PROM output
C       offsets introduced at each Tower for that quantity.
C       
C       This numbers must be subtracted on the last summing CAT card of each
C       corresponding energy tree in order to produce the correct global energy
C       sum for that quantity. The data block will receive this corrected sum. 
C 
C       The array will hold a zero for any value for which the lookup is not
C       implemented. 
C       
C       The Total Et Sum will be constructed with the corrected EM Et and HD Et
C       and thus does not need to be corrected again.
C 
      INTEGER      TREE_OFFSET ( EM_ET_QUANT:TOT_L2_QUANT ) 
C 
C       ----------------------------------------------------------------------
C       
      COMMON / LEVEL1_LOOKUP / L0_BIN_COVERAGE,
     &                         FIRST_LOOKUP_TYPE, SECOND_LOOKUP_TYPE,
     &                         LUQ_PAGE_NUMBER, LUQ_PAGE_INDEX,
     &                         GLOBAL_ADC_SCALE, GLOBAL_ENERGY_SCALE,
     &                         INPUT_ENERGY_ERROR,                  
     &                         ANALOG_INPUT_SCALING,              
     &                         ADC_CNT_VS_RAW_E,                
     &                         ADC_ZERESP,
     &                         DAC_BYTE,
     &                         ELEC_NOISE_SIGMA,
     &                         LUQ_LOCAL_RESCALING,
     &                         LOOKUP_ZERESP, PROM_CUT, PROM_SLOPE,
     &                         TREE_OFFSET
C
