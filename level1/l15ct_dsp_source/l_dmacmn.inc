******************************************************************************
*                                                                            *
*   File  L_DMACmn.Inc                                  Rev.    8-MAR-1994   *
*                                                                            *
* This is an include file which defines the "common across all nodes" parts  *
* of the Local DSP DMA Lists.                                                *
*                                                                            *
* It is stored in an include file to allow easy changes to be made.          *
*                                                                            *
* It cannot be a separate file because it uses constants which must be       *
* defined in the Node-Specific Initialization module.                        *
*                                                                            *
* This file is included in each Node-Specific Initialization module.         *
*                                                                            *
******************************************************************************

        .data

Rack_1_EM_DMA_List_Loc:                                 ; The following 7
                                                        ; memory locations
                                                        ; define the DMA List
                                                        ; for the Rack 1 EM
                                                        ; Et Trigger Tower
                                                        ; data

Rack_1_EM_DMA_Control_Loc:                              ; Memory location
                                                        ; which holds the
        .word     DMA_Start_Word_for_ERPB_Input         ; Control Register

Rack_1_EM_DMA_Source_Loc:                               ; Memory location
                                                        ; which holds the
        .word     Rack_1_EM_ComPort_Input               ; Source Address
                                                        ; Register

Rack_1_EM_DMA_Source_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word     0                                     ; Source Address Index
                                                        ; Register

Rack_1_EM_DMA_Counter_Loc:                              ; Memory location
                                                        ; which holds the
        .word     One_TT_Data_Block_Size                ; Transfer Counter
                                                        ; Register

Rack_1_EM_DMA_Destination_Loc:                          ; Memory location
                                                        ; which holds the
        .word     Rack_1_EM_Et_TT_Data_Loc              ; Destination Address
                                                        ; Register

Rack_1_EM_DMA_Dest_Index_Loc:                           ; Memory location
                                                        ; which holds the
        .word     1                                     ; Destination Address
                                                        ; Index Register

Rack_1_EM_DMA_Link_Pointer_Loc:                         ; Memory location
                                                        ; which holds the
        .word    Rack_1_EM_DMA_List_Loc                 ; Link Pointer
                                                        ; Register

Rack_1_Tot_DMA_List_Loc:                                ; The following 7
                                                        ; memory locations
                                                        ; define the DMA List
                                                        ; for the Rack 1 Total
                                                        ; Et Trigger Tower
                                                        ; data
Rack_1_Tot_DMA_Control_Loc:                             ; Memory location
                                                        ; which holds the
        .word    DMA_Start_Word_for_ERPB_Input          ; Control Register

Rack_1_Tot_DMA_Source_Loc:                              ; Memory location
                                                        ; which holds the
        .word    Rack_1_Tot_ComPort_Input               ; Source Address
                                                        ; Register

Rack_1_Tot_DMA_Source_Index_Loc:                        ; Memory location
                                                        ; which holds the
        .word    0                                      ; Source Address Index
                                                        ; Register

Rack_1_Tot_DMA_Counter_Loc:                             ; Memory location
                                                        ; which holds the
        .word    One_TT_Data_Block_Size                 ; Transfer Counter
                                                        ; Register

Rack_1_Tot_DMA_Destination_Loc:                         ; Memory location
                                                        ; which holds the
        .word    Rack_1_Tot_Et_TT_Data_Loc              ; Destination Address
                                                        ; Register

Rack_1_Tot_DMA_Dest_Index_Loc:                          ; Memory location
                                                        ; which holds the
        .word    1                                      ; Destination Address
                                                        ; Index Register

Rack_1_Tot_DMA_Link_Pointer_Loc:                        ; Memory location
                                                        ; which holds the
        .word    Rack_1_Tot_DMA_List_Loc                ; Link Pointer
                                                        ; Register

Rack_2_EM_DMA_List_Loc:                                 ; The following 7
                                                        ; memory locations
                                                        ; define the DMA List
                                                        ; for the Rack 2 EM
                                                        ; Et Trigger Tower
                                                        ; data

Rack_2_EM_DMA_Control_Loc:                              ; Memory location
                                                        ; which holds the
        .word    DMA_Start_Word_for_ERPB_Input          ; Control Register

Rack_2_EM_DMA_Source_Loc:                               ; Memory location
                                                        ; which holds the
        .word    Rack_2_EM_ComPort_Input                ; Source Address
                                                        ; Register

Rack_2_EM_DMA_Source_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Source Address Index
                                                        ; Register

Rack_2_EM_DMA_Counter_Loc:                              ; Memory location
                                                        ; which holds the
        .word    One_TT_Data_Block_Size                 ; Transfer Counter
                                                        ; Register

Rack_2_EM_DMA_Destination_Loc:                          ; Memory location
                                                        ; which holds the
        .word    Rack_2_EM_Et_TT_Data_Loc               ; Destination Address
                                                        ; Register

Rack_2_EM_DMA_Dest_Index_Loc:                           ; Memory location
                                                        ; which holds the
        .word    1                                      ; Destination Address
                                                        ; Index Register

Rack_2_EM_DMA_Link_Pointer_Loc:                         ; Memory location
                                                        ; which holds the
        .word    Rack_2_EM_DMA_List_Loc                 ; Link Pointer
                                                        ; Register

Rack_2_Tot_DMA_List_Loc:                                ; The following 7
                                                        ; memory locations
                                                        ; define the DMA List
                                                        ; for the Rack 2 Total
                                                        ; Et Trigger Tower
                                                        ; data

Rack_2_Tot_DMA_Control_Loc:                             ; Memory location
                                                        ; which holds the
        .word    DMA_Start_Word_for_ERPB_Input          ; Control Register

Rack_2_Tot_DMA_Source_Loc:                              ; Memory location
                                                        ; which holds the
        .word    Rack_2_Tot_ComPort_Input               ; Source Address
                                                        ; Register

Rack_2_Tot_DMA_Source_Index_Loc:                        ; Memory location
                                                        ; which holds the
        .word    0                                      ; Source Address Index
                                                        ; Register

Rack_2_Tot_DMA_Counter_Loc:                             ; Memory location
                                                        ; which holds the
        .word    One_TT_Data_Block_Size                 ; Transfer Counter
                                                        ; Register

Rack_2_Tot_DMA_Destination_Loc:                         ; Memory location
                                                        ; which holds the
        .word    Rack_2_Tot_Et_TT_Data_Loc              ; Destination Address
                                                        ; Register

Rack_2_Tot_DMA_Dest_Index_Loc:                          ; Memory location
                                                        ; which holds the
        .word    1                                      ; Destination Address
                                                        ; Index Register

Rack_2_Tot_DMA_Link_Pointer_Loc:                        ; Memory location
                                                        ; which holds the
        .word    Rack_2_Tot_DMA_List_Loc                ; Link Pointer
                                                        ; Register


LG_MFP_Next_DMA_List_Handle:                            ; Memory locatio
                                                        ; to store the
        .word   LG_MFP_Next_DMA_List_Loc                ; base address of
                                                        ; the DMA List for
                                                        ; for the Mark and
                                                        ; Force Pass section
                                                        ; of the Data Block

LG_MFP_Next_DMA_List_Loc:                               ; The following
                                                        ; memory locations
                                                        ; define the DMA List
                                                        ; Mark and Force Pass
                                                        ; section of the Data
                                                        ; Block.
                                                        ; The Mark and Force
                                                        ; Pass section is
                                                        ; composed of Trigger
                                                        ; Tower data, Ref Set
                                                        ; Data, and Tool-
                                                        ; Dependent data.
                                                        ; Recall that the
                                                        ; Tool-Dependent
                                                        ; part of the List
                                                        ; is defined in
                                                        ; the Node-Specific
                                                        ; data section



MFP_Et_Head_DMA_List_Loc:                               ; The following 7
                                                        ; memory locations
                                                        ; define the Header
                                                        ; for the TT data
                                                        ; subsection
                                                        ; of the DMA List for
                                                        ; the Mark and Force
                                                        ; Pass section of
                                                        ; the Data Block

MFP_Et_Head_DMA_Control_Loc:                            ; Memory location
                                                        ; which holds the
        .word    DMA_Cont_Word_for_MFP_Next             ; Control Register

MFP_Et_Head_DMA_Source_Loc:                             ; Memory location
                                                        ; which holds the
        .word    MFP_TT_Et_Header_Loc                   ; Source Address
                                                        ; Register

MFP_Et_Head_DMA_Source_Index_Loc:                       ; Memory location
                                                        ; which holds the
        .word    1                                      ; Source Address Index
                                                        ; Register

MFP_Et_Head_DMA_Counter_Loc:                            ; Memory location
                                                        ; which holds the
        .word    1                                      ; Transfer Counter
                                                        ; Register

MFP_Et_Head_DMA_Destination_Loc:                        ; Memory location
                                                        ; which holds the
        .word    LG_to_Next_ComPort_Output              ; Destination Address
                                                        ; Register

MFP_Et_Head_DMA_Dest_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Destination Address
                                                        ; Index Register

MFP_Et_Head_DMA_Link_Pointer_Loc:                       ; Memory location
                                                        ; which holds the
        .word    MFP_E1_Data_DMA_List_Loc               ; Link Pointer
                                                        ; Register


MFP_E1_Data_DMA_List_Loc:                               ; The following 7
                                                        ; memory locations
                                                        ; define the Rack 1 EM
                                                        ; Et    data subsection
                                                        ; of the DMA List for
                                                        ; the Mark and Force
                                                        ; Pass section of
                                                        ; the Data Block

MFP_E1_Data_DMA_Control_Loc:                            ; Memory location
                                                        ; which holds the
        .word    DMA_Cont_Word_for_MFP_Next             ; Control Register

MFP_E1_Data_DMA_Source_Loc:                             ; Memory location
                                                        ; which holds the
        .word    Rack_1_EM_Et_TT_Data_Loc               ; Source Address
                                                        ; Register

MFP_E1_Data_DMA_Source_Index_Loc:                       ; Memory location
                                                        ; which holds the
        .word    1                                      ; Source Address Index
                                                        ; Register

MFP_E1_Data_DMA_Counter_Loc:                            ; Memory location
                                                        ; which holds the
        .word    One_TT_Data_Block_Size                 ; Transfer Counter
                                                        ; Register

MFP_E1_Data_DMA_Destination_Loc:                        ; Memory location
                                                        ; which holds the
        .word    LG_to_Next_ComPort_Output              ; Destination Address
                                                        ; Register

MFP_E1_Data_DMA_Dest_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Destination Address
                                                        ; Index Register

MFP_E1_Data_DMA_Link_Pointer_Loc:                       ; Memory location
                                                        ; which holds the
        .word    MFP_T1_Data_DMA_List_Loc               ; Link Pointer
                                                        ; Register


MFP_T1_Data_DMA_List_Loc:                               ; The following 7
                                                        ; memory locations
                                                        ; define the Rack 1 Tot
                                                        ; Et    data subsection
                                                        ; of the DMA List for
                                                        ; the Mark and Force
                                                        ; Pass section of
                                                        ; the Data Block

MFP_T1_Data_DMA_Control_Loc:                            ; Memory location
                                                        ; which holds the
        .word    DMA_Cont_Word_for_MFP_Next             ; Control Register

MFP_T1_Data_DMA_Source_Loc:                             ; Memory location
                                                        ; which holds the
        .word    Rack_1_Tot_Et_TT_Data_Loc              ; Source Address
                                                        ; Register

MFP_T1_Data_DMA_Source_Index_Loc:                       ; Memory location
                                                        ; which holds the
        .word    1                                      ; Source Address Index
                                                        ; Register

MFP_T1_Data_DMA_Counter_Loc:                            ; Memory location
                                                        ; which holds the
        .word    One_TT_Data_Block_Size                 ; Transfer Counter
                                                        ; Register

MFP_T1_Data_DMA_Destination_Loc:                        ; Memory location
                                                        ; which holds the
        .word    LG_to_Next_ComPort_Output              ; Destination Address
                                                        ; Register

MFP_T1_Data_DMA_Dest_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Destination Address
                                                        ; Index Register

MFP_T1_Data_DMA_Link_Pointer_Loc:                       ; Memory location
                                                        ; which holds the
        .word    MFP_E2_Data_DMA_List_Loc               ; Link Pointer
                                                        ; Register


MFP_E2_Data_DMA_List_Loc:                               ; The following 7
                                                        ; memory locations
                                                        ; define the Rack 2 EM
                                                        ; Et    data subsection
                                                        ; of the DMA List for
                                                        ; the Mark and Force
                                                        ; Pass section of
                                                        ; the Data Block

MFP_E2_Data_DMA_Control_Loc:                            ; Memory location
                                                        ; which holds the
        .word    DMA_Cont_Word_for_MFP_Next             ; Control Register

MFP_E2_Data_DMA_Source_Loc:                             ; Memory location
                                                        ; which holds the
        .word    Rack_2_EM_Et_TT_Data_Loc               ; Source Address
                                                        ; Register

MFP_E2_Data_DMA_Source_Index_Loc:                       ; Memory location
                                                        ; which holds the
        .word    1                                      ; Source Address Index
                                                        ; Register

MFP_E2_Data_DMA_Counter_Loc:                            ; Memory location
                                                        ; which holds the
        .word    One_TT_Data_Block_Size                 ; Transfer Counter
                                                        ; Register

MFP_E2_Data_DMA_Destination_Loc:                        ; Memory location
                                                        ; which holds the
        .word    LG_to_Next_ComPort_Output              ; Destination Address
                                                        ; Register

MFP_E2_Data_DMA_Dest_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Destination Address
                                                        ; Index Register

MFP_E2_Data_DMA_Link_Pointer_Loc:                       ; Memory location
                                                        ; which holds the
        .word    MFP_T2_Data_DMA_List_Loc               ; Link Pointer
                                                        ; Register


MFP_T2_Data_DMA_List_Loc:                               ; The following 7
                                                        ; memory locations
                                                        ; define the Rack 2 Tot
                                                        ; Et    data subsection
                                                        ; of the DMA List for
                                                        ; the Mark and Force
                                                        ; Pass section of
                                                        ; the Data Block

MFP_T2_Data_DMA_Control_Loc:                            ; Memory location
                                                        ; which holds the
        .word    DMA_Cont_Word_for_MFP_Next             ; Control Register

MFP_T2_Data_DMA_Source_Loc:                             ; Memory location
                                                        ; which holds the
        .word    Rack_2_Tot_Et_TT_Data_Loc              ; Source Address
                                                        ; Register

MFP_T2_Data_DMA_Source_Index_Loc:                       ; Memory location
                                                        ; which holds the
        .word    1                                      ; Source Address Index
                                                        ; Register

MFP_T2_Data_DMA_Counter_Loc:                            ; Memory location
                                                        ; which holds the
        .word    One_TT_Data_Block_Size                 ; Transfer Counter
                                                        ; Register

MFP_T2_Data_DMA_Destination_Loc:                        ; Memory location
                                                        ; which holds the
        .word    LG_to_Next_ComPort_Output              ; Destination Address
                                                        ; Register

MFP_T2_Data_DMA_Dest_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Destination Address
                                                        ; Index Register

MFP_T2_Data_DMA_Link_Pointer_Loc:                       ; Memory location
                                                        ; which holds the
        .word    MFP_Ref_Hdr_DMA_List_Loc               ; Link Pointer
                                                        ; Register


MFP_Ref_Hdr_DMA_List_Loc:                               ; The following 7
                                                        ; memory locations
                                                        ; define header for
                                                        ; the Ref Set
                                                        ; data subsection
                                                        ; of the DMA List for
                                                        ; the Mark and Force
                                                        ; Pass section of
                                                        ; the Data Block

MFP_Ref_Hdr_DMA_Control_Loc:                            ; Memory location
                                                        ; which holds the
        .word    DMA_Cont_Word_for_MFP_Next             ; Control Register

MFP_Ref_Hdr_DMA_Source_Loc:                             ; Memory location
                                                        ; which holds the
        .word    MFP_Ref_Set_Header_Loc                 ; Source Address
                                                        ; Register

MFP_Ref_Hdr_DMA_Source_Index_Loc:                       ; Memory location
                                                        ; which holds the
        .word    1                                      ; Source Address Index
                                                        ; Register

MFP_Ref_Hdr_DMA_Counter_Loc:                            ; Memory location
                                                        ; which holds the
        .word    2                                      ; Transfer Counter
                                                        ; Register

MFP_Ref_Hdr_DMA_Destination_Loc:                        ; Memory location
                                                        ; which holds the
        .word    LG_to_Next_ComPort_Output              ; Destination Address
                                                        ; Register

MFP_Ref_Hdr_DMA_Dest_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Destination Address
                                                        ; Index Register

MFP_Ref_Hdr_DMA_Link_Pointer_Loc:                       ; Memory location
                                                        ; which holds the
        .word    MFP_Ref_Set_DMA_List_Loc               ; Link Pointer
                                                        ; Register


MFP_Ref_Set_DMA_List_Loc:                               ; The following 7
                                                        ; memory locations
                                                        ; define the Ref Set
                                                        ; data subsection
                                                        ; of the DMA List for
                                                        ; the Mark and Force
                                                        ; Pass section of
                                                        ; the Data Block

MFP_Ref_Set_DMA_Control_Loc:                            ; Memory location
                                                        ; which holds the
        .word    DMA_Cont_Word_for_MFP_Next             ; Control Register

MFP_Ref_Set_DMA_Source_Loc:                             ; Memory location
                                                        ; which holds the
        .word    Ref_Set_Data_Loc                       ; Source Address
                                                        ; Register

MFP_Ref_Set_DMA_Source_Index_Loc:                       ; Memory location
                                                        ; which holds the
        .word    1                                      ; Source Address Index
                                                        ; Register

MFP_Ref_Set_DMA_Counter_Loc:                            ; Memory location
                                                        ; which holds the
        .word    4*One_Ref_Set_Block_Size               ; Transfer Counter
                                                        ; Register

MFP_Ref_Set_DMA_Destination_Loc:                        ; Memory location
                                                        ; which holds the
        .word    LG_to_Next_ComPort_Output              ; Destination Address
                                                        ; Register

MFP_Ref_Set_DMA_Dest_Index_Loc:                         ; Memory location
                                                        ; which holds the
        .word    0                                      ; Destination Address
                                                        ; Index Register

MFP_Ref_Set_DMA_Link_Pointer_Loc:                       ; Memory location
                                                        ; which holds the
        .word    MFP_DC_DMA_List_Loc                    ; Link Pointer
                                                        ; Register

