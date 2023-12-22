>>> Inputs <<< !
    ! ng                                                    nested grid number
    ! species                                               seagrass species id
    ! Nspecies                                              total number of species
    ! i, j                                                  i,j: position
    ! dz                          [m]                         vertical layer thickness
    ! dt                          [s]                         Time step interval duration
    ! p_sgrass(Nspecies)          [] (0.0~1.0)                Seagrass coverage (every species)
    ! PFD                         [umol.photon m-2 s-1]       Photon flux density
    ! rho_sw                      [g cm-3]                    Density of seawater 
    ! ocean_DIC_conc_in           [umol.DIC kg-1]             DIC concentration of ocean water (input only, do not use in calculations)
    ! ocean_DO__conc_in           [umol.DO  L-1]              DO  concentration of ocean water (input only, do not use in calculations)
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    ! ocean_NO3_conc_in           [umol.NO3 L-1]              NO3 concentration of ocean water (input only, do not use in calculations)
    ! ocean_NH4_conc_in           [umol.NH4 L-1]              NH4 concentration of ocean water (input only, do not use in calculations)
    ! ocean_PO4_conc_in           [umol.PO4 L-1]              PO4 concentration of ocean water (input only, do not use in calculations)
# endif

>>> Error-Corrected Inputs <<< !      rewrite input variables to new variables for edgecase handling and safety
    ! ocean_DIC_conc              [umol.DIC L-1]              DIC concentration of ocean water
    ! ocean_DO__conc              [umol.DO  L-1]              DO  concentration of ocean water
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    ! ocean_NO3_conc              [umol.NO3 L-1]              NO3 concentration of ocean water
    ! ocean_NH4_conc              [umol.NH4 L-1]              NH4 concentration of ocean water
    ! ocean_PO4_conc              [umol.PO4 L-1]              PO4 concentration of ocean water
# endif

>>> Outputs <<< !
    ! DIC_uptake_rate_from_ocean  [mmol.DIC m-2.sg.hab s-1]   DIC uptake rate  * direction of ocean to seagrass is positive
    ! DO__uptake_rate_from_ocean  [mmol.DO  m-2.sg.hab s-1]   DO  uptake rate  * direction of ocean to seagrass is positive
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    ! NO3_uptake_rate_from_ocean  [mmol.NO3 m-2.sg.hab s-1]   NO3 uptake rate  * direction of ocean to seagrass is positive
    ! NH4_uptake_rate_from_ocean  [mmol.NH4 m-2.sg.hab s-1]   NH4 uptake rate  * direction of ocean to seagrass is positive
    ! PO4_uptake_rate_from_ocean  [mmol.PO4 m-2.sg.hab s-1]   PO4 uptake rate  * direction of ocean to seagrass is positive
# endif

>>> Housekeeping Vars <<< !
    ! day                                                     Simulation whole day (integer round down)
    ! is_growth_interval                                      Boolean flag for if the current interval is a growth interval

>>> Saved Constants <<< !
    model params
    ! GridArea                    [m2.ground]                 Area of grid cell
    biophysical params
   *! C_N_P_ratios(chem,sp)                                   C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
    ! SgNperC                     [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    ! SgPperC                     [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    ! f_above                     [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
    ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    ! k_proj                      [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! root_mass_profile(sp,sed)   []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif
   photoynthesis: light-limit
    ! P_max                       [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    ! I_k                         [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    photoynthesis: DIC-limit
    ! Lf_DIC_uptake_max           [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    ! k_lf_DIC                    [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
    ! Rt_DIC_flux_max             [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    ! k_rt_DIC                    [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
    photoynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! Lf_PO4_uptake_max           [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    ! k_lf_PO4                    [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! Rt_PO4_flux_max             [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    ! k_rt_PO4                    [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    respiration: DO-limit
    ! Lf_DO_uptake_max            [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    ! k_lf_DO                     [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
    respiration: required
    ! R_basal                     [umol.C m-2.leaf s-1]       Maximum respiration rate
    dieoff when respiration is insufficient
    ! max_dieoff_rate             [mol.C mol-1.C day-1]       maximum dieoff in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    Update at end of growth interval
    allocate growth
    update bed-density/habitat area
    ! trans_rate                  [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    ! opt_sg_bed_density          [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

>>> Values Read From Sediment Module <<< !
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! Nsed                                                    Number of sediment layers
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
# endif
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! sed_DIC(Nsed)               [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    ! sed_DO (Nsed)               [umol.DO L-1]               DO  consentration of sediment (in each sediment layer)
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! sed_NO3(Nsed)               [umol.NO3 L-1]              NO3 consentration of sediment (in each sediment layer)
    ! sed_NH4(Nsed)               [umol.NH4 L-1]              NH4 consentration of sediment (in each sediment layer)
    ! sed_PO4(Nsed)               [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
# endif

>>> Saved Values <<< !
    Update at end of main interval
    ! SgCBm                       [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    Update at end of growth interval
    update total carbon-biomass
    ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    allocate growth
    ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
    ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    update bed-density/habitat area
    ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    leaf-root biomass translocation
    update carbon-biomass per area
    ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    update dry-weight biomass
    ! TotSgDryBm                  [g.drywt]                   Total seagrass dry weight biomass in grid cell
    ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    ! TotRtDryBm                  [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    ! SgDryBm                     [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    ! LfDryBm                     [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    update leaf area index
    ! LAI                         [m2.lf m-2.sg.hab]          Leaf area index: one-sided green leaf area per unit ground surface area 

>>> Delta Values <<< !      amount to add to stored variables at end of the calculation time step to update values: positive adds, negative subtracts (erased each time step)
    ! delta_SgCBm                 [umol.C m-2.sg.hab]         Change in SgCBm this time step
    values to write to sediment module
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! delta_sed_DIC(Nsed)         [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    ! delta_sed_DO(Nsed)          [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! delta_sed_NO3(Nsed)         [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    ! delta_sed_NH4(Nsed)         [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    ! delta_sed_PO4(Nsed)         [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
# endif


>>> Intermediate Calculation Variables <<< !      (erased each time step)
    looping index vars
    ! sedlayer                                                Sediment layer index (suface layer = 1, bottom layer = Nsed)
    Photosynthesis/Growth
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    photoynthesis: light-limit
    ! Growth_limit_light          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    ! ELAP                        [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    photoynthesis: DIC-limit
    ! Growth_limit_DIC            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    photoynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE || YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! Growth_limit_PO4            [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by PO4 uptake
    ! NO3_uptake_possible(:)      [umol.NO3 m-2.sg.hab s-1]   NO3 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! NH4_uptake_possible(:)      [umol.NH4 m-2.sg.hab s-1]   NH4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! PO4_uptake_possible(:)      [umol.PO4 m-2.sg.hab s-1]   PO4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# endif
    Respiration/Un-growth
    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    respiration: DO-limit
    ! Resp_limit_DO               [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake
    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    respiration: possible
    ! Resp_pos                    [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    respiration: required
    ! Resp_req                    [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
    respiration: dieoff
    ! dieoff                      [umol.C m-2.sg.hab]         Seagrass dieoff this time step
    Update at end of growth interval
    update total carbon-biomass
    ! oldTotSgCBm                 [mol.C.tot]                 Old total seagrass carbon-biomass in grid cell
    ! oldTotLfCBm                 [mol.C.lf]                  Old total seagrass leaf carbon-biomass in grid cell
    ! oldTotRtCBm                 [mol.C.rt]                  Old total seagrass root carbon-biomass in grid cell
    allocate growth
    ! mass_to_allocate            [mol.C]                     mass to allocate in growth or death to either leaves or roots
    update bed-density/habitat area
    ! desiredHabArea              [m2.hab]                    The desired habitat area given the biomass and optimal density
    ! translocatableArea          [m2.hab]                    Area that can be translocated (shrink or expand) as limited by the translocation rate
    leaf-root biomass translocation
    ! translocatable_mass         [mol.C]                     Biomass that can be translocated between leaf and root as limited by the translocation rate





