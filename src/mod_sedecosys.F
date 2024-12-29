!!!=== Copyright (c) 2013-2025 Takashi NAKAMURA  =====!!!
!!!         - Tokyo Institute of Technology -         !!!
!!!                                                   !!!
!!!          Sediment Biogeochemical Module           !!!
!!!            Developed by Masa Muto 2023-           !!!
!!!===================================================!!!

! ================================================================================================================ !
!  STRUCTURE OF SEDIMENT COLUMN
! ================================================================================================================ !
!   zw(Nsed):    Sediment depths, at horizontal RHO-points and vertical W-points (cm)
!   zr(Nsed):    Sediment depths, at horizontal RHO-points and vertical RHO-points (cm) 
!   dzw(Nsed):   Vertical W-grid spacing (cm)
!   dzr(Nsed):   Vertical RHO-grid spacing (cm)
!   C(Nsed):     Consentration in porewater or dry weight, SEDECO(ng)%C(Nsed,:,:)  (µmol L-1),(nmol g-1)
!   poro(Nsed):  Porosity = Vwater/(Vwater+Vsed) (non-dimensional: 0.0-1.0)
!   Fw(Nsed):    Flux, dwonward is positive (nmol cm-2 s-1)
!-----------------------------------------------------------------------                       
!                                                                                                        
!     rho point                                       C(0) = Camb (ambient value)                          
!       (i,j)                                       -------                                      ------------------
!     ____|_______ __0 _______0cm (surface) _________________________________ ↓ Fw(1)  ____ 0cm ___   } dzw(1)=DBL    
!   /     |      /|           |                                                             |                       
!  /___________ / |--1        |         } dzr(1)      C(1)      poro(1)                   --zr(1)------------------
!  |           |  |__1 _______zw(1)__________________________________________ ↓ Fw(2) ______|         } dzw(2)      
!  |           | /|           |                                                             |             
!  |___________|/ |--2        |         } dzr(2)      C(2)      poro(1)                   --zr(2)------------------
!  |           |  |__2 _______zw(2)__________________________________________ ↓ Fw(3) ______|                  
!  |           | /|           |                                                             :                  
!  |___________|/ :                                                                                            
!  |           |  :           :                                                                 -------------------
!  :           :  |__Nsed-2_ _zw(Nsed-2)_____________________________________ ↓ Fw(Nsed-1)__:        } dzw(Nsed-1) 
!  :           : /|           |                                                             |           
!  |___________|/ |--Nsed-1   |         } dzr(Nsed-1) C(Nsed-1) poro(Nsed-1)              --zr(Nsed-1)-------------
!  |           |  |__Nsed-1___zw(Nsed-1)_____________________________________ ↓ Fw(Nsed) ___|        } dzw(Nsed)   
!  |           | /|           |                                                             |         
!  |___________|/ |--Nsed     |         } dzr(Nsed)   C(Nsed)   poro(Nsed)                --zr(Nsed)---------------
!  |           |  |__Nsed   __zw(Nsed) ______________ = bottom _____________________________|                  
!  |           | /                                      boundary                                                                 
!  |___________|/                                       value                                                 
!                                                                                           
! ================================================================================================================ !
 
#include "cppdefs.h"

!!!**** MODULE OF SEDIMENT ECOSYSTEM MODEL ************************************
!!! **********************************************************************
!!!  Sediment model
!!! **********************************************************************

MODULE mod_sedecosys
#if defined SEDIMENT_ECOSYS

  implicit none

! Variables Declaration
  integer, parameter :: Nsed=21
!  integer, allocatable :: Nsed(:)
!  real(8), allocatable :: SedEcoLayerDepths(:, :)   ! (ng, k)          ! depth (cm) from surface at bottom of each biological sediment layer

!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

  TYPE T_SEDECO
! general parameters
    real(8), pointer :: poro(:,:,:)   !! Porosity (cm_w3 cm_s-3)
    ! real(8), pointer :: rhos_d(:,:,:) !! dry soil density (g　cm_s-3)
    ! real(8), pointer :: rhos_w(:,:,:) !! wet soil density (g cm_w-3)
    real(8), pointer :: dzr(:)         !! layer thickness (cm)
    real(8), pointer :: dzw(:)         !! layer thickness (cm)
    real(8), pointer :: zr(:)          !! sediment depth  (cm)
    real(8), pointer :: zw(:)          !! sediment depth  (cm)

    real(8), pointer :: Tmp(:,:,:)    !! Temperature    (oC)
    real(8), pointer :: Sal(:,:,:)    !! Salinity       (PSU)
    real(8), pointer :: TA (:,:,:)    !! Total Alkanity (umol kg-1)
    real(8), pointer :: O2 (:,:,:)    !! Dissolved Oxygen    (µmol l-1)
    real(8), pointer :: DIC(:,:,:,:)    !! Carbon dyoxide      (µmol l-1)
!    real(8), pointer :: N2 (:,:,:)    !! Nitrogen            (µmol l-1)

! Organic matters
# if defined ORGANIC_MATTER
    real(8), pointer :: DOC(:,:,:,:,:)    !! Dissolved organic carbon   (µmol l-1)
    real(8), pointer :: POC(:,:,:,:,:)    !! Particulate organic carbon (nmol g-1)
# endif

!  Nutrients dynamics
# if defined NUTRIENTS
    real(8), pointer :: NO3(:,:,:,:)     !! Nitrate     (µmol l-1)
    real(8), pointer :: NH4(:,:,:,:)     !! Ammonium    (µmol l-1)
    real(8), pointer :: PO4(:,:,:,:)     !! Phosphate   (µmol l-1)
#  if defined ORGANIC_MATTER
    real(8), pointer :: DON(:,:,:,:,:)     !! Dissolved organic nitrogen  (µmol l-1)
    real(8), pointer :: PON(:,:,:,:,:)     !! Particulate organic nitrogen (nmol g-1)
    real(8), pointer :: DOP(:,:,:,:,:)     !! Dissolved organic phosphorus  (µmol l-1)
    real(8), pointer :: POP(:,:,:,:,:)     !! Particulate organic phosphorus (nmol g-1)
#  endif
# endif

! Sulfur dynamics
# if defined SULFATE
    real(8), pointer :: Mn2(:,:,:)    !! Manganese ion        (µmol l-1)
    real(8), pointer :: MnO2(:,:,:)   !! Manganese dioxide    (nmol g-1)

    real(8), pointer :: Fe2 (:,:,:)       !! iron(II)          (µmol l-1)
    real(8), pointer :: FeS (:,:,:,:)       !! iron sulfide      (nmol g-1)
    real(8), pointer :: FeS2(:,:,:,:)      !! pyrite            (nmol g-1)
    real(8), pointer :: FeOOH(:,:,:)     !! iron hydroxide    (nmol g-1)
    real(8), pointer :: FeOOH_PO4(:,:,:,:) !! iron-bound phosphate (FeOOH=PO43-)   (nmol g-1)

    real(8), pointer :: H2S(:,:,:,:)       !! hydrogen sulfide         (µmol l-1)
    real(8), pointer :: SO4(:,:,:,:)      !! sulfate   (µmol l-1)
    real(8), pointer :: S0 (:,:,:,:)       !! sulfur    (nmol g-1)
# endif

!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: sg_dDIC_dt(:,:,:,:) ! (umol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dO2_dt (:,:,:,:) ! (umol L-1 s-1) change in DO due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: sg_dNO3_dt(:,:,:,:) ! (umol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dNH4_dt(:,:,:,:) ! (umol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
    real(8), pointer :: sg_dPO4_dt(:,:,:,:) ! (umol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_POM
    real(8), pointer :: sg_dPOC_dt(:,:,:,:) ! (umol L-1 s-1) change in POC due to root death in sediment (positive means seagrass to sediment)
    real(8), pointer :: sg_dPON_dt(:,:,:,:) ! (umol L-1 s-1) change in PON due to root death in sediment (positive means seagrass to sediment)
    real(8), pointer :: sg_dPOP_dt(:,:,:,:) ! (umol L-1 s-1) change in POP due to root death in sediment (positive means seagrass to sediment)
#  endif
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

  END TYPE T_SEDECO
  TYPE (T_SEDECO), allocatable :: SEDECO(:)

CONTAINS


!!! **********************************************************************
!!!  Initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE allocate_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj  &
# if defined SEAGRASS && (defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || \
     defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM)
                                , Nsg                           &
# endif
    )

    USE mod_geochem ! another module in a same folder (function list)
    USE mod_foodweb
    
    implicit none

    ! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
# if defined SEAGRASS && (defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || \
     defined SEAGRASS_ROOT_NUTRIENT_UPTAKE || defined SEAGRASS_ROOT_POM)
    integer, intent(in) :: Nsg
# endif

    ! allocate sizes of each parameters for (LBi:UBi, LBj:UBj)
    IF (ng.eq.1) allocate( SEDECO(Ngrids) )
    !! General
    allocate( SEDECO(ng)%poro(Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%dzr (Nsed) )
    allocate( SEDECO(ng)%dzw (Nsed) )
    allocate( SEDECO(ng)%zr  (Nsed) )
    allocate( SEDECO(ng)%zw  (Nsed) )
    allocate( SEDECO(ng)%Tmp (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%Sal (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%TA  (0:Nsed,LBi:UBi,LBj:UBj) )  
    allocate( SEDECO(ng)%O2  (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%DIC (0:N_Csp,Nsed,LBi:UBi,LBj:UBj) )
!    allocate( SEDECO(ng)%N2  (Nsed,LBi:UBi,LBj:UBj) )
    !! Organic Matter(Carbon)
# if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DOC(N_Csp,Ndom  ,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%POC(N_Csp,Npom-1,0:Nsed,LBi:UBi,LBj:UBj) )
# endif
    !! Nutrient dynamics
# if defined NUTRIENTS
    allocate( SEDECO(ng)%NO3(N_Nsp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%NH4(N_Nsp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%PO4(N_Psp,0:Nsed,LBi:UBi,LBj:UBj) )
#  if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DON(N_Nsp,Ndom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%PON(N_Nsp,Npom-1,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%DOP(N_Psp,Ndom,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%POP(N_Psp,Npom-1,0:Nsed,LBi:UBi,LBj:UBj) )
#  endif
# endif     
    !! Sulfur dynamics
# if defined SULFATE
    allocate( SEDECO(ng)%Mn2 (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%MnO2(0:Nsed,LBi:UBi,LBj:UBj) )

    allocate( SEDECO(ng)%Fe2 (0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeS (N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeS2(N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeOOH(0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%FeOOH_PO4(N_Psp,0:Nsed,LBi:UBi,LBj:UBj) )

    allocate( SEDECO(ng)%H2S(N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%SO4(N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
    allocate( SEDECO(ng)%S0 (N_Ssp,0:Nsed,LBi:UBi,LBj:UBj) )
# endif

!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SEDECO(ng)%sg_dDIC_dt(Nsg,N_Csp,Nsed,LBi:UBi,LBj:UBj) ) ! (µmol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dO2_dt (Nsg,Nsed,LBi:UBi,LBj:UBj) ) ! (nmol g-1 s-1) change in DO due to flux from seagrass (positive means seagrass to sediment)
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SEDECO(ng)%sg_dNO3_dt(Nsg,N_Nsp,Nsed,LBi:UBi,LBj:UBj) ) ! (µmol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dNH4_dt(Nsg,N_Nsp,Nsed,LBi:UBi,LBj:UBj) ) ! (µmol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dPO4_dt(Nsg,N_Psp,Nsed,LBi:UBi,LBj:UBj) ) ! (µmol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
#  endif    
#  if defined SEAGRASS_ROOT_POM
    allocate( SEDECO(ng)%sg_dPOC_dt(Nsg,N_Csp,Npom-1,Nsed,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in POC due to root death in sediment (positive means seagrass to sediment)
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SEDECO(ng)%sg_dPON_dt(Nsg,N_Nsp,Npom-1,Nsed,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in PON due to root death in sediment (positive means seagrass to sediment)
    allocate( SEDECO(ng)%sg_dPOP_dt(Nsg,N_Psp,Npom-1,Nsed,LBi:UBi,LBj:UBj) ) ! (umol L-1 s-1) change in POP due to root death in sediment (positive means seagrass to sediment)
#   endif  
#  endif  
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add

    RETURN
  END SUBROUTINE allocate_sedecosys


!!! **********************************************************************
!!!  Initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE initialize_sedecosys(ng, LBi, UBi, LBj, UBj, start_of_new_run)
    USE mod_geochem ! another module in a same folder (function list)
    USE mod_foodweb
    ! USE mod_readinput
    
    implicit none

    ! input parameters
    integer, intent(in) :: ng, LBi, UBi, LBj, UBj
    logical, intent(in) :: start_of_new_run               ! TRUE = initialize coral, seagass, macroalgae, sediment from start; FALSE = continue from previous run

    integer k,i,j ! symbols for loop calculations
    !! depth : Diffusive Boundary Layer (DBL)
    real(8), parameter :: dbl = 0.03d0 ! DBL:difussive boundary layer (cm)

    real(8), parameter :: dzmin = 0.05d0
    real(8), parameter :: dzmax = 2.0d0
    real(8), parameter :: k_lg = 0.3d0

    !! porosity   ( poro = a + b * exp(c * z)   (fossing et al 2004) )
    !! Kasih 2009
    ! real(8), parameter :: coefa_poro = 0.88d0
    ! real(8), parameter :: coefa_poro = 0.90d0
    ! real(8), parameter :: coefb_poro = 0.04d0
    ! real(8), parameter :: coefc_poro = -0.4d0
    !! Fossing 2004
    real(8), parameter :: coefa_poro = 0.763d0
    real(8), parameter :: coefb_poro = 0.086d0
    real(8), parameter :: coefc_poro = -0.216d0

    real(8), parameter :: rCPph = 106.0d0/1.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)
    real(8), parameter :: rCNph = 11.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)        from analysis

! - - - - - - - - - - - - - - - - - - - - - - - -
    !  Set initial conditions

    ! layer thickness and depth
    do k = 1,Nsed                                    ! dz is constantly 0.03 until z=1.2cm
      SEDECO(ng)%dzr(k) = dzmin*dzmax/   &
                          (dzmin+(dzmax-dzmin)*exp(-k_lg*dble(k-1)))
    enddo
    SEDECO(ng)%dzw(1)=SEDECO(ng)%dzr(1)*0.5d0
    SEDECO(ng)%zw(1)=SEDECO(ng)%dzw(1)
    SEDECO(ng)%zr(1)=SEDECO(ng)%dzr(1)
    do k = 2,Nsed                                    ! dz is constantly 0.03 until z=1.2cm
      SEDECO(ng)%dzw(k) = SEDECO(ng)%zr(k) - SEDECO(ng)%zr(k-1)
      SEDECO(ng)%zw(k)  = SEDECO(ng)%zw(k-1) + SEDECO(ng)%dzw(k)
      SEDECO(ng)%zr(k)  = SEDECO(ng)%zr(k-1) + SEDECO(ng)%dzr(k)
    enddo

!    do k = 1, Nsed(ng)
!      SEDECO(ng)%z(k) = SedEcoLayerDepths(ng, k)
!    enddo
!    SEDECO(ng)%dz(1) = SedEcoLayerDepths(ng, 1)
!    do k = 2, Nsed(ng)
!      SEDECO(ng)%dz(k) = SedEcoLayerDepths(ng, k) - SedEcoLayerDepths(ng, k-1)
!    enddo

    !!! porosity
    ! SEDECO(ng)%poro(:,:,1) = 1.0d0
    do k = 1,Nsed
      ! SEDECO(ng)%poro(k,:,:) = coefa_poro + coefb_poro * exp(coefc_poro * SEDECO(ng)%zr(k))
      !! --- Shizugawa --- !!
      ! SEDECO(ng)%poro(k,:,:) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%zr(k))
      !! --- Shiraho --- !!
      SEDECO(ng)%poro(k,:,:) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%zr(k))
      !! --- Fukido --- !!
      ! SEDECO(ng)%poro(k,:,:) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%zr(k))
    enddo

    if ( start_of_new_run ) then
    !!! ------------------- For determinating initial concentration ----------------------- !!!
      ! General
      SEDECO(ng)%Tmp(:,:,:)  = 20.0d0     
      SEDECO(ng)%Sal(:,:,:)  = 34.0d0
      SEDECO(ng)%TA(:,:,:)  = 2232.0d0 ! TA (umol kg-1)
      SEDECO(ng)%O2 (:,:,:)  = 0.0d0        !DO  (µmol L-1)
      SEDECO(ng)%DIC(iCt ,:,:,:)  = 2000.0d0        !DIC  (µmol L-1)
      ! OM
#  if defined ORGANIC_MATTER
      SEDECO(ng)%DOC(iCt,iLDOM,:,:,:)  = 0.0d0       !DOCf  (µmol L-1) 
      SEDECO(ng)%DOC(iCt,iRDOM,:,:,:)  = 2000.0d0       !DOCs  (µmol L-1) 
      SEDECO(ng)%POC(iCt,iLPOM,:,:,:)  = 0.0d0       !POCf  (nmol g-1) 
      SEDECO(ng)%POC(iCt,iRPOM,:,:,:)  = 1000.0d0      !POCs  (nmol g-1)
#  endif
        ! Nutrient dynamics
#  if defined NUTRIENTS
      SEDECO(ng)%NO3(iNt,:,:,:) = 5.0d0        !NO3  (µmol L-1) 
      SEDECO(ng)%NH4(iNt,:,:,:) = 50.0d0        !NH4  (µmol L-1) 
      SEDECO(ng)%PO4(iPt,:,:,:) = 10.0d0        !PO4  (µmol L-1) 
#  if defined ORGANIC_MATTER                 
      SEDECO(ng)%DON(iNt,iLDOM,:,:,:) = SEDECO(ng)%DOC(iCt,iLDOM,:,:,:) / rCNph   !DON  (µmol L-1) 
      SEDECO(ng)%DON(iNt,iRDOM,:,:,:) = SEDECO(ng)%DOC(iCt,iRDOM,:,:,:) / rCNph   !DON  (µmol L-1) 
      SEDECO(ng)%PON(iNt,iLPOM,:,:,:) = SEDECO(ng)%POC(iCt,iLPOM,:,:,:) / rCNph   !PON  (µmol L-1) 
      SEDECO(ng)%PON(iNt,iRPOM,:,:,:) = SEDECO(ng)%POC(iCt,iRPOM,:,:,:) / rCNph   !PON  (µmol L-1) 
      SEDECO(ng)%DOP(iPt,iLDOM,:,:,:) = SEDECO(ng)%DOC(iCt,iLDOM,:,:,:) / rCPph   !DOP  (µmol L-1) 
      SEDECO(ng)%DOP(iPt,iRDOM,:,:,:) = SEDECO(ng)%DOC(iCt,iRDOM,:,:,:) / rCPph   !DOP  (µmol L-1) 
      SEDECO(ng)%POP(iPt,iLPOM,:,:,:) = SEDECO(ng)%POC(iCt,iLPOM,:,:,:) / rCPph   !POP  (µmol L-1) 
      SEDECO(ng)%POP(iPt,iRPOM,:,:,:) = SEDECO(ng)%POC(iCt,iRPOM,:,:,:) / rCPph   !POP  (µmol L-1) 
#  endif
#  endif
        ! Sulfur dynamics
#  if defined SULFATE
      SEDECO(ng)%Mn2(:,:,:) = 60.0d0    !Mn2  (µmol L-1) 
      SEDECO(ng)%MnO2(:,:,1:10) = 1.0d4   !MnO2  (nmol g-1) 
      SEDECO(ng)%MnO2(:,:,11:Nsed(ng)) = 0.0d0   !MnO2  (nmol g-1) 

      SEDECO(ng)%Fe2 (:,:,:) = 0.0d0          !Fe2  (µmol L-1) 
      SEDECO(ng)%FeS (iSt,:,:,:) = 0.0d0          !FeS  (nmol g-1) 
      SEDECO(ng)%FeS2(iSt,:,:,:) = 1.0d4         !FeS2  (nmol g-1) 
      SEDECO(ng)%FeOOH(1:10,:,:) = 1.0d5        !FeOOH  (nmol g-1)
      SEDECO(ng)%FeOOH(11:Nsed,:,:) = 0.0d0        !FeOOH  (nmol g-1)
      SEDECO(ng)%FeOOH_PO4(iPt,:,:,:) = 0.0d0    !FeOOH=PO4  (nmol g-1) 

      SEDECO(ng)%H2S(iSt,:,:,:) = 100.0d0     !H2S  (µmol L-1)
      SEDECO(ng)%SO4(iSt,:,:,:) = 28.0d3     !SO4  (µmol L-1)
      SEDECO(ng)%S0 (iSt,:,:,:) = 0.0d0     !S0  (nmol g-1)
#  endif

!    else
!     !!! -------------- initial concentration from sed initial concentration ----------------- !!!
!      ! ! General
!      open(199, file='input/sedinitial_DO.txt', status='old')
!      read(199,*) SEDECO(ng)%O2(:,:,:)
!      close(199)

!      open(200, file='input/sedinitial_DOCf.txt', status='old')
!      read(200,*) SEDECO(ng)%DOCf(:,:,:)
!      close(200)

!      open(201, file='input/sedinitial_DOCs.txt', status='old')
!      read(201,*) SEDECO(ng)%DOCs(:,:,:)
!      close(201)

!      open(202, file='input/sedinitial_POCf.txt', status='old')
!      read(202,*) SEDECO(ng)%POCf(:,:,:)
!      close(202)

!      open(203, file='input/sedinitial_POCs.txt', status='old')
!      read(203,*) SEDECO(ng)%POCs(:,:,:)
!      close(203)

!      open(204, file='input/sedinitial_POCn.txt', status='old')
!      read(204,*) SEDECO(ng)%POCn(:,:,:)
!      close(204)

!      open(205, file='input/sedinitial_NO3.txt', status='old')
!      read(205,*) SEDECO(ng)%NO3(:,:,:)
!      close(205)

!      open(206, file='input/sedinitial_NH4.txt', status='old')
!      read(206,*) SEDECO(ng)%NH4(:,:,:)
!      close(206)

!      open(207, file='input/sedinitial_PO4.txt', status='old')
!      read(207,*) SEDECO(ng)%PO4(:,:,:)
!      close(207)
       
!      open(208, file='input/sedinitial_Mn2.txt', status='old')
!      read(208,*) SEDECO(ng)%Mn2(:,:,:)
!      close(208)

!      open(209, file='input/sedinitial_MnO2.txt', status='old')
!      read(209,*) SEDECO(ng)%MnO2(:,:,:)
!      close(209)

!      open(210, file='input/sedinitial_Fe2.txt', status='old')
!      read(210,*) SEDECO(ng)%Fe2(:,:,:)
!      close(210)

!      open(211, file='input/sedinitial_FeOOH.txt', status='old')
!      read(211,*) SEDECO(ng)%FeOOH(:,:,:)
!      close(211)

!      open(212, file='input/sedinitial_FeS.txt', status='old')
!      read(212,*) SEDECO(ng)%FeS(:,:,:)
!      close(212)

!      open(213, file='input/sedinitial_FeS2.txt', status='old')
!      read(213,*) SEDECO(ng)%FeS2(:,:,:)
!      close(213)

!      open(214, file='input/sedinitial_FeOOH_PO4.txt', status='old')
!      read(214,*) SEDECO(ng)%FeOOH_PO4(:,:,:)
!      close(214)

!      open(215, file='input/sedinitial_H2S.txt', status='old')
!      read(215,*) SEDECO(ng)%H2S(:,:,:)
!      close(215)

!      open(216, file='input/sedinitial_S0.txt', status='old')
!      read(216,*) SEDECO(ng)%S0(:,:,:)
!      close(216)

!      open(217, file='input/sedinitial_SO4.txt', status='old')
!      read(217,*) SEDECO(ng)%SO4(:,:,:)
!      close(217)

    endif

    !! for debug
    ! write(*,*) SEDECO(ng)%SO4(1,1,:)


    RETURN

  END SUBROUTINE initialize_sedecosys
 ! ****************************************************************** !     



!!! **********************************************************************
!!!  Main program of sediment ecosystem model
!!! **********************************************************************
! amb:ambient = bottom grid of water column = Just above seafloor
! amb will be set as value(1) in mod_reef_ecosys.F

  SUBROUTINE sedecosys           &
!   input parameters
    ( ng, i, j       &   ! ng: nested grid number; i,j: position
    , Nsed           &   ! Number of biological sediment layers ! yuta_edits_for_masa
    , date           &   ! Date (days since x or elapsed days)
    , dt             &   ! Time step (sec)
    , Tamb           &   ! Tmp: Temperature (oC)
    , Samb           &   ! Sal: Salinity (PSU)
    , TAamb          &   ! TA: Total Alkalinity (kg L-1)
    , O2amb          &   ! O2: Dissolved oxygen (umol L-1)
    , DICamb         &   ! DIC: carbon dioxide  (µmol L-1)
!    , N2amb          &   ! N2: Nitrogen         (µmol L-1)
# if defined ORGANIC_MATTER
    , DOCamb         &   ! DOC: Dissolved organic carbon (µmol L-1)
    , POCamb         &   ! POC: Particulate organic carbon (nmol g-1)
# endif
# if defined NUTRIENTS
    , NO3amb         &   ! NO3: NO3 (umol L-1)
    , NH4amb         &   ! NH4: NH4 (umol L-1)
    , PO4amb         &   ! PO4: PO4 (umol L-1)
#  if defined ORGANIC_MATTER
    , DONamb         &   ! DOC: Dissolved organic nitrogen (µmol L-1)
    , DOPamb         &   ! DOC: Dissolved organic phosporius (µmol L-1)
    , PONamb         &   ! PON: Particulate organic nitrogen (PON: umol L-1)
    , POPamb         &   ! POP: Particulate organic phosporius (POP: umol L-1)
#  endif
# endif
# if defined SEDIMENT
    , Fdep_sed       &  ! Sedimentation rate (cm s-1)
# endif
# if defined SULFATE
    , Mn2amb         &  ! Mn2 (µmol L-1)
    , Fe2amb         &  ! Fe2 (µmol L-1)
    , H2Samb         &  ! H2S (µmol L-1)
    , SO4amb         &  ! SO4 (µmol L-1)
    , Fdep_MnO2      &  ! MnO2 deposition flux (nmol cm-2 s-1)
    , Fdep_FeOOH     &  ! FeOOH depositoin flux (nmol cm-2 s-1)
# endif
# if defined ORGANIC_MATTER
    , Fdep_POC       &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined NUTRIENTS
    , Fdep_PON       &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_POP       &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  endif
# endif
# if defined SEDECO_ADVECTION
    , sgd_flux          &   ! sumbarine groundwater discharge rate (cm s-1)  This flux is assumed to be the same throughout the sediment ecosystem layers (no compression/expansion) So volume flux doesn't change but concentrations of tracers do
    , sgd_Tmp_in        &   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    , sgd_Sal_in        &   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    , sgd_DIC_in        &   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (umol L-1)
    , sgd_TA_in         &   ! SGD concentration coming in through bottom: TA : Total alkalinity (umol L-1)
    , sgd_DOx_in        &   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
#  if defined NUTRIENTS            
    , sgd_NO3_in        &   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    , sgd_NH4_in        &   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    , sgd_PO4_in        &   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
#  endif
# endif
!   output parameters
    , Flux_Tmp       &   ! Temperature of discharged water (oC)
    , Flux_Sal       &   ! Salinity flux (cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_TA        &   ! Total Alkalinity flux (???? cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DIC       &   ! DIC flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
# if defined ORGANIC_MATTER
    , Flux_POC       &   ! POC(f,s,n) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
# if defined NUTRIENTS
    , Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined ORGANIC_MATTER
    , Flux_PON       &   ! PON(f,s,n) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POP       &   ! POP(f,s,n) fluxes (nmol cm-2 s-1) * direction of water column to sediment is positive
#  endif
# endif
# if defined SULFATE
    , Flux_Mn2       &   ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_MnO2      &   ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive

    , Flux_Fe2       &   ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
    ! , Flux_FeS       &   ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
    ! , Flux_FeS2      &   ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
    , Flux_FeOOH     &   ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
    ! , Flux_FeOOH_PO4 &   ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero

    , Flux_H2S       &   ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
    ! , Flux_S0        &   ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive  ! yt_edit diffusion is not modeled so flux is zero
    , Flux_SO4       &   ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
    )

    USE mod_geochem
    USE mod_foodweb
    
    implicit none

! ***********************************************************************************
!!! Declarative statement
!! These variable names are different from those in mod_reef_ecosys.F

    !! General
    integer, intent(in) :: ng, i, j, Nsed
    real(8), intent(in) :: date
    real(8), intent(in) :: dt
    real(8), intent(in) :: Tamb     
    real(8), intent(in) :: Samb     
    real(8), intent(in) :: TAamb     
    real(8), intent(in) :: O2amb
    real(8), intent(in) :: DICamb(N_Csp)
    !! Organic matters (Carbon)
# if defined ORGANIC_MATTER
    real(8), intent(in) :: DOCamb(N_Csp,Ndom)
    real(8), intent(in) :: POCamb(N_Csp,Npom-1)
# endif
    !! Nutrient
# if defined NUTRIENTS
    real(8), intent(in) :: NO3amb(N_Nsp)   
    real(8), intent(in) :: NH4amb(N_Nsp)   
    real(8), intent(in) :: PO4amb(N_Psp)   
#  if defined ORGANIC_MATTER
    real(8), intent(in) :: DONamb(N_Nsp,Ndom)
    real(8), intent(in) :: DOPamb(N_Psp,Ndom)
    real(8), intent(in) :: PONamb(N_Nsp,Npom-1)
    real(8), intent(in) :: POPamb(N_Psp,Npom-1)
#  endif
# endif
    !! Sulfate
# if defined SULFATE
    real(8), intent(in) :: Mn2amb
    real(8), intent(in) :: Fe2amb
    real(8), intent(in) :: H2Samb(N_Ssp)
    real(8), intent(in) :: SO4amb(N_Ssp) 
    !! Deposition Flux (test) Kasih 2009 -------- 
    real(8), intent(in) :: Fdep_MnO2
    real(8), intent(in) :: Fdep_FeOOH
# endif

# if defined ORGANIC_MATTER
    real(8), intent(in) :: Fdep_POC(N_Csp,Npom-1)   
#  if defined NUTRIENTS
    real(8), intent(in) :: Fdep_PON(N_Nsp,Npom-1)   
    real(8), intent(in) :: Fdep_POP(N_Psp,Npom-1)   
#  endif
# endif

# if defined SEDECO_ADVECTION
    real(8), intent(in) :: sgd_flux     ! Submarine Groundwater Discharge (SDG) volume flux (cm s-1)
    real(8), intent(in) :: sgd_Tmp_in   ! SGD concentration coming in through bottom: Tmp: Temperature (oC)
    real(8), intent(in) :: sgd_Sal_in   ! SGD concentration coming in through bottom: Sal: Salinity (PSU)
    real(8), intent(in) :: sgd_DIC_in   ! SGD concentration coming in through bottom: DIC: Total dissolved inorganic carbon (umol L-1)
    real(8), intent(in) :: sgd_TA_in    ! SGD concentration coming in through bottom: TA : Total alkalinity (umol L-1)
    real(8), intent(in) :: sgd_DOx_in   ! SGD concentration coming in through bottom: DOx: Dissolved oxygen (umol L-1)
#  if defined NUTRIENTS            
    real(8), intent(in) :: sgd_NO3_in   ! SGD concentration coming in through bottom: NO3: NO3 (umol L-1)
    real(8), intent(in) :: sgd_NH4_in   ! SGD concentration coming in through bottom: NH4: NH4 (umol L-1)
    real(8), intent(in) :: sgd_PO4_in   ! SGD concentration coming in through bottom: PO4: PO4 (umol L-1)
#  endif
# endif

!! output parameters (Flux)
    real(8), intent(out) :: Flux_Tmp
    real(8), intent(out) :: Flux_Sal
    real(8), intent(out) :: Flux_TA

    real(8), intent(out) :: Flux_DO
    real(8), intent(out) :: Flux_DIC(N_Csp)
# if defined ORGANIC_MATTER
    real(8), intent(out) :: Flux_POC(N_Csp,Npom-1)
# endif
# if defined NUTRIENTS
    real(8), intent(out) :: Flux_NO3(N_Nsp)
    real(8), intent(out) :: Flux_NH4(N_Nsp)
    real(8), intent(out) :: Flux_PO4(N_Psp)
#  if defined ORGANIC_MATTER
    real(8), intent(out) :: Flux_PON(N_Nsp,Npom-1)
    real(8), intent(out) :: Flux_POP(N_Psp,Npom-1)
#  endif
# endif
# if defined SULFATE
    real(8), intent(out) :: Flux_Mn2
    real(8), intent(out) :: Flux_MnO2

    real(8), intent(out) :: Flux_Fe2
    ! real(8), intent(out) :: Flux_FeS (N_Ssp)
    ! real(8), intent(out) :: Flux_FeS2(N_Ssp)
    real(8), intent(out) :: Flux_FeOOH
    ! real(8), intent(out) :: Flux_FeOOH_PO4(N_Psp)

    real(8), intent(out) :: Flux_H2S(N_Ssp)
    ! real(8), intent(out) :: Flux_S0(N_Ssp)
    real(8), intent(out) :: Flux_SO4(N_Ssp)
# endif

!!! Set parameters 
    integer :: k, m, isp, isp2    ! symbol for loop cal

!! -- Physical properties of sediment ------------------------------
    real(8) :: rho_sw(Nsed)    ! seawater density (g cm-3)
    real(8) :: rhos_d(Nsed)    ! soil density (g cm_s-3)
    
!! -- Diffusion coefficient -----------------------------------------
    real(8) :: D_Tmp(Nsed) = 0.0d0
    real(8) :: D_Sal(Nsed) = 0.0d0
    real(8) :: D_O2 (Nsed) = 0.0d0
    real(8) :: D_TA (Nsed) = 0.0d0
    real(8) :: D_DIC(N_Csp,Nsed) = 0.0d0
# if defined ORGANIC_MATTER
    real(8) :: D_DOM(Ndom,Nsed) = 0.0d0    ! 1:labile, 2:refractory
!    real(8) :: D_DOC(N_Csp,Ndom,Nsed) = 0.0d0    ! 1:labile, 2:refractory
!    real(8) :: D_POC(N_Csp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory, 3:non-degratable
# endif
# if defined NUTRIENTS
    real(8) :: D_NO3(N_Nsp,Nsed) = 0.0d0
    real(8) :: D_NH4(N_Nsp,Nsed) = 0.0d0
    real(8) :: D_PO4(N_Psp,Nsed) = 0.0d0
#  if defined ORGANIC_MATTER
!    real(8) :: D_DON(N_Nsp,Ndom,Nsed) = 0.0d0    ! 1:labile, 2:refractory
!    real(8) :: D_PON(N_Nsp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory, 3:non-degratable
!    real(8) :: D_DOP(N_Psp,Ndom,Nsed) = 0.0d0    ! 1:labile, 2:refractory
!    real(8) :: D_POP(N_Psp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory, 3:non-degratable
#  endif
# endif
# if defined SULFATE
    real(8) :: D_Mn2(Nsed) = 0.0d0
    real(8) :: D_MnO2(Nsed) = 0.0d0
    real(8) :: D_Fe2 (N_Ssp,Nsed) = 0.0d0
    real(8) :: D_FeS (N_Ssp,Nsed) = 0.0d0
    real(8) :: D_FeS2(N_Ssp,Nsed) = 0.0d0
    real(8) :: D_FeOOH(Nsed) = 0.0d0
    real(8) :: D_FeOOH_PO4(N_Psp,Nsed) = 0.0d0
    real(8) :: D_H2S(N_Ssp,Nsed) = 0.0d0
    real(8) :: D_S0 (N_Ssp,Nsed) = 0.0d0
    real(8) :: D_SO4(N_Ssp,Nsed) = 0.0d0
# endif
    !! Biodifuusivity　(cm2 s-1)
    real(8) :: DBd(Nsed) = 0.0d0    !(cm2 s-1)
    real(8) :: DBp(Nsed) = 0.0d0    !(cm2 s-1)

    !! Adsorption constant K (cm3 g-1)
    real(8),parameter :: Kad_NO3 = 5.4d0   !(cm3 g-1)
    real(8),parameter :: Kad_NH4 = 2.2d0   !(cm3 g-1)
    real(8),parameter :: Kad_PO4 = 2.0d0   !(cm3 g-1)
    real(8),parameter :: Kad_Mn2 = 13.0d0  !(cm3 g-1)
    real(8),parameter :: Kad_Fe2 = 500.0d0 !(cm3 g-1)

    !! Sedimentation rate (cm/sec)
# if defined SEDECO_BURIAL
    ! real(8),parameter :: u = 0.064d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Fossing 2004
    ! real(8),parameter :: u = 0.38d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Irie 2018
    ! real(8),parameter :: u = 0.5d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Mochida 2023
# endif

    real(8) :: Fw_Tmp(Nsed) = 0.0d0
    real(8) :: Fw_Sal(Nsed) = 0.0d0
    real(8) :: Fw_O2 (Nsed) = 0.0d0
    real(8) :: Fw_TA (Nsed) = 0.0d0
    real(8) :: Fw_DIC(N_Csp,Nsed) = 0.0d0
# if defined ORGANIC_MATTER
    real(8) :: Fw_DOC(N_Csp,Ndom,Nsed) = 0.0d0    ! 1:labile, 2:refractory
    real(8) :: Fw_POC(N_Csp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory, 3:non-degratable
# endif
# if defined NUTRIENTS
    real(8) :: Fw_NO3(N_Nsp,Nsed) = 0.0d0
    real(8) :: Fw_NH4(N_Nsp,Nsed) = 0.0d0
    real(8) :: Fw_PO4(N_Psp,Nsed) = 0.0d0
#  if defined ORGANIC_MATTER
    real(8) :: Fw_DON(N_Nsp,Ndom,Nsed) = 0.0d0    ! 1:labile, 2:refractory
    real(8) :: Fw_PON(N_Nsp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory, 3:non-degratable
    real(8) :: Fw_DOP(N_Psp,Ndom,Nsed) = 0.0d0    ! 1:labile, 2:refractory
    real(8) :: Fw_POP(N_Psp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory, 3:non-degratable
#  endif
# endif
# if defined SULFATE
    real(8) :: Fw_Mn2(Nsed) = 0.0d0
    real(8) :: Fw_MnO2(Nsed) = 0.0d0
    real(8) :: Fw_Fe2 (N_Ssp,Nsed) = 0.0d0
    real(8) :: Fw_FeS (N_Ssp,Nsed) = 0.0d0
    real(8) :: Fw_FeS2(N_Ssp,Nsed) = 0.0d0
    real(8) :: Fw_FeOOH(Nsed) = 0.0d0
    real(8) :: Fw_FeOOH_PO4(N_Psp,Nsed) = 0.0d0
    real(8) :: Fw_H2S(N_Ssp,Nsed) = 0.0d0
    real(8) :: Fw_S0 (N_Ssp,Nsed) = 0.0d0
    real(8) :: Fw_SO4(N_Ssp,Nsed) = 0.0d0
# endif

    !! Advection velocity
    real(8) :: Wd(Nsed) = 0.0d0    ! Solutes advection velocity (cm s-1)
    real(8) :: Wp(Nsed) = 0.0d0    ! Solids advection velocity (cm s-1)

!! -- Reaction parameters -----------------------------------------
!!!! ============================================================== !!!
    !! ORGANIC MATTER DECOMPOSITION
        ! POMf --> DOMf + DOMs
        ! POMs --> DOMf + DOMs
    !! PRIMARY REACTIONS  (CH2O :: Organic Matters(DOMf,s))
        ! R1:   O2 + CH2O --> CO2 + H2O
        ! R2:   4/5 NO3- + CH2O + 4/5 H+ --> 4/5 N2 + CO2 + 7/5 H2O
        ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
        ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
        ! R5:   1/2 SO42- + CH2O + H+ --> 1/2 H2S + CO2 + H2O
    !! SECONDARY REACTIONS
        ! R6:   NH4+ + 2O2 --> NO3- + H2O + 2H+
        ! R7:   FeOOH + PO43- --> FeOOH=PO43-
        ! R8:   2Fe2+ + MnO2 + 2H2O --> 2FeOOH + Mn2+ + 2H+
        ! R9:   2Mn2+ + O2 + 2H2O --> 2MnO2 + 4H+
        ! R10a: H2S + 2FeOOH=PO43- + 4H+ --> S0 + 2Fe2+ + 4H2O + 2PO43-
        ! R10b: H2S + 2FeOOH + 4H+ --> S0 + 2Fe2+ + 4H2O
        ! R11:  4Fe2+ + O2 + 6H2O --> 4FeOOH + 8H+
        ! R12:  H2S + MnO2 + 4H+ --> S0 + Mn2+ + 2H2O
        ! R13:  H2S + Fe2+ --> FeS + 2H+
        ! R14:  FeS + S0 --> FeS2
        ! R15:  SO42- + 3H2S + 4FeS + 2H+ --> 4FeS2 + 4H2O
        ! R16:  H2S + 2O2 --> SO42- + 2H+
        ! R17:  FeS + 2O2 --> Fe2+ + SO42-
        ! R18:  2FeS2 + 7O2 + 2H2O --> 2Fe2+ + 4SO42- + 4H+
        ! R19:  4S0 + 4H2O --> 3H2S + SO42- + 2H+
!!!! ============================================================== !!!

    !! Reaction rate constants
    real(8),parameter :: k6  = 2.5d-6  ! For R6  (µM-1 s-1)           Fossing(2004)
    ! real(8),parameter :: k6  = 2.5d-7  ! For R6  (µM-1 s-1)           Berg(2003),Kasih2004
    real(8),parameter :: k7  = 5.0d-14 ! For R7  (s-1)                Kasih2009
    ! real(8),parameter :: k7  = 5.0d-11 ! For R7  (s-1)                Fossing2004
    ! real(8),parameter :: k8  = 7.5d-11 ! For R8  (µM-1 s-1)           Berg2003
    real(8),parameter :: k8  = 1.7d-9 ! For R8  (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k9  = 1.5d-5  ! For R9  (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k10 = 2.0d-7  ! For R10 (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k11 = 5.0d-4  ! For R11 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k12 = 3.0d-9  ! For R12 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k13 = 3.75d-5  ! For R13 (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k14 = 3.0d-10 ! For R14 (cm3 s-1 nmol-1)     Fossing(2004)
    real(8),parameter :: k15 = 7.5d-12 ! For R15 (s-1)                Kasih(2009)
    real(8),parameter :: k16 = 5.0d-5  ! For R16 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k17 = 6.0d-7  ! For R17 (µM-1 s-1)           Fossing(2004)
    ! real(8),parameter :: k18 = 1.6d-8  ! For R18 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k18 = 3.0d-10  ! For R18 (µM-1 s-1)           Berg2003, Kasih2009
    real(8),parameter :: k19 = 7.0d-7  ! For R19 (s-1)               Fossing(2004)
    !! non-degratable
    real(8),parameter :: k20 = 1.3d-9     !For R20 (MnO2_ded --> MnO2_undeg) (s-1)    Fossing2004
    real(8),parameter :: k21 = 9.0d-10    !For R21 (FeOOH_ded --> FeOOH_undeg) (s-1)  Fossing2004


    !! ----------- Decomposition rate constant for organic matter ------------- !!
    !! DOM
    ! real(8),parameter :: kdomf = 1.0d-3   ! For  domf (s-1)      Kasih (2009), Irie (2018)
    ! real(8),parameter :: kdomf = 1.0d-4   ! For  domf (s-1)           
    ! real(8),parameter :: kdomf = 2.0d-3   ! For  domf (s-1)
    ! real(8),parameter :: kdomf = 1.2d-1   ! For  domf (s-1)

    ! real(8),parameter :: kdoms = 5.0d-9   ! For  doms (s-1)      Kasih (2009)
    ! real(8),parameter :: kdoms = 5.0d-8   ! For  doms (s-1)      Irie (2018)
    ! real(8),parameter :: kdoms = 1.0d-8
    ! real(8),parameter :: kdoms = 2.5d-9   ! For  doms (s-1)      
    ! real(8),parameter :: kdoms = 1.0d-7   ! For  doms (s-1)   
    ! real(8),parameter :: kdoms = 1.4d-10   ! For  doms (s-1)
    ! real(8),parameter :: kdoms = 1.4d-12   ! For  doms (s-1)            

    !! POM
    ! real(8),parameter :: kpomf = 2.5d-6   ! For  pomf (s-1)      Kasih (2009), Irie (2018)
    ! real(8),parameter :: kpomf = 2.5d-7   ! For  pomf (s-1)
    ! real(8),parameter :: kpomf = 2.5d-8   ! For  pomf (s-1)          
    ! real(8),parameter :: kpomf = 2.0d-9   ! For  pomf (s-1)            
    ! real(8),parameter :: kpomf = 5.0d-10   ! For  pomf (s-1)            

    ! real(8),parameter :: kpoms = 1.2d-10   ! For  poms (s-1)     Kasih (2009)
    ! real(8),parameter :: kpoms = 6.0d-10   ! For  poms (s-1)     Irie (2018)
    ! real(8),parameter :: kpoms = 6.0d-11   ! For  poms (s-1)      
    ! real(8),parameter :: kpoms = 1.2d-9   ! For  poms (s-1)     
    ! real(8),parameter :: kpoms = 1.2d-11   ! For  poms (s-1) 
    ! real(8),parameter :: kpoms = 7.0d-12   ! For  poms (s-1)

!                                    LDOM    RDOM
    real(8),parameter :: kdom(2) = [ 1.0d-4, 1.4d-10 ]  ! For  dom (s-1)           
!                                    LPOM    RPOM
    real(8),parameter :: kpom(2) = [ 2.5d-7, 1.2d-11 ]  ! For  pom (s-1)


    !! ------------------------------------------------------------------------- !!


    !! Temperature coefficient Q10
    real(8),parameter :: Q10_pri = 3.8d0   !  For primary reactions    Fossing (2004)
    real(8),parameter :: Q10_sec = 2.0d0   !  For secondary reactions  Fossing (2004)

    !! Reaction rate constants considering temperature dependency
    real(8) :: k6_Q10,  k7_Q10,  k8_Q10,  k9_Q10,  k10_Q10
    real(8) :: k11_Q10, k12_Q10, k13_Q10, k14_Q10, k15_Q10
    real(8) :: k16_Q10, k17_Q10, k18_Q10, k19_Q10, k20_Q10, k21_Q10 
    real(8) :: kdom_Q10(Ndom), kpom_Q10(Npom-1)

    !! C:N:P ratio
    ! real(8), parameter :: rCNph = 106.0d0/16.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)
    real(8), parameter :: rCPph = 106.0d0/1.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)
    real(8), parameter :: rCNph = 11.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)        from analysis
    ! real(8), parameter :: rCPph = 80.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)     Fossing2004

    !! OM ratio
    real(8), parameter :: OMFratio = 0.6d0    ! (no dim.) OMf/OM ratio       Irie 0.57 / Kasih0.4 / Fossing 0.42
    real(8), parameter :: OMSratio = 0.4d0    ! (no dim.) OMs/OM ratio   
!    real(8), parameter :: OMNratio = 0.3d0    ! (no dim.) OMn/OM ratio       Irie 0.2 / Kasih0.2 / Fossing 0.08
   ! real(8), parameter :: 
 
!! Reaction Rate  -------------------------------------------------------------
    !! unit: (nmol cm-3 s-1)
    real(8) :: vdoc(Ndom,Nsed),vdon(Ndom,Nsed),vdop(Ndom,Nsed)    ! 1:labile, 2:refractory
    real(8) :: vpoc(Npom-1,Nsed),vpon(Npom-1,Nsed),vpop(Npom-1,Nsed)    ! 1:labile, 2:refractory
    real(8) :: vOC,vON,vOP
    real(8) :: v1c, v2c, v3c, v4c, v5c
    real(8) :: v1n, v2n, v3n, v4n, v5n
    real(8) :: v1p, v2p, v3p, v4p, v5p
    real(8) :: v6,   v7,  v8,  v9,v10a,v10b
    real(8) :: v11, v12, v13, v14, v15
    real(8) :: v16, v17, v18, v19, v20, v21

    real(8) :: r1,  r2, r3, r4, r5
    
    !! Inhibiting concentration of R19
    real(8),parameter :: H2Sstop = 10d0  ! a certain limit of [H2S] in R19 (µM) (fossing et al. 2004)

    !! Concentration limit
    real(8),parameter :: O2lim    = 20.0d0    !(µmol l-1)     Fossing2004
    real(8),parameter :: NO3lim   = 5.0d0     !(µmol l-1)     Fossing2004
    real(8),parameter :: MnO2lim  = 50000d0   !(nmol g-1)     Fossing2004
    real(8),parameter :: FeOOHlim = 100000d0  !(nmol g-1)     Fossing2004
    real(8),parameter :: SO4lim   = 5.0d0     !(µmol g-1)


!! Net Production and Consumption  -------------------------------------------
    !! unit: (µmol L-1 s-1 = nmol cm-3 s-1)
    !! unit: (nmol g-1 s-1)
    real(8) :: R_O2 (Nsed) = 0.0d0
    real(8) :: R_TA (Nsed) = 0.0d0
    real(8) :: R_DIC(N_Csp,Nsed) = 0.0d0
# if defined ORGANIC_MATTER
    real(8) :: R_DOC(N_Csp,Ndom,Nsed) = 0.0d0      ! 1:labile, 2:refractory
    real(8) :: R_POC(N_Csp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory
# endif
# if defined NUTRIENTS
    real(8) :: R_NO3(N_Nsp,Nsed) = 0.0d0
    real(8) :: R_NH4(N_Nsp,Nsed) = 0.0d0
    real(8) :: R_PO4(N_Psp,Nsed) = 0.0d0
#  if defined ORGANIC_MATTER
    real(8) :: R_DON(N_Nsp,Ndom,Nsed) = 0.0d0      ! 1:labile, 2:refractory 
    real(8) :: R_PON(N_Nsp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory
    real(8) :: R_DOP(N_Psp,Ndom,Nsed) = 0.0d0      ! 1:labile, 2:refractory 
    real(8) :: R_POP(N_Psp,Npom-1,Nsed) = 0.0d0    ! 1:labile, 2:refractory
#  endif
# endif
# if defined SULFATE
    real(8) :: R_Mn2 (Nsed) = 0.0d0
    real(8) :: R_MnO2(Nsed) = 0.0d0
    real(8) :: R_Fe2 (Nsed) = 0.0d0
    real(8) :: R_FeS (Nsed) = 0.0d0
    real(8) :: R_FeS2(N_Ssp,Nsed) = 0.0d0
    real(8) :: R_FeOOH(Nsed) = 0.0d0
    real(8) :: R_FeOOH_PO4(N_Psp,Nsed) = 0.0d0
    real(8) :: R_H2S(N_Ssp,Nsed) = 0.0d0
    real(8) :: R_S0 (N_Ssp,Nsed) = 0.0d0
    real(8) :: R_SO4(N_Ssp,Nsed) = 0.0d0
# endif
    real(8) :: cff, cff2, cff3, cfdd, cfsd, cfss

!!  Output ------------------------------------------------------------------
# if defined SEDIMENT_TESTMODE
    real(8), parameter :: OUTPUT_INTERVAL = 1440.0d0 !*365     ! Output interval (min)
    real(8), save :: dsec = 0.d0 !sec
    real(8) :: pHamb 
    ! real(8) :: redoxlevel(Nsed)    ! for debug / 1:O2, 2:NO3, 3:MnO2, 4:FeOOH, 5:SO4
# endif

! ***********************************************************************************
!!! executable statement

! ********************************************************************* !
!   physical properties of sediment layers 
! ********************************************************************* !
    !! soil density (This must be given as an input data actually)
    rhos_d(:) = 2.5d0 ! (g/cm3)   ! Note: depth varied density is not properly implemented in geochem

! ********************************************************************* !
!   Set surface boundary values
! ********************************************************************* !
    SEDECO(ng)%Tmp(0,i,j)  = Tamb           ! Tmp: Temperature (oC)
    SEDECO(ng)%Sal(0,i,j)  = Samb           ! Sal: Salinity (PSU)
    SEDECO(ng)%TA (0,i,j)  = TAamb          ! TA: Total Alkalinity (kg L-1)
    SEDECO(ng)%O2 (0,i,j)  = O2amb          ! O2: Dissolved oxygen (umol L-1)
    SEDECO(ng)%DIC(:,0,i,j)= DICamb(:)    ! DIC: carbon dioxide  (µmol L-1)
# if defined ORGANIC_MATTER
    SEDECO(ng)%DOC(:,:,0,i,j) = DOCamb(:,:)   ! DOC: Dissolved organic carbon (µmol L-1)
    SEDECO(ng)%POC(:,:,0,i,j) = POCamb(:,:)   ! POC: Particulate organic carbon (nmol g-1)
# endif
# if defined NUTRIENTS
    SEDECO(ng)%NO3(:,0,i,j) = NO3amb(:)         ! NO3: NO3 (umol L-1)
    SEDECO(ng)%NH4(:,0,i,j) = NH4amb(:)         ! NH4: NH4 (umol L-1)
    SEDECO(ng)%PO4(:,0,i,j) = PO4amb(:)         ! PO4: PO4 (umol L-1)
#  if defined ORGANIC_MATTER
    SEDECO(ng)%DON(:,:,0,i,j) = DONamb(:,:)   ! DOC: Dissolved organic nitrogen (µmol L-1)
    SEDECO(ng)%DOP(:,:,0,i,j) = DOPamb(:,:)   ! DOC: Dissolved organic phosporius (µmol L-1)
    SEDECO(ng)%PON(:,:,0,i,j) = PONamb(:,:)   ! PON: Particulate organic nitrogen (PON: umol L-1)
    SEDECO(ng)%POP(:,:,0,i,j) = POPamb(:,:)   ! POP: Particulate organic phosporius (POP: umol L-1)
#  endif
# endif
# if defined SULFATE
    SEDECO(ng)%Mn2(0,i,j) = Mn2amb         ! Mn2 (µmol L-1)
    SEDECO(ng)%MnO2(0,i,j) = 0.0d0
    SEDECO(ng)%Fe2 (0,i,j) = Fe2amb        ! Fe2 (µmol L-1)
    SEDECO(ng)%FeS2(:,0,i,j) = 0.0d0
    SEDECO(ng)%FeOOH(0,i,j) = 0.0d0
    SEDECO(ng)%H2S(:,0,i,j) = H2Samb(:)     ! H2S (µmol L-1)
    SEDECO(ng)%SO4(:,0,i,j) = SO4amb(:)     ! SO4 (µmol L-1)
    SEDECO(ng)%S0 (:,0,i,j) = 0.0d0
# endif
! ********************************************************************* !
!   Set bottom boundary values
! ********************************************************************* !
# if defined SGD_ON
! ???????????????????? To be updateed ?????????????????????????????????????????????????????????????????????????????????
    SEDECO(ng)%Tmp(Nsed,i,j)  = sgd_Tmp_in           ! Tmp: Temperature (oC)
    SEDECO(ng)%Sal(Nsed,i,j)  = sgd_Sal_in           ! Sal: Salinity (PSU)
    SEDECO(ng)%TA (Nsed,i,j)  = sgd_TA_in          ! TA: Total Alkalinity (kg L-1)
    SEDECO(ng)%O2 (Nsed,i,j)  = sgd_DOx_in          ! O2: Dissolved oxygen (umol L-1)
    SEDECO(ng)%DIC(:,Nsed,i,j)  = sgd_DIC_in(:)    ! DIC: carbon dioxide  (µmol L-1)
#  if defined ORGANIC_MATTER
    SEDECO(ng)%DOC(:,:,Nsed,i,j) = DOCamb(:,:)   ! DOC: Dissolved organic carbon (µmol L-1)
    SEDECO(ng)%POC(:,:,Nsed,i,j) = POCamb(:,:)   ! POC: Particulate organic carbon (nmol g-1)
#  endif
#  if defined NUTRIENTS
    SEDECO(ng)%NO3(:,Nsed,i,j) = sgd_NO3_in(:)         ! NO3: NO3 (umol L-1)
    SEDECO(ng)%NH4(:,Nsed,i,j) = sgd_NH4_in(:)         ! NH4: NH4 (umol L-1)
    SEDECO(ng)%PO4(:,Nsed,i,j) = sgd_PO4_in(:)         ! PO4: PO4 (umol L-1)
#   if defined ORGANIC_MATTER
    SEDECO(ng)%DON(:,:,Nsed,i,j) = DONamb(:,:)   ! DOC: Dissolved organic nitrogen (µmol L-1)
    SEDECO(ng)%DOP(:,:,Nsed,i,j) = DOPamb(:,:)   ! DOC: Dissolved organic phosporius (µmol L-1)
    SEDECO(ng)%PON(:,:,Nsed,i,j) = PONamb(:,:)   ! PON: Particulate organic nitrogen (PON: umol L-1)
    SEDECO(ng)%POP(:,:,Nsed,i,j) = POPamb(:,:)   ! POP: Particulate organic phosporius (POP: umol L-1)
#   endif
#  endif
#  if defined SULFATE
    SEDECO(ng)%Mn2(Nsed,i,j) = Mn2amb         ! Mn2 (µmol L-1)
    SEDECO(ng)%MnO2(Nsed,i,j) = 0.0d0
    SEDECO(ng)%Fe2 (Nsed,i,j) = Fe2amb        ! Fe2 (µmol L-1)
    SEDECO(ng)%FeS2(:,Nsed,i,j) = 0.0d0
    SEDECO(ng)%FeOOH(Nsed,i,j) = 0.0d0
    SEDECO(ng)%H2S(:,Nsed,i,j) = H2Samb(:)     ! H2S (µmol L-1)
    SEDECO(ng)%SO4(:,Nsed,i,j) = SO4amb(:)     ! SO4 (µmol L-1)
    SEDECO(ng)%S0 (:,Nsed,i,j) = 0.0d0
#  endif
# else
! Closed boundary condition
    SEDECO(ng)%Tmp(Nsed,i,j)  = SEDECO(ng)%Tmp(Nsed-1,i,j) ! Tmp: Temperature (oC)
    SEDECO(ng)%Sal(Nsed,i,j)  = SEDECO(ng)%Sal(Nsed-1,i,j) ! Sal: Salinity (PSU)
    SEDECO(ng)%TA (Nsed,i,j)  = SEDECO(ng)%TA (Nsed-1,i,j) ! TA: Total Alkalinity (kg L-1)
    SEDECO(ng)%O2 (Nsed,i,j)  = SEDECO(ng)%O2 (Nsed-1,i,j) ! O2: Dissolved oxygen (umol L-1)
    SEDECO(ng)%DIC(:,Nsed,i,j)= SEDECO(ng)%DIC(:,Nsed-1,i,j)    ! DIC: carbon dioxide  (µmol L-1)
#  if defined ORGANIC_MATTER
    SEDECO(ng)%DOC(:,:,Nsed,i,j) = SEDECO(ng)%DOC(:,:,Nsed-1,i,j)   ! DOC: Dissolved organic carbon (µmol L-1)
    SEDECO(ng)%POC(:,:,Nsed,i,j) = SEDECO(ng)%POC(:,:,Nsed-1,i,j)   ! POC: Particulate organic carbon (nmol g-1)
#  endif
#  if defined NUTRIENTS
    SEDECO(ng)%NO3(:,Nsed,i,j) = SEDECO(ng)%NO3(:,Nsed-1,i,j)    ! NO3: NO3 (umol L-1)
    SEDECO(ng)%NH4(:,Nsed,i,j) = SEDECO(ng)%NH4(:,Nsed-1,i,j)    ! NH4: NH4 (umol L-1)
    SEDECO(ng)%PO4(:,Nsed,i,j) = SEDECO(ng)%PO4(:,Nsed-1,i,j)    ! PO4: PO4 (umol L-1)
#   if defined ORGANIC_MATTER
    SEDECO(ng)%DON(:,:,Nsed,i,j) = SEDECO(ng)%DON(:,:,Nsed-1,i,j)  ! DOC: Dissolved organic nitrogen (µmol L-1)
    SEDECO(ng)%DOP(:,:,Nsed,i,j) = SEDECO(ng)%DOP(:,:,Nsed-1,i,j)  ! DOC: Dissolved organic phosporius (µmol L-1)
    SEDECO(ng)%PON(:,:,Nsed,i,j) = SEDECO(ng)%PON(:,:,Nsed-1,i,j)  ! PON: Particulate organic nitrogen (PON: umol L-1)
    SEDECO(ng)%POP(:,:,Nsed,i,j) = SEDECO(ng)%POP(:,:,Nsed-1,i,j)  ! POP: Particulate organic phosporius (POP: umol L-1)
#   endif
#  endif
#  if defined SULFATE
    SEDECO(ng)%Mn2 (Nsed,i,j) = SEDECO(ng)%Mn2 (Nsed-1,i,j)    ! Mn2 (µmol L-1)
    SEDECO(ng)%MnO2(Nsed,i,j) = SEDECO(ng)%MnO2(Nsed-1,i,j)
    SEDECO(ng)%Fe2 (Nsed,i,j) = SEDECO(ng)%Fe2 (Nsed-1,i,j)  ! Fe2 (µmol L-1)
    SEDECO(ng)%FeS2(:,Nsed,i,j) = SEDECO(ng)%FeS2(:,Nsed-1,i,j)
    SEDECO(ng)%FeOOH(Nsed,i,j) = SEDECO(ng)%FeOOH(Nsed-1,i,j)
    SEDECO(ng)%H2S(:,Nsed,i,j) = SEDECO(ng)%H2S(:,Nsed-1,i,j)  ! H2S (µmol L-1)
    SEDECO(ng)%SO4(:,Nsed,i,j) = SEDECO(ng)%SO4(:,Nsed-1,i,j)  ! SO4 (µmol L-1)
    SEDECO(ng)%S0 (:,Nsed,i,j) = SEDECO(ng)%S0 (:,Nsed-1,i,j)
#  endif
# endif

! ********************************************************************* !
!          Diffusion
! ********************************************************************* !
!! ----- Diffusion coefficient in water column (cm-2 s-1) ----- !!
    DO k = 1,Nsed
      D_Tmp(k) = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(k,i,j) + 0.00505d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
      D_Sal(k) = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(k,i,j) + 0.00505d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
      D_O2 (k) = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(k,i,j) + 0.00505d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
      D_DIC(iCt ,k) = (9.39d0 + 0.267d0*SEDECO(ng)%Tmp(k,i,j) + 0.0041d0 *SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6) 
!      D_DIC(iC13,k) = (9.39d0 + 0.267d0*SEDECO(ng)%Tmp(k,i,j) + 0.0041d0 *SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6) 
# if defined ORGANIC_MATTER
      D_DOM(iLDOM,k) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j)) *(1.0d-7) ! DOMf: DOCf DONf DOPf ??????????????????
      D_DOM(iRDOM,k) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j)) *(1.0d-9) ! DOMs: DOCs DONs DOPs ??????????????????
# endif
# if defined NUTRIENTS
      D_NO3(iNt ,k)  = (9.72d0 + 0.365d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_NH4(iNt ,k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
!      D_NO3(iN15,k)  = (9.72d0 + 0.365d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
!      D_NH4(iN15,k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_PO4(iPt,k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
!      D_PO4(iP1,k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
# endif
# if defined SULFATE
      D_Mn2(k)  = (3.04d0 + 0.153d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_Fe2(k)  = (3.36d0 + 0.148d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_SO4(iSt ,k)  = (4.96d0 + 0.226d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
      D_H2S(iSt ,k)  = (8.74d0 + 0.264d0*SEDECO(ng)%Tmp(k,i,j) + 0.004d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
!      D_SO4(iS34,k)  = (4.96d0 + 0.226d0*SEDECO(ng)%Tmp(k,i,j))  *(1.0d-6)
!      D_H2S(iS34,k)  = (8.74d0 + 0.264d0*SEDECO(ng)%Tmp(k,i,j) + 0.004d0*SEDECO(ng)%Tmp(k,i,j)**2d0) *(1.0d-6)
# endif
    ENDDO

!! --- Bio-diffusion coefficient in sediment column (cm-2 s-1) (For solutes and solids) ------------
    DBd(1)=0.0d0
    DBp(1)=0.0d0

    DO k = 2,Nsed
      !! determine DBd and DBp depending on z
      if (SEDECO(ng)%zw(k) <= 11.8d0 )then
        DBd(k) = 3.51d-6
      else
        DBd(k) = 3.51d-6 * exp(-0.378d0*(SEDECO(ng)%zw(k)-11.8d0))
      endif
      DBp(k) = DBd(k)/9.3d0 * rhos_d(k)*(1d0-SEDECO(ng)%poro(k,i,j))
      DBd(k) = DBd(k) * SEDECO(ng)%poro(k,i,j)
    ENDDO

!! --- Total diffusion coefficient in sediment column (cm-2 s-1) --- !!
    DO k = 2,Nsed
      cff = SEDECO(ng)%poro(k,i,j)/(1.0d0+3.0d0*(1.0d0-SEDECO(ng)%poro(k,i,j)))

      D_O2(k) = cff*D_O2(k) + DBd(k)
      DO isp=1,N_Csp
        D_DIC(isp,k) = cff*D_DIC(isp,k) + DBd(k)
      ENDDO
# if defined ORGANIC_MATTER
      DO m = 1,Ndom
        D_DOM(m,k) = cff*D_DOM(m,k) + DBd(k) ! DOMf: DOCf DONf DOPf
      ENDDO
# endif
# if defined NUTRIENTS
      DO isp=1,N_Nsp
        D_NO3(isp,k)  = cff*D_NO3(isp,k) + DBd(k)
        D_NH4(isp,k)  = cff*D_NH4(isp,k) + DBd(k)
      ENDDO
      DO isp=1,N_Psp
        D_PO4(isp,k)  = cff*D_PO4(isp,k) + DBd(k)
      ENDDO
# endif
# if defined SULFATE
      D_Mn2(k)  = cff*D_Mn2(k) + DBd(k)
      D_Fe2(k)  = cff*D_Fe2(k) + DBd(k)
      DO isp=1,N_Ssp
        D_SO4(isp,k)  = cff*D_SO4(isp,k) + DBd(k)
        D_H2S(isp,k)  = cff*D_H2S(isp,k) + DBd(k)
      ENDDO
# endif
    ENDDO

!! ----- Diffusive boundary layer tickness ----- !!
    SEDECO(ng)%dzw(1) = 0.03d0 ! (cm) ???????????? Need update for adding dependency on flow rate ??????????????????????????

!! === Diffusion Flux calculation ======================================
    DO k = 1,Nsed
! --- Solutes ----
      Fw_Tmp(k) = Ficks(D_Tmp(k),SEDECO(ng)%Tmp(k-1,i,j),SEDECO(ng)%Tmp(k,i,j),SEDECO(ng)%dzw(k))
      Fw_Sal(k) = Ficks(D_Sal(k),SEDECO(ng)%Sal(k-1,i,j),SEDECO(ng)%Sal(k,i,j),SEDECO(ng)%dzw(k))
      Fw_O2 (k) = Ficks(D_O2 (k),SEDECO(ng)%O2 (k-1,i,j),SEDECO(ng)%O2 (k,i,j),SEDECO(ng)%dzw(k))
      Fw_TA (k) = Ficks(D_O2 (k),SEDECO(ng)%TA (k-1,i,j),SEDECO(ng)%TA (k,i,j),SEDECO(ng)%dzw(k))
      DO isp = 1,N_Csp
        Fw_DIC(isp,k) = Ficks(D_DIC(isp,k),SEDECO(ng)%DIC(isp,k-1,i,j),SEDECO(ng)%DIC(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
# if defined NUTRIENTS
      DO isp = 1,N_Nsp
        Fw_NO3(isp,k) = Ficks(D_NO3(k),SEDECO(ng)%NO3(k-1,i,j),SEDECO(ng)%NO3(k,i,j),SEDECO(ng)%dzw(k))
        Fw_NH4(isp,k) = Ficks(D_NH4(k),SEDECO(ng)%NH4(k-1,i,j),SEDECO(ng)%NH4(k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
      DO isp = 1,N_Psp
        Fw_PO4(isp,k) = Ficks(D_PO4(k),SEDECO(ng)%PO4(k-1,i,j),SEDECO(ng)%PO4(k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
# endif
# if defined ORGANIC_MATTER
      DO m = 1,Ndom
        DO isp = 1,N_Csp
          Fw_DOC(isp,m,k) = Ficks(D_DOM(m,k),SEDECO(ng)%DOC(isp,m,k-1,i,j),SEDECO(ng)%DOC(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
#  if defined NUTRIENTS
        DO isp = 1,N_Nsp
          Fw_DON(isp,m,k) = Ficks(D_DOM(m,k),SEDECO(ng)%DON(isp,m,k-1,i,j),SEDECO(ng)%DON(isp,m,k,i,j),SEDECO(ng)%dzw(k)) ! DONf
        ENDDO
        DO isp = 1,N_Psp
          Fw_DOP(isp,m,k) = Ficks(D_DOM(m,k),SEDECO(ng)%DOP(isp,m,k-1,i,j),SEDECO(ng)%DOP(isp,m,k,i,j),SEDECO(ng)%dzw(k)) ! DONf
        ENDDO
      ENDDO
#  endif
# endif
# if defined SULFATE
      Fw_Mn2(k)   = Ficks(D_Mn2(k),SEDECO(ng)%Mn2(k-1,i,j),SEDECO(ng)%Mn2(k,i,j),SEDECO(ng)%dzw(k))
      Fw_Fe2(k)   = Ficks(D_Fe2(k),SEDECO(ng)%Fe2(k-1,i,j),SEDECO(ng)%Fe2(k,i,j),SEDECO(ng)%dzw(k))
      DO isp=1,N_Ssp
        Fw_H2S(isp,k) = Ficks(D_H2S(isp,k),SEDECO(ng)%H2S(isp,k-1,i,j),SEDECO(ng)%H2S(isp,k,i,j),SEDECO(ng)%dzw(k))
        Fw_SO4(isp,k) = Ficks(D_SO4(isp,k),SEDECO(ng)%SO4(isp,k-1,i,j),SEDECO(ng)%SO4(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
# endif
! --- Solids -----
# if defined ORGANIC_MATTER
      DO m = 1,Npom-1
        DO isp = 1,N_Csp
          Fw_POC(isp,m,k) = Ficks(DBp(k),SEDECO(ng)%POC(isp,m,k-1,i,j),SEDECO(ng)%POC(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
#  if defined NUTRIENTS
        DO isp = 1,N_Nsp
          Fw_PON(isp,m,k) = Ficks(DBp(k),SEDECO(ng)%PON(isp,m,k-1,i,j),SEDECO(ng)%PON(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
        DO isp = 1,N_Psp
          Fw_POP(isp,m,k) = Ficks(DBp(k),SEDECO(ng)%POP(isp,m,k-1,i,j),SEDECO(ng)%POP(isp,m,k,i,j),SEDECO(ng)%dzw(k))
        ENDDO
      ENDDO
#  endif
# endif
# if defined SULFATE
      Fw_MnO2(k)  = Ficks(DBp(k),SEDECO(ng)%MnO2(k-1,i,j),SEDECO(ng)%MnO2(k,i,j),SEDECO(ng)%dzw(k))
      Fw_FeOOH(k) = Ficks(DBp(k),SEDECO(ng)%FeOOH(k-1,i,j),SEDECO(ng)%FeOOH(k,i,j),SEDECO(ng)%dzw(k))
      DO isp = 1,N_Psp
        Fw_FeOOH_PO4(k) &
          = Ficks(DBp(k),SEDECO(ng)%FeOOH_PO4(isp,k-1,i,j),SEDECO(ng)%FeOOH_PO4(isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
      DO isp=1,N_Ssp
        Fw_FeS(k)  = Ficks(DBp(k),SEDECO(ng)%FeS (isp,k-1,i,j),SEDECO(ng)%FeS (isp,k,i,j),SEDECO(ng)%dzw(k))
        Fw_FeS2(k) = Ficks(DBp(k),SEDECO(ng)%FeS2(isp,k-1,i,j),SEDECO(ng)%FeS2(isp,k,i,j),SEDECO(ng)%dzw(k))
        Fw_S0(k)   = Ficks(DBp(k),SEDECO(ng)%S0  (isp,k-1,i,j),SEDECO(ng)%S0  (isp,k,i,j),SEDECO(ng)%dzw(k))
      ENDDO
# endif
    ENDDO

!!! (3) Adsorption (nmol cm-2 s-1) ------------------------------------------
!# if defined adsorption
!    DO k = 2,Nsed
!#  if defined NUTRIENTS
!      Fb_NO3(k)   = Fb_NO3(k) + Ficks_sedp(DBp(k) * Kad_NO3,SEDECO(ng)%NO3(k,i,j-1), &
!                                          SEDECO(ng)%NO3(k,i,j),dz(k),SEDECO(ng)%poro(k,i,j),rhos_d(k))
!      Fb_NH4(k)   = Fb_NH4(k) + Ficks_sedp(DBp(k) * Kad_NH4,SEDECO(ng)%NH4(k,i,j-1), &
!                                          SEDECO(ng)%NH4(k,i,j),dz(k),SEDECO(ng)%poro(k,i,j),rhos_d(k))
!      Fb_PO4(k)   = Fb_PO4(k) + Ficks_sedp(DBp(k) * Kad_PO4,SEDECO(ng)%PO4(k,i,j-1), &
!                                          SEDECO(ng)%PO4(k,i,j),dz(k),SEDECO(ng)%poro(k,i,j),rhos_d(k))
!#  endif
!#  if defined SULFATE
!      Fb_Mn2(k)   = Fb_Mn2(k) + Ficks_sedp(DBp(k) * Kad_Mn2,SEDECO(ng)%Mn2(k,i,j-1), & 
!                                          SEDECO(ng)%Mn2(k,i,j),dz(k),SEDECO(ng)%poro(k,i,j),rhos_d(k))
!      Fb_Fe2(k)   = Fb_Fe2(k) + Ficks_sedp(DBp(k) * Kad_Fe2,SEDECO(ng)%Fe2(k,i,j-1),& 
!                                          SEDECO(ng)%Fe2(k,i,j),dz(k),SEDECO(ng)%poro(k,i,j),rhos_d(k))
!#  endif
!    ENDDO
!# endif



! ********************************************************************* !
!     Advection term
! ********************************************************************* !
# if defined SEDECO_BURIAL || defined SGD_ON
!! --- Advection velocity in sediment column (cm s-1) -----
    DO k = 1,Nsed
#  if defined SEDECO_BURIAL
      Wd(k) = Wd(k) + Fdep_sed * SEDECO(ng)%poro(k,i,j)
      Wp(k) = Wd(k) + Fdep_sed * rhos_d(k)*(1.0d0-SEDECO(ng)%poro(k,i,j))
#  endif
#  if defined SGD_ON
      Wd(k)=Wd(k)-sgd_flux * SEDECO(ng)%poro(k,i,j)
#  endif
    ENDDO

!! === Advection Flux calculation ======================================

! --- Solutes, Flux in sediment column ----
    DO k = 1,Nsed
      Fw_Tmp(k) = Fw_Tmp(k) + AdvUp(Wd(k),SEDECO(ng)%Tmp(k-1,i,j),SEDECO(ng)%Tmp(k,i,j))
      Fw_Sal(k) = Fw_Sal(k) + AdvUp(Wd(k),SEDECO(ng)%Sal(k-1,i,j),SEDECO(ng)%Sal(k,i,j))
      Fw_O2 (k) = Fw_O2 (k) + AdvUp(Wd(k),SEDECO(ng)%O2 (k-1,i,j),SEDECO(ng)%O2 (k,i,j))
      Fw_TA (k) = Fw_TA (k) + AdvUp(Wd(k),SEDECO(ng)%TA (k-1,i,j),SEDECO(ng)%TA (k,i,j))
      DO isp = 1,N_Csp
        Fw_DIC(isp,k) = Fw_DIC(isp,k) + AdvUp(Wd(isp,k),SEDECO(ng)%DIC(isp,k-1,i,j),SEDECO(ng)%DIC(isp,k,i,j))
      ENDDO
#  if defined NUTRIENTS
      DO isp = 1,N_Nsp
        Fw_NO3(isp,k) = Fw_NO3(isp,k) + AdvUp(Wd(k),SEDECO(ng)%NO3(k-1,i,j),SEDECO(ng)%NO3(k,i,j))
        Fw_NH4(isp,k) = Fw_NH4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%NH4(k-1,i,j),SEDECO(ng)%NH4(k,i,j))
      ENDDO
      DO isp = 1,N_Psp
        Fw_PO4(isp,k) = Fw_PO4(isp,k) + AdvUp(Wd(k),SEDECO(ng)%PO4(k-1,i,j),SEDECO(ng)%PO4(k,i,j))
      ENDDO
#  endif
#  if defined ORGANIC_MATTER
      DO m = 1,Ndom
        DO isp = 1,N_Csp
          Fw_DOC(isp,m,k) = Fw_DOC(isp,m,k) + AdvUp(Wd(m,k),SEDECO(ng)%DOC(isp,m,k-1,i,j),SEDECO(ng)%DOC(isp,m,k,i,j))
        ENDDO
#   if defined NUTRIENTS
        DO isp = 1,N_Nsp
          Fw_DON(isp,m,k) = Fw_DON(isp,m,k) + AdvUp(Wd(m,k),SEDECO(ng)%DON(isp,m,k-1,i,j),SEDECO(ng)%DON(isp,m,k,i,j)) ! DONf
        ENDDO
        DO isp = 1,N_Psp
          Fw_DOP(isp,m,k) = Fw_DOP(isp,m,k) + AdvUp(Wd(m,k),SEDECO(ng)%DOP(isp,m,k-1,i,j),SEDECO(ng)%DOP(isp,m,k,i,j)) ! DONf
        ENDDO
      ENDDO
#   endif
#  endif
#  if defined SULFATE
      Fw_Mn2(k) = Fw_Mn2(k) + AdvUp(Wd(k),SEDECO(ng)%Mn2(k-1,i,j),SEDECO(ng)%Mn2(k,i,j))
      Fw_Fe2(k) = Fw_Fe2(k) + AdvUp(Wd(k),SEDECO(ng)%Fe2(k-1,i,j),SEDECO(ng)%Fe2(k,i,j))
      DO isp=1,N_Ssp
        Fw_H2S(isp,k) = Fw_H2S(isp,k) + AdvUp(Wd(isp,k),SEDECO(ng)%H2S(isp,k-1,i,j),SEDECO(ng)%H2S(isp,k,i,j))
        Fw_SO4(isp,k) = Fw_SO4(isp,k) + AdvUp(Wd(isp,k),SEDECO(ng)%SO4(isp,k-1,i,j),SEDECO(ng)%SO4(isp,k,i,j))
      ENDDO
#  endif
    ENDDO

! --- Solids, Flux in sediment column  ----
    DO k = 2,Nsed
#  if defined ORGANIC_MATTER
      DO m = 1,Npom-1
        DO isp = 1,N_Csp
          Fw_POC(isp,m,k) = Fw_POC(isp,m,k) + AdvUp(Wp(k),SEDECO(ng)%POC(isp,m,k-1,i,j),SEDECO(ng)%POC(isp,m,k,i,j))
        ENDDO
#   if defined NUTRIENTS
        DO isp = 1,N_Nsp
          Fw_PON(isp,m,k) = Fw_PON(isp,m,k) + AdvUp(Wp(k),SEDECO(ng)%PON(isp,m,k-1,i,j),SEDECO(ng)%PON(isp,m,k,i,j))
        ENDDO
        DO isp = 1,N_Psp
          Fw_POP(isp,m,k) = Fw_POP(isp,m,k) + AdvUp(Wp(k),SEDECO(ng)%POP(isp,m,k-1,i,j),SEDECO(ng)%POP(isp,m,k,i,j))
        ENDDO
      ENDDO
#   endif
#  endif
#  if defined SULFATE
      Fw_MnO2(k)  = Fw_MnO2(k) + AdvUp(Wp(k),SEDECO(ng)%MnO2(k-1,i,j),SEDECO(ng)%MnO2(k,i,j))
      Fw_FeOOH(k) = Fw_FeOOH(k)+ AdvUp(Wp(k),SEDECO(ng)%FeOOH(k-1,i,j),SEDECO(ng)%FeOOH(k,i,j))
      DO isp = 1,N_Psp
        Fw_FeOOH_PO4(k) = 
          ENDDO
      DO isp=1,N_Ssp
        Fw_FeS(k)  = Fw_FeS(k) + AdvUp(Wp(k),SEDECO(ng)%FeS (isp,k-1,i,j),SEDECO(ng)%FeS (isp,k,i,j))
        Fw_FeS2(k) = Fw_FeS2(k)+ AdvUp(Wp(k),SEDECO(ng)%FeS2(isp,k-1,i,j),SEDECO(ng)%FeS2(isp,k,i,j))
        Fw_S0(k)   = Fw_S0(k)  + AdvUp(Wp(k),SEDECO(ng)%S0  (isp,k-1,i,j),SEDECO(ng)%S0  (isp,k,i,j))
      ENDDO
#  endif
    ENDDO
# endif

! ********************************************************************* !
!    Set settling Flux (solids)
! ********************************************************************* !
#  if defined ORGANIC_MATTER
    DO m = 1,Npom-1
      DO isp = 1,N_Csp
        Fw_POC(isp,m,1) = Fdep_POC(isp,m)
      ENDDO
#   if defined NUTRIENTS
      DO isp = 1,N_Nsp
        Fw_PON(isp,m,1) = Fdep_PON(isp,m)
      ENDDO
      DO isp = 1,N_Psp
        Fw_POP(isp,m,1) = Fdep_POP(isp,m)
      ENDDO
    ENDDO
#   endif
#  endif
#  if defined SULFATE
    Fw_MnO2(1)  = Fdep_MnO2
    Fw_FeOOH(1) = Fdep_FeOOH
    DO isp = 1,N_Psp
      Fw_FeOOH_PO4(1) = 0.0d0  !???????????????????????????????????
    ENDDO
    DO isp=1,N_Ssp
      Fw_FeS(1)  = 0.0d0  !???????????????????????????????????
      Fw_FeS2(1) = 0.0d0  !???????????????????????????????????
      Fw_S0(1)   = 0.0d0  !???????????????????????????????????
    ENDDO
#  endif

! ********************************************************************* !
!    Reaction term
! ********************************************************************* !

    DO k = 1,Nsed
    !! Reaction rate constants depending on temperature
      cff = Q10_sec**((SEDECO(ng)%Tmp(k,i,j)-9d0)/10d0)
      k6_Q10  = k6  * cff     ! (Fossing2004)
      k7_Q10  = k7  * cff     ! (Berg2003)      (s-1)
      k8_Q10  = k8  * cff     ! (G.A.Anggra2009)
      k9_Q10  = k9  * cff     ! (Fossing2004)
      k10_Q10 = k10 * cff     ! (G.A.Anggra2009)
      k11_Q10 = k11 * cff     ! (Fossing2004)
      k12_Q10 = k12 * cff     ! (Fossing2004)
      k13_Q10 = k13 * cff     ! (G.A.Anggra2009)
      k14_Q10 = k14 * cff     ! (Fossing2004)
      k15_Q10 = k15 * cff     ! (G.A.Anggra2009)(s-1)
      k16_Q10 = k16 * cff     ! (Fossing2004)
      k17_Q10 = k17 * cff     ! (Fossing2004)
      k18_Q10 = k18 * cff     ! (Fossing2004)
      k19_Q10 = k19 * cff     ! (Fossing2004)   (s-1)
      k20_Q10 = k20 * cff     ! (Fossing2004)   (s-1)
      k21_Q10 = k21 * cff     ! (Fossing2004)   (s-1)

      cff = Q10_pri**((SEDECO(ng)%Tmp(k,i,j)-9d0)/10d0)
      DO m = 1,Ndom
        kdom_Q10(m) = kdom(m) * cff
      ENDDO
      DO m = 1,Npom-1
        kpom_Q10(m) = kpom(m) * cff
      ENDDO

    !! Reaction rate calculatoin (functions Vdd, Vsd, Vss are defined in mod_geochem.F) -- !!
        ! Vdd : reaction with solute * solute (R6, ...)
        ! Vsd : reaction with solid * solute  (R7, ...)
        ! Vss : reaction with solid * solid   (R14)
    !! ----------------------------------------------------------------------------------- !!
        ! Vdd = poro * k * Cd1 * Cd2
        ! Vsd = rho_s * (1d0 - poro) * k * Cs1 * Cd1
        ! Vss = ((rho_s * (1d0 - poro))**2) * k * Cs1 * Cs2
      cfdd = SEDECO(ng)%poro(k,i,j)
      cfsd = rhos_d(k) * (1.0d0 - SEDECO(ng)%poro(k,i,j))
      cfss = cfsd*cfsd

    !! PRIMARY REACTIONS

       ! R1:   O2 + CH2O --> CO2 + H2O
       ! R2:   4/5 NO3- + CH2O + 4/5 H+ --> 4/5 N2 + CO2 + 7/5 H2O
       ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
       ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
       ! R5:   1/2 SO42- + CH2O + H+ --> 1/2 H2S + CO2 + H2O

# if defined sedORP
      !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
      !! redox stages change depending on ORP input
      !! create condition of it here
      !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
          !! ギブスエネルギーで反応が変化するようにするべき
# else
      !! Redox concentration limit in organic matter degradation (Berg et al.2003)
      if (SEDECO(ng)%O2(k,i,j) > O2lim) then            !! -- Redox of CH2O       (CH2O)  (µmol L-1) -- !!   if O2 > O2lim
        r1 = 1.0d0
        r2 = 0.0d0
        r3 = 0.0d0
        r4 = 0.0d0
        r5 = 0.0d0
        ! redoxlevel(k) = 1d0 !!for debug
      else if(SEDECO(ng)%NO3(iNt,k,i,j) > NO3lim) then      !! -- Denitrification     (NO3)   (µmol L-1) -- !!   if O2 < O2lim   &&   NO3 > NO3lim
        r1 = SEDECO(ng)%O2(k,i,j)/O2lim
        r2 = 1.0d0 - r1
        r3 = 0.0d0
        r4 = 0.0d0
        r5 = 0.0d0       
        ! redoxlevel(k) = 2d0 !!for debug
      else if(SEDECO(ng)%MnO2(k,i,j) > MnO2lim) then    !! -- Manganese reduction (MnO2)  (nmol g-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 > MnO2lim
        r1 = SEDECO(ng)%O2(k,i,j)/O2lim
        r2 =(1.0d0-r1)                          &
            *SEDECO(ng)%NO3(iNt,k,i,j)/NO3lim
        r3 = 1.0d0-r1-r2
        r4 = 0.0d0
        r5 = 0.0d0       
        ! redoxlevel(k) = 3d0 !!for debug
      else if(SEDECO(ng)%FeOOH(k,i,j) > FeOOHlim) then  !! -- Iron reduction      (FeOOH) (nmol g-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH > FeOOHlim
        r1 = SEDECO(ng)%O2(k,i,j)/O2lim
        r2 =(1.0d0-r1)                          &
            *SEDECO(ng)%NO3(iNt,k,i,j)/NO3lim
        r3 =(1.0d0-r1-r2)                       &
            *SEDECO(ng)%MnO2(k,i,j)/MnO2lim
        r4 = 1.0d0-r1-r2-r3
        r5 = 0.0d0       
        ! redoxlevel(k) = 4d0 !!for debug
      else if(SEDECO(ng)%SO4(k,i,j) > SO4lim) then      !! -- Sulfate reduction   (SO4)   (µmol L-1) -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH < FeOOHlim   &&   SO4 > SO4lim
        r1 = SEDECO(ng)%O2(k,i,j)/O2lim
        r2 =(1.0d0-r1)                          &
            *SEDECO(ng)%NO3(iNt,k,i,j)/NO3lim
        r3 =(1.0d0-r1-r2)                       &
            *SEDECO(ng)%MnO2(k,i,j)/MnO2lim
        r4 =(1.0d0-r1-r2-r3)                    &
            *SEDECO(ng)%FeOOH(k,i,j)/FeOOHlim
        r5 = 1.0d0-r1-r2-r3-r4
        ! redoxlevel(k) = 5d0 !!for debug
      else                                              !! limit OM decomposition if SO4 < SO4lim    -- !!   if O2 < O2lim   &&   NO3 < NO3lim   &&   Mn02 < MnO2lim   &&   FeOOH < FeOOHlim   &&   SO4 < SO4lim
        r1 = SEDECO(ng)%O2(k,i,j)/O2lim
        r2 =(1.0d0-r1)                          &
            *SEDECO(ng)%NO3(iNt,k,i,j)/NO3lim
        r3 =(1.0d0-r1-r2)                       &
            *SEDECO(ng)%MnO2(k,i,j)/MnO2lim
        r4 =(1.0d0-r1-r2-r3)                    &
            *SEDECO(ng)%FeOOH(k,i,j)/FeOOHlim
        r5 =(1.0d0-r1-r2-r3-r4)                 &
            *SEDECO(ng)%SO4(iSt,k,i,j)/SO4lim
        ! redoxlevel(k) = 5d0 !!for debug
      endif
# endif
      ! POM remineralization
      DO m = 1,Npom-1
        cff = cfsd * kpom_Q10(m) * SEDECO(ng)%POC(iCt,m,k,i,j)
        v1c = r1*cff
        v2c = r2*cff
        v3c = r3*cff
        v4c = r4*cff
        v5c = r5*cff

        cff = cfsd * kpom_Q10(m) * SEDECO(ng)%PON(iNt,m,k,i,j)
        v1n = r1*cff
        v2n = r2*cff
        v3n = r3*cff
        v4n = r4*cff
        v5n = r5*cff

        cff = cfsd * kpom_Q10(m) * SEDECO(ng)%POP(iPt,m,k,i,j)
        v1p = r1*cff
        v2p = r2*cff
        v3p = r3*cff
        v4p = r4*cff
        v5p = r5*cff

        DO isp=1,N_Csp
          cff = SEDECO(ng)%POC(isp,m,k,i,j)/SEDECO(ng)%POC(iCt,m,k,i,j)
          R_POC(isp,m,k) = R_POC(isp,m,k) - (v1c +v2c +v3c +v4c +v5c)*cff
          R_DIC(isp,k)   = R_DIC(isp,k)   + (v1c +v2c +v3c +v4c +v5c)*cff
        ENDDO
        R_O2(k) = R_O2(k) - v1c
        R_TA(k) = R_TA(k) +0.8d0*v2c +4.0d0*v3c + 4.0d0*v4c +v5c

        DO isp=1,N_Nsp
          cff = SEDECO(ng)%PON(isp,m,k,i,j)/SEDECO(ng)%PON(iNt,m,k,i,j)
          R_PON(isp,m,k) = R_PON(isp,m,k) - (v1n +v2n +v3n +v4n +v5n)*cff
          R_NH4(isp,k)   = R_NH4(isp,k)   + (v1n +v2n +v3n +v4n +v5n)*cff

          cff = SEDECO(ng)%NH4(isp,k,i,j)/SEDECO(ng)%NH4(iNt,k,i,j)
          R_NO3(isp,k)   = R_NO3(isp,k) -0.8d0*v2n*cff
        ENDDO
        R_TA(k) = R_TA(k) +v1n +v2n +v3n +v4n +v5n

        DO isp=1,N_Psp
            cff = SEDECO(ng)%POP(isp,m,k,i,j)/SEDECO(ng)%POP(iPt,m,k,i,j)
            R_POP(isp,m,k) = R_POP(isp,m,k) - (v1p +v2p +v3p +v4p +v5p)*cff
            R_PO4(isp,k)   = R_PO4(isp,k)   + (v1p +v2p +v3p +v4p +v5p)*cff
        ENDDO
        R_TA(k) = R_TA(k) -(v1p +v2p +v3p +v4p +v5p)
  
        DO isp=1,N_Ssp
          cff = SEDECO(ng)%SO4(isp,k,i,j)/SEDECO(ng)%SO4(iSt,k,i,j)
          R_SO4(isp,k) = R_SO4(isp,k) - v5c*cff
          R_H2S(isp,k) = R_H2S(isp,k) + v5c*cff
        ENDDO
      ENDDO

      ! DOM remineralization
      DO m = 1,Ndom
        cff = cfsd * kdom_Q10(m) * SEDECO(ng)%DOC(iCt,m,k,i,j)
        v1c = r1*cff
        v2c = r2*cff
        v3c = r3*cff
        v4c = r4*cff
        v5c = r5*cff

        cff = cfsd * kdom_Q10(m) * SEDECO(ng)%DON(iNt,m,k,i,j)
        v1n = r1*cff
        v2n = r2*cff
        v3n = r3*cff
        v4n = r4*cff
        v5n = r5*cff

        cff = cfsd * kdom_Q10(m) * SEDECO(ng)%DOP(iPt,m,k,i,j)
        v1p = r1*cff
        v2p = r2*cff
        v3p = r3*cff
        v4p = r4*cff
        v5p = r5*cff

        DO isp=1,N_Csp
          cff = SEDECO(ng)%DOC(isp,m,k,i,j)/SEDECO(ng)%DOC(iCt,m,k,i,j)
          R_DOC(isp,m,k) = R_DOC(isp,m,k) - (v1c +v2c +v3c +v4c +v5c)*cff
          R_DIC(isp,k)   = R_DIC(isp,k)   + (v1c +v2c +v3c +v4c +v5c)*cff
        ENDDO
        R_O2(k) = R_O2(k) - v1c
        R_TA(k) = R_TA(k) +0.8d0*v2c +4.0d0*v3c + 4.0d0*v4c +v5c

        DO isp=1,N_Nsp
          cff = SEDECO(ng)%DON(isp,m,k,i,j)/SEDECO(ng)%DON(iNt,m,k,i,j)
          R_DON(isp,m,k) = R_DON(isp,m,k) - (v1n +v2n +v3n +v4n +v5n)*cff
          R_NH4(isp,k)   = R_NH4(isp,k)   + (v1n +v2n +v3n +v4n +v5n)*cff

          cff = SEDECO(ng)%NH4(isp,k,i,j)/SEDECO(ng)%NH4(iNt,k,i,j)
          R_NO3(isp,k)   = R_NO3(isp,k) -0.8d0*v2n*cff
        ENDDO
        R_TA(k) = R_TA(k) +v1n +v2n +v3n +v4n +v5n

        DO isp=1,N_Psp
            cff = SEDECO(ng)%DOP(isp,m,k,i,j)/SEDECO(ng)%DOP(iPt,m,k,i,j)
            R_DOP(isp,m,k) = R_DOP(isp,m,k) - (v1p +v2p +v3p +v4p +v5p)*cff
            R_PO4(isp,k)   = R_PO4(isp,k)   + (v1p +v2p +v3p +v4p +v5p)*cff
        ENDDO
        R_TA(k) = R_TA(k) -(v1p +v2p +v3p +v4p +v5p)
  
        DO isp=1,N_Ssp
          cff = SEDECO(ng)%SO4(isp,k,i,j)/SEDECO(ng)%SO4(iSt,k,i,j)
          R_SO4(isp,k) = R_SO4(isp,k) - v5c*cff
          R_H2S(isp,k) = R_H2S(isp,k) + v5c*cff
        ENDDO
      ENDDO

    !! SECONDARY REACTIONS

      ! R6:   NH4+ + 2O2 --> NO3- + H2O + 2H+
      v6 = cfdd * k6_Q10*SEDECO(ng)%NH4(iNt,k,i,j)*SEDECO(ng)%O2(k,i,j)
      R_O2(k)  = R_O2(k) - 2.0d0*v6
      R_TA(k)  = R_TA(k) - 2.0d0*v6
      DO isp=1,N_Nsp
        cff = SEDECO(ng)%NH4(isp,k,i,j)/SEDECO(ng)%NH4(iNt,k,i,j)
        R_NO3(isp,k) = R_NO3(isp,k) + v6*cff
        R_NH4(isp,k) = R_NH4(isp,k) - v6*cff
      ENDDO

      ! R7:   FeOOH + PO43- --> FeOOH=PO43-
      v7 = cfsd * k7_Q10*SEDECO(ng)%FeOOH(k,i,j)*SEDECO(ng)%PO4(iPt,k,i,j)
      R_FeOOH(k) = R_FeOOH(k) -v7
      DO isp=1,N_Psp
        cff = SEDECO(ng)%PO4(isp,k,i,j)/SEDECO(ng)%PO4(iPt,k,i,j)
        R_PO4(isp,k)       = R_PO4(isp,k)       - v7*cff
        R_FeOOH_PO4(isp,k) = R_FeOOH_PO4(isp,k) + v7*cff
      ENDDO

      ! R8:   2Fe2+ + MnO2 + 2H2O --> 2FeOOH + Mn2+ + 2H+
      v8 = cfsd * k8_Q10*SEDECO(ng)%MnO2(k,i,j)*SEDECO(ng)%Fe2(k,i,j)
      R_TA(k)  = R_TA(k) - 2.0d0*v8
      R_Fe2(k)   = R_Fe2(k)   -2.0d0*v8
      R_FeOOH(k) = R_FeOOH(k) +2.0d0*v8
      R_MnO2(k) = R_MnO2(k) - v8
      R_Mn2(k)  = R_Mn2(k)  + v8

      ! R9:   2Mn2+ + O2 + 2H2O --> 2MnO2 + 4H+
      v9 = cfdd * k9_Q10*SEDECO(ng)%O2(k,i,j)  *SEDECO(ng)%Mn2(k,i,j)
      R_O2(k)  = R_O2(k) - v9
      R_TA(k)  = R_TA(k) - 4.0d0*v9
      R_Mn2(k)  = R_Mn2(k)  - 2.0d0*v9
      R_MnO2(k) = R_MnO2(k) + 2.0d0*v9

      ! R10a: H2S + 2FeOOH=PO43- + 4H+ --> S0 + 2Fe2+ + 4H2O + 2PO43-
      v10a = cfsd * k10_Q10                                           &
                    *SEDECO(ng)%FeOOH_PO4(iPt,k,i,j)*SEDECO(ng)%H2S(iSt,k,i,j)
      R_TA(k)  = R_TA(k) + 4.0d0*v10a
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%H2S(isp,k,i,j)/SEDECO(ng)%H2S(iSt,k,i,j)
        R_H2S(isp,k) = R_H2S(isp,k) - v10a*cff
        R_S0(isp,k)  = R_S0(isp,k)  + v10a*cff
      ENDDO
      DO isp=1,N_Psp
        cff = SEDECO(ng)%FeOOH_PO4(isp,k,i,j)/SEDECO(ng)%FeOOH_PO4(iPt,k,i,j)
        R_PO4(isp,k)       = R_PO4(isp,k)       +2.0d0*v10a*cff
        R_FeOOH_PO4(isp,k) = R_FeOOH_PO4(isp,k) -2.0d0*v10a*cff
      ENDDO

      ! R10b: H2S + 2FeOOH + 4H+ --> S0 + 2Fe2+ + 4H2O
      v10b = cfsd * k10_Q10*SEDECO(ng)%FeOOH(k,i,j)*SEDECO(ng)%H2S(iSt,k,i,j)
      R_TA(k)  = R_TA(k) + 4.0d0*v10b
      R_FeOOH(k) = R_FeOOH(k) - 2.0d0*v10b
      R_Fe2(k)   = R_Fe2(k)   + 2.0d0*v10b
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%H2S(isp,k,i,j)/SEDECO(ng)%H2S(iSt,k,i,j)
        R_H2S(isp,k) = R_H2S(isp,k) - v10b*cff
        R_S0(isp,k)  = R_S0(isp,k)  + v10b*cff
      ENDDO

      ! R11:  4Fe2+ + O2 + 6H2O --> 4FeOOH + 8H+
      v11  = cfdd * k11_Q10*SEDECO(ng)%Fe2(k,i,j) *SEDECO(ng)%O2(k,i,j) 
      R_O2(k)  = R_O2(k) - v11
      R_TA(k)  = R_TA(k) - 8.0d0*v11
      R_Fe2(k)   = R_Fe2(k)   - 4.0d0*v11
      R_FeOOH(k) = R_FeOOH(k) + 4.0d0*v11

      ! R12:  H2S + MnO2 + 4H+ --> S0 + Mn2+ + 2H2O
      v12  = cfsd * k12_Q10*SEDECO(ng)%MnO2(k,i,j)*SEDECO(ng)%H2S(iSt,,k,i,j)
      R_TA(k)  = R_TA(k) + 4.0d0*v12
      R_MnO2(k) = R_MnO2(k) - v12
      R_Mn2(k)  = R_Mn2(k)  + v12
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%H2S(isp,k,i,j)/SEDECO(ng)%H2S(iSt,k,i,j)
        R_H2S(isp,k) = R_H2S(isp,k) - v12*cff
        R_S0(isp,k)  = R_S0(isp,k)  + v12*cff
      ENDDO

      ! R13:  H2S + Fe2+ --> FeS + 2H+
      v13  = cfdd * k13_Q10*SEDECO(ng)%Fe2(k,i,j) *SEDECO(ng)%H2S(iSt,,k,i,j)
      R_TA(k)  = R_TA(k) + 2.0d0*v13
      R_Fe2(k) = R_Fe2(k) - v13
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%H2S(isp,k,i,j)/SEDECO(ng)%H2S(iSt,k,i,j)
        R_H2S(isp,k) = R_H2S(isp,k) - v13*cff
        R_FeS(isp,k) = R_FeS(isp,k) + v13*cff
      ENDDO

      ! R14:  FeS + S0 --> FeS2
      v14  = cfss * k14_Q10*SEDECO(ng)%FeS(iSt,k,i,j)*SEDECO(ng)%S0(iSt,k,i,j)
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%FeS(isp,k,i,j)/SEDECO(ng)%FeS(iSt,k,i,j)
        R_FeS(isp,k) = R_FeS(isp,k) - v14*cff
        cff2 = SEDECO(ng)%S0(isp,k,i,j)/SEDECO(ng)%S0(iSt,k,i,j)
        R_S0(isp,k)  = R_S0(isp,k) - v14*cff2
        R_FeS2(isp,k)= R_FeS2(isp,k) + v14*(cff + cff2)*0.5d0 !!?????????????????????????????????????Check needed!!!
      ENDDO

      ! R15:  SO42- + 3H2S + 4FeS + 2H+ --> 4FeS2 + 4H2O
      v15  = cfsd * k15_Q10*SEDECO(ng)%FeS(iSt,k,i,j)*SEDECO(ng)%H2S(iSt,k,i,j)
      R_TA(k)  = R_TA(k) + 2.0d0*v15
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%SO4(isp,k,i,j)/SEDECO(ng)%SO4(iSt,k,i,j)
        R_SO4(isp,k) = R_SO4(isp,k) - v15*cff
        cff2 = SEDECO(ng)%H2S(isp,k,i,j)/SEDECO(ng)%H2S(iSt,k,i,j)
        R_H2S(isp,k) = R_H2S(isp,k) - v15*cff2
        cff3 = SEDECO(ng)%FeS(isp,k,i,j)/SEDECO(ng)%FeS(iSt,k,i,j)
        R_FeS(isp,k) = R_FeS(isp,k) - v15*cff3
        R_FeS2(isp,k) = R_FeS2(isp,k) + v15*(cff + cff2 + cff3)/3.0d0 !!?????????????????????????????????????Check needed!!!
      ENDDO

      ! R16:  H2S + 2O2 --> SO42- + 2H+
      v16  = cfdd * k16_Q10*SEDECO(ng)%H2S(iSt,k,i,j)*SEDECO(ng)%O2(k,i,j) 
      R_TA(k)  = R_TA(k) + 2.0d0*v16
      R_O2(k)  = R_O2(k) - 2.0d0*v16
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%H2S(isp,k,i,j)/SEDECO(ng)%H2S(iSt,k,i,j)
        R_H2S(isp,k) = R_H2S(isp,k) - v16*cff
        R_SO4(isp,k) = R_SO4(isp,k) + v16*cff
      ENDDO

      ! R17:  FeS + 2O2 --> Fe2+ + SO42-
      v17  = cfsd * k17_Q10*SEDECO(ng)%FeS(iSt,k,i,j)*SEDECO(ng)%O2(k,i,j)
      R_O2(k)  = R_O2(k) - 2.0d0*v17
      R_Fe2(k) = R_Fe2(k) + v17
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%FeS(isp,k,i,j)/SEDECO(ng)%FeS(iSt,k,i,j)
        R_FeS(isp,k) = R_FeS(isp,k) - v17*cff
        R_SO4(isp,k) = R_SO4(isp,k) + v17*cff
      ENDDO

      ! R18:  2FeS2 + 7O2 + 2H2O --> 2Fe2+ + 4SO42- + 4H+
      v18  = cfsd * k18_Q10*SEDECO(ng)%FeS2(iSt,k,i,j)*SEDECO(ng)%O2(k,i,j)
      R_TA(k)  = R_TA(k) - 4.0d0*v18
      R_O2(k)  = R_O2(k) - 7.0d0*v18
      R_Fe2(k) = R_Fe2(k) + 2.0d0*v18
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%FeS2(isp,k,i,j)/SEDECO(ng)%FeS2(iSt,k,i,j)
        R_FeS2(isp,k) = R_FeS2(isp,k) - 2.0d0*v18*cff
        R_SO4(isp,k)  = R_SO4(isp,k)  + 4.0d0*v18*cff
      ENDDO

      ! R19:  4S0 + 4H2O --> 3H2S + SO42- + 2H+
      if (SEDECO(ng)%H2S(k,i,j) <= H2Sstop) then  
        v19  = cfsd * k19_Q10                                                  & 
                *SEDECO(ng)%S0(iSt,k,i,j)*(1d0 - (SEDECO(ng)%H2S(iSt,k,i,j)/H2Sstop))  !!??????????????????????
      else
        v19  = 0.0d0
      endif
      R_TA(k)  = R_TA(k) - 2.0d0*v19
      DO isp=1,N_Ssp
        cff = SEDECO(ng)%S0(isp,k,i,j)/SEDECO(ng)%S0(iSt,k,i,j)
        R_S0(isp,k) = R_S0(isp,k) - 4.0d0*v19*cff
        R_H2S(isp,k) = R_H2S(isp,k)  + 3.0d0*v19*cff !!!??????????????????????????
        R_SO4(isp,k) = R_SO4(isp,k)  + 1.0d0*v19*cff !!!??????????????????????????
      ENDDO

      ! R20:  MnO2 --> MnO2_undeg
      v20  = cfsd * k20_Q10*SEDECO(ng)%MnO2(k,i,j)
      R_MnO2(k) = R_MnO2(k) - v20 
      ! R21:  FeOOH --> FeOOH_undeg
      v21  = cfsd * k21_Q10*SEDECO(ng)%FeOOH(k,i,j)
      R_FeOOH(k) = R_FeOOH(k) - v21 

    ENDDO
    
! ********************************************************************* !
!   Miscellaneous other sources
! ********************************************************************* !
!!! yuta_seagrass >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>YT:Add
# if defined SEAGRASS
    DO k = 1,Nsed
#  if defined SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! [umol/L s] = [umol/L s]
      R_O2 (k) = R_O2 (k) + sum(SEDECO(ng)%sg_dO2_dt (:,k,i,j),DIM=1) ! (umol L-1 s-1) change in DO  due to flux from seagrass (positive means seagrass to sediment)
      DO isp=1,N_Csp
        R_DIC(isp,k) = R_DIC(isp,k) + sum(SEDECO(ng)%sg_dDIC_dt(:,isp,k,i,j),DIM=1) ! (umol L-1 s-1) change in DIC due to flux from seagrass (positive means seagrass to sediment)
      ENDDO
#  endif
#  if defined SEAGRASS_ROOT_NUTRIENT_UPTAKE
      DO isp=1,N_Nsp
        R_NO3(isp,k) = R_NO3(isp,k) + sum(SEDECO(ng)%sg_dNO3_dt(:,isp,k,i,j),DIM=1) ! (umol L-1 s-1) change in NO3 due to flux from seagrass (positive means seagrass to sediment)
        R_NH4(isp,k) = R_NH4(isp,k) + sum(SEDECO(ng)%sg_dNH4_dt(:,isp,k,i,j),DIM=1) ! (umol L-1 s-1) change in NH4 due to flux from seagrass (positive means seagrass to sediment)
      ENDDO
      DO isp=1,N_Psp
        R_PO4(isp,k) = R_PO4(isp,k) + sum(SEDECO(ng)%sg_dPO4_dt(:,isp,k,i,j),DIM=1) ! (umol L-1 s-1) change in PO4 due to flux from seagrass (positive means seagrass to sediment)
      ENDDO
#  endif    
#  if defined SEAGRASS_ROOT_POM
    ! [nmol/g s] = [nmol/g s]   +            [umol/L s]                                [cm3/g] (1000 nmol/umol) (L/1000 cm3)
      DO isp=1,N_Csp
        R_POC(isp,iLPOM,k) = R_POC(isp,iLPOM,k) + OMFratio * sum(SEDECO(ng)%sg_dPOC_dt(:,isp,k,i,j),DIM=1) / rhos_d(k) ! (nmol g-1 s-1) change in POC labile due to root death in sediment (positive means seagrass to sediment)
        R_POC(isp,iRPOM,k) = R_POC(isp,iRPOM,k) + OMSratio * sum(SEDECO(ng)%sg_dPOC_dt(:,isp,k,i,j),DIM=1) / rhos_d(k) ! (nmol g-1 s-1) change in POC refractory due to root death in sediment (positive means seagrass to sediment)
      ENDDO
#   if defined SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined  SEAGRASS_ROOT_NUTRIENT_UPTAKE
      DO isp=1,N_Nsp
        R_PON(isp,iLPOM,k) = R_PON(isp,iLPOM,k) + OMFratio * sum(SEDECO(ng)%sg_dPON_dt(:,isp,k,i,j),DIM=1) / rhos_d(k) ! (nmol g-1 s-1) change in PON labile due to root death in sediment (positive means seagrass to sediment)
        R_PON(isp,iRPOM,k) = R_PON(isp,iRPOM,k) + OMSratio * sum(SEDECO(ng)%sg_dPON_dt(:,isp,k,i,j),DIM=1) / rhos_d(k) ! (nmol g-1 s-1) change in PON refractory due to root death in sediment (positive means seagrass to sediment)
      ENDDO
      DO isp=1,N_Psp
        R_POP(isp,iLPOM,k) = R_POP(isp,iLPOM,k) + OMFratio * sum(SEDECO(ng)%sg_dPOP_dt(:,isp,k,i,j),DIM=1) / rhos_d(k) ! (nmol g-1 s-1) change in POP labile due to root death in sediment (positive means seagrass to sediment)
        R_POP(isp,iRPOM,k) = R_POP(isp,iRPOM,k) + OMSratio * sum(SEDECO(ng)%sg_dPOP_dt(:,isp,k,i,j),DIM=1) / rhos_d(k) ! (nmol g-1 s-1) change in POP refractory due to root death in sediment (positive means seagrass to sediment)
      ENDDO
#   endif  
#  endif  
    ENDDO
# endif
!!!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<YT:Add


! ********************************************************************* !
!   Mass balance equations
! ********************************************************************* !

    !! Mass Balance cal (Concentration at each layer in sed column)
    DO k = 1,Nsed
        ! -- O2 -- !
        SEDECO(ng)%O2(k,i,j)  = massbalance_sed_d(SEDECO(ng)%O2(k,i,j),FT_O2(k),FT_O2(k+1),R_O2(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscO2(k))
        ! -- DIC -- !
        SEDECO(ng)%DIC(k,i,j) = massbalance_sed_d(SEDECO(ng)%DIC(k,i,j),FT_DIC(k),FT_DIC(k+1),R_DIC(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscDIC(k))
        ! -- N2 -- !
!        SEDECO(ng)%N2(k,i,j)  = massbalance_sed_d(SEDECO(ng)%N2(k,i,j),FT_N2(k),FT_N2(k+1),R_N2(k), & 
!                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscN2(k))
# if defined ORGANIC_MATTER
        ! -- DOC -- !
        SEDECO(ng)%DOCf(k,i,j) = massbalance_sed_d(SEDECO(ng)%DOCf(k,i,j),FT_DOC(k,1),FT_DOC(k+1,1),R_DOC(k,1), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscDOC(k,1))  ! DOCf    
        SEDECO(ng)%DOCs(k,i,j) = massbalance_sed_d(SEDECO(ng)%DOCs(k,i,j),FT_DOC(k,2),FT_DOC(k+1,2),R_DOC(k,2), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscDOC(k,2))  ! DOCs
        ! -- POC -- !
        SEDECO(ng)%POCf(k,i,j) = massbalance_sed_p(SEDECO(ng)%POCf(k,i,j),FT_POC(k,1),FT_POC(k+1,1),R_POC(k,1), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPOC(k,1))  ! POCf
        SEDECO(ng)%POCs(k,i,j) = massbalance_sed_p(SEDECO(ng)%POCs(k,i,j),FT_POC(k,2),FT_POC(k+1,2),R_POC(k,2), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPOC(k,2))  ! POCs
        SEDECO(ng)%POCn(k,i,j) = massbalance_sed_p(SEDECO(ng)%POCn(k,i,j),FT_POC(k,3),FT_POC(k+1,3),0d0, &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPOC(k,3))  ! POCn
# endif
# if defined NUTRIENTS
#  if defined adsorption
        ! -- NO3 -- !
        SEDECO(ng)%NO3(k,i,j) = massbalance_sed_d(SEDECO(ng)%NO3(k,i,j),FT_NO3(k),FT_NO3(k+1),R_NO3(k), &
                                            dz(k),(SEDECO(ng)%poro(k,i,j)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(k,i,j))*Kad_NO3),dt,miscNO3(k)) 
        ! -- NH4 -- !
        SEDECO(ng)%NH4(k,i,j) = massbalance_sed_d(SEDECO(ng)%NH4(k,i,j),FT_NH4(k),FT_NH4(k+1),R_NH4(k), &
                                            dz(k),(SEDECO(ng)%poro(k,i,j)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(k,i,j))*Kad_NH4),dt,miscNH4(k)) 
        ! -- PO4 -- !
        SEDECO(ng)%PO4(k,i,j) = massbalance_sed_d(SEDECO(ng)%PO4(k,i,j),FT_PO4(k),FT_PO4(k+1),R_PO4(k), &
                                            dz(k),(SEDECO(ng)%poro(k,i,j)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(k,i,j))*Kad_PO4),dt,miscPO4(k))
#  else
        ! -- NO3 -- !
        SEDECO(ng)%NO3(k,i,j) = massbalance_sed_d(SEDECO(ng)%NO3(k,i,j),FT_NO3(k),FT_NO3(k+1),R_NO3(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscNO3(k)) 
        ! -- NH4 -- !
        SEDECO(ng)%NH4(k,i,j) = massbalance_sed_d(SEDECO(ng)%NH4(k,i,j),FT_NH4(k),FT_NH4(k+1),R_NH4(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscNH4(k)) 
        ! -- PO4 -- !
        SEDECO(ng)%PO4(k,i,j) = massbalance_sed_d(SEDECO(ng)%PO4(k,i,j),FT_PO4(k),FT_PO4(k+1),R_PO4(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscPO4(k))
#  endif
#  if defined ORGANIC_MATTER

        ! -- DON -- !
        SEDECO(ng)%DONf(k,i,j) = massbalance_sed_d(SEDECO(ng)%DONf(k,i,j),FT_DON(k,1),FT_DON(k+1,1),R_DON(k,1), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscDON(k,1))  ! DONf
        SEDECO(ng)%DONs(k,i,j) = massbalance_sed_d(SEDECO(ng)%DONs(k,i,j),FT_DON(k,2),FT_DON(k+1,2),R_DON(k,2), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscDON(k,2))  ! DONs
        ! -- PON -- !
        SEDECO(ng)%PONf(k,i,j) = massbalance_sed_p(SEDECO(ng)%PONf(k,i,j),FT_PON(k,1),FT_PON(k+1,1),R_PON(k,1), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPON(k,1))  ! PONf
        SEDECO(ng)%PONs(k,i,j) = massbalance_sed_p(SEDECO(ng)%PONs(k,i,j),FT_PON(k,2),FT_PON(k+1,2),R_PON(k,2), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPON(k,2))  ! PONs
        SEDECO(ng)%PONn(k,i,j) = massbalance_sed_p(SEDECO(ng)%PONn(k,i,j),FT_PON(k,3),FT_PON(k+1,3),0d0, &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPON(k,3))  ! PONn
        ! -- DOP -- !
        SEDECO(ng)%DOPf(k,i,j) = massbalance_sed_d(SEDECO(ng)%DOPf(k,i,j),FT_DOP(k,1),FT_DOP(k+1,1),R_DOP(k,1), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscDOP(k,1))  ! DOPf
        SEDECO(ng)%DOPs(k,i,j) = massbalance_sed_d(SEDECO(ng)%DOPs(k,i,j),FT_DOP(k,2),FT_DOP(k+1,2),R_DOP(k,2), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscDOP(k,2))  ! DOPs
        ! -- POP -- !
        SEDECO(ng)%POPf(k,i,j) = massbalance_sed_p(SEDECO(ng)%POPf(k,i,j),FT_POP(k,1),FT_POP(k+1,1),R_POP(k,1), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPOP(k,1))  ! POPf
        SEDECO(ng)%POPs(k,i,j) = massbalance_sed_p(SEDECO(ng)%POPs(k,i,j),FT_POP(k,2),FT_POP(k+1,2),R_POP(k,2), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPOP(k,2))  ! POPs
        SEDECO(ng)%POPn(k,i,j) = massbalance_sed_p(SEDECO(ng)%POPn(k,i,j),FT_POP(k,3),FT_POP(k+1,3),0d0, &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscPOP(k,3))  ! POPn
#  endif
# endif
# if defined SULFATE
#  if defined adsorption
        ! -- Mn2 -- !
        SEDECO(ng)%Mn2(k,i,j) = massbalance_sed_d(SEDECO(ng)%Mn2(k,i,j),FT_Mn2(k),FT_Mn2(k+1),R_Mn2(k), &
                                            dz(k),(SEDECO(ng)%poro(k,i,j)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(k,i,j))*Kad_Mn2),dt,miscMn2(k)) 
        ! -- Fe2 -- !
        SEDECO(ng)%Fe2(k,i,j) = massbalance_sed_d(SEDECO(ng)%Fe2(k,i,j),FT_Fe2(k),FT_Fe2(k+1),R_Fe2(k), &
                                            dz(k),(SEDECO(ng)%poro(k,i,j)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(k,i,j))*Kad_Fe2),dt,miscFe2(k)) 
#  else
        ! -- Mn2 -- !
        SEDECO(ng)%Mn2(k,i,j) = massbalance_sed_d(SEDECO(ng)%Mn2(k,i,j),FT_Mn2(k),FT_Mn2(k+1),R_Mn2(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscMn2(k)) 
        ! -- Fe2 -- !
        SEDECO(ng)%Fe2(k,i,j) = massbalance_sed_d(SEDECO(ng)%Fe2(k,i,j),FT_Fe2(k),FT_Fe2(k+1),R_Fe2(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscFe2(k)) 
#  endif
        ! -- MnO2 -- !
        SEDECO(ng)%MnO2(k,i,j) = massbalance_sed_p(SEDECO(ng)%MnO2(k,i,j),FT_MnO2(k),FT_MnO2(k+1),R_MnO2(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscMnO2(k)) 
        ! -- FeS -- !
        SEDECO(ng)%FeS(k,i,j) = massbalance_sed_p(SEDECO(ng)%FeS(k,i,j),FT_FeS(k),FT_FeS(k+1),R_FeS(k),  &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscFeS(k)) 
        ! -- FeS2 -- !
        SEDECO(ng)%FeS2(k,i,j) = massbalance_sed_p(SEDECO(ng)%FeS2(k,i,j),FT_FeS2(k),FT_FeS2(k+1),R_FeS2(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscFeS2(k)) 
        ! -- FeOOH -- !
        SEDECO(ng)%FeOOH(k,i,j) = massbalance_sed_p(SEDECO(ng)%FeOOH(k,i,j),FT_FeOOH(k),FT_FeOOH(k+1),R_FeOOH(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscFeOOH(k)) 
        ! -- FeOOH=PO4 -- !
        SEDECO(ng)%FeOOH_PO4(k,i,j) = massbalance_sed_p(SEDECO(ng)%FeOOH_PO4(k,i,j),FT_FeOOH_PO4(k),FT_FeOOH_PO4(k+1), & 
                                                  R_FeOOH_PO4(k),dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscFeOOH_PO4(k)) 
        ! -- H2S -- !
        SEDECO(ng)%H2S(k,i,j) = massbalance_sed_d(SEDECO(ng)%H2S(k,i,j),FT_H2S(k),FT_H2S(k+1),R_H2S(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscH2S(k)) 
         ! -- S0 -- !
        SEDECO(ng)%S0(k,i,j) = massbalance_sed_p(SEDECO(ng)%S0(k,i,j),FT_S0(k),FT_S0(k+1),R_S0(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,rhos_d(k),miscS0(k)) 
        ! -- SO4 -- !
        SEDECO(ng)%SO4(k,i,j) = massbalance_sed_d(SEDECO(ng)%SO4(k,i,j),FT_SO4(k),FT_SO4(k+1),R_SO4(k), &
                                                  dz(k),SEDECO(ng)%poro(k,i,j),dt,miscSO4(k))

# endif
    ENDDO


! ********************************************************************* !
!   set output parameters
! ********************************************************************* !
# if defined SEDECO_ADVECTION
    Flux_Tmp = Fw_Tmp(1)
    Flux_Sal = Fw_Sal(1) 
    Flux_DO  = Fw_O2 (1) 
    Flux_TA  = Fw_TA (1) 
    Flux_DIC(:) = Fw_DIC(:,1)
# if defined ORGANIC_MATTER
    Flux_POC(:) = Fw_POC(:,1)
# endif
# if defined NUTRIENTS
    Flux_NO3(:) = Fw_NO3(:,1)
    Flux_NH4(:) = Fw_NH4(:,1)
    Flux_PO4(:) = Fw_PO4(:,1)
#  if defined ORGANIC_MATTER
    Flux_PON(:) = Fw_PON(:,1)
    Flux_POP(:) = Fw_POP(:,1)
#  endif
# endif
# if defined SULFATE
    Flux_Mn2  = Fw_Mn2 (1)
    Flux_MnO2 = Fw_MnO2(1)
    Flux_Fe2  = Fw_Fe2 (1)
    ! Flux_FeS (:) = Fw_FeS (:,1)  ! yt_edit diffusion is not modeled so flux is zero
    ! Flux_FeS2(:) = Fw_FeS2(:,1)  ! yt_edit diffusion is not modeled so flux is zero
    Flux_FeOOH = Fw_FeOOH(1)
    ! Flux_FeOOH_PO4 = Fw_FeOOH_PO4(:,1)  ! yt_edit diffusion is not modeled so flux is zero
    Flux_H2S(:) = Fw_H2S(:,1)
    ! Flux_S0(:)  = Fw_S0(:,1)  ! yt_edit diffusion is not modeled so flux is zero
    Flux_SO4(:) = Fw_SO4(:,1)
# endif

! ********************************************************************* !
!   Print section
! ********************************************************************* !
# if defined SEDIMENT_TESTMODE
    IF( date*86400.0d0 >= dsec ) THEN
        if(dsec == 0.0d0) then
          dsec=date*86400.0d0+OUTPUT_INTERVAL*60.0d0
        else
          dsec=dsec+OUTPUT_INTERVAL*60.0d0
        endif
        
        write(58,*) date, SEDECO(ng)%O2(i,j,:)
        write(80,*) date, SEDECO(ng)%DIC(i,j,:)
!        write(81,*) date, SEDECO(ng)%N2(i,j,:)
#  if defined ORGANIC_MATTER
        write(65,*) date, SEDECO(ng)%DOCf(i,j,:)
        write(82,*) date, SEDECO(ng)%DOCs(i,j,:)
        write(66,*) date, SEDECO(ng)%POCf(i,j,:)
        write(83,*) date, SEDECO(ng)%POCs(i,j,:)
        write(100,*) date, SEDECO(ng)%POCn(i,j,:)
#  endif
#  if defined NUTRIENTS
        write(61,*) date, SEDECO(ng)%NH4(i,j,:)
        write(63,*) date, SEDECO(ng)%NO3(i,j,:)
        write(64,*) date, SEDECO(ng)%PO4(i,j,:)
    ! #   if defined ORGANIC_MATTER
    !       write(67,*) date, SEDECO(ng)%DON(i,j,:)
    !       write(68,*) date, SEDECO(ng)%PON(i,j,:)
    !       write(69,*) date, SEDECO(ng)%DOP(i,j,:)
    !       write(70,*) date, SEDECO(ng)%POP(i,j,:)
    ! #   endif
#  endif
# if defined SULFATE
        write(84,*) date, SEDECO(ng)%Mn2(i,j,:)
        write(85,*) date, SEDECO(ng)%MnO2(i,j,:)
        write(86,*) date, SEDECO(ng)%Fe2(i,j,:)
        write(87,*) date, SEDECO(ng)%FeS(i,j,:)
        write(88,*) date, SEDECO(ng)%FeS2(i,j,:)
        write(89,*) date, SEDECO(ng)%FeOOH(i,j,:)
        write(90,*) date, SEDECO(ng)%FeOOH_PO4(i,j,:)
        write(91,*) date, SEDECO(ng)%H2S(i,j,:)
        write(93,*) date, SEDECO(ng)%S0(i,j,:)
        write(94,*) date, SEDECO(ng)%SO4(i,j,:)
# endif
            !! for debug !!
        !  write(96,'(*(g0:,", "))') date, Tamb, Samb, O2amb   &
        !                         ,Dw_O2(0), 0d0, Fd_O2(0), 0d0, 0d0, FT_O2(0), 0d0, 0d0
        !  write(97,'(*(g0:,", "))') date, SEDECO(ng)%Tmp(i,j,1), SEDECO(ng)%Sal(i,j,1), SEDECO(ng)%O2(i,j,1)    &
        !                         ,Dw_O2(1), Ds_O2(1), Fd_O2(1), Fb_O2(1), Fc_O2(1), FT_O2(1), v1(1), R_O2(1)
        ! write(96,'(*(g0:,", "))') date, SEDECO(ng)%Tmp(i,j,1), SEDECO(ng)%Sal(i,j,1), SEDECO(ng)%DOCf(i,j,1)    &
        !                         ,Dw_DOC(1,1), 0d0, Fd_DOC(1,1), 0d0, 0d0, FT_DOC(1,1), 0d0, 0d0
        ! write(97,'(*(g0:,", "))') date, SEDECO(ng)%Tmp(i,j,2), SEDECO(ng)%Sal(i,j,2), SEDECO(ng)%DOCf(i,j,2)    &
        !                         ,Dw_DOC(2,1), Ds_DOC(2,1), Fd_DOC(2,1), Fb_DOC(2,1), Fc_DOC(2,1), FT_DOC(2,1), vdom(2,1), R_DOC(2,1)
        ! write(97,'(*(g0:,", "))') date, vOM(10), v1(10),v2(10),v3(10),v4(10),v5(10)
        ! write(98,*) date, redoxlevel(:) !! for debug
        write(99,*) dz(:), z(:), SEDECO(ng)%poro(i,j,:)
        write(101,*) date, v1(:)
        write(102,*) date, v2(:)
        write(103,*) date, v3(:)
        write(104,*) date, v4(:)
        write(105,*) date, v5(:)
!        write(106,*) date, vOM(:)
        write(107,*) date, R_O2(:)   !! for debug
        write(108,*) date, Flux_DO   !! for debug
        ! endif
        write(*,*) 'Time (day): ', date 
    endif
# endif

    RETURN

  END SUBROUTINE sedecosys

!!!***********************************************************************
!!!   Sediment biogeochemical parameters
!!!*********************************************************************** 
!     Added by Masa Muto (2023)

!! Ficks law (diffusion)
  ! General
  real(8) function Ficks(D,C1,C2,dz)
    implicit none
    real(8), intent(in) :: D
    real(8), intent(in) :: C1
    real(8), intent(in) :: C2
    real(8), intent(in) :: dz
    Ficks =  D * (C1 - C2)/dz
    return
  end function Ficks

!! --- Upstream Advection Flux calculation ------------------------------------------ !!
  real(8) function AdvUp(u,C1,C2)
    implicit none
    real(8), intent(in) :: u
    real(8), intent(in) :: C1
    real(8), intent(in) :: C2
    ! if      u>0, AdvUp = u*C1  
    ! else if u<0, AdvUp = u*C2
    AdvUp = ((u+abs(u))*C1 + (u-abs(u))*C2)*0.5d0
    return
  end function AdvUp

!! --- Mass balance calculation in sediment column ---------------------------- !!
  !! solutes (dissolved)
  real(8) function massbalance_sed_d(C,FTin,FTout,R,dz,poro,dt)
    implicit none
    real(8),intent(in) :: C             ! concentration                   (µmol L-1 = nmol cm-3)
    real(8),intent(in) :: FTin          ! total flux (IN)                 (nmol cm-2 s-1)
    real(8),intent(in) :: FTout         ! total flux (OUT)                (nmol cm-2 s-1)
    real(8),intent(in) :: R             ! Net prodution and consumption   (nmol cm-3 s-1)
    real(8),intent(in) :: dz            ! layer thickness                 (cm)
    real(8),intent(in) :: poro          ! porosity                        (cm_w3 cm_s-3)
    real(8),intent(in) :: dt            ! time step                       (s)
    real(8) :: Ccal

    Ccal = C + ((FTin - FTout)/dz + R)/poro * dt
!  [nmol cm_w-3] = [nmol cm_w-3] + ([nmol cm_s-2 s-1]/(cm_s) + [nmol cm_s-3 s-1])/[cm_w3 cm_s-3] * [s] 
    Ccal = max(Ccal,0.0d0)
    ! Ccal = min(Ccal,3.0d4)
    ! Ccal = min(Ccal,1.0d4)
    ! massbalance_sed_d = min(Ccal,1.0d4)
    massbalance_sed_d = Ccal

    return
  end function massbalance_sed_d

  !! solids (particulate)
  real(8) function massbalance_sed_p(C,FTin,FTout,R,dz,poro,dt,rho_s)
    implicit none
    real(8),intent(in) :: C             ! concentration                  (nmol g-1)
    real(8),intent(in) :: FTin          ! total flux  (IN)               (nmol cm-2 s-1)
    real(8),intent(in) :: FTout         ! total flux  (OUT)              (nmol cm-2 s-1)
    real(8),intent(in) :: R             ! Net prodution and consumption  (nmol cm-3 s-1)
    real(8),intent(in) :: dz            ! layer thickness                (cm)
    real(8),intent(in) :: poro          ! porosity                       (cm_w3 cm_s-3)
    real(8),intent(in) :: dt            ! time step                      (s)
    real(8),intent(in) :: rho_s         ! soil(wet) density              (g cm_w-3)
    real(8) :: Ccal

    Ccal = C + ((FTin -FTout)/dz + R)/(rho_s*(1.0d0-poro)) * dt
  ! [nmol g-1] = [nmol g-1] + ([nmol cm-2 s-1]/[cm] + [nmol cm-3 s-1])/([g cm_w-3] * [cm_w3 cm_s-3]) * [s]
    Ccal = max(Ccal,0.0d0)
    ! Ccal = min(Ccal,1.0d4)
    ! Ccal = min(Ccal,1.0d7)
    ! massbalance_sed_p = min(Ccal,1.0d4)
    massbalance_sed_p = Ccal

    return
  end function massbalance_sed_p

#endif
END MODULE mod_sedecosys
