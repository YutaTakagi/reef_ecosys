/*#define USE_HEAT*/
/*#define LONGWAVE_IN*/
/*#define INPUT_BOTTOM_PFD*/

/*#define REEF_FLOW*/

/*** submarine groundwater discharge ***/

#define SGD_ON    /*Original CPP flag */

/***  Biological model options. (Original CPP flags) ***/

/***  Biological model options. (Original CPP flags) ***/

#define REEF_ECOSYS

#if defined REEF_ECOSYS || defined SEDIMENT
# define ANA_TOBC_BIO  /*Original CPP flag */
# define ANA_TOBC_SED   /*Original CPP flag */
/*# define BIO_VPROFILE_YAEYAMA*/   /*Original CPP flag */
#endif

#if defined REEF_ECOSYS
# define BIOLOGY
# define DIAGNOSTICS_BIO
# define ANA_BIOLOGY

/* compartments */
# define ORGANIC_MATTER
/*# define CARBON_ISOTOPE*/
# define NUTRIENTS

/*# define CORAL_POLYP*/  /* USE coral module */
/*# define SEAGRASS*/     /* USE seagrass module */
/*# define MACROALGAE*/        /* USE algae module  */
# define SEDIMENT_ECOSYS        /* USE sedecosys module  */

# if defined ORGANIC_MATTER
#  define FOODWEB      /* USE foodweb module */
# endif
# define AIR_SEA_GAS_EXCHANGE

/*# define DYNAMIC_COVERAGE*/ /* yt_edit not yet implemented in coawst */


/*** Coral Polyp model options. ***/
# if defined CORAL_POLYP
#  define CORAL_ZOOXANTHELLAE
#  define CORAL_MUCUS           /*Mucus release from coral */
#  if defined ORGANIC_MATTER
#   define CORAL_INGESTION
#  endif
/*#  define CORAL_SIZE_DYNAMICS*/
#  if defined CARBON_ISOTOPE
#   define CORAL_CARBON_ISOTOPE
/*#   define CORAL_NONE_CO2_EQ*/
#  endif
#  if defined NUTRIENTS
/*#   define CORAL_NUTRIENTS*/
#  endif
/*#  define CORAL_BORON_ISOTOPE*/
# endif


/*** Seagrass model options. ***/
# if defined SEAGRASS
#  if defined NUTRIENTS
#   define SEAGRASS_LEAF_NUTRIENT_UPTAKE
#  endif
#  if defined SEDIMENT_ECOSYS
/*#   define SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE*/
#  endif
#  if defined NUTRIENTS && defined SEDIMENT_ECOSYS
#   define SEAGRASS_ROOT_NUTRIENT_UPTAKE
#  endif
#  if defined ORGANIC_MATTER
#   define SEAGRASS_LEAF_POM
#   if defined SEDIMENT_ECOSYS
#    define SEAGRASS_ROOT_POM
#   endif
#  endif
# endif


/*** Sediment model options. ***/
# if defined SEDIMENT_ECOSYS  /* Masa_edits */
/*#  define SEDIMENT_EMPIRICAL*/     /* USE empirical sediment module  */
#  define SEDECO_CLOSED_BOTTOM_DIFFUSION_BOUNDARY /* closed boundary condition at the bottom sediment layer */
#  define SULFATE      /* For sulfate reduction in sediment */
/*#  define SEDECO_BURIAL*/    /* For Burial term in sediment transport (massbalance) */
/*#  define SEDECO_ADVECTION*/
#  define ORGANIC_MATTER
#  define NUTRIENTS
# endif


#endif
/*----------------------------------------------------*/
/*** Box model option ***/

#define ECOSYS_TESTMODE

#if defined CORAL_POLYP
# define CORAL_TESTMODE
#endif
#if defined SEDIMENT_ECOSYS
# define SEDIMENT_TESTMODE
#endif
#if defined MM_SEDIMENT_ECOSYS
# define SEDIMENT_TESTMODE
#endif
#if defined FOODWEB 
# define FOODWEB_TESTMODE
#endif
/*** Chamber experiments option ***/
/*#define CHAMBER_SITE4*/

/*----------------------------------------------------*/

