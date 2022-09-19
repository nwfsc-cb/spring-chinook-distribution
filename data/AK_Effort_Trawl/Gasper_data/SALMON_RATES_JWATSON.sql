SELECT a.catch_report_type_code,
a.year,
a.el_report_id,
a.trip_target_date,
r.landing_date,
r.port_code,
a.processing_sector,
a.catcher_vessel_id,
a.management_program_code,
a.agency_gear_code,
a.adfg_stat_area_code,
a.reporting_area_code,
a.pscnq_estimate as estimated_count,
A.rate_precedence,A.rate,
A.retained_gf_basis_weight AS RETAINED_GF_WEIGHT,
a.total_gf_basis_weight,
a.rate*a.total_gf_basis_weight as manual_CALC_example

  FROM akfish_report.v_cas_txn_primary_psc a
  left JOIN  akfish_report.v_ellr_report r on R.report_id=a.el_report_id
  where a.year=2018

  and a.species_group_code='CHNK'
  and a.FMP_SUB_AREA_CODE in ('GOA','AI')
  and a.agency_gear_code in ('NPT','PTR')


  
  

  
  
