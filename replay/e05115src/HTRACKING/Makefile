include ../Makefile.in

R_OBJS=r_hks_aero_parms.o r_hks_bypass_switches.o \
       r_hks_filenames.o r_hks_geometry.o r_hks_id_histid.o \
       r_hks_water_parms.o r_hks_pedestals.o r_hks_physics_sing.o \
       r_hks_recon_elements.o r_hks_scin_parms.o r_hks_scin_tof.o \
       r_hks_statistics.o r_hks_tracking.o \
       r_hks_phys_histid.o r_hks_lucite_parms.o

H_OBJS=h_aero.o h_analyze_pedestal.o \
       h_analyze_tul.o h_calc_pedestal.o \
       h_calc_trigtime.o h_chamnum.o h_coll_trans.o \
       h_choose_single_hit.o h_dc_eff.o h_dc_eff_shutdown.o \
       h_dc_trk_eff.o h_dc_trk_eff_shutdown.o h_dpsifun.o \
       h_drift_dist_calc.o h_drift_time_calc.o h_dump_peds.o \
       h_dump_tof.o h_fcnchisq.o \
       h_fill_aero_hist.o h_fill_aer_raw_hist.o \
       h_fill_dc_dec_hist.o h_fill_dc_raw_hist.o \
       h_fill_dc_sp_hist.o h_fill_target_hist.o \
       h_fill_dc_track_hist.o h_fill_phys_hist.o \
       h_fill_water_hist.o h_fill_water_raw_hist.o \
       h_fill_scin_raw_hist.o h_fill_scin_dec_hist.o \
       h_find_best_stub.o h_find_easy_space_point.o \
       h_find_space_points.o h_generate_geometry.o \
       h_init_histid.o h_init_physics.o \
       h_init_scin.o h_initialize_fitting.o h_left_right.o \
       h_link_stubs.o h_link_tracks.o h_pattern_recognition.o \
       h_physics.o h_physics_stat.o \
       h_print_decoded_dc.o h_print_links.o h_print_pr.o h_print_raw_dc.o \
       h_print_stubs.o h_print_tar_tracks.o h_print_tracks.o \
       h_prt_dec_scin.o h_prt_raw_aero.o h_prt_raw_water.o \
       h_prt_raw_scin.o h_prt_tof.o h_prt_track_tests.o \
       h_psifun.o h_raw_dump_all.o h_reconstruction.o h_register_param.o \
       h_report_bad_data.o h_scin_eff.o h_scin_eff_shutdown.o \
       h_select_good_tracks.o h_solve_3by3.o \
       h_targ_trans.o h_targ_trans_init.o \
       h_targ_trans_rt.o h_targ_trans_init_rt.o \
       h_tof.o h_track.o h_track_fit.o \
       h_trans_dc.o h_trans_dc_group.o h_trans_misc.o h_trans_scin.o \
       h_wire_center_calc.o h_water.o h_lucite.o \
       h_fill_lucite_hist.o h_fill_lucite_raw_hist.o h_prt_raw_lucite.o\
       h_tof_pre.o h_track_selection_pre.o h_select_good_combination.o \
       h_dc_tofcut.o h_dc_eff_count.o h_dc_eff_out.o

ALLOBJS= $(R_OBJS) $(H_OBJS)

LIB= libhtracking.a

#

F77=/usr/bin/gfortran

FFLAGS= -g -ffixed-line-length-132 -fallow-argument-mismatch
#FFLAGS= -m32 -g -ffixed-line-length-132 -fbounds-check
#FFLAGS= -g -ffixed-line-length-132 -fbounds-check

INC=-I../INCLUDE/

%.o:%.f
	$(F77) $(FFLAGS) $(INC) -c $< -o $@
#


none: $(LIB)

all: $(LIB)

$(LIB): $(ALLOBJS)
	$(AR) r $@ $?

install:
	cp $(LIB) $(LIBROOT)

clean:
	$(RM) *.o $(LIB) r_*.f
