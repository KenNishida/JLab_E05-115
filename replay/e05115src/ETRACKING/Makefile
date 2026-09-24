include ../Makefile.in

E_OBJS=e_analyze_pedestal.o e_analyze_tul.o \
       e_calc_pedestal.o \
       e_calc_trigtime.o \
       e_coll_trans.o \
       e_dc1_cleanup.o \
       e_dc1_dpsifun.o \
       e_dc1_find_best_stub.o \
       e_dc1_generate_geometry.o \
       e_dc1_layer_eff.o \
       e_dc1_left_right.o \
       e_dc1_pattern_recognition.o \
       e_dc1_pre_link.o \
       e_dc1_print_tracks.o \
       e_dc1_track_fit.o \
       e_dc2_drift_dist_calc.o\
       e_dc2_left_right.o\
       e_dc2_choose_single_hit.o \
       e_dc2_find_best_stub.o \
       e_dc2_find_easy_space_point.o \
       e_dc2_generate_geometry.o \
       e_dc2_pattern_recognition.o \
       e_dc2_track_fit.o \
       e_dump_peds.o \
       e_drift_dist_calc.o e_drift_dist_calc2.o \
       e_fill_dc1_raw_hist.o \
       e_fill_dc1_dec_hist.o \
       e_fill_dc1_pretrk_hist.o \
       e_fill_dc1_sp_hist.o \
       e_fill_dc1_track_hist.o \
       e_fill_dc2_dec_hist.o \
       e_fill_dc2_pretrk_hist.o \
       e_fill_dc2_raw_hist.o \
       e_fill_dc2_sp_hist.o \
       e_fill_dc2_track_hist.o \
       e_fill_link_hist.o e_fill_phys_hist.o \
       e_fill_scin_dec_hist.o e_fill_scin_raw_hist.o \
       e_fill_target_hist.o \
       e_find_space_points.o \
       e_init_histid.o \
       e_init_physics.o \
       e_link_tracks.o \
       e_physics.o \
       e_physics_stat.o \
       e_print_dc1_stubs.o \
       e_print_dc2_stubs.o \
       e_print_tar_tracks.o \
       e_prt_physics.o \
       e_prt_dec_scin.o \
       e_raw_dump_all.o e_reconstruction.o \
       e_register_param.o \
       e_select_good_tracks.o \
       e_scin_eff.o \
       e_scin_eff_shutdown.o \
       e_targ_trans.o \
       e_targ_trans_init.o \
       e_track.o \
       e_trans_misc.o \
       e_trans_scin.o \
       e_trans_dc1.o e_trans_dc2.o \
       e_dc_eff_count.o e_dc_eff_out.o

R_OBJS=r_hes_bypass_swiches.o \
       r_hes_filenames.o r_hes_geometry.o \
       r_hes_id_histid.o \
       r_hes_pedestals.o r_hes_physics_sing.o \
       r_hes_recon_elements.o \
       r_hes_statistics.o \
       r_hes_scin_parms.o r_hes_tracking.o 


ALLOBJS= $(E_OBJS) $(R_OBJS)

LIB= libetracking.a

F77=/usr/bin/gfortran

FFLAGS= -g -ffixed-line-length-132
#FFLAGS= -m32 -g -ffixed-line-length-132 -fbounds-check -p
#FFLAGS= -g -ffixed-line-length-132 -fbounds-check -p

INC=-I../INCLUDE/

%.o:%.f
	$(F77) $(FFLAGS) $(INC) -c $< -o $@

none: $(LIB)

all: $(LIB)

$(LIB): $(ALLOBJS)
	$(AR) r $@ $?

install:
	cp $(LIB) $(LIBROOT)

clean:
	$(RM) *.o $(LIB) r_*.f
