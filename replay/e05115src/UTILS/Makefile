include ../Makefile.in

ALLOBJS= g_add_path.o g_append.o g_build_note.o g_important_length.o \
         g_int_sort.o g_io_control.o g_log_message.o g_normalize.o \
         g_prepend.o g_reg_c.o g_rep_err.o g_rep_where.o g_shift_len.o \
         g_sph_xyz.o g_sub_run_number.o g_utc_date.o g_wrap_note.o \
         g_xyz_sph.o \
         clear_after_null.o data_row.o get_values.o match.o no_blanks.o \
         no_comments.o no_leading_blanks.o no_nulls.o no_tabs.o \
         only_one_blank.o regparmstringarray.o shiftall.o squeeze.o \
         sub_string.o up_case.o up_shift.o


LIB= libutils.a

#


F77=/usr/bin/gfortran

FFLAGS=-g -ffixed-line-length-132
#FFLAGS=-m32 -g -ffixed-line-length-132

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
	$(RM) *.o $(LIB)
