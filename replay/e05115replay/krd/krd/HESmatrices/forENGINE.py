#!/usr/bin/python

def f2t():
	name_oFILE="hes_recon_coeff.dat"
	name_Mmom="mom6_27Aug10.dat"
	name_Mxpt="xpt6_27Aug10.dat"
	name_Mypt="ypt6_27Aug10.dat"
	name_Mlen="len6_27Aug10.dat"

	fMom=open(name_Mmom,"r")
	fXpt=open(name_Mxpt,"r")
	fYpt=open(name_Mypt,"r")
	fLen=open(name_Mlen,"r")

	f=open(name_oFILE,"w")
	f.write("!Nominal recon coefficients for HES\n")
	f.write("!\n")
	f.write("! focal plane rotation coeffs(4)\n")
	f.write("! detector offsets\n")
	f.write("! Z position of true focus\n")
	f.write("! recostruction matrix elements\n")
	f.write("! <theta y phi delta | nmpq>;(x**n xp**m y**p yp**q)\n")
	f.write("!\n")
	f.write("e_ang_slope_x   = -0.00\n")
	f.write("e_ang_slope_y   = -0.00\n")
	f.write("e_ang_offset_x  = 0.0\n")
	f.write("e_ang_offset_y  = -0.000\n")
	f.write("e_det_offset_x  = 0.00\n")
	f.write("e_det_offset_y  = 0.0\n")
	f.write("e_z_true_focus  = 0.0\n")
	f.write("Center_of_exf   = 0.0\n")
	f.write("Center_of_expf  = 0.0\n")
	f.write("Center_of_eyf   = 0.0\n")
	f.write("Center_of_eypf  = -0.03\n")
	f.write("Nfactor_of_exf  = 25.0\n")
	f.write("Nfactor_of_expf = 0.1\n")
	f.write("Nfactor_of_eyf  = 3.0\n")
	f.write("Nfactor_of_eypf = 0.01\n")

	f.write(" ---------------------------------------------------------------------\n")

	nPar=210	
	n=0
	while n<nPar:
		pMoms = fMom.readline()
		pMom = pMoms.split(' ')
		pXpts = fXpt.readline()
		pXpt = pXpts.split(' ')
		pYpts = fYpt.readline()
		pYpt = pYpts.split(' ')
		pLens = fLen.readline()
		pLen = pLens.split(' ')
		aaa = float(pMom[0])
		bbb = float(pXpt[0])
		ccc = float(pYpt[0])
		ddd = float(pLen[0])
		kkk = int(pMom[1])
		lll = int(pMom[2])
		mmm = int(pMom[3])
		nnn = int(pMom[4])
		f.write("%10lf %10lf %10lf %10lf %d %d %d %d\n"%(aaa,bbb,ccc,ddd,kkk,lll,mmm,nnn))
		n+=1

	f.write(" ---------------------------------------------------------------------\n")
	f.close()

def t2s():
	name_oFILE="hes_t2s_coeff.dat"
	name_Mxss="xss4_27Aug10.dat"
	name_Myss="yss4_27Aug10.dat"

	fXss=open(name_Mxss,"r")
	fYss=open(name_Myss,"r")

	f=open(name_oFILE,"w")
	f.write("!Nominal recon coefficients for HES\n")
	f.write("!\n")
	f.write("Center_of_expt  = 0.\n")
	f.write("Center_of_eypt  = -0.06\n")
	f.write("Center_of_emom  = 0.844\n")
	f.write("Nfactor_of_expt = 0.1\n")
	f.write("Nfactor_of_eypt = 0.03\n")
	f.write("Nfactor_of_emom = 0.2\n")

	f.write(" ---------------------------------------------------------------------\n")

	nPar=70	
	n=0
	while n<nPar:
		pXsss = fXss.readline()
		pXss = pXsss.split(' ')
		pYsss = fYss.readline()
		pYss = pYsss.split(' ')
		aaa = float(pXss[0])
		bbb = float(pYss[0])
		kkk = int(pXss[1])
		lll = int(pXss[2])
		mmm = int(pXss[3])
		f.write("%10lf %10lf %d %d %d\n"%(aaa,bbb,kkk,lll,mmm))
		n+=1

	f.write(" ---------------------------------------------------------------------\n")
	f.close()

f2t()
#t2s()
