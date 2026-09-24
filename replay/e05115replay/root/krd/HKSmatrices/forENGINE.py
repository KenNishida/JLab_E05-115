#!/usr/bin/python

def f2t():
	name_oFILE="hks_recon_coeff.dat"
	name_Mmom="mom6_22Sep10.dat"
	name_Mxpt="xpt6_22Sep10.dat"
	name_Mypt="ypt6_22Sep10.dat"
	name_Mlen="len6_22Sep10.dat"

	fMom=open(name_Mmom,"r")
	fXpt=open(name_Mxpt,"r")
	fYpt=open(name_Mypt,"r")
	fLen=open(name_Mlen,"r")

	f=open(name_oFILE,"w")
	f.write("!Nominal recon coefficients for HKS\n")
	f.write("!\n")
	f.write("! focal plane rotation coeffs(4)\n")
	f.write("! detector offsets\n")
	f.write("! Z position of true focus\n")
	f.write("! recostruction matrix elements\n")
	f.write("! <theta y phi delta | nmpq>;(x**n xp**m y**p yp**q)\n")
	f.write("!\n")
	f.write("h_ang_slope_x   = -0.00\n")
	f.write("h_ang_slope_y   = -0.00\n")
	f.write("h_ang_offset_x  = 0.0\n")
	f.write("h_ang_offset_y  = -0.000\n")
	f.write("h_det_offset_x  = 0.00\n")
	f.write("h_det_offset_y  = 0.0\n")
	f.write("h_z_true_focus  = 0.0\n")
	f.write("Center_of_hxf   = 0.0\n")
	f.write("Center_of_hxpf  = 0.0\n")
	f.write("Center_of_hyf   = 0.0\n")
	f.write("Center_of_hypf  = 0.0\n")
	f.write("Nfactor_of_hxf  = 60.0\n")
	f.write("Nfactor_of_hxpf = 0.15\n")
	f.write("Nfactor_of_hyf  = 6.0\n")
	f.write("Nfactor_of_hypf = 0.01\n")

	f.write(" ---------------------------------------------------------------------\n")

	nPar=70	
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
	name_oFILE="hks_t2s_coeff.dat"
	name_Mxss="xss4_22Sep10.dat"
	name_Myss="yss4_22Sep10.dat"

	fXss=open(name_Mxss,"r")
	fYss=open(name_Myss,"r")

	f=open(name_oFILE,"w")
	f.write("!Nominal recon coefficients for HKS\n")
	f.write("!\n")
	f.write("Center_of_hxpt  = 0.0\n")
	f.write("Center_of_hypt  = 0.0\n")
	f.write("Center_of_hmom  = 1.2\n")
	f.write("Nfactor_of_hxpt = 0.1\n")
	f.write("Nfactor_of_hypt = 0.05\n")
	f.write("Nfactor_of_hmom = 0.15\n")

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
t2s()
