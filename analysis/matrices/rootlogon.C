{
	gROOT->ForceStyle(kTRUE);
	gStyle->SetPalette(1);
	gStyle->SetHistFillStyle(3002);
	gStyle->SetHistFillColor(40);
	gStyle->SetPadGridX(1);
	gStyle->SetPadGridY(1);
	gStyle->SetOptDate(1);
	
	TCut cut1 = "hsxpfp<2.5e-3*hsxfp+.15";
	TCut cut2 = "(haernpe1<5&&haernpe2<5)||(haernpe2<5&&haernpe3<5)||(haernpe1<5&&haernpe3<5)";
	//TCut cut3 = "hwatnkn1+hwatnkn2>1";
	TCut cut3 = "hwatnkn1+hwatnkn2>6*hmsq-1.5";
	TCut cut4 = "abs(hmsq-0.25)<.3 ";
	TCut cut5 = "abs(esypfp+0.03)<.03 && abs(esxpfp)<.2";
	TCut cut6 = "hsypfp<abs(4e-3*hsyfp+.02) && abs(hsypfp)<0.03";
	TCut cut7 = "etrkchi2/etrkndf<20";
	TCut cut8 = "abs(kypt)<0.06&&abs(eypt+0.07)<0.05&&abs(expt)<.15";
	TCut cut9 = "etheta>0.05 && runnum>=76825";
   TCut cutcoin[7];
	float cpeak[7];
	float cp=54.9;
	char str[100];
   for (int i=0;i<7;i++){
		cpeak[i]=cp-6+2*i;
		sprintf(str,"abs(ctime+%lf)<1",cpeak[i]);
		cutcoin[i]=str;
	}
	TCut cutKaon = cut1 && cut2 && cut3 && cut4 && cut5;
	TCut cutFP = cut6&&cut7&&cut8&&cut9;
	TCut cutacc = (cutcoin[0]||cutcoin[1]||cutcoin[2]||
	cutcoin[4]||cutcoin[5]||cutcoin[6]);
	TCut cutA = cutKaon && cutFP && cutcoin[3] ;
	TCut cutB = cut2 && cut3 && cut4 && cut5;
	TCut cutC = cutKaon && cutFP && cutacc ;
}
