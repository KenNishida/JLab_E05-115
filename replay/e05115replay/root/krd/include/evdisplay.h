
#ifndef krd_h
#define krd_h

void evdisplay();
void Loop();
void SetGeometry();
void InitGeometry();
void DrawGeometry(int hit_1X,int hit_AC1,int hit_AC2,
int hit_AC3,int hit_2X,int hit_WC1,int hit_WC2,
int N, float *X, float *Y, float *Z);
	
Double_t z_DC1=187.04-304.974;
Double_t z_DC2=287.04-304.974;
Double_t z_1X=0.;
Double_t z_AC1=347.191-304.974;
Double_t z_AC2=377.989-304.974;
Double_t z_AC3=408.787-304.974;
Double_t z_2X=454.974-304.974;
Double_t z_WC1=466.048-304.974;
Double_t z_WC2=476.945-304.974;

char DrawOption[5];
	
TGeoManager *HKSworld;  
TGeoVolume *HKSDets;
TGeoVolume *DC1 ;
TGeoVolume *DC2 ;
TGeoVolume *TOF1X[17];
TGeoVolume *AC1[7] ;
TGeoVolume *AC2[7] ;
TGeoVolume *AC3[7] ;
TGeoVolume *TOF2X[18] ;
TGeoVolume *WC1[12]; 
TGeoVolume *WC2[12]; 

#endif
